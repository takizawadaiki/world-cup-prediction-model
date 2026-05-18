# =============================================================================
# 2026 FIFA World Cup Monte Carlo Simulation — Full 32-team bracket
# 48 teams, 12 groups, top 2 + best 8 third-place teams advance
# Based on World Football Elo Ratings (eloratings.net)
# =============================================================================

library(dplyr)

set.seed(42)
N_SIM <- 10000

# -----------------------------------------------------------------------------
# Team data
# -----------------------------------------------------------------------------
teams <- data.frame(
  team = c(
    "Mexico","South Korea","Czechia","South Africa",
    "Switzerland","Canada","Qatar","Bosnia and Herzegovina",
    "Brazil","Morocco","Scotland","Haiti",
    "USA","Turkiye","Paraguay","Australia",
    "Germany","Ecuador","Ivory Coast","Curacao",
    "Netherlands","Japan","Sweden","Tunisia",
    "Belgium","Iran","Egypt","New Zealand",
    "Spain","Uruguay","Saudi Arabia","Cape Verde",
    "France","Senegal","Norway","Iraq",
    "Argentina","Austria","Algeria","Jordan",
    "Portugal","Colombia","Congo DR","Uzbekistan",
    "England","Croatia","Ghana","Panama"
  ),
  group = c(
    "A","A","A","A",
    "B","B","B","B",
    "C","C","C","C",
    "D","D","D","D",
    "E","E","E","E",
    "F","F","F","F",
    "G","G","G","G",
    "H","H","H","H",
    "I","I","I","I",
    "J","J","J","J",
    "K","K","K","K",
    "L","L","L","L"
  ),
  rating = c(
    1858, 1752, 1726, 1524,
    1889, 1784, 1427, 1594,
    1984, 1821, 1767, 1532,
    1721, 1902, 1833, 1783,
    1923, 1933, 1676, 1436,
    1961, 1904, 1719, 1636,
    1866, 1760, 1689, 1585,
    2165, 1892, 1568, 1549,
    2082, 1879, 1912, 1607,
    2113, 1827, 1743, 1690,
    1984, 1975, 1655, 1727,
    2020, 1930, 1505, 1737
  ),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# Core functions
# -----------------------------------------------------------------------------

elo_win_prob <- function(rating_a, rating_b) {
  1 / (1 + 10^((rating_b - rating_a) / 400))
}

# Returns "A" (team A wins), "B" (team B wins), or "D" (draw)
simulate_match <- function(rating_a, rating_b) {
  p_a_win <- elo_win_prob(rating_a, rating_b)
  rating_diff <- abs(rating_a - rating_b)
  p_draw  <- 0.27 * exp(-rating_diff / 600)
  p_b_win <- max(0, 1 - p_a_win - p_draw)
  p_draw  <- max(0, p_draw)
  p_a_win <- max(0, 1 - p_draw - p_b_win)

  r <- runif(1)
  if (r < p_a_win) return("A")
  if (r < p_a_win + p_draw) return("D")
  return("B")
}

# Full round-robin group simulation; returns standings data frame
simulate_group <- function(group_teams) {
  n   <- nrow(group_teams)
  pts <- setNames(rep(0, n), group_teams$team)
  gd  <- setNames(rep(0.0, n), group_teams$team)

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      result <- simulate_match(group_teams$rating[i], group_teams$rating[j])
      rd <- abs(group_teams$rating[i] - group_teams$rating[j]) / 400
      if (result == "A") {
        pts[group_teams$team[i]] <- pts[group_teams$team[i]] + 3
        gd[group_teams$team[i]]  <- gd[group_teams$team[i]]  + rd
        gd[group_teams$team[j]]  <- gd[group_teams$team[j]]  - rd
      } else if (result == "B") {
        pts[group_teams$team[j]] <- pts[group_teams$team[j]] + 3
        gd[group_teams$team[j]]  <- gd[group_teams$team[j]]  + rd
        gd[group_teams$team[i]]  <- gd[group_teams$team[i]]  - rd
      } else {
        pts[group_teams$team[i]] <- pts[group_teams$team[i]] + 1
        pts[group_teams$team[j]] <- pts[group_teams$team[j]] + 1
      }
    }
  }

  group_teams %>%
    mutate(points = pts[team], goal_diff = gd[team]) %>%
    arrange(desc(points), desc(goal_diff), desc(rating))
}

# Knockout match — no draws (penalty shootout if needed)
simulate_ko_match <- function(team_a, team_b, ratings) {
  p_a <- elo_win_prob(ratings[team_a], ratings[team_b])
  ifelse(runif(1) < p_a, team_a, team_b)
}

# -----------------------------------------------------------------------------
# Accumulators
# -----------------------------------------------------------------------------
stats <- teams %>%
  select(team, group, rating) %>%
  mutate(
    group_winner  = 0,
    advance_group = 0,
    round32       = 0,
    round16       = 0,
    quarterfinal  = 0,
    semifinal     = 0,
    final         = 0,
    champion      = 0
  )
rownames(stats) <- stats$team

rating_vec <- setNames(teams$rating, teams$team)
groups     <- c("A","B","C","D","E","F","G","H","I","J","K","L")

# -----------------------------------------------------------------------------
# Simulation loop
# -----------------------------------------------------------------------------
cat(sprintf("Running %s simulations...\n", format(N_SIM, big.mark = ",")))

for (sim in 1:N_SIM) {

  # ---- Group stage ----
  all_standings <- list()

  for (g in groups) {
    gt <- teams %>% filter(group == g)
    standings <- simulate_group(gt)
    all_standings[[g]] <- standings

    stats[standings$team[1], "group_winner"]  <- stats[standings$team[1], "group_winner"]  + 1
    stats[standings$team[1], "advance_group"] <- stats[standings$team[1], "advance_group"] + 1
    stats[standings$team[2], "advance_group"] <- stats[standings$team[2], "advance_group"] + 1
  }

  # ---- Best 8 third-place teams ----
  third_place <- do.call(rbind, lapply(groups, function(g) {
    s <- all_standings[[g]]
    data.frame(
      team      = s$team[3],
      rating    = s$rating[3],
      points    = s$points[3],
      goal_diff = s$goal_diff[3],
      from_group = g,
      stringsAsFactors = FALSE
    )
  }))

  best8_third <- third_place %>%
    arrange(desc(points), desc(goal_diff), desc(rating)) %>%
    head(8) %>%
    arrange(match(from_group, groups))  # re-sort by group order for bracket seeding

  for (t in best8_third$team) {
    stats[t, "advance_group"] <- stats[t, "advance_group"] + 1
  }

  # ---- Shortcuts ----
  W <- function(g) all_standings[[g]]$team[1]   # group winner
  RU <- function(g) all_standings[[g]]$team[2]  # runner-up
  T3 <- function(i) best8_third$team[i]         # i-th best third-place (by group order)

  # ---- Round of 32 (16 matches) ----
  # M1-M8:  Group winners A-H vs the 8 best third-place teams (seeded by group order)
  # M9-M12: Group winners I-L vs runners-up of D,C,B,A
  # M13-M16: Cross runners-up matches E/F, G/H, I/J, K/L
  r32_pairs <- list(
    c(W("A"), T3(1)), c(W("B"), T3(2)), c(W("C"), T3(3)), c(W("D"), T3(4)),
    c(W("E"), T3(5)), c(W("F"), T3(6)), c(W("G"), T3(7)), c(W("H"), T3(8)),
    c(W("I"), RU("D")), c(W("J"), RU("C")), c(W("K"), RU("B")), c(W("L"), RU("A")),
    c(RU("E"), RU("F")), c(RU("G"), RU("H")), c(RU("I"), RU("J")), c(RU("K"), RU("L"))
  )

  r32_winners <- character(16)
  for (i in seq_along(r32_pairs)) {
    ta <- r32_pairs[[i]][1]; tb <- r32_pairs[[i]][2]
    stats[ta, "round32"] <- stats[ta, "round32"] + 1
    stats[tb, "round32"] <- stats[tb, "round32"] + 1
    r32_winners[i] <- simulate_ko_match(ta, tb, rating_vec)
  }

  # ---- Round of 16 (8 matches) ----
  r16_winners <- character(8)
  for (i in 1:8) {
    ta <- r32_winners[2*i - 1]; tb <- r32_winners[2*i]
    stats[ta, "round16"] <- stats[ta, "round16"] + 1
    stats[tb, "round16"] <- stats[tb, "round16"] + 1
    r16_winners[i] <- simulate_ko_match(ta, tb, rating_vec)
  }

  # ---- Quarterfinals (4 matches) ----
  qf_winners <- character(4)
  for (i in 1:4) {
    ta <- r16_winners[2*i - 1]; tb <- r16_winners[2*i]
    stats[ta, "quarterfinal"] <- stats[ta, "quarterfinal"] + 1
    stats[tb, "quarterfinal"] <- stats[tb, "quarterfinal"] + 1
    qf_winners[i] <- simulate_ko_match(ta, tb, rating_vec)
  }

  # ---- Semifinals (2 matches) ----
  sf_winners <- character(2)
  for (i in 1:2) {
    ta <- qf_winners[2*i - 1]; tb <- qf_winners[2*i]
    stats[ta, "semifinal"] <- stats[ta, "semifinal"] + 1
    stats[tb, "semifinal"] <- stats[tb, "semifinal"] + 1
    sf_winners[i] <- simulate_ko_match(ta, tb, rating_vec)
  }

  # ---- Final ----
  stats[sf_winners[1], "final"] <- stats[sf_winners[1], "final"] + 1
  stats[sf_winners[2], "final"] <- stats[sf_winners[2], "final"] + 1
  champion <- simulate_ko_match(sf_winners[1], sf_winners[2], rating_vec)
  stats[champion, "champion"] <- stats[champion, "champion"] + 1
}

# -----------------------------------------------------------------------------
# Convert counts to percentages
# -----------------------------------------------------------------------------
prob_cols <- c("group_winner","advance_group","round32","round16",
               "quarterfinal","semifinal","final","champion")

results <- stats %>%
  mutate(across(all_of(prob_cols), ~ round(. / N_SIM * 100, 2))) %>%
  arrange(group, desc(rating))

# -----------------------------------------------------------------------------
# Print results
# -----------------------------------------------------------------------------
cat("\n=== 2026 WORLD CUP SIMULATION RESULTS ===\n")
cat(sprintf("Simulations: %s | 48 teams | 12 groups | Full 32-team knockout\n\n",
            format(N_SIM, big.mark = ",")))

for (g in groups) {
  cat(sprintf("--- Group %s ---\n", g))
  grp <- results %>% filter(group == g)
  for (i in 1:nrow(grp)) {
    cat(sprintf("  %-28s Elo: %4d | Adv: %5.1f%% | R32: %5.1f%% | R16: %5.1f%% | Champion: %5.2f%%\n",
                grp$team[i], grp$rating[i],
                grp$advance_group[i], grp$round32[i],
                grp$round16[i], grp$champion[i]))
  }
  cat("\n")
}

cat("--- Top 10 Champion Probabilities ---\n")
top10 <- results %>% arrange(desc(champion)) %>% head(10)
for (i in 1:nrow(top10)) {
  cat(sprintf("  %2d. %-28s Elo: %4d  Champion: %5.2f%%\n",
              i, top10$team[i], top10$rating[i], top10$champion[i]))
}

# -----------------------------------------------------------------------------
# Save CSV
# -----------------------------------------------------------------------------
output_file <- "World_Cup_Prediction_simulated.csv"

results %>%
  mutate(across(all_of(prob_cols), ~ paste0(., "%"))) %>%
  write.csv(output_file, row.names = FALSE)

cat(sprintf("\nResults saved to: %s\n", output_file))
