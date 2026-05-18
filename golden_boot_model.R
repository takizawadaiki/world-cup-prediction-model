# =============================================================================
# 2026 FIFA World Cup — Golden Boot Prediction Model v2
# Monte Carlo simulation: 10,000 full tournaments
#
# Key improvements over v1:
#   - Full game-by-game simulation (not just expected games average)
#   - Opponent strength penalises scoring (stronger opponent = fewer goals)
#   - Group difficulty built in via actual round-robin matchups
#   - Advancement tied directly to champion probability path
#   - Each game scored independently using Poisson with opponent-adjusted rate
# =============================================================================

library(dplyr)

set.seed(42)
N_SIM <- 10000

# -----------------------------------------------------------------------------
# SECTION 1: Team Elo ratings & groups
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
    "A","A","A","A","B","B","B","B","C","C","C","C",
    "D","D","D","D","E","E","E","E","F","F","F","F",
    "G","G","G","G","H","H","H","H","I","I","I","I",
    "J","J","J","J","K","K","K","K","L","L","L","L"
  ),
  rating = c(
    1858,1752,1726,1524, 1889,1784,1427,1594,
    1984,1821,1767,1532, 1721,1902,1833,1783,
    1923,1933,1676,1436, 1961,1904,1719,1636,
    1866,1760,1689,1585, 2165,1892,1568,1549,
    2082,1879,1912,1607, 2113,1827,1743,1690,
    1984,1975,1655,1727, 2020,1930,1505,1737
  ),
  stringsAsFactors = FALSE
)

rating_vec <- setNames(teams$rating, teams$team)
groups     <- c("A","B","C","D","E","F","G","H","I","J","K","L")
group_map  <- split(teams$team, teams$group)

# -----------------------------------------------------------------------------
# SECTION 2: Player data
# -----------------------------------------------------------------------------
players <- data.frame(
  player = c(
    "Kylian Mbappé",       "Harry Kane",          "Erling Haaland",
    "Lautaro Martínez",    "Julián Álvarez",       "Lionel Messi",
    "Cristiano Ronaldo",   "Vinicius Jr",          "Lamine Yamal",
    "Raphinha",            "Viktor Gyökeres",      "Alexander Isak",
    "Santiago Giménez",    "Raúl Jiménez",         "Patrik Schick",
    "Ermedin Demirovic",   "Cody Gakpo",           "Romelu Lukaku",
    "Loïs Openda"
  ),
  team = c(
    "France","England","Norway",
    "Argentina","Argentina","Argentina",
    "Portugal","Brazil","Spain",
    "Brazil","Sweden","Sweden",
    "Mexico","Mexico","Czechia",
    "Bosnia and Herzegovina","Netherlands","Belgium","Belgium"
  ),
  # Blended rate: 70% club (2025-26) + 30% international career (goals/90)
  # This is the BASE rate vs an average-strength opponent (Elo ~1800)
  base_rate = c(
    0.95,  # Mbappé: 38g/33 Real Madrid + 12 WC goals in 14 games, elite
    0.88,  # Kane: leading European Golden Shoe, goal every 66 mins Bundesliga
    0.72,  # Haaland: 22+ PL goals, solid but below Kane/Mbappe this season
    0.72,  # Lautaro: 18g+9a, 9 in 12 UCL, Copa America winner
    0.65,  # Álvarez: 18g all comps, excellent form at Atletico
    0.58,  # Messi: deeper role, set pieces, age 38 — still world class
    0.60,  # Ronaldo: 24g in 24 Saudi games (heavily discounted), penalty taker
    0.52,  # Vinicius: 16 La Liga/UCL goals but only 8 in 47 int caps
    0.58,  # Yamal: 33 Barca goals, winger/creator but clinical; hamstring risk
    0.52,  # Raphinha: good club form, wide role, Brazil pen taker
    0.55,  # Gyökeres: 39 Sporting goals last season; Arsenal struggles this term
    0.65,  # Isak: strong Liverpool form, good Sweden record
    0.55,  # Giménez: Milan, Mexico #1 striker
    0.42,  # Jiménez: veteran Fulham, rotation risk
    0.50,  # Schick: Leverkusen, clinical finisher
    0.52,  # Demirovic: Stuttgart, in-form this season
    0.50,  # Gakpo: Liverpool, decent Netherlands record
    0.48,  # Lukaku: veteran Belgium target man
    0.58   # Openda: Leipzig, strong season
  ),
  starter_prob = c(
    0.97, 0.96, 0.95,
    0.90, 0.75, 0.78,
    0.85, 0.92, 0.85,
    0.82, 0.90, 0.85,
    0.88, 0.68, 0.85,
    0.82, 0.88, 0.78, 0.82
  ),
  penalty_taker = c(
    TRUE,  TRUE,  TRUE,
    TRUE,  FALSE, FALSE,
    TRUE,  FALSE, FALSE,
    TRUE,  TRUE,  FALSE,
    TRUE,  FALSE, TRUE,
    TRUE,  FALSE, TRUE, FALSE
  ),
  # 0=fit, higher=more risk; reduces starter_prob and rate
  injury_risk = c(
    0.00, 0.00, 0.05,
    0.00, 0.00, 0.10,
    0.08, 0.05, 0.20,
    0.05, 0.05, 0.05,
    0.00, 0.12, 0.05,
    0.05, 0.05, 0.10, 0.05
  ),
  stringsAsFactors = FALSE
)

# Apply injury risk to starter_prob and base_rate
players <- players %>%
  mutate(
    eff_starter  = starter_prob * (1 - injury_risk * 0.4),
    eff_rate     = base_rate    * (1 - injury_risk * 0.5),
    pen_bonus    = ifelse(penalty_taker, 0.030, 0.0),
    adj_rate     = eff_rate + pen_bonus,
    mins_per_game = eff_starter * 85 + (1 - eff_starter) * 20
  )

# -----------------------------------------------------------------------------
# SECTION 3: Opponent strength adjustment
#
# Scoring rate is adjusted by the difficulty of the opponent:
# We use a logistic function of Elo difference.
# vs avg opponent (Elo 1800): multiplier = 1.0
# vs elite (Elo 2100+): multiplier ≈ 0.55
# vs weak (Elo 1500-): multiplier ≈ 1.55
# Formula: multiplier = exp(k * (avg_elo - opp_elo) / 400)
# k tuned so that a 300-point Elo difference gives ~35% change
# -----------------------------------------------------------------------------
AVG_ELO <- 1800
OPP_STRENGTH_K <- 0.50

opp_multiplier <- function(opp_elo) {
  exp(OPP_STRENGTH_K * (AVG_ELO - opp_elo) / 400)
}

# -----------------------------------------------------------------------------
# SECTION 4: Tournament simulation helpers
# (same structure as WC simulation — group stage + knockout)
# -----------------------------------------------------------------------------
elo_win_prob <- function(ra, rb) 1 / (1 + 10^((rb - ra) / 400))

simulate_match_result <- function(ra, rb) {
  pa   <- elo_win_prob(ra, rb)
  pd   <- 0.27 * exp(-abs(ra - rb) / 600)
  pb   <- max(0, 1 - pa - pd)
  pd   <- max(0, pd); pa <- max(0, 1 - pd - pb)
  r    <- runif(1)
  if (r < pa) "A" else if (r < pa + pd) "D" else "B"
}

simulate_group_stage <- function(g) {
  gt  <- group_map[[g]]
  pts <- setNames(rep(0, 4), gt)
  gd  <- setNames(rep(0.0, 4), gt)
  matchups <- list()
  for (i in 1:3) for (j in (i+1):4) {
    ta <- gt[i]; tb <- gt[j]
    res <- simulate_match_result(rating_vec[ta], rating_vec[tb])
    rd  <- abs(rating_vec[ta] - rating_vec[tb]) / 400
    if (res == "A") { pts[ta] <- pts[ta]+3; gd[ta] <- gd[ta]+rd; gd[tb] <- gd[tb]-rd
    } else if (res == "B") { pts[tb] <- pts[tb]+3; gd[tb] <- gd[tb]+rd; gd[ta] <- gd[ta]-rd
    } else { pts[ta] <- pts[ta]+1; pts[tb] <- pts[tb]+1 }
    matchups[[length(matchups)+1]] <- list(home=ta, away=tb, result=res)
  }
  standings <- gt[order(-pts[gt], -gd[gt], -rating_vec[gt])]
  list(standings=standings, pts=pts, gd=gd, matchups=matchups)
}

sim_ko <- function(ta, tb) {
  if (runif(1) < elo_win_prob(rating_vec[ta], rating_vec[tb])) ta else tb
}

# -----------------------------------------------------------------------------
# SECTION 5: Main Monte Carlo loop
# Each simulation runs a full tournament and tracks:
#   - which games each player played (and vs which opponent)
#   - goals scored per game using Poisson with opponent adjustment
# -----------------------------------------------------------------------------

# Accumulate goals per player across all sims
total_goals_matrix <- matrix(0, nrow=nrow(players), ncol=N_SIM)
rownames(total_goals_matrix) <- players$player

# Accumulate tournament stage reached (for stage probability output)
stage_counts <- matrix(0, nrow=nrow(players), ncol=6)
colnames(stage_counts) <- c("group","round32","round16","qf","sf","final")
rownames(stage_counts) <- players$player

score_goals <- function(player_row, opp_team) {
  # Goals scored by this player in one game vs opp_team
  opp_elo  <- rating_vec[opp_team]
  mult     <- opp_multiplier(opp_elo)
  rate_90  <- player_row$adj_rate * mult
  mins     <- player_row$mins_per_game
  lambda   <- rate_90 * (mins / 90)
  rpois(1, lambda)
}

cat(sprintf("Running %s tournament simulations...\n", format(N_SIM, big.mark=",")))

for (sim in 1:N_SIM) {

  sim_goals <- setNames(rep(0, nrow(players)), players$player)

  # ---- Group stage ----
  all_standings <- list()
  all_pts       <- list()
  all_gd        <- list()

  for (g in groups) {
    gs <- simulate_group_stage(g)
    all_standings[[g]] <- gs$standings
    all_pts[[g]]       <- gs$pts
    all_gd[[g]]        <- gs$gd

    gt <- group_map[[g]]
    # Round-robin: each team plays 3 opponents
    for (i in 1:3) for (j in (i+1):4) {
      ta <- gt[i]; tb <- gt[j]
      # Score goals for players on each team
      for (pi in which(players$team == ta)) {
        sim_goals[players$player[pi]] <- sim_goals[players$player[pi]] +
          score_goals(players[pi,], tb)
      }
      for (pi in which(players$team == tb)) {
        sim_goals[players$player[pi]] <- sim_goals[players$player[pi]] +
          score_goals(players[pi,], ta)
      }
    }
  }

  # Stage tracking: all players get group stage
  for (pi in seq_len(nrow(players))) {
    stage_counts[players$player[pi], "group"] <- stage_counts[players$player[pi], "group"] + 1
  }

  # ---- Best 8 third-place ----
  thirds <- do.call(rbind, lapply(groups, function(g) {
    s <- all_standings[[g]]
    data.frame(team=s[3], pts=all_pts[[g]][s[3]], gd=all_gd[[g]][s[3]],
               from_group=g, stringsAsFactors=FALSE)
  }))
  best8 <- thirds %>% arrange(desc(pts), desc(gd), desc(rating_vec[team])) %>%
    head(8) %>% arrange(match(from_group, groups))

  # ---- R32 bracket ----
  W  <- function(g) all_standings[[g]][1]
  RU <- function(g) all_standings[[g]][2]
  T3 <- function(i) best8$team[i]

  r32_pairs <- list(
    c(W("A"),T3(1)), c(W("B"),T3(2)), c(W("C"),T3(3)), c(W("D"),T3(4)),
    c(W("E"),T3(5)), c(W("F"),T3(6)), c(W("G"),T3(7)), c(W("H"),T3(8)),
    c(W("I"),RU("D")), c(W("J"),RU("C")), c(W("K"),RU("B")), c(W("L"),RU("A")),
    c(RU("E"),RU("F")), c(RU("G"),RU("H")), c(RU("I"),RU("J")), c(RU("K"),RU("L"))
  )

  play_ko_game <- function(ta, tb, stage_col) {
    winner <- sim_ko(ta, tb)
    loser  <- ifelse(winner == ta, tb, ta)
    # Score goals for players on both teams
    for (pi in which(players$team == ta)) {
      sim_goals[players$player[pi]] <<- sim_goals[players$player[pi]] +
        score_goals(players[pi,], tb)
    }
    for (pi in which(players$team == tb)) {
      sim_goals[players$player[pi]] <<- sim_goals[players$player[pi]] +
        score_goals(players[pi,], ta)
    }
    # Track stage reached
    for (pi in which(players$team %in% c(ta, tb))) {
      stage_counts[players$player[pi], stage_col] <<-
        stage_counts[players$player[pi], stage_col] + 1
    }
    winner
  }

  r32w <- sapply(r32_pairs, function(p) play_ko_game(p[1], p[2], "round32"))
  r16w <- sapply(1:8,  function(i) play_ko_game(r32w[2*i-1], r32w[2*i], "round16"))
  qfw  <- sapply(1:4,  function(i) play_ko_game(r16w[2*i-1], r16w[2*i], "qf"))
  sfw  <- sapply(1:2,  function(i) play_ko_game(qfw[2*i-1],  qfw[2*i],  "sf"))
  play_ko_game(sfw[1], sfw[2], "final")

  total_goals_matrix[, sim] <- sim_goals[players$player]
}

# -----------------------------------------------------------------------------
# SECTION 6: Results
# -----------------------------------------------------------------------------

# Golden Boot: most goals in sim; ties broken by random noise already in Poisson
gb_wins  <- rowSums(total_goals_matrix == apply(total_goals_matrix, 2, max))
gb_pct   <- round(gb_wins / N_SIM * 100, 2)
avg_g    <- round(rowMeans(total_goals_matrix), 2)
p5plus   <- round(rowMeans(total_goals_matrix >= 5) * 100, 1)
p7plus   <- round(rowMeans(total_goals_matrix >= 7) * 100, 1)
p90_g    <- round(apply(total_goals_matrix, 1, quantile, 0.90), 1)

results <- players %>%
  select(player, team, adj_rate, eff_starter, penalty_taker, injury_risk) %>%
  mutate(
    golden_boot_pct = gb_pct,
    avg_goals       = avg_g,
    p90_goals       = p90_g,
    p_5plus         = p5plus,
    p_7plus         = p7plus,
    pct_games_r32   = round(stage_counts[player, "round32"] / N_SIM * 100, 1),
    pct_games_final = round(stage_counts[player, "final"]   / N_SIM * 100, 1)
  ) %>%
  arrange(desc(golden_boot_pct))

# -----------------------------------------------------------------------------
# SECTION 7: Print
# -----------------------------------------------------------------------------
cat("\n=== 2026 WORLD CUP — GOLDEN BOOT PREDICTIONS (v2) ===\n")
cat(sprintf("Simulations: %s | Full game-by-game | Opponent-adjusted scoring\n\n",
            format(N_SIM, big.mark=",")))

cat(sprintf("%-22s %-24s %6s %6s %6s %7s %7s\n",
    "Player", "Team", "AvgG", "P(5+)", "P(7+)", "P(Final)", "GB%"))
cat(strrep("-", 82), "\n")

for (i in 1:nrow(results)) {
  r <- results[i,]
  cat(sprintf("%-22s %-24s %6.2f %5.1f%% %5.1f%% %6.1f%% %7.2f%%\n",
      r$player, r$team, r$avg_goals, r$p_5plus, r$p_7plus,
      r$pct_games_final, r$golden_boot_pct))
}

cat("\n--- Top 5 Deep Dive ---\n")
for (i in 1:5) {
  r <- results[i,]
  cat(sprintf(
    "\n%d. %s (%s)\n   Adj rate: %.3f g/90 | Starter: %.0f%% | Pen taker: %s | Injury risk: %.0f%%\n   Avg goals: %.2f | P(5+ goals): %.1f%% | P(Final): %.1f%% | Golden Boot: %.2f%%\n",
    i, r$player, r$team,
    r$adj_rate, r$eff_starter*100,
    ifelse(r$penalty_taker,"Yes","No"), r$injury_risk*100,
    r$avg_goals, r$p_5plus, r$pct_games_final, r$golden_boot_pct))
}

# -----------------------------------------------------------------------------
# SECTION 8: Save CSV
# -----------------------------------------------------------------------------
output_file <- "Golden_Boot_Predictions.csv"
results %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  write.csv(output_file, row.names = FALSE)
cat(sprintf("\nResults saved to: %s\n", output_file))

cat("\n--- Model Notes ---\n")
cat("- Opponent strength multiplier: exp(0.5 * (1800 - opp_elo) / 400)\n")
cat("  e.g. vs Spain (2165): ~0.65x scoring rate | vs Haiti (1532): ~1.45x\n")
cat("- Goals drawn game-by-game from Poisson(adj_rate * opp_mult * mins/90)\n")
cat("- Advancement path fully simulated each tournament (not averaged)\n")
cat("- Squads not yet finalised — final lists due June 1-2, 2026\n")
cat("- Yamal hamstring: 20% injury risk applied to starter prob and rate\n")
cat("- Vinicius int rate (8 goals/47 caps) significantly drags his projection\n")
