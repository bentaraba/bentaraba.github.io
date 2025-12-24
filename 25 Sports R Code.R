install.packages('tidyverse')
library(tidyverse)
install.packages('dplyr')
library(dplyr)
install.packages('ggplot2')
library(ggplot2)

# read your Excel file
dat = RWInProbData1


# basic summary of the main WP variables
dat %>%
  summarise(
    n_plays           = n(),
    mean_TuProb       = mean(TuProb, na.rm = TRUE),
    mean_TuProbAdded  = mean(TuProbAdded, na.rm = TRUE),
    sd_TuProbAdded    = sd(TuProbAdded, na.rm = TRUE),
    mean_DriveProbAdd = mean(DriveProbAdded, na.rm = TRUE)
  )
dat <- dat %>%
  mutate(
    DistanceBucket = cut(
      DISTANCE,
      breaks = c(-Inf, 3, 6, 10, Inf),
      labels = c("1-3", "4-6", "7-10", "11+")
    ),
    FieldZone = case_when(
      FIELD_POSITION <= -40                ~ "Backed up (own 1-20)",
      FIELD_POSITION > -40 & FIELD_POSITION <= -20 ~ "Own 21-40",
      FIELD_POSITION > -20 & FIELD_POSITION < 20   ~ "Midfield",
      FIELD_POSITION >= 20 & FIELD_POSITION < 40   ~ "Opp 21-40",
      FIELD_POSITION >= 40                ~ "Red zone",
      TRUE                                ~ "Other"
    ),
    QUARTER = factor(QUARTER),
    DOWN    = factor(DOWN),
    TEMPO   = factor(TEMPO),
    MOTION  = factor(MOTION),
    FORMATION = factor(FORMATION)
  )


by_q_down <- dat %>%
  group_by(QUARTER, DOWN) %>%
  summarise(
    n_plays      = n(),
    mean_TuProb  = mean(TuProb, na.rm = TRUE),
    mean_WPA     = mean(TuProbAdded, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(QUARTER, DOWN)

by_q_down

#--------------------------------------------------------------------

by_q_down <- dat %>%
  group_by(QUARTER, DOWN) %>%
  summarise(
    n_plays      = n(),
    mean_TuProb  = mean(TuProb, na.rm = TRUE),
    mean_WPA     = mean(TuProbAdded, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(QUARTER, DOWN)

by_q_down


by_dist_zone <- dat %>%
  group_by(DistanceBucket, FieldZone) %>%
  summarise(
    n_plays  = n(),
    mean_WPA = mean(TuProbAdded, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(FieldZone, DistanceBucket)

by_dist_zone


ggplot(dat, aes(x = TuProbAdded)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of Play-level Win Probability Added (TU)",
    x = "TuProbAdded",
    y = "Count"
  )

#===
ggplot(dat, aes(x = TuProbAdded)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of Play-level Win Probability Added (TU)",
    x = "TuProbAdded",
    y = "Count"
  )


ggplot(
  dat %>%
    group_by(DOWN, DistanceBucket) %>%
    summarise(mean_WPA = mean(TuProbAdded, na.rm = TRUE), .groups = "drop"),
  aes(x = DistanceBucket, y = mean_WPA, group = DOWN)
) +
  geom_line() +
  geom_point() +
  facet_wrap(~DOWN) +
  labs(
    title = "Mean WPA by Down & Distance Bucket",
    x = "Distance Bucket",
    y = "Mean TuProbAdded"
  )

#----------------------------------------------------------------------------------
play_model_dat <- dat %>%
  filter(
    !is.na(TuProbAdded),
    !is.na(DOWN),
    !is.na(DISTANCE),
    !is.na(FIELD_POSITION),
    !is.na(QUARTER)
  )

model_situation <- lm(
  TuProbAdded ~ QUARTER + DOWN + scale(DISTANCE) + scale(FIELD_POSITION),
  data = play_model_dat
)

summary(model_situation)


play_model_dat2 <- play_model_dat %>%
  filter(!is.na(TEMPO), !is.na(MOTION), !is.na(FORMATION))

model_full <- lm(
  TuProbAdded ~ QUARTER + DOWN + DistanceBucket + FieldZone +
    TEMPO + MOTION + FORMATION,
  data = play_model_dat2
)

summary(model_full)



#----------------------------------------------------------------------------


# 1) Create FieldZone and clean data 

dat <- dat %>%
  mutate(
    FieldZone = case_when(
      FIELD_POSITION <= -40                       ~ "Backed up (own 1-20)",
      FIELD_POSITION > -40 & FIELD_POSITION <= -20 ~ "Own 21-40",
      FIELD_POSITION > -20 & FIELD_POSITION < 20   ~ "Midfield",
      FIELD_POSITION >= 20 & FIELD_POSITION < 40   ~ "Opp 21-40",
      FIELD_POSITION >= 40                        ~ "Red zone",
      TRUE                                        ~ NA_character_
    ),
    FieldZone = factor(FieldZone)
  )

# Keep only rows with WPA and FieldZone
play_mid <- dat %>%
  filter(!is.na(TuProbAdded), !is.na(FieldZone))

# 2) Make "Midfield" the reference level 

play_mid <- play_mid %>%
  mutate(FieldZone = relevel(FieldZone, ref = "Midfield"))

# 3) Regression: how much better is midfield vs other zones 

# Simple model: just field zone
mid_model <- lm(TuProbAdded ~ FieldZone, data = play_mid)
summary(mid_model)

# 4) Optional: control for down & distance as well 
mid_model_ctrl <- lm(
  TuProbAdded ~ FieldZone + DOWN + scale(DISTANCE),
  data = play_mid
)
summary(mid_model_ctrl)
#-------------------------------------------------------------------------------------------------------------------




set.seed(123)

# 1) Enter Temple spreads (Vegas-style: negative = Temple favored)
temple_spread <- c(
  9.5,    # @ Duke  (Temple +9.5)
  -13.5,   # vs LAFF (Temple -13.5)
  18,      # vs RUTG (Temple +18)
  -10,     # vs UMass
  18.5,    # @ Memphis
  23.5,    # @ UCF
  13.5,    # vs Tulsa
  14.5,    # @ Navy
  3.5,     # vs USF
  20,      # @ Houston
  17,      # vs Cincinnati
  9.5      # vs ECU
)

H_A <- c("A","H","H","H","A","A","H","A","H","A","H","H")  # just for the table

length(temple_spread)  # should be 12

# 2) Convert to "Temple line" used in your TU win-prob formula
#    line_TU > 0 means TU is expected to win by that many points (favored)
temple_line <- -temple_spread

# 3) Your pre-game win probability function
wp_from_line <- function(line) {
  sigma <- 18.4126
  pnorm((line + 0.5) / sigma)
}

game_wp <- wp_from_line(temple_line)

# See each game with implied win%
cbind(
  game        = 1:12,
  H_A         = H_A,
  Temple_spread = temple_spread,
  Temple_line   = round(temple_line, 1),
  win_prob      = round(game_wp, 3)
)

# 4) Monte Carlo: simulate many seasons ------------------------

n_sims  <- 10000
n_games <- length(game_wp)
season_wins <- numeric(n_sims)

for (s in 1:n_sims) {
  game_results <- rbinom(n_games, size = 1, prob = game_wp)
  season_wins[s] <- sum(game_results)
}

# 5) Summaries --------------------------------------------------

mean(season_wins)          # expected wins out of 12

table(season_wins)         # distribution of win totals

quantile(season_wins,
         c(0.1, 0.25, 0.5, 0.75, 0.9))

# Optional: histogram
hist(
  season_wins,
  breaks = -0.5:(n_games + 0.5),
  main = "Monte Carlo Distribution of TU Season Wins",
  xlab = "Wins in 12-game season"
)

# Probabilities of hitting certain thresholds
p_at_least_0  <- mean(season_wins == 0)   
p_at_least_1  <- mean(season_wins >= 1)
p_at_least_2  <- mean(season_wins >= 2)
p_at_least_3  <- mean(season_wins >= 3)
p_at_least_4  <- mean(season_wins >= 4)
p_at_least_5  <- mean(season_wins >= 5)
p_at_least_6  <- mean(season_wins >= 6)
p_at_least_7  <- mean(season_wins >= 7)
p_at_least_8  <- mean(season_wins >= 8)
p_at_least_9  <- mean(season_wins >= 9)
p_at_least_10 <- mean(season_wins >= 10)



prob_4_plus; prob_5_plus; prob_6_plus; prob_7_plus

prob_0_plus; prob_1_plus; prob_3_plus; prob_4_plus; prob_5_plus; prob_6_plus; prob_7_plus; prob_8_plus; prob_9_plus; prob_10_plus

p_at_least <- sapply(0:10, function(k) mean(season_wins >= k))
p_at_least

prob_6_plus <- mean(season_wins >= 6)

prob_6_plus 

p_at_least_0 











# Exact probability of each win total
p_exact <- prop.table(table(season_wins))

# Cumulative probability P(W >= k) for 0 through 12
p_at_least <- sapply(0:12, function(k) mean(season_wins >= k))

# Combine into a table
cum_table <- data.frame(
  Wins_At_Least = 0:12,
  Probability = round(p_at_least, 4)
)

cum_table

#_shit ass attemp to find play drive sig ___________________________________________________________________


drive_dat <- dat %>%
  group_by(WEEK, DRIVE) %>%
  summarise(
    DriveProbAdded = first(DriveProbAdded),
    PlaysInDrive   = n(),
    StartFieldPos  = first(DRIVE_START_FIELD_POSITION),
    EndFieldPos    = first(DRIVE_END_FIELD_POSITION),
    StartQuarter   = first(QUARTER),
    StartScoreDiff = first(SCORE_DIFFERENTIAL),
    TEMPO          = first(TEMPO),
    MOTION         = first(MOTION),
    FORMATION      = first(FORMATION),
    .groups = "drop"
  ) %>%
  mutate(
    # CLEAN FIELD POSITIONS (remove "+" and convert to numeric)
    StartFieldPos = as.numeric(gsub("\\+", "", as.character(StartFieldPos))),
    EndFieldPos   = as.numeric(gsub("\\+", "", as.character(EndFieldPos))),
    
    # CLEAN QUARTER
    StartQuarter = as.numeric(as.character(StartQuarter)),
    
    #fIX SCORE DIFF — convert to numeric
    StartScoreDiff = as.numeric(as.character(StartScoreDiff)),
    
    #FIX TEMPO — replace NA with 0, then convert to factor
    TEMPO = ifelse(is.na(TEMPO), 0, TEMPO),
    TEMPO = factor(TEMPO),
    
    # CLEAN categorical variables
    MOTION = factor(MOTION),
    FORMATION = factor(FORMATION),
    
    # SUCCESS FLAG
    Success = DriveProbAdded > 0
  )

# Inspect result
str(drive_dat)

lm_len2 <- lm(
  DRIVE_PLAY ~  RUN_PASS + TIME_OUTS_REMAINING_OFFENSE + DEF_PACKAGE ,
  data = dat
)

summary(lm_len2)


lm_len3 <- lm(
  DRIVE_PLAY ~  ,
  data = dat
)

summary(lm_len3)

dat$PENALTY_YARDS = as.numeric(dat$PENALTY_YARDS)






dat_model <- dat[complete.cases(dat[, c("DRIVE_PLAY", "RUN_PASS", "ROUTE_THROWN")]), ]

nrow(dat_model)   # how many rows survive?

table(dat_model$RUN_PASS, useNA = "ifany")
length(unique(dat_model$RUN_PASS))

table(dat_model$ROUTE_THROWN, useNA = "ifany")
length(unique(dat_model$ROUTE_THROWN))

