# libraries
libs <- c(
  "dplyr", "zoo", "rsample", "lme4", "tidyverse",
  "nflfastR", "randomForest", "glmnet", "ggplot2",
  "lmerTest", "caret", "ggrepel", "nflplotR", "stringr",
  "nflreadr", "tictoc", "leaps", "tidymodels"
)
invisible(lapply(libs, library, character.only = TRUE))

# mark mandatory form fields
mand_roster <- c(
  "myRosterQB",
  "myRosterRB1",
  "myRosterWR1",
  "myRosterTE",
  "myRosterK"
)

# marking with a red asterisk
star_mand <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
appCSS <- ".mandatory_star { color: red; }"

# all roster positions
full_roster <- c(
  "myRosterQB",
  "myRosterRB1",
  "myRosterRB2",
  "myRosterWR1",
  "myRosterWR2",
  "myRosterTE",
  "myRosterFlex",
  "myRosterDST",
  "myRosterK"
)

# time - used in save
epoch_time <- function() {
  as.integer(Sys.time())
}
human_time <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# user saved roster
saved_roster <- "responses"

# define roster candidates from nflreadr, by position
nfl_roster <- nflreadr::load_rosters()
qb_choices <- nfl_roster[nfl_roster$position == 'QB', "full_name"]
rb_choices <- nfl_roster[nfl_roster$position == 'RB', "full_name"]
wr_choices <- nfl_roster[nfl_roster$position == 'WR', "full_name"]
te_choices <- nfl_roster[nfl_roster$position == 'TE', "full_name"]
flex_choices <- nfl_roster[["full_name"]]
dst_choices <- nfl_roster %>%
  filter(position %in% c("LS", "DL", "P", "DB", "LB")) %>%
  select(full_name)
k_choices <- nfl_roster[nfl_roster$position == 'K', "full_name"]














###########################################################################
###########################################################################
########## WEEKLY STAT PREDICTOR ##########################################
###########################################################################
###########################################################################

# load stats from nflreadr: train 2020-2022, test 2023
pbp_stats <- nflreadr::load_player_stats(2020:2022)
def_stats <- nflreadr::load_pbp(2020:2022)
pbp_stats_test <- nflreadr::load_player_stats(2023)
def_stats_test <- nflreadr::load_pbp(2023)
duplicates <- duplicated(pbp_stats$player_id)
unique_stats <- pbp_stats[!duplicates, ]

# Define variable player_short to access tables without full names
build_player_mapping <- function() {
  name_mapping <- data.frame(
    Full_name = unique_stats[["player_display_name"]],
    Short_name = unique_stats[["player_name"]]
  )
  #return(name_mapping)
}


# defensive stats allowed - teams
# aggregating defensive statistics
def_stats_allowed <- def_stats %>%
  filter(!is.na(posteam)) %>%
  select(
    game_id,
    season,
    week,
    posteam,
    defteam,
    passing_yards,
    rushing_yards,
    success,
    rush_touchdown,
    pass_touchdown,
    interception,
    fumble_lost,
    sack
  ) %>%
  replace(is.na(.), 0) %>%
  group_by(game_id, season, week, posteam, defteam) %>%
  summarize(
    total_rushing_yards_allowed = sum(rushing_yards),
    success_rate_against = mean(success),
    total_rush_tds_allowed = sum(rush_touchdown),
    total_passing_yards_allowed = sum(passing_yards),
    total_pass_tds_allowed = sum(pass_touchdown),
    interceptions_forced = sum(interception),
    fumbles_forced = sum(fumble_lost),
    sacks_forced = sum(sack)
  ) %>%
  ungroup()
def_stats_allowed_test <- def_stats_test %>%
  filter(!is.na(posteam)) %>%
  select(
    game_id,
    season,
    week, posteam,
    defteam,
    passing_yards,
    rushing_yards,
    success,
    rush_touchdown,
    pass_touchdown,
    interception,
    fumble_lost,
    sack
  ) %>%
  replace(is.na(.), 0) %>%
  group_by(game_id, season, week, posteam, defteam) %>%
  summarize(
    total_rushing_yards_allowed = sum(rushing_yards),
    success_rate_against = mean(success),
    total_rush_tds_allowed = sum(rush_touchdown),
    total_passing_yards_allowed = sum(passing_yards),
    total_pass_tds_allowed = sum(pass_touchdown),
    interceptions_forced = sum(interception),
    fumbles_forced = sum(fumble_lost),
    sacks_forced = sum(sack)
  ) %>%
  ungroup()

# individual stats based on play-by-play data
weekly_stats <- pbp_stats %>%
  select(
    -c(headshot_url, pacr, dakota, racr, opponent_team, position_group)
  ) %>%
  replace(is.na(.), 0) %>%
  mutate(
    fan_points_half_ppr = (
      (.04 * passing_yards) +
        (4 * passing_tds) +
        (-2 * interceptions) +
        (2 * passing_2pt_conversions) +
        (.1 * rushing_yards) +
        (6 * rushing_tds) +
        (-2 * rushing_fumbles_lost) +
        (2 * rushing_2pt_conversions) +
        (.1 * receiving_yards) +
        (.5 * receptions) +
        (6 * receiving_tds) +
        (-2 * receiving_fumbles_lost) +
        (2 * receiving_2pt_conversions) +
        (-2 * sack_fumbles_lost)
    ),
    posteam = recent_team
  ) %>%
  select(-c(recent_team))
weekly_stats_test <- pbp_stats_test %>%
  select(
    -c(
      headshot_url,
      pacr,
      dakota,
      racr,
      opponent_team,
      position_group
    )
  ) %>%
  replace(is.na(.), 0) %>%
  mutate(
    fan_points_half_ppr = (
      (.04 * passing_yards) +
        (4 * passing_tds) +
        (-2 * interceptions) +
        (2 * passing_2pt_conversions) +
        (.1 * rushing_yards) +
        (6 * rushing_tds) +
        (-2 * rushing_fumbles_lost) +
        (2 * rushing_2pt_conversions) +
        (.1 * receiving_yards) +
        (.5 * receptions) +
        (6 * receiving_tds) +
        (-2 * receiving_fumbles_lost) +
        (2 * receiving_2pt_conversions) +
        (-2 * sack_fumbles_lost)
    ),
    posteam = recent_team
  ) %>%
  select(-c(recent_team))

# seperate individual stats by position
positions <- c("QB", "RB", "WR", "TE")

merged_stats <- left_join(
  weekly_stats,
  def_stats_allowed,
  by = c("season", "week", "posteam")
) %>%
  filter(position %in% positions) %>%
  select(-c(player_display_name, season, season_type, wopr, special_teams_tds))

merged_stats_test <- left_join(
  weekly_stats_test,
  def_stats_allowed_test,
  by = c("season", "week", "posteam")
) %>%
  filter(position %in% positions) %>%
  select(-c(player_display_name, season, season_type, wopr, special_teams_tds))

weekly_stats_test_qb <- weekly_stats_test %>%
  filter(position == "QB") %>%
  mutate(player_team = posteam) %>%
  mutate(max_week = max(week)) %>%
  group_by(player_id) %>%
  reframe(
    player_name = player_name,
    posteam = posteam,
    player_team = player_team,
    position = position,
    week = max_week,
    completions = mean(completions),
    attempts = mean(attempts),
    passing_yards = mean(passing_yards),
    passing_tds = mean(passing_tds),
    interceptions = mean(interceptions),
    sacks = mean(sacks),
    sack_yards = mean(sack_yards),
    sack_fumbles = mean(sack_fumbles),
    sack_fumbles_lost = mean(sack_fumbles_lost),
    passing_air_yards = mean(passing_air_yards),
    passing_yards_after_catch = mean(passing_yards_after_catch),
    passing_first_downs = mean(passing_first_downs),
    passing_epa = mean(passing_epa),
    passing_2pt_conversions = mean(passing_2pt_conversions),
    carries = mean(carries),
    rushing_yards = mean(rushing_yards),
    rushing_tds = mean(rushing_tds),
    rushing_fumbles = mean(rushing_fumbles),
    rushing_fumbles_lost = mean(rushing_fumbles_lost),
    rushing_first_downs = mean(rushing_first_downs),
    rushing_epa = mean(rushing_epa),
    rushing_2pt_conversions = mean(rushing_2pt_conversions),
    receptions = mean(receptions),
    targets = mean(targets),
    receiving_yards = mean(receiving_yards),
    receiving_tds = mean(receiving_tds),
    receiving_fumbles = mean(receiving_fumbles),
    receiving_fumbles_lost = mean(receiving_fumbles_lost),
    receiving_air_yards = mean(receiving_air_yards),
    receiving_yards_after_catch = mean(receiving_yards_after_catch),
    receiving_first_downs = mean(receiving_first_downs),
    receiving_epa = mean(receiving_epa),
    target_share = mean(target_share),
    air_yards_share = mean(air_yards_share),
    receiving_2pt_conversions = mean(receiving_2pt_conversions)
  ) %>%
  distinct()
weekly_stats_test_rb <- weekly_stats_test %>%
  filter(position == "RB") %>%
  mutate(player_team = posteam) %>%
  mutate(max_week = max(week)) %>%
  group_by(player_id) %>%
  reframe(
    player_name = player_name,
    posteam = posteam,
    player_team = player_team,
    position = position,
    week = max_week,
    completions = mean(completions),
    attempts = mean(attempts),
    passing_yards = mean(passing_yards),
    passing_tds = mean(passing_tds),
    interceptions = mean(interceptions),
    sacks = mean(sacks), sack_yards = mean(sack_yards),
    sack_fumbles = mean(sack_fumbles),
    sack_fumbles_lost = mean(sack_fumbles_lost),
    passing_air_yards = mean(passing_air_yards),
    passing_yards_after_catch = mean(passing_yards_after_catch),
    passing_first_downs = mean(passing_first_downs),
    passing_epa = mean(passing_epa),
    passing_2pt_conversions = mean(passing_2pt_conversions),
    carries = mean(carries),
    rushing_yards = mean(rushing_yards),
    rushing_tds = mean(rushing_tds),
    rushing_fumbles = mean(rushing_fumbles),
    rushing_fumbles_lost = mean(rushing_fumbles_lost),
    rushing_first_downs = mean(rushing_first_downs),
    rushing_epa = mean(rushing_epa),
    rushing_2pt_conversions = mean(rushing_2pt_conversions),
    receptions = mean(receptions),
    targets = mean(targets),
    receiving_yards = mean(receiving_yards),
    receiving_tds = mean(receiving_tds),
    receiving_fumbles = mean(receiving_fumbles),
    receiving_fumbles_lost = mean(receiving_fumbles_lost),
    receiving_air_yards = mean(receiving_air_yards),
    receiving_yards_after_catch = mean(receiving_yards_after_catch),
    receiving_first_downs = mean(receiving_first_downs),
    receiving_epa = mean(receiving_epa),
    target_share = mean(target_share),
    air_yards_share = mean(air_yards_share),
    receiving_2pt_conversions = mean(receiving_2pt_conversions)
  ) %>%
  distinct()
weekly_stats_test_wr <- weekly_stats_test %>%
  filter(position == "WR") %>%
  mutate(player_team = posteam) %>%
  mutate(max_week = max(week)) %>%
  group_by(player_id) %>%
  reframe(
    player_name = player_name,
    posteam = posteam,
    player_team = player_team,
    position = position,
    week = max_week,
    completions = mean(completions),
    attempts = mean(attempts),
    passing_yards = mean(passing_yards),
    passing_tds = mean(passing_tds),
    interceptions = mean(interceptions),
    sacks = mean(sacks),
    sack_yards = mean(sack_yards),
    sack_fumbles = mean(sack_fumbles),
    sack_fumbles_lost = mean(sack_fumbles_lost),
    passing_air_yards = mean(passing_air_yards),
    passing_yards_after_catch = mean(passing_yards_after_catch),
    passing_first_downs = mean(passing_first_downs),
    passing_epa = mean(passing_epa),
    passing_2pt_conversions = mean(passing_2pt_conversions),
    carries = mean(carries),
    rushing_yards = mean(rushing_yards),
    rushing_tds = mean(rushing_tds),
    rushing_fumbles = mean(rushing_fumbles),
    rushing_fumbles_lost = mean(rushing_fumbles_lost),
    rushing_first_downs = mean(rushing_first_downs),
    rushing_epa = mean(rushing_epa),
    rushing_2pt_conversions = mean(rushing_2pt_conversions),
    receptions = mean(receptions),
    targets = mean(targets),
    receiving_yards = mean(receiving_yards),
    receiving_tds = mean(receiving_tds),
    receiving_fumbles = mean(receiving_fumbles),
    receiving_fumbles_lost = mean(receiving_fumbles_lost),
    receiving_air_yards = mean(receiving_air_yards),
    receiving_yards_after_catch = mean(receiving_yards_after_catch),
    receiving_first_downs = mean(receiving_first_downs),
    receiving_epa = mean(receiving_epa),
    target_share = mean(target_share),
    air_yards_share = mean(air_yards_share),
    receiving_2pt_conversions = mean(receiving_2pt_conversions)
  ) %>%
  distinct()
weekly_stats_test_te <- weekly_stats_test %>%
  filter(position == "TE") %>%
  mutate(player_team = posteam) %>%
  mutate(max_week = max(week)) %>%
  group_by(player_id) %>%
  reframe(
    player_name = player_name,
    posteam = posteam,
    player_team = player_team,
    position = position,
    week = max_week,
    completions = mean(completions), attempts = mean(attempts),
    passing_yards = mean(passing_yards),
    passing_tds = mean(passing_tds),
    interceptions = mean(interceptions),
    sacks = mean(sacks),
    sack_yards = mean(sack_yards),
    sack_fumbles = mean(sack_fumbles),
    sack_fumbles_lost = mean(sack_fumbles_lost),
    passing_air_yards = mean(passing_air_yards),
    passing_yards_after_catch = mean(passing_yards_after_catch),
    passing_first_downs = mean(passing_first_downs),
    passing_epa = mean(passing_epa),
    passing_2pt_conversions = mean(passing_2pt_conversions),
    carries = mean(carries),
    rushing_yards = mean(rushing_yards),
    rushing_tds = mean(rushing_tds),
    rushing_fumbles = mean(rushing_fumbles),
    rushing_fumbles_lost = mean(rushing_fumbles_lost),
    rushing_first_downs = mean(rushing_first_downs),
    rushing_epa = mean(rushing_epa),
    rushing_2pt_conversions = mean(rushing_2pt_conversions),
    receptions = mean(receptions),
    targets = mean(targets),
    receiving_yards = mean(receiving_yards),
    receiving_tds = mean(receiving_tds),
    receiving_fumbles = mean(receiving_fumbles),
    receiving_fumbles_lost = mean(receiving_fumbles_lost),
    receiving_air_yards = mean(receiving_air_yards),
    receiving_yards_after_catch = mean(receiving_yards_after_catch),
    receiving_first_downs = mean(receiving_first_downs),
    receiving_epa = mean(receiving_epa),
    target_share = mean(target_share),
    air_yards_share = mean(air_yards_share),
    receiving_2pt_conversions = mean(receiving_2pt_conversions)
  ) %>%
  distinct()
def_stats_allowed_test <- def_stats_allowed_test %>%
  mutate(max_week = max(week)) %>%
  group_by(defteam) %>%
  reframe(
    week = max_week,
    total_rushing_yards_allowed = mean(total_rushing_yards_allowed),
    total_rush_tds_allowed = mean(total_rush_tds_allowed),
    total_passing_yards_allowed = mean(total_passing_yards_allowed),
    total_pass_tds_allowed = mean(total_pass_tds_allowed),
    success_rate_against = mean(success_rate_against),
    interceptions_forced = mean(interceptions_forced),
    sacks_forced = mean(sacks_forced),
    fumbles_forced = mean(fumbles_forced)
  ) %>%
  distinct()

# loading team schedules
sched <- nflreadr::load_schedules(2023)
sched <- sched %>%
  filter(week == def_stats_allowed_test$week) %>%
  select(game_id, week, home_team, away_team)

# add to weekly stats, joining on player's team as away team
result_home_qb <- sched %>%
  left_join(weekly_stats_test_qb, by = c("home_team" = "posteam"))
result_away_qb <- sched %>%
  left_join(weekly_stats_test_qb, by = c("away_team" = "posteam"))
final_result_qb <- bind_rows(result_home_qb, result_away_qb) %>%
  mutate(
    posteam = ifelse(
      player_team == home_team,
      home_team,
      ifelse(
        player_team == away_team,
        away_team,
        NA
      )
    )
  ) %>%
  mutate(
    defteam = ifelse(
      player_team == home_team,
      away_team,
      ifelse(
        player_team == away_team,
        home_team,
        NA
      )
    )
  ) %>%
  select(-week.x, -week.y, -home_team, -away_team, -game_id)
final_result_qb <- final_result_qb %>%
  left_join(def_stats_allowed_test, by = c("defteam" = "defteam"))

###################################################################
result_home_rb <- sched %>%
  left_join(weekly_stats_test_rb, by = c("home_team" = "posteam"))
result_away_rb <- sched %>%
  left_join(weekly_stats_test_rb, by = c("away_team" = "posteam"))
final_result_rb <- bind_rows(result_home_rb, result_away_rb) %>%
  mutate(
    posteam = ifelse(
      player_team == home_team,
      home_team,
      ifelse(
        player_team == away_team,
        away_team,
        NA
      )
    )
  ) %>%
  mutate(
    defteam = ifelse(
      player_team == home_team,
      away_team,
      ifelse(
        player_team == away_team,
        home_team,
        NA
      )
    )
  ) %>%
  select(-week.x, -week.y, -home_team, -away_team, -game_id)
final_result_rb <- final_result_rb %>%
  left_join(def_stats_allowed_test, by = c("defteam" = "defteam"))

###################################################################
result_home_wr <- sched %>%
  left_join(weekly_stats_test_wr, by = c("home_team" = "posteam"))
result_away_wr <- sched %>%
  left_join(weekly_stats_test_wr, by = c("away_team" = "posteam"))
final_result_wr <- bind_rows(result_home_wr, result_away_wr) %>%
  mutate(
    posteam = ifelse(
      player_team == home_team,
      home_team,
      ifelse(
        player_team == away_team,
        away_team,
        NA
      )
    )
  ) %>%
  mutate(
    defteam = ifelse(
      player_team == home_team,
      away_team,
      ifelse(
        player_team == away_team,
        home_team,
        NA
      )
    )
  ) %>%
  select(-week.x, -week.y, -home_team, -away_team, -game_id)
final_result_wr <- final_result_wr %>%
  left_join(def_stats_allowed_test, by = c("defteam" = "defteam"))

###################################################################
result_home_te <- sched %>%
  left_join(weekly_stats_test_te, by = c("home_team" = "posteam"))
result_away_te <- sched %>%
  left_join(weekly_stats_test_te, by = c("away_team" = "posteam"))
final_result_te <- bind_rows(result_home_te, result_away_te) %>%
  mutate(
    posteam = ifelse(
      player_team == home_team,
      home_team,
      ifelse(
        player_team == away_team,
        away_team,
        NA
      )
    )
  ) %>%
  mutate(
    defteam = ifelse(
      player_team == home_team,
      away_team,
      ifelse(
        player_team == away_team,
        home_team,
        NA
      )
    )
  ) %>%
  select(-week.x, -week.y, -home_team, -away_team, -game_id)
final_result_te <- final_result_te %>%
  left_join(def_stats_allowed_test, by = c("defteam" = "defteam"))


# target variables for feature selection
target_variables <- c(
  "passing_yards",
  "passing_tds",
  "interceptions",
  "sack_fumbles_lost",
  "passing_2pt_conversions",
  "rushing_yards",
  "rushing_tds",
  "rushing_fumbles_lost",
  "rushing_2pt_conversions",
  "receptions",
  "receiving_yards",
  "receiving_tds",
  "receiving_fumbles_lost",
  "receiving_2pt_conversions"
)
exclude_columns <- c(
  "player_id",
  "player_name",
  "player_team",
  "position",
  "week",
  "fantasy_points",
  "fantasy_points_ppr",
  "fan_points_half_ppr",
  "posteam",
  "defteam"
)

selected_data <- merged_stats %>%
  select(-one_of(exclude_columns)) %>%
  select_if(function(col) is.factor(col) || is.numeric(col))
selected_data_qb <- merged_stats %>%
  filter(position == "QB") %>%
  select(-one_of(exclude_columns)) %>%
  select_if(function(col) is.factor(col) || is.numeric(col))
selected_data_rb <- merged_stats %>%
  filter(position == "RB") %>%
  select(-one_of(exclude_columns)) %>%
  select_if(function(col) is.factor(col) || is.numeric(col))
selected_data_wr <- merged_stats %>%
  filter(position == "WR") %>%
  select(-one_of(exclude_columns)) %>%
  select_if(function(col) is.factor(col) || is.numeric(col))
selected_data_te <- merged_stats %>%
  filter(position == "TE") %>%
  select(-one_of(exclude_columns)) %>%
  select_if(function(col) is.factor(col) || is.numeric(col))

feature_selection_results <- data.frame(
  Target = character(0),
  Features = character(0)
)

# All Positions - create a formula for lin reg model
# Perform stepwise regression on selected_data
# Make predictions for target variable
# Create new column in merged_preds with the name pred_target_var

for (target_var in target_variables) {
  formula <- as.formula(paste(target_var, "~ ."))
  model <- lm(formula, data = selected_data)
  test_predictions <- predict(model, newdata = merged_stats_test)
  pred_column_name <- paste("pred", target_var, sep = "_")
  merged_stats_test[[pred_column_name]] <- test_predictions
}

merged_stats_test_important <- merged_stats_test %>%
  select(
    player_name,
    position,
    passing_yards,
    pred_passing_yards,
    passing_tds,
    pred_passing_tds,
    interceptions,
    pred_interceptions,
    sack_fumbles_lost,
    pred_sack_fumbles_lost,
    passing_2pt_conversions,
    pred_passing_2pt_conversions,
    rushing_yards,
    pred_rushing_yards,
    rushing_tds,
    pred_rushing_tds,
    rushing_fumbles_lost,
    pred_rushing_fumbles_lost,
    rushing_2pt_conversions,
    pred_rushing_2pt_conversions,
    receptions,
    pred_receptions,
    receiving_yards,
    pred_receiving_yards,
    receiving_tds,
    pred_receiving_tds,
    receiving_2pt_conversions,
    pred_receiving_2pt_conversions,
    fantasy_points_ppr
  )

####################################
# Individual Positions: Add pred_fantasy_points column
## QB
qb_pred <- final_result_qb %>%
  select(player_name, player_team, position, defteam)
for (target_var in target_variables) {
  formula <- as.formula(paste(target_var, "~ ."))
  model <- lm(formula, data = selected_data_qb)
  test_predictions <- predict(model, newdata = final_result_qb)
  pred_column_name <- paste("pred", target_var, sep = "_")
  qb_pred[[pred_column_name]] <- test_predictions
}

####################################
## RB
rb_pred <- final_result_rb %>%
  select(player_name, player_team, position, defteam)
for (target_var in target_variables) {
  formula <- as.formula(paste(target_var, "~ ."))
  model <- lm(formula, data = selected_data_rb)
  test_predictions <- predict(model, newdata = final_result_rb)
  pred_column_name <- paste("pred", target_var, sep = "_")
  rb_pred[[pred_column_name]] <- test_predictions
}

####################################
## WR
wr_pred <- final_result_wr %>%
  select(player_name, player_team, position, defteam)
for (target_var in target_variables) {
  formula <- as.formula(paste(target_var, "~ ."))
  model <- lm(formula, data = selected_data_wr)
  test_predictions <- predict(model, newdata = final_result_wr)
  pred_column_name <- paste("pred", target_var, sep = "_")
  wr_pred[[pred_column_name]] <- test_predictions
}

####################################
## TE
te_pred <- final_result_te %>%
  select(player_name, player_team, position, defteam)
for (target_var in target_variables) {
  # Create a formula for the linear regression model
  formula <- as.formula(paste(target_var, "~ ."))
  model <- lm(formula, data = selected_data_te)
  test_predictions <- predict(model, newdata = final_result_te)
  pred_column_name <- paste("pred", target_var, sep = "_")
  te_pred[[pred_column_name]] <- test_predictions
}

#########################################################################
# write final prediction in csv files
qb_pred <- qb_pred %>%
  mutate(
    pred_fantasy_points = (
      (.04 * pred_passing_yards) +
        (4 * pred_passing_tds) +
        (-2 * pred_interceptions) +
        (2 * pred_passing_2pt_conversions) +
        (.1 * pred_rushing_yards) +
        (6 * pred_rushing_tds) +
        (-2 * pred_rushing_fumbles_lost) +
        (2 * pred_rushing_2pt_conversions) +
        (.1 * pred_receiving_yards) +
        (.5 * pred_receptions) +
        (6 * pred_receiving_tds) +
        (-2 * pred_receiving_fumbles_lost) +
        (2 * pred_receiving_2pt_conversions) +
        (-2 * pred_sack_fumbles_lost)
    )
  )
write.csv(qb_pred, "Data/qb_pred.csv")
rb_pred <- rb_pred %>%
  mutate(
    pred_fantasy_points = (
      (.04 * pred_passing_yards) +
        (4 * pred_passing_tds) +
        (-2 * pred_interceptions) +
        (2 * pred_passing_2pt_conversions) +
        (.1 * pred_rushing_yards) +
        (6 * pred_rushing_tds) +
        (-2 * pred_rushing_fumbles_lost) +
        (2 * pred_rushing_2pt_conversions) +
        (.1 * pred_receiving_yards) +
        (.5 * pred_receptions) +
        (6 * pred_receiving_tds) +
        (-2 * pred_receiving_fumbles_lost) +
        (2 * pred_receiving_2pt_conversions) +
        (-2 * pred_sack_fumbles_lost)
    )
  )
write.csv(rb_pred, "Data/rb_pred.csv")
wr_pred <- wr_pred %>%
  mutate(
    pred_fantasy_points = (
      (.04 * pred_passing_yards) +
        (4 * pred_passing_tds) +
        (-2 * pred_interceptions) +
        (2 * pred_passing_2pt_conversions) +
        (.1 * pred_rushing_yards) +
        (6 * pred_rushing_tds) +
        (-2 * pred_rushing_fumbles_lost) +
        (2 * pred_rushing_2pt_conversions) +
        (.1 * pred_receiving_yards) +
        (.5 * pred_receptions) +
        (6 * pred_receiving_tds) +
        (-2 * pred_receiving_fumbles_lost) +
        (2 * pred_receiving_2pt_conversions) +
        (-2 * pred_sack_fumbles_lost)
    )
  )
write.csv(wr_pred, "Data/wr_pred.csv")
te_pred <- te_pred %>%
  mutate(
    pred_fantasy_points = (
      (.04 * pred_passing_yards) +
        (4 * pred_passing_tds) +
        (-2 * pred_interceptions) +
        (2 * pred_passing_2pt_conversions) +
        (.1 * pred_rushing_yards) +
        (6 * pred_rushing_tds) +
        (-2 * pred_rushing_fumbles_lost) +
        (2 * pred_rushing_2pt_conversions) +
        (.1 * pred_receiving_yards) +
        (.5 * pred_receptions) +
        (6 * pred_receiving_tds) +
        (-2 * pred_receiving_fumbles_lost) +
        (2 * pred_receiving_2pt_conversions) +
        (-2 * pred_sack_fumbles_lost)
    )
  )
write.csv(te_pred, "Data/te_pred.csv")















###########################################################################
###########################################################################
########## POSITION MODELING ##############################################
###########################################################################
###########################################################################

# load data by position
pbp_2020 <- nflreadr::load_pbp(2020)
qbs <- nflreadr::load_player_stats(2020) %>%
  filter(position == "QB")
wrs <- nflreadr::load_player_stats(2020) %>%
  filter(position == "WR")
rbs <- nflreadr::load_player_stats(2020) %>%
  filter(position == "RB")
tes <- nflreadr::load_player_stats(2020) %>%
  filter(position == "TE")

# load play-by-play data
pbp_2020_join <- pbp_2020 %>%
  dplyr::select(
    home_team,
    away_team,
    week,
    posteam,
    posteam_type,
    defteam,
    roof,
    surface,
    spread_line,
    temp,
    wind
  )
pbp_2020_join$recent_team <- pbp_2020_join$posteam
pbp_2020_join <- pbp_2020_join %>%
  filter(!is.na(recent_team)) %>%
  distinct()

# load quarterbacks
qbs <- qbs %>%
  group_by(player_id) %>%
  mutate(
    avg_fantasy_pts = lag(
      rollapplyr(fantasy_points, 5, mean, partial = TRUE)
    ),
    avg_passing_yards = lag(
      rollapplyr(passing_yards, 5, mean, partial = TRUE)
    ),
    avg_passing_tds = lag(
      rollapplyr(passing_tds, 5, mean, partial = TRUE)
    ),
    avg_rushing_yards = lag(
      rollapplyr(rushing_yards, 5, mean, partial = TRUE)
    ),
    avg_rushing_tds = lag(
      rollapplyr(rushing_tds, 5, mean, partial = TRUE)
    ),
    last_rush_yards = lag(rushing_yards),
    last_passing_yards = lag(passing_yards),
    avg_passing_epa = lag(
      rollapplyr(passing_epa, 5, mean, partial = TRUE)
    ),
    avg_pacr = lag(rollapplyr(pacr, 5, mean, partial = TRUE)),
    avg_dakota = lag(rollapplyr(dakota, 5, mean, partial = TRUE))
  )
qbs <- qbs %>% ungroup()
qb_rolling <- qbs %>%
  filter(
    !is.na(avg_fantasy_pts),
    season_type == "REG"
  ) %>%
  mutate(week = as.factor(week)) %>%
  dplyr::select(
    player_name,
    week,
    fantasy_points,
    avg_fantasy_pts,
    avg_passing_yards,
    avg_passing_tds,
    avg_rushing_yards,
    avg_rushing_tds,
    last_rush_yards,
    last_passing_yards,
    avg_passing_epa,
    avg_pacr,
    avg_dakota
  ) %>%
  ungroup() %>%
  group_by("player_id")

# load wide receivers 
wrs <- wrs %>%
  group_by(player_id) %>%
  mutate(
    avg_fantasy_pts = lag(
      rollapplyr(fantasy_points, 5, mean, partial = TRUE)
    ),
    avg_receiving_yards = lag(
      rollapplyr(passing_yards, 5, mean, partial = TRUE)
    ),
    avg_receiving_tds = lag(
      rollapplyr(passing_tds, 5, mean, partial = TRUE)
    ),
    avg_rushing_yards = lag(
      rollapplyr(rushing_yards, 5, mean, partial = TRUE)
    ),
    avg_rushing_tds = lag(
      rollapplyr(rushing_tds, 5, mean, partial = TRUE)
    ),
    last_rush_yards = lag(rushing_yards),
    last_receiving_yards = lag(receiving_yards)
  )
wrs <- wrs %>% ungroup()
wr_rolling <- wrs %>%
  filter(
    !is.na(avg_fantasy_pts),
    season_type == "REG"
  ) %>%
  mutate(week = as.factor(week)) %>%
  dplyr::select(
    player_name,
    week,
    fantasy_points,
    avg_fantasy_pts,
    avg_receiving_yards,
    avg_receiving_tds,
    avg_rushing_yards,
    avg_rushing_tds,
    last_rush_yards,
    last_receiving_yards
  ) %>%
  ungroup()

# load running backs
rbs <- rbs %>%
  group_by(player_id) %>%
  mutate(
    avg_fantasy_pts = lag(
      rollapplyr(fantasy_points, 5, mean, partial = TRUE)
    ),
    avg_receiving_yards = lag(
      rollapplyr(passing_yards, 5, mean, partial = TRUE)
    ),
    avg_receiving_tds = lag(
      rollapplyr(passing_tds, 5, mean, partial = TRUE)
    ),
    avg_rushing_yards = lag(
      rollapplyr(rushing_yards, 5, mean, partial = TRUE)
    ),
    avg_rushing_tds = lag(
      rollapplyr(rushing_tds, 5, mean, partial = TRUE)
    ),
    last_rush_yards = lag(rushing_yards),
    last_receiving_yards = lag(receiving_yards)
  )
rb_rolling <- rbs %>%
  filter(
    !is.na(avg_fantasy_pts),
    season_type == "REG"
  ) %>%
  mutate(week = as.factor(week)) %>%
  dplyr::select(
    player_name,
    week,
    fantasy_points,
    avg_fantasy_pts,
    avg_receiving_yards,
    avg_receiving_tds,
    avg_rushing_yards,
    avg_rushing_tds,
    last_rush_yards,
    last_receiving_yards
  ) %>%
  ungroup()

# load tight ends
tes <- tes %>%
  group_by(player_id) %>%
  mutate(
    avg_fantasy_pts = lag(
      rollapplyr(fantasy_points, 5, mean, partial = TRUE)
    ),
    avg_receiving_yards = lag(
      rollapplyr(passing_yards, 5, mean, partial = TRUE)
    ),
    avg_receiving_tds = lag(
      rollapplyr(passing_tds, 5, mean, partial = TRUE)
    ),
    avg_rushing_yards = lag(
      rollapplyr(rushing_yards, 5, mean, partial = TRUE)
    ),
    avg_rushing_tds = lag(
      rollapplyr(rushing_tds, 5, mean, partial = TRUE)
    ),
    last_rush_yards = lag(rushing_yards),
    last_receiving_yards = lag(receiving_yards)
  )
te_rolling <- tes %>%
  filter(
    !is.na(avg_fantasy_pts),
    season_type == "REG"
  ) %>%
  mutate(week = as.factor(week)) %>%
  dplyr::select(
    player_name,
    week,
    fantasy_points,
    avg_fantasy_pts,
    avg_receiving_yards,
    avg_receiving_tds,
    avg_rushing_yards,
    avg_rushing_tds,
    last_rush_yards,
    last_receiving_yards
  ) %>%
  ungroup()


# join position players & play-by-play 
qb_extra <- inner_join(
  pbp_2020_join, qbs, by = c("week", "recent_team")
)
wr_extra <- inner_join(
  pbp_2020_join, wrs, by = c("week", "recent_team")
)
rb_extra <- inner_join(
  pbp_2020_join, rbs, by = c("week", "recent_team")
)
te_extra <- inner_join(
  pbp_2020_join,
  tes,
  by = c("week", "recent_team")
)

# fixing spread variable
qb_extra <- qb_extra %>%
  mutate(
    spread = ifelse(
      posteam_type == "home",
      -1 * spread_line,
      1 * spread_line
    ),
    week = as.factor(week)
  ) %>%
  group_by(player_name) %>%
  mutate(row_count = n()) %>%
  filter(completions > 2)
wr_extra <- wr_extra %>%
  mutate(
    spread = ifelse(
      posteam_type == "home",
      -1 * spread_line,
      1 * spread_line
    ),
    week = as.factor(week)
  ) %>%
  group_by(player_name) %>%
  mutate(row_count = n()) %>%
  dplyr::select(
    week,
    posteam,
    defteam,
    spread,
    player_name,
    avg_rushing_yards,
    avg_receiving_yards,
    avg_fantasy_pts,
    fantasy_points
  ) %>%
  filter(
    !is.na(avg_fantasy_pts),
    !is.na(fantasy_points)
  )
rb_extra <- rb_extra %>%
  mutate(
    spread = ifelse(
      posteam_type == "home",
      -1 * spread_line,
      1 * spread_line
    ),
    week = as.factor(week)
  ) %>%
  group_by(player_name) %>%
  mutate(row_count = n())
te_extra <- te_extra %>%
  mutate(
    spread = ifelse(
      posteam_type == "home",
      -1 * spread_line,
      1 * spread_line
    ),
    week = as.factor(week)
  ) %>%
  group_by(player_name) %>%
  mutate(row_count = n())

set.seed(10)

# Model train/test
data_split <- initial_split(
  qb_model,
  strata = "fantasy_points",
  prop = 3 / 4
)

qb_model <- qb_extra %>%
  dplyr::select(
    week,
    posteam,
    defteam,
    spread,
    player_name,
    avg_rushing_yards,
    avg_passing_yards,
    avg_fantasy_pts,
    fantasy_points
  ) %>%
  filter(
    !is.na(avg_fantasy_pts),
    !is.na(fantasy_points)
  )
qb_model <- na.omit(qb_model)
qb_train <- na.omit(training(data_split))
qb_test <- na.omit(testing(data_split))
qb_lm_mod <- lm(
  fantasy_points ~ avg_rushing_yards +
    avg_passing_yards +
    avg_fantasy_pts +
    posteam +
    defteam +
    spread +
    player_name,
  data = qb_extra
)
qb_pred_lm <- predict(qb_lm_mod, newdata = qb_test)
qb_mse <- mean((qb_pred_lm - qb_test$fantasy_points) ** 2)
qb_remse <- sqrt(qb_mse)
# plot(residuals(qb_lm_mod) ~ fitted(qb_lm_mod), data = qb_test)
qb_lmer <- lmer(
  fantasy_points ~ avg_passing_yards +
    avg_rushing_yards +
    (1 | player_name) +
    (1 | defteam),
  data = qb_extra
)
qb_pred_mem <- predict(qb_lmer, newdata = qb_test)
qb_test$pred <- qb_pred_mem
qb_extra$pred <- predict(qb_lmer) # ERROR: CANT RECYCLE INPUT OF SIZE
qb_test <- qb_test %>%                # 521 to size 580 
  group_by(player_name) %>%
  mutate(row_count = n())
qb_top <- qb_extra %>%
  filter(row_count > 10)
#ggplot(
#  data = qb_top,
#  aes(x = week, y = fantasy_points, group = player_name)
#) + geom_point(
#    x = qb_top$week,
#    y = qb_top$pred,
#    group = gp_top$player_name
#  ) + facet_wrap(~player_name, ncol = 9)


wr_model <- wr_extra %>%
  dplyr::select(
    week,
    posteam,
    defteam,
    spread,
    player_name,
    avg_rushing_yards,
    avg_receiving_yards,
    avg_fantasy_pts,
    fantasy_points
  ) %>%
  filter(
    !is.na(avg_fantasy_pts),
    !is.na(fantasy_points)
  )
wr_lm_mod <- lm(
  fantasy_points ~ avg_rushing_yards +
    avg_receiving_yards +
    avg_fantasy_pts +
    posteam +
    defteam +
    spread +
    player_name,
  data = wr_extra
)
wr_preds <- predict(wr_lm_mod, newdata = wr_test)
wr_lmer <- lmer(
  fantasy_points ~ avg_receiving_yards +
    avg_rushing_yards +
    (1 | player_name) +
    (1 | defteam),
  data = wr_extra
)
# summary(wr_lmer)
wr_extra <- wr_extra %>%
  filter(!is.na(avg_fantasy_pts))
wr_extra$pred <- predict(wr_lmer)
wr_top <- wr_extra %>%        # ERROR: OBJECT ROW COUNT NOT FOUND
  filter(row_count > 10)
#ggplot( # FIX WR_TOP FIRST
#  data = wr_top,
#  aes(x = week, y = fantasy_points, group = player_name)
#) + geom_line() + geom_point(
#  x = wr_top$week, y = wr_top$pred, group = wr_top$player_name
#) + facet_wrap(~player_name, ncol = 9)


rb_model <- rb_extra %>%
  dplyr::select(
    week,
    posteam,
    defteam,
    spread,
    player_name,
    avg_rushing_yards,
    avg_receiving_yards,
    avg_fantasy_pts,
    fantasy_points
  ) %>%
  filter(
    !is.na(avg_fantasy_pts),
    !is.na(fantasy_points)
  )
rb_lmer <- lmer(
  fantasy_points ~ avg_receiving_yards +
    avg_rushing_yards +
    (1 | player_name) +
    (1 | defteam),
  data = rb_extra
)
# summary(rb_lmer)
rb_extra <- rb_extra %>%
  filter(!is.na(avg_fantasy_pts))
rb_extra$pred <- predict(rb_lmer)
rb_top <- rb_extra %>%
  filter(row_count > 10)
#ggplot( # ERROR - NO FUNCTION GEOM_POINTS
#  data = rb_top,
#  aes(x = week, y = fantasy_points, group = player_name)
#) + geom_line() + geom_points(
#  x = rb_top$week, y = rb_top$pred, group = player_name
#) + facet_wrap(~player_name, ncol = 0)


te_model <- te_extra %>%
  dplyr::select(
    week,
    posteam,
    defteam,
    spread,
    player_name,
    avg_rushing_yards,
    avg_receiving_yards,
    avg_fantasy_pts,
    fantasy_points
  ) %>%
  filter(
    !is.na(avg_fantasy_pts),
    !is.na(fantasy_points)
  )

# Mixed Effects

te_lmer <- lmer(
  fantasy_points ~ avg_receiving_yards +
    avg_rushing_yards +
    (1 | player_name) +
    (1 | defteam),
  data = te_extra
)

summary(te_lmer)

te_extra <- te_extra %>%
  filter(!is.na(avg_fantasy_pts))

te_extra$pred <- predict(te_lmer)

te_top <- te_extra %>%
  filter(row_count > 10)





























#################################################################
#################################################################
########### DEFENSIVE MODELING ##################################
#################################################################
#################################################################

# pbp_2020 defined in start of position modeling nflreadr::loadpbp(2020)
all <- load_player_stats(2020)
all2 <- pbp_2020 %>%
  filter(season_type == "REG") %>%
  nflfastR::calculate_player_stats_def(weekly = TRUE)

# Defensive Players
all3 <- left_join(
  all2, all,
  by = c(
    "season", "player_id", "player_name",
    "player_display_name", "position", "position_group",
    "week"
  )
)

defense_group <- all3 %>%
  filter(position_group == "DB" | position_group == "LB") %>%
  select(-c("headshot_url.x", "headshot_url.y")) %>%
  mutate_if(is.numeric, list(~replace_na(., 0))) %>%
  mutate(
    defense_total = def_sacks +
    2 * interceptions +
    2 * def_fumble_recovery_own +
    2 * def_safety
  )

def_select <- defense_group %>%
  select(
    "season", "week", "team", "def_sacks", "interceptions",
    "def_fumble_recovery_own", "def_safety", "fantasy_points",
    "defense_total"
  ) %>%
  mutate(fantasy_total = fantasy_points + defense_total)

def_team_stats <- def_select %>%
  group_by(season, week, team) %>%
  summarize(
    across(everything(), sum),
    .groups = 'drop'
  ) %>%
  as.data.frame() %>%
  rename(defteam = team)

offensive_team_stats <- pbp_2020 %>%
  nflfastR::calculate_player_stats(weekly = TRUE) %>%
  filter(!position_group %in% c("DB", "LB"), season_type == "REG")

offense_sel <- offensive_team_stats %>%
  select(
    "recent_team", "season", "week", "passing_yards", "passing_tds",
    "rushing_yards", "rushing_tds", "receiving_yards", "receiving_tds"
  ) %>%
  mutate(
    td_points = rushing_tds * 7 + passing_tds * 7,
    total_yards = rushing_yards + passing_yards
  )

# offensive team stats by team
off_team_stats <- offense_sel %>%
  group_by(season, week, recent_team) %>%
  summarize(
    across(everything(), sum),
    .groups = "drop"
  ) %>%
  as.data.frame() %>%
  rename(posteam = recent_team)

pbp_join <- pbp_2020 %>%
  select(week, posteam, defteam)

off_team_total <- left_join(
  off_team_stats,
  pbp_join,
  by = c("posteam", "week")
) %>%
  distinct()

def_team_stats_nec <- def_team_stats %>%
  select("week", "defteam", "fantasy_total")

combo <- left_join(
  off_team_total,
  def_team_stats_nec,
  by = c("week", "defteam")
)

# kicking / field goal points
kicking_stats <- pbp_2020 %>%
  nflfastR::calculate_player_stats_kicking(weekly = TRUE) %>%
  mutate(fg_points = fg_made * 3) %>%
  select("week", "team", "fg_points") %>%
  rename(posteam = team)
kicking_stats[is.na(kicking_stats)] <- 0

final_set <- left_join(
  combo,
  kicking_stats,
  by = c("week", "posteam")
) %>%
  mutate(off_points = fg_points + td_points) %>%
  mutate(
    points_allowed = case_when(
      off_points < 1 ~ 10,
      off_points > 0 & off_points < 7 ~ 7,
      off_points > 6 & off_points < 14 ~ 4,
      off_points > 13 & off_points < 21 ~ 1,
      off_points > 20 & off_points < 28 ~ 0,
      off_points > 27 & off_points < 35 ~ -1,
      off_points > 34 ~ -4
    )
  ) %>%
  mutate(points = points_allowed + fantasy_total) %>%
  arrange(defteam, week) %>%
  group_by(defteam) %>%
  mutate(
    avg_yards = lag(rollapplyr(total_yards, 5, mean, partial = TRUE)),
    avg_tdpoints = lag(rollapplyr(off_points, 5, mean, partial = TRUE)),
    avg_offpoints = lag(rollapplyr(off_points, 5, mean, partial = TRUE)),
    avg_points = lag(rollapplyr(points, 5, mean, partial = TRUE))
  ) %>%
  filter(!is.na(avg_yards))

# linear modeling 
linear_mod <- lm(
  points ~ avg_yards + avg_points + avg_offpoints,
  data = final_set
)
def_lin_predictions <- predict(
  linear_mod,
  new_data = final_set,
  allow.new.levels = TRUE
)

# mixed effects model
def_lmer <- lmer(
  points ~ avg_yards + avg_offpoints + (1|posteam),
  data = final_set
)
def_mixed_predictions <- predict(
  def_lmer,
  new_data = final_set,
  allow.new.levels = TRUE
)


# Individual Defense
defense_group_indiv <- defense_group %>%
  mutate(
    individual_fantasy = def_tackles_solo +
    0.5 * def_tackle_assists + def_tackles_for_loss +
    def_qb_hits + 2 * def_sacks +
    3 * interceptions + def_pass_defended +
    3 * def_fumbles_forced + 6 * def_tds
  )

def_sel2 <- defense_group_indiv %>%
  select(
    "season", "week", "team", "def_sacks",
    "interceptions", "def_fumbles_forced",
    "def_tackles_solo", "def_tackles_for_loss",
    "def_qb_hits", "individual_fantasy"
  ) %>%
  rename(defteam = team)

indiv_defense <- left_join(
  def_sel2, off_team_total,
  by = c("defteam", "week")
) %>%
  arrange(defteam, week) %>%
  groupby(defteam) %>%
  mutate(
    avg_sacks = lag(rollapplyr(def_sacks, 5, mean, partial = TRUE)),
    avg_ints = lag(rollapplyr(interceptions, 5, mean, partial = TRUE)),
    avg_tackles = lag(rollapplyr(def_tackles_solo, 5, mean, partial = TRUE)),
    avg_qbhits = lag(rollapplyr(def_qb_hits, 5, mean, partial = TRUE))
  ) %>%
  filter(!is.na(avg_sacks))

indiv_def_lmer <- lmer(
  inidividual_fantasy ~ avg_sacks + avg_ints + avg_tackles +
  avg_qbhits + (1|posteam),
  data = indiv_defense
)




































###########################################################################
###########################################################################
################ KICKING FANTASY PREDICTION ###############################
###########################################################################
###########################################################################

# previously define def_stats load_pbp(2020:2022)
kick_stats <- def_stats %>%
  filter(field_goal_attempt == 1 | extra_point_attempt == 1) %>%
  select(
    game_id, week, kicker_player_id, kicker_player_name,
    posteam, field_goal_result, kick_distance, extra_point_result
  ) %>%
  filter(!is.na(posteam)) %>%
  mutate(
    field_goal_result = ifelse(
      is.na(field_goal_result), "None", field_goal_result
    ),
    extra_point_result = ifelse(
      is.na(extra_point_result), "None", extra_point_result
    ),
    fg_points = case_when(
      field_goal_result == "made" & kick_distance >= 0 & kick_distance <= 39 ~ 3,
      field_goal_result == "made" & kick_distance >= 40 & kick_distance <= 49 ~ 4,
      field_goal_result == "made" & kick_distance >= 50 & kick_distance <= 59 ~ 5,
      field_goal_result == "made" & kick_distance >= 60 ~ 5),
    fg_missed = ifelse(
      field_goal_result == "missed" | field_goal_result == "blocked", 1, 0
    ),
    xp_missed = ifelse(
      extra_point_result == "missed" | extra_point_result == "blocked", 1, 0
    ),
    fg_points = ifelse(is.na(fg_points), 0, fg_points)
  ) %>%
  group_by(game_id, kicker_player_id, kicker_player_name, posteam) %>%
  summarise(
    fg_points = sum(fg_points),
    fg_missed = sum(fg_missed),
    xp_missed = sum(xp_missed)
  ) %>%
  mutate(kick_fan_points = fg_points - fg_missed - xp_missed)

blocked_kicks <- def_stats %>%
  select(game_id, week, defteam, field_goal_result, extra_point_result) %>%
  filter(!is.na(defteam)) %>%
  mutate(
    fg_block = ifelse(is.na(field_goal_result), "none", field_goal_result),
    fg_blocked = ifelse(fg_block == "blocked", 1,0),
    xp_block = ifelse(is.na(extra_point_result), "none", extra_point_result),
    xp_blocked = ifelse(xp_block == "blocked", 1,0)
  ) %>%
  select(game_id, week, defteam, fg_blocked, xp_blocked) %>%
  group_by(game_id, week, defteam) %>%
  summarise(fg_blocked = sum(fg_blocked), xp_blocked = sum(xp_blocked)) %>%
  ungroup()

def_stats <- def_stats %>%
  filter(!is.na(posteam)) %>%
  select(
    game_id, week, posteam, defteam, yards_gained, success, touchdown,
    interception, fumble_lost, sack
  ) %>%
  replace(is.na(.), 0) %>%
  group_by(game_id, week, posteam, defteam) %>%
  summarize(
    total_yards_allowed = sum(yards_gained),
    success_rate_against = mean(success),
    total_tds_allowed = sum(touchdown),
    interceptions_forced = sum(interception),
    fumbles_forced = sum(fumble_lost),
    sacks_forced = sum(sack)
  ) %>%
  ungroup()

off_stats <- def_stats %>%
  select(
    game_id, week, posteam, defteam, yards_gained,
    drive_inside20, success, field_goal_attempt, extra_point_attempt
  ) %>%
  filter(!is.na(posteam)) %>%
  replace(is.na(.), 0) %>%
  group_by(game_id, week, posteam, defteam) %>%
  summarise(
    off_total_yds = sum(yards_gained),
    off_drives_inside20 = sum(drive_inside20),
    off_avg_success = mean(success),
    fg_attempts = sum(field_goal_attempt),
    xp_attempts = sum(extra_point_attempt)
  ) %>%
  ungroup()

kick_fantasy_points <- kick_stats %>%
  select(game_id, kicker_player_name, posteam, kick_fan_points )

combined <- off_stats %>%
  left_join(kick_fantasy_points, by = c("game_id", "posteam")) %>%
  left_join(def_stats, by = c("game_id", "week", "posteam", "defteam")) %>%
  left_join(blocked_kicks, by = c("game_id", "week", "defteam"))

kick_fan_score_model_train <- combined %>%
  select(-c(
    game_id, week, defteam, posteam, kicker_player_id, kicker_player_name
  ))

kick_fan_score_model_test <- combined_test %>%
  select(-c(
    game_id, week, defteam, posteam, kicker_player_id,
    kicker_player_name, kick_fan_points
  ))

kicking_lm_model <- lm(kick_fan_points ~ ., data = kick_fan_score_model_train)

kick_fantasy_points_actual_lm <- kick_fantasy_points_actual

test_predictions_lm <- predict(
  kicking_lm_model, newdata = kick_fan_score_model_test
)

kick_fantasy_points_actual_lm$pred_fan_points <- test_predictions_lm

model <- regsubsets(kick_fan_points ~ ., data = kick_fan_score_model_train)

best_model_1 <- which.max(summary(model)$adjr2)

best_model_2 <- lm(
  kick_fan_points ~ off_total_yds + off_drives_inside20 +
  interceptions_forced + fumbles_forced + total_yards_allowed  +
  fg_blocked + fg_attempts,
  data = kick_fan_score_model_train
)

kick_fantasy_points_actual_best <- kick_fantasy_points_actual

test_predictions_best <- predict(
  best_model, newdata = kick_fan_score_model_test
)

kick_fantasy_points_actual_best$pred_fan_points <- test_predictions_best

# regression

response_variable <- "kick_fan_points"
# Select predictors and response variable for training set
kick_fan_score_model_train <- combined %>%
  select(-c(
    game_id, week, defteam, posteam, kicker_player_id, kicker_player_name
  )) %>%
  na.omit()
# Select predictors for testing set
kick_fan_score_model_test <- combined_test %>%
  select(-c(
    game_id, defteam, posteam, kicker_player_name, kick_fan_points
  ))

# Convert to matrix format (required by glmnet)
X_train <- as.matrix(kick_fan_score_model_train %>% select(-response_variable))
y_train <- kick_fan_score_model_train[[response_variable]]
X_test <- as.matrix(kick_fan_score_model_test)
alpha <- 0  # Ridge regression (alpha = 0)
ridge_model <- glmnet(X_train, y_train, alpha = alpha)
test_predictions_ridge <- predict(ridge_model, newx = X_test)
ridge_coeffs <- coef(ridge_model)
ridge_intercept <- ridge_coeffs[1]
ridge_coef <- as.vector(ridge_coeffs[-1])
ridge_coef_names <- colnames(X_train)
ridge_model_summary <- data.frame(
  variable = c("(Intercept)", ridge_coef_names),
  coefficient = c(ridge_intercept, ridge_coef)
)































##################################################################
##################################################################
############ FANTASY POINTS DATAFRAME ############################
##################################################################
##################################################################

positions_2023 = read.csv('Data/roster_weekly_2023.csv')
positions_2022 = read.csv('Data/roster_weekly_2022.csv')
positions_2021 = read.csv('Data/roster_weekly_2021.csv')
positions_2020 = read.csv('Data/roster_weekly_2020.csv')
positions <- rbind(
  positions_2023, positions_2022, positions_2021, positions_2020
) %>%
  mutate(player_id = gsis_id) %>%
  select(player_id, position, full_name) %>%
  filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  distinct() %>%
  separate(
    full_name,
    into = c("first_name", "last_name"),
    sep = " ",
    remove = FALSE
  )

type_of_plays_train <- def_stats %>%
  group_by(game_id, posteam) %>%
  summarize(runs = sum(rush), passes = sum(pass)) %>%
  filter(!is.na(posteam))

receiving_dataset_train <- def_stats %>%
  filter(pass == 1) %>%
  group_by(receiver_player_id, receiver_player_name, game_id, posteam ) %>%
  mutate(
    yards_after_catch = ifelse(is.na(yards_after_catch), 0, yards_after_catch)
  ) %>%
  summarize(
    aDot = mean(air_yards),
    yac_rec = sum(yards_after_catch),
    receiving_yards = sum(yards_gained),
    incompletions = sum(incomplete_pass),
    catches = sum(complete_pass),
    targets = (incompletions + catches),
    touchdowns = sum(touchdown),
    fumbles = sum(fumble),
    avg_rec_epa = mean(epa)
  ) %>%
  ungroup()

# Merge receiving and type_of_plays datasets
receiving_merged_train <- merge(
  x = receiving_dataset_train, y = type_of_plays_train, all.x = TRUE
) %>%
  filter(!is.na(receiver_player_id)) %>%
  mutate(target_share = targets/passes) %>%
  mutate(
    player_id = receiver_player_id,
    player_name = receiver_player_name
  ) %>%
  select(
    game_id, player_id, player_name, posteam, aDot, yac_rec,
    receiving_yards, catches, targets, target_share, touchdowns,
    fumbles, avg_rec_epa
  )

# Create a dataset for rushing statistics
rushing_dataset_train <- def_stats %>%
  filter(rush == 1) %>%
  group_by(rusher_player_id, rusher_player_name, game_id, posteam ) %>%
  summarize(
    rushes = sum(rush),
    rush_yards = sum(yards_gained),
    touchdowns = sum(touchdown),
    player_id = rusher_player_id,
    player_name =  rusher_player_name,
    fumbles = sum(fumble),
    avg_rush_epa = mean(epa)
  ) %>%
  ungroup() %>%
  select(
    game_id, player_id, player_name, posteam, rushes, rush_yards,
    touchdowns, fumbles, avg_rush_epa
  ) %>%
  filter(!is.na(player_id)) %>%
  arrange(game_id)

# Merge rushing and receiving datasets to create a fantasy dataset
fantasy_merged_train <- full_join(
  x = rushing_dataset_train,
  y = receiving_merged_train,
  by = c("game_id", "player_id","player_name", "posteam")
) %>%
  distinct() %>%
  mutate(
    touchdowns = (touchdowns.x + touchdowns.y),
    fumbles = (fumbles.x + fumbles.y)
  ) %>%
  select(-c(touchdowns.x, touchdowns.y, fumbles.x, fumbles.y)) %>%
  filter(!is.na(player_id)) %>%
  replace(is.na(.), 0)

# Create a dataset for quarterback statistics
qb_data_train <- def_stats %>% 
  filter(pass == 1) %>%
  group_by(passer_player_id, passer_player_name, game_id, posteam ) %>%
  summarize(
    passing_yards = sum(yards_gained),
    incompletions = sum(incomplete_pass),
    completions = sum(complete_pass),
    attempts = (incompletions + completions),
    passing_touchdowns = sum(touchdown),
    fumbles = sum(fumble),
    interceptions = sum(interception),
    avg_throw_epa = mean(epa)
  ) %>%
  mutate(player_id = passer_player_id, player_name = passer_player_name) %>%
  ungroup() %>%
  select(-c(passer_player_id, passer_player_name)) %>%
  select(
    game_id, player_id, player_name, posteam, passing_yards,
    incompletions, completions, attempts, passing_touchdowns,
    fumbles, interceptions, avg_throw_epa
  )

# Merge quarterback statistics with the fantasy dataset
fantasy_merged_new_train <- full_join(
  x=fantasy_merged_train,
  y=qb_data_train,
  by=c("game_id", "player_id","player_name", "posteam")
) %>%
  distinct() %>%
  mutate(fumbles = (fumbles.x + fumbles.y)) %>%
  select(-c(fumbles.x, fumbles.y)) %>%
  filter(!is.na(player_id)) %>%
  replace(is.na(.), 0)

sapply(fantasy_merged_new, function(x) sum(is.na(x)))

calc_fantasy_pts <- fantasy_merged_new_train %>%
  mutate(
    fan_points = (.1 * rush_yards) +
    (.1 * receiving_yards) + (.5 * catches) + (.04 * passing_yards) +
    (4 * passing_touchdowns) + (-2 * interceptions) + (6 * touchdowns)
  )

new <- left_join(x = calc_fantasy_pts, y = positions, by = "player_id") %>%
  filter(!is.na(position))

sapply(new, function(x) sum(is.na(x)))
