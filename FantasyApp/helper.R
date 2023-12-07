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
dst_choices <- nfl_roster[nfl_roster$position %in% c(
  "LS", "DL", "P", "DB", "LB"
), full_name]
k_choices <- nfl_roster[nfl_roster$position == 'K', "full_name"]


###########################################################################
########## WEEKLY STAT PREDICTOR ##########################################
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

# weekly stats - individuals
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
########## POSITION MODELING ##########################################
###########################################################################

# load data by position
pbp_2020 <- nflreadr::load_pbp(2020)
qbs <- load_player_stats(2020) %>%
  filter(position == "QB")
wrs <- load_player_stats(2020) %>%
  filter(position == "WR")
rbs <- load_player_stats(2020) %>%
  filter(position == "RB")
tes <- load_player_stats(2020) %>%
  filter(position == "TE")

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
####### QUARTERBACK MODELING
# Model train/test
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
data_split <- initial_split(
  qb_model,
  strata = "fantasy_points",
  prop = 3 / 4
)
qb_train <- na.omit(training(data_split))
qb_test <- na.omit(testing(data_split))
# Linear Model 
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
# Mixed Effects Model
qb_lmer <- lmer(
  fantasy_points ~ avg_passing_yards +
    avg_rushing_yards +
    (1 | player_name) +
    (1 | defteam),
  data = qb_extra
)
qb_pred_mem <- predict(qb_lmer, newdata = qb_test)
qb_test$pred <- predictions
qb_extra$pred <- predict(qb_lmer)
qb_test <- qb_test %>%
  group_by(player_name) %>%
  mutate(row_count = n())
qb_top <- qb_extra %>%
  filter(row_count > 10)

####### WIDE RECIEVER MODELING
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

# Linear ---------------------------------------
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
predictions <- predict(lm_mod, newdata = wr_test)

# Mixed Effects ---------------------------------------
wr_lmer <- lmer(
  fantasy_points ~ avg_receiving_yards +
    avg_rushing_yards +
    (1 | player_name) +
    (1 | defteam),
  data = wr_extra
)
summary(wr_lmer)
wr_extra <- wr_extra %>%
  filter(!is.na(avg_fantasy_pts))

wr_extra$pred <- predict(wr_lmer)

wr_top <- wr_extra %>%
  filter(row_count > 10)

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

# Mixed Effects ---------------------------

rb_lmer <- lmer(
  fantasy_points ~ avg_receiving_yards +
    avg_rushing_yards +
    (1 | player_name) +
    (1 | defteam),
  data = rb_extra
)
summary(rb_lmer)
rb_extra <- rb_extra %>%
  filter(!is.na(avg_fantasy_pts))

rb_extra$pred <- predict(rb_lmer)

rb_top <- rb_extra %>%
  filter(row_count > 10)

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
