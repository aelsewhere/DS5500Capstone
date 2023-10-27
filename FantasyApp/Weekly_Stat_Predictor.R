library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(stringr)
library(readr)


options(scipen = 9999)

pbp_stats <- load_player_stats(2020:2022)

def_stats <- load_pbp(2020:2022)

pbp_stats_test <- load_player_stats(2023)

def_stats_test <- load_pbp(2023)



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


sched <- load_schedules(2023)

sched <- sched %>%
  filter(week == def_stats_allowed_test$week) %>%
  select(game_id, week, home_team, away_team)


result_home_qb <- sched %>%
  left_join(weekly_stats_test_qb, by = c("home_team" = "posteam"))

# Join based on the player's team as the away team
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

# Join based on the player's team as the away team
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

# Join based on the player's team as the away team
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

# Join based on the player's team as the away team
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

positions <- c("QB", "RB", "WR", "TE")

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



# All Positions




for (target_var in target_variables) {
  # Create a formula for the linear regression model
  formula <- as.formula(paste(target_var, "~ ."))

  # Perform forward stepwise regression on selected_data
  model <- lm(formula, data = selected_data)

  # Make predictions for the target variable
  test_predictions <- predict(model, newdata = merged_stats_test)

  # Create a new column in merged_preds with the name pred_target_var
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
  # Create a formula for the linear regression model
  formula <- as.formula(paste(target_var, "~ ."))

  # Perform forward stepwise regression on selected_data
  model <- lm(formula, data = selected_data_qb)

  # Make predictions for the target variable
  test_predictions <- predict(model, newdata = final_result_qb)

  # Create a new column in merged_preds with the name pred_target_var
  pred_column_name <- paste("pred", target_var, sep = "_")
  qb_pred[[pred_column_name]] <- test_predictions
}



####################################
## RB



rb_pred <- final_result_rb %>%
  select(player_name, player_team, position, defteam)

for (target_var in target_variables) {
  # Create a formula for the linear regression model
  formula <- as.formula(paste(target_var, "~ ."))

  # Perform forward stepwise regression on selected_data
  model <- lm(formula, data = selected_data_rb)

  # Make predictions for the target variable
  test_predictions <- predict(model, newdata = final_result_rb)

  # Create a new column in merged_preds with the name pred_target_var
  pred_column_name <- paste("pred", target_var, sep = "_")
  rb_pred[[pred_column_name]] <- test_predictions
}


####################################
## WR



wr_pred <- final_result_wr %>%
  select(player_name, player_team, position, defteam)

for (target_var in target_variables) {
  # Create a formula for the linear regression model
  formula <- as.formula(paste(target_var, "~ ."))

  # Perform forward stepwise regression on selected_data
  model <- lm(formula, data = selected_data_wr)

  # Make predictions for the target variable
  test_predictions <- predict(model, newdata = final_result_wr)

  # Create a new column in merged_preds with the name pred_target_var
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

  # Perform forward stepwise regression on selected_data
  model <- lm(formula, data = selected_data_te)

  # Make predictions for the target variable
  test_predictions <- predict(model, newdata = final_result_te)

  # Create a new column in merged_preds with the name pred_target_var
  pred_column_name <- paste("pred", target_var, sep = "_")
  te_pred[[pred_column_name]] <- test_predictions
}




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





