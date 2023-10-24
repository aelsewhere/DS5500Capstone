# packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(zoo)
library(lme4)
library(afex)


# data
pbp_2019 <- nflreadr::load_pbp(2019)
pbp_2020 <- nflreadr::load_pbp(2020)

# Calculating player stats: passing yards leaderboard
pbp_2019 %>%
  filter(season_type == "REG") %>%
  calculate_player_stats() %>%
  arrange(-passing_yards) %>%
  dplyr::select(player_name, recent_team, completions, attempts, passing_yards) %>%
  head(10)


# Calculating player stats: rushing yards leaderboard
pbp_2019 %>%
  filter(season_type == "REG") %>%
  calculate_player_stats() %>%
  arrange(-rushing_yards) %>%
  dplyr::select(player_name, recent_team, carries, rushing_yards, rushing_tds, rushing_fumbles_lost) %>%
  head(10) 

# Calculating PBR fantasy points per game in the first 16 weeks of the season
# among wide receivers appearing in more than 5 games


wrs <- pbp_2019 %>%
  dplyr::filter(week <= 16) %>%
  nflfastR::calculate_player_stats() %>%
  group_by(player_id) %>%
  dplyr::mutate(
    ppg = fantasy_points_ppr / games,
    ypg = receiving_yards / games,
    rpg = rushing_yards / games,
    passpg = passing_yards / games,
    recep = sum(receptions) / games,
    rec_tds = sum(receiving_tds) / games,
    rush_tds = sum(rushing_tds) / games
  ) %>%
  dplyr::filter(games > 5) %>%
  # only keep the WRs
  dplyr::inner_join(
    nflfastR::fast_scraper_roster(2019) %>% 
      dplyr::filter(position == "WR") %>% 
      dplyr::select(player_id = gsis_id),
    by = "player_id"
  ) %>%
  dplyr::arrange(-ppg) %>%
  dplyr::select(player_name, recent_team, games, fantasy_points_ppr, ppg, receiving_yards, ypg, rpg, passpg,
                recep, rec_tds, rush_tds) 


rbs <- wrs <- pbp_2019 %>%
  dplyr::filter(week <= 16) %>%
  nflfastR::calculate_player_stats() %>%
  group_by(player_id) %>%
  dplyr::mutate(
    ppg = fantasy_points_ppr / games,
    ypg = receiving_yards / games,
    rpg = rushing_yards / games,
    passpg = passing_yards / games,
    recep = sum(receptions) / games,
    rec_tds = sum(receiving_tds) / games,
    rush_tds = sum(rushing_tds) / games
  ) %>%
  dplyr::filter(games > 5) %>%
  # only keep the WRs
  dplyr::inner_join(
    nflfastR::fast_scraper_roster(2019) %>% 
      dplyr::filter(position == "RB") %>% 
      dplyr::select(player_id = gsis_id),
    by = "player_id"
  ) %>%
  dplyr::arrange(-ppg) %>%
  dplyr::select(player_name, recent_team, games, fantasy_points_ppr, ppg, receiving_yards, ypg, rpg, passpg,
                recep, rec_tds, rush_tds) 


qbs <- pbp_2019 %>%
  dplyr::filter(week <= 16) %>%
  nflfastR::calculate_player_stats() %>%
  dplyr::mutate(
    ppg = fantasy_points_ppr / games
  ) %>%
  dplyr::filter(games > 5) %>%
  # only keep the WRs
  dplyr::inner_join(
    nflfastR::fast_scraper_roster(2019) %>% 
      dplyr::filter(position == "QB") %>% 
      dplyr::select(player_id = gsis_id),
    by = "player_id"
  ) %>%
  dplyr::arrange(-ppg) %>%
  dplyr::select(player_name, recent_team, games, fantasy_points_ppr, ppg) 


tes <- pbp_2019 %>%
  dplyr::filter(week <= 16) %>%
  nflfastR::calculate_player_stats() %>%
  dplyr::mutate(
    ppg = fantasy_points_ppr / games
  ) %>%
  dplyr::filter(games > 5) %>%
  # only keep the WRs
  dplyr::inner_join(
    nflfastR::fast_scraper_roster(2019) %>% 
      dplyr::filter(position == "TE") %>% 
      dplyr::select(player_id = gsis_id),
    by = "player_id"
  ) %>%
  dplyr::arrange(-ppg) %>%
  dplyr::select(player_name, recent_team, games, fantasy_points_ppr, ppg) 

# Weekly Level Stats

check <- load_player_stats(2020)
check2 <- calculate_player_stats(pbp_2019, weekly = TRUE)

qbs <- 
  load_player_stats(2020) %>%
  filter(position == 'QB')



# 5 day rolling average for numerical stats
qbs <-
  qbs %>%
  group_by(player_id) %>%
  mutate(avg_fantasy_pts = lag(rollapplyr(fantasy_points, 5, mean, partial = TRUE)),
         avg_passing_yards = lag(rollapplyr(passing_yards, 5, mean, partial = TRUE)),
         avg_passing_tds = lag(rollapplyr(passing_tds, 5, mean, partial = TRUE)),
         avg_rushing_yards = lag(rollapplyr(rushing_yards, 5, mean, partial = TRUE)),
         avg_rushing_tds = lag(rollapplyr(rushing_tds, 5, mean, partial = TRUE)),
          last_rush_yards = lag(rushing_yards),
        last_passing_yards = lag(passing_yards),
        avg_passing_epa = lag(rollapplyr(passing_epa, 5, mean, partial = TRUE)),
        avg_pacr = lag(rollapplyr(pacr, 5, mean, partial = TRUE)),
        avg_dakota = lag(rollapplyr(dakota, 5, mean, partial = TRUE)))

qb_rolling <-
  qbs %>%
  filter(!is.na(avg_fantasy_pts),
         season_type == "REG") %>%
  mutate(week = as.factor(week)) %>%
  dplyr::select(player_name, week, fantasy_points, avg_fantasy_pts, avg_passing_yards,
                avg_passing_tds, avg_rushing_yards, avg_rushing_tds, last_rush_yards,
                last_passing_yards, avg_passing_epa, avg_pacr, avg_dakota) %>%
  ungroup()

# prepping to join
pbp_2020_join <-
  pbp_2020 %>%
  dplyr::select(home_team, away_team, week, posteam, posteam_type, defteam, roof, surface, spread_line, temp, wind)
pbp_2020_join$recent_team <- pbp_2020_join$posteam
pbp_2020_join <-
  pbp_2020_join %>%
  filter(!is.na(recent_team)) %>%
  distinct()

qb_extra <-
  inner_join(pbp_2020_join, qbs, by = c("week", "recent_team"))

# fixing spread variable
qb_extra <-
  qb_extra %>%
  mutate(spread = ifelse(posteam_type == 'home', -1 * spread_line, 1* spread_line),
         week = as.factor(week)) 

qb_extra <-
  qb_extra %>%
  group_by(player_name) %>%
  mutate(row_count = n())

qb_extra <-
  qb_extra %>%
  filter(completions > 2)



# Linear model
qb_lm <- lm(fantasy_points ~ avg_passing_yards +
             avg_rushing_yards + spread + player_name + posteam_type + defteam + roof + surface,
            data = qb_extra)

summary(qb_lm)

# mixed effects
qb_extra <-
  qb_extra %>%
  filter(!is.na(avg_fantasy_pts))
qb_lmer <- lmer(fantasy_points ~ avg_passing_yards + avg_rushing_yards + (1|player_name) +
                  (1|defteam),
                data = qb_extra)
summary(qb_lmer)
coef(qb_lmer)
anova(qb_lmer)
AIC(qb_lmer)

qb_extra$pred <-predict(qb_lmer)
qb_top <-
  qb_extra %>%
  filter(row_count > 10)

ggplot(data = qb_top, aes(x = week, y = fantasy_points, group = player_name)) + geom_line() + 
  geom_point(x = qb_top$week, y = qb_top$pred, group = qb_top$player_name) +
  facet_wrap(~player_name, ncol = 9)


# afex
library(sjPlot)
af_qb_lmer <- mixed(fantasy_points ~ avg_passing_yards + avg_rushing_yards + 
                      (1|player_name) + (1|defteam) + spread + posteam_type + roof + surface, data = qb_extra,
                    check_contrasts = FALSE, test_intercept = TRUE, method = "KR")
af_qb_lmer
tab_model(af_qb_lmer)

wbw <- qbs %>%
  dplyr::select(player_display_name, position, recent_team, week, season_type, passing_yards, rushing_yards, fantasy_points)

qb_rolling %>%
  filter(avg_passing_yards > 10) %>%
ggplot(data = ., aes(x = avg_passing_yards, y = avg_fantasy_pts)) + geom_point()



