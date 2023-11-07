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


epoch_time <- function() {
  as.integer(Sys.time())
}
human_time <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

saved_roster <- "responses"

nfl_roster <- nflreadr::load_rosters()

qb_choices <- nfl_roster[nfl_roster$position == 'QB', full_name]

rb_choices <- nfl_roster[nfl_roster$position == 'RB', full_name]

wr_choices <- nfl_roster[nfl_roster$position == 'WR', full_name]

te_choices <- nfl_roster[nfl_roster$position == 'TE', full_name]

flex_choices <- nfl_roster[["full_name"]]

dst_choices <- nfl_roster[nfl_roster$position %in% c(
  "LS", "DL", "P", "DB", "LB"
), full_name]

k_choices <- nfl_roster[nfl_roster$position == 'K', full_name]




##################################################

schedule <- nflfastR::fast_scraper_schedules(2022:2023)
