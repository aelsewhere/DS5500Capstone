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

schedule <- nflreadr::load_schedules()

epochTime <- function() {
    as.integer(Sys.time())
}
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

saved_roster <- "responses"

nfl_roster <- nflreadr::load_rosters()

qb_choices <- nfl_roster[["full_name"]]

rb_choices <- nfl_roster[["full_name"]]

wr_choices <- nfl_roster[["full_name"]]

te_choices <- nfl_roster[["full_name"]]

flex_choices <- nfl_roster[["full_name"]]

dst_choices <- nfl_roster[["full_name"]]

k_choices <- nfl_roster[["full_name"]]

get_player_info <- function(player_name){
  player_info <- nfl_roster %>%
    filter(full_name == player_name)
  if(nrow(player_info) == 0){
    return("Player Not Found")
  } else{
    return(player_info)
  }
}

print_player_info <- function(player_info){
  cat("Name: ", player_info$full_name, "\n")
  cat("Position: ", "\n")
}
