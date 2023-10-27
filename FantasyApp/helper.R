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


#saved_roster <- file.path("resonses")
epochTime <- function() {
    as.integer(Sys.time())
}
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")


