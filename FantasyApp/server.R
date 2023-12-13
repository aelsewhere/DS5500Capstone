# libraries
libs <- c(
  "shiny", "shinyMobile", "ggplot2", "shinyjs", "DBI",
  "dplyr", "dbplyr", "pool", "plotly", "fmsb", "DT",
  "tidyr", "forcats"
)
invisible(lapply(libs, library, character.only = TRUE))



server <- function(input, output, session) {
  dir.create(saved_roster, showWarnings = FALSE, recursive = TRUE)
  player_mapping <- build_player_mapping()
  # ----------------------------------------------
  # ROSTER SUBMISSION HANDLING
  # ----------------------------------------------
  selected_values <- reactiveValues()
  observe({ for (field in full_roster) {
    value <- isolate(input[[field]])
     selected_values[[field]] <- value
  } })
  observe({
    roster_filled <- vapply(
      mand_roster, function(field) {
        !is.null(input[[field]]) && input[[field]] != ""
      }, logical(1)
    )
    roster_filled <- all(roster_filled)
    toggleState(id = "submit", condition = roster_filled)
  })
  form_data <- reactive({
    data <- sapply(full_roster, function(x) input[[x]])
    data <- c(data, timestamp = epoch_time())
    data <- t(data)
    data
  })
  file_path <- reactiveVal(NULL)
  save_data <- function(data) {
    file_name <- sprintf("%s_%s.csv", human_time(), digest::digest(data))
    new_file_path <- file.path(saved_roster, file_name)
    file_path(new_file_path)
    write.csv(x = data, file = new_file_path,
      row.names = FALSE, quote = TRUE
    )
  }
  roster_data <- reactive({
    if(!is.null(file_path()) && file.exists(file_path())){
      roster <- read.csv(file_path())
      formatted_data <- paste(
        "Quarterback: ", roster$myRosterQB,
        "<br> Running Back: ", roster$myRosterRB1,
        "<br> Running Back: ", roster$myRosterRB2,
        "<br> Wide Reciever: ", roster$myRosterWR1,
        "<br> Wide Reciever: ", roster$myRosterWR2,
        "<br> Tight End: ", roster$myRosterTE,
        "<br> Flex: ", roster$myRosterFlex,
        "<br> Defense/Special Team: ", roster$myRosterDST,
        "<br> Kicker: ", roster$myRosterK
      )
      formatted_data
    }
  })
  observeEvent(input$submit,{
    shinyjs::hide("roster_form")
    shinyjs::show("roster_submit_msg")
  })
  observeEvent(input$submit,{save_data(form_data())})
  output$saved_roster_info <- renderUI({ HTML(paste(roster_data())) })


  # ----------------------------------------------
  # RESUBMISSION HANDLING
  # ----------------------------------------------
  observeEvent(input$resubmit_, {
    shinyjs::show("roster_form")
    shinyjs::hide("roster_submit_msg")
  })


  # ----------------------------------------------
  # FETCHING PLAYER DATA
  # ----------------------------------------------
  player_mapping <- build_player_mapping()
  qb_pred_file <- read.csv("Data/qb_pred.csv")
  rb_pred_file <- read.csv("Data/rb_pred.csv")
  wr_pred_file <- read.csv("Data/wr_pred.csv")
  te_pred_file <- read.csv("Data/te_pred.csv")
  qb_pred <- data.frame(qb_pred_file)
  rb_pred <- data.frame(rb_pred_file)
  wr_pred <- data.frame(wr_pred_file)
  te_pred <- data.frame(te_pred_file)


  # ----------------------------------------------
  # PLAYER INFO DISPLAYS
  # ----------------------------------------------
  # neat player display
  
  #quarterback displays
  qb_pred_filtered <- reactive({
    req(input$myRosterQB)
    qb_name_mapping <- player_mapping[player_mapping$Full_name == input$myRosterQB, ]
    if (nrow(qb_name_mapping) == 0) { return(NULL) }
    qb_short_name <- qb_name_mapping$Short_name
    qb_pred[qb_pred$player_name == qb_short_name, ]
  })
  output$qb_pred_table <- renderDataTable({ qb_pred })
  output$qb_info <- renderUI({
    req(qb_pred_filtered())
    HTML(paste("Quarterback: ", input$myRosterQB))
  })
  output$qb_table <- renderDataTable({
    req(qb_pred_filtered())
    qb_table <- qb_pred_filtered() %>%
      select(
        pred_passing_yards, pred_passing_tds,
        pred_interceptions, pred_rushing_yards,
        pred_rushing_tds, pred_fantasy_points
      ) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Stat",
        values_to = "Value"
      ) %>%
      mutate(
        Stat = factor(Stat, levels = c(
          "pred_passing_yards", "pred_passing_tds",
          "pred_interceptions", "pred_rushing_yards",
          "pred_rushing_tds", "pred_fantasy_points"
        )),
        Stat_Label = c(
          "Predicted Passing Yards", "Predicted Passing Touchdowns",
          "Predicted Interceptions", "Predicted Rushing Yards",
          "Predicted Rushing Touchdowns", "Predicted Fantasy Points"
        )[as.numeric(Stat)],
        Value = round(Value, 2)
      )  # Round Value to 2 decimal places
    # Ensure "Predicted Fantasy Points" is always on top
    qb_table <- qb_table %>% arrange(desc(fct_reorder(Stat_Label, -abs(Value))))
    datatable(qb_table[, c("Value", "Stat_Label")], rownames = FALSE, options = list(dom = 't'))
  })
  # rb1 display
  rb1_pred_filtered <- reactive({
    req(input$myRosterRB1)
    rb1_name_mapping <-
      player_mapping[player_mapping$Full_name == input$myRosterRB1, ]
    if (nrow(rb1_name_mapping) == 0) { return(NULL) }
    rb1_short_name <- rb1_name_mapping$Short_name
    rb_pred[rb_pred$player_name == rb1_short_name, ]
  })
  output$rb_pred_table <- renderDataTable({ rb_pred })
  output$rb1_info <- renderUI({
    req(rb1_pred_filtered())
    HTML(paste("Running Back 1: ", input$myRosterRB1))
  })
  output$rb1_table <- renderTable({
    req(rb1_pred_filtered())
    rb1_table_data <- rb1_pred_filtered() %>%
      select(
        pred_rushing_yards, pred_rushing_tds,
        pred_rushing_fumbles_lost, receptions,
        pred_receiving_yards, pred_receiving_tds,
        pred_fantasy_points
      )
    as.data.frame(rb1_table_data)
  })
  # rb2 display
  rb2_pred_filtered <- reactive({
    req(input$myRosterRB2)
    rb2_name_mapping <-
      player_mapping[player_mapping$Full_name == input$myRosterRB2, ]
    if (nrow(rb2_name_mapping) == 0) { return(NULL) }
    rb2_short_name <- rb2_name_mapping$Short_name
    rb_pred[rb_pred$player_name == rb2_short_name, ]
  })
  output$rb2_info <- renderUI({
    req(rb2_pred_filtered())
    HTML(paste("Running Back 2: ", input$myRosterRB2))
  })
  output$rb2_table <- renderTable({
    req(rb2_pred_filtered())
    rb2_table_data <- rb2_pred_filtered() %>%
      select(
        pred_rushing_yards, pred_rushing_tds,
        pred_rushing_fumbles_lost, receptions,
        pred_receiving_yards, pred_receiving_tds,
        pred_fantasy_points
      )
    as.data.frame(rb2_table_data)
  })
  # wr1 display
  wr1_pred_filtered <- reactive({
    req(input$myRosterWR1)
    wr1_name_mapping <-
      player_mapping[player_mapping$Full_name == input$myRosterWR1, ]
    if (nrow(wr1_name_mapping) == 0) { return(NULL) }
    wr1_short_name <- wr1_name_mapping$Short_name
    wr_pred[wr_pred$player_name == wr1_short_name, ]
  })
  output$wr_pred_table <- renderDataTable({ wr_pred })
  output$wr1_info <- renderUI({
    req(wr1_pred_filtered())
    HTML(paste("Wide Receiver 1: ", input$myRosterWR1))
  })
  output$wr1_table <- renderTable({
    req(wr1_pred_filtered())
    wr1_table_data <- wr1_pred_filtered() %>%
      select(
        pred_receiving_yards, pred_receiving_tds,
        receptions, pred_fantasy_points
      )
    as.data.frame(wr1_table_data)
  })
  # wr2 display
  wr2_pred_filtered <- reactive({
    req(input$myRosterWR2)
    wr2_name_mapping <-
      player_mapping[player_mapping$Full_name == input$myRosterWR2, ]
    if (nrow(wr2_name_mapping) == 0) { return(NULL) }
    wr2_short_name <- wr2_name_mapping$Short_name
    wr_pred[wr_pred$player_name == wr2_short_name, ]
  })
  output$wr2_info <- renderUI({
    req(wr2_pred_filtered())
    HTML(paste("Wide Receiver 2: ", input$myRosterWR2))
  })
  output$wr2_table <- renderTable({
    req(wr2_pred_filtered())
    wr2_table_data <- wr2_pred_filtered() %>%
      select(
        pred_receiving_yards, pred_receiving_tds,
        receptions, pred_fantasy_points
      )
    as.data.frame(wr2_table_data)
  })
  # te display
  te_pred_filtered <- reactive({
    req(input$myRosterTE)
    te_name_mapping <-
      player_mapping[player_mapping$Full_name == input$myRosterTE, ]
    if (nrow(te_name_mapping) == 0) { return(NULL) }
    te_short_name <- te_name_mapping$Short_name
    te_pred[te_pred$player_name == te_short_name, ]
  })
  output$te_pred_table <- renderDataTable({ te_pred })
  output$te_info <- renderUI({
    req(te_pred_filtered())
    HTML(paste("Tight End: ", input$myRosterTE))
  })
  output$te_table <- renderTable({
    req(te_pred_filtered())
    te_table_data <- te_pred_filtered() %>%
      select(
        pred_receiving_yards, pred_receiving_tds,
        receptions, pred_fantasy_points
      )
    as.data.frame(te_table_data)
  })



  # ----------------------------------------------
  # SCHEDULE DISPLAY
  # ----------------------------------------------
  schedule <- as.data.frame(sched)
  output$schedule <- renderDataTable({ return(schedule) })


  # ----------------------------------------------
  # HOME PAGE INFORMATION
  # ----------------------------------------------
  output$intro_description <- renderUI({
    text <- HTML(paste(
      "<p>Fantasy football is a popular online game that allows ",
      "participants, known as fantasy owners, to create and manage ",
      "virtual football teams composed of real NFL players. The ",
      "objective is to assemble a roster that accumulates the most ",
      "points based on the statistical performance of the players ",
      "during actual NFL games. Fantasy owners draft their teams ",
      "before the start of the NFL season, selecting players from ",
      "various positions such as quarterbacks, running backs, ",
      "wide receivers, and defenses. As the NFL season progresses, ",
      "fantasy teams earn points for their players' achievements, ",
      "such as touchdowns, yardage gained, and defensive plays. ",
      "Fantasy football adds an extra layer of excitement and ",
      "engagement for fans, fostering competition, strategy, ",
      "and camaraderie among participants as they follow the ",
      "performances of their chosen players throughout the season. ",
      "The success of fantasy football lies in the ability of ",
      "fantasy owners to make strategic decisions, including ",
      "trades, waiver wire pickups, and lineup adjustments, ",
      "to outmaneuver their opponents and claim victory in their ",
      "fantasy leagues. </p>"
    ))
    return(text)
  })
  output$problem_description <- renderUI({
    text <- HTML(paste(
      "<p>Entering the realm of fantasy football can be challenging ",
      "for some individuals due to various factors. One common ",
      "barrier is a lack of familiarity with the intricacies of the ",
      "sport itself, as well as the complex statistical metrics that ",
      "underpin fantasy scoring. For those not well-versed in ",
      "American football, understanding player positions, scoring ",
      "rules, and the significance of various statistics can be ",
      "initially overwhelming. Additionally, the sheer volume of ",
      "player information, team dynamics, and injury updates may ",
      "seem daunting to newcomers. The statistical nature of fantasy ",
      "football, which involves analyzing player performance data ",
      "and predicting future outcomes, can also deter individuals ",
      "who are not accustomed to interpreting sports analytics. ",
      "Moreover, the competitive and strategic aspects of the game, ",
      "such as drafting a well-balanced team and making timely ",
      "roster decisions, can be intimidating for those without ",
      "prior experience. Overcoming these hurdles often requires a ",
      "gradual immersion into the game, coupled with guidance and ",
      "resources to help individuals grasp both the nuances of the ",
      "sport and the complexities of fantasy football strategy.</p>"
    ))
  })
  output$short_app_description <- renderUI({
    text <- HTML(paste(
      "<p>The Fantasy for Everyone app was meticulously designed ",
      "to overcome common barriers and make fantasy football accessible ",
      "to all. Our mission is to demystify fantasy by offering a ",
      "comprehensive resource for all educational needs related to the ",
      "game. We've developed our own predictive models, akin to those ",
      "used by major sports networks like ESPN, breaking down crucial ",
      "factors for various player positions, offensive/defensive ",
      "modeling, and external influences. The complexity of ",
      "statistical analysis is simplified, sparing users from ",
      "intricate math.</p>",
      "<p>With our app, users can effortlessly build their rosters ",
      "and assess team strengths and weaknesses, not just by ",
      "individual players but also by understanding which descriptive ",
      "features impact overall roster dynamics. The platform enables ",
      "users to explore past and upcoming games, delve into player ",
      "histories, and access data models to comprehend the ",
      "considerations behind different features. For those curious ",
      "about statistical methods, our app provides insights into ",
      "linear regression and mixed effects models, elucidating ",
      "how these techniques contribute to predictions. Additionally, ",
      "users can access in-depth information about the game and ",
      "various statistics, fostering a richer understanding of ",
      "fantasy football. </p>"
    ))
  })
  output$outro_description <- renderUI({
    text <- HTML(paste(
      "<p>The Fantasy for Everyone app offers a user-friendly ",
      "interface with key features to enhance the fantasy football ",
      "experience. On the left side, users can manage their roster ",
      "efficiently by selecting players or teams for each position ",
      "from a dropdown menu. Submissions and changes are easy with ",
      "the 'submit' and 'resubmit' options. The 'upcoming games' tab ",
      "provides an interactive schedule for the current year, ",
      "allowing users to access information on both upcoming and past ",
      "games, with detailed data for the latter, including weather ",
      "conditions. In the 'data' tab, users can find tables ",
      "featuring player point predictions by position, particularly ",
      "focusing on offensive player positions, offering insights ",
      "into passing yards, touchdowns, interceptions, and more. ",
      "The 'predictions' tab offers a weekly forecast for your ",
      "roster, presenting radar charts for offensive players, ",
      "indicating their expected performance. For those looking to ",
      "expand their knowledge, the 'learn' tab offers resources ",
      "on the game of American football, insights into the app's ",
      "code and algorithms, and information about the creators ",
      "behind the app. This comprehensive set of features aims to ",
      "make fantasy football accessible and enjoyable for everyone.</p>"
    ))
    return(text)
  })


  # ----------------------------------------------
  # LEARN MORE PAGE INFORMATION
  # ----------------------------------------------
  output$football_intro <- renderUI({
    text <- HTML(paste(
      "<p>American football, a quintessential part of U.S. sports ",
      "culture, is a dynamic and strategic team sport played between ",
      "two opposing teams on a rectangular field. The objective is ",
      "straightforward: advance an oval-shaped ball, primarily by ",
      "running or passing, across the opponent's goal line to score ",
      "points. The game is divided into four quarters, each lasting 15 ",
      "minutes (with potential overtime periods if the score is tied). ",
      "Teams have four downs (attempts) to advance the ball 10 yards; ",
      "if successful, they receive another set of downs. A team can ",
      "score six points by reaching the opponent's end zone, followed ",
      "by an opportunity to earn extra points through a touchdown ",
      "conversion or two-point conversion. Alternatively, teams can ",
      "score three points by kicking a field goal through the uprights. ",
      "Defensively, teams aim to prevent their opponents from scoring ",
      "through tackles, interceptions, and fumble recoveries. American ",
      "football is renowned for its strategic complexity, combining ",
      "physical prowess with intricate play designs, making it a ",
      "thrilling and highly watched sport in the United States.</p>",
      "<p>A regulation game is divided into four quarters, each ",
      "lasting 15 minutes. These quarters are essentially segments ",
      "of play that make up the entire duration of the game. At ",
      "the end of the first and third quarters, there is a break, ",
      "known as halftime, where teams leave the field and have a ",
      "longer rest period.</p>",
      "<p>If the score is tied at the end of the fourth quarter, ",
      "the game can proceed to overtime to determine a winner. Overtime ",
      "in American football is an additional period of play designed ",
      "to break a tie. In the NFL (National Football League) and some ",
      "other leagues, the overtime period is 10 minutes. Each team ",
      "gets an opportunity to possess the ball, starting from a ",
      "designated yard line. If the team possessing the ball scores ",
      "a touchdown on its possession, it wins the game. If the team ",
      "with the first possession scores a field goal, the other team ",
      "has a chance to possess the ball and either tie the game with ",
      "a field goal or win with a touchdown. If the game remains tied ",
      "after the first possession by each team, subsequent possessions ",
      "operate on a sudden-death basis, where the first team to score ",
      "any points wins the game. Overtime continues until a winner ",
      "is determined.</p>"
    ))
    return(text)
  })
  output$explaining_plays <- renderUI({
    text <- HTML(paste(
      "<p>In American football, a team has a series of four plays, ",
      "known as downs, to advance the ball a total of 10 yards ",
      "toward the opponent's end zone. The offensive team starts ",
      "each series of downs at the line of scrimmage, which is the ",
      "spot on the field where the ball was when the last play ended.</p>",
      "<p>Here's how the downs work: </p>",
      "<ol>",
      "   <li><strong>First Down: </strong>The offensive team begins ",
      "on first down with the goal of advancing the ball at least 10 ",
      "yards. If they successfully reach or exceed this distance, they ",
      "are awarded a new set of downs, meaning they get another four",
      " attempts to advance the ball.</li>",
      "   <li><strong>Unsuccessful Down: </strong>If the offensive ",
      "team fails to advance the ball 10 yards within the first ",
      "three downs, they face a critical decision on the fourth ",
      "down. They can choose to:",
      "         <ul>",
      "               <li><strong>Punt: </strong>Kick the ball to the ",
      "opposing team to gain field position.</li>",
      "               <li><strong>Try for a Field Goal: </strong>Kick ",
      "the ball through the goalposts to score three points.</li>",
      "               <li><strong>Go for it: </strong>Attempt to ",
      "gain the necessary yardage for a new set of downs.</li>",
      "         </ul></li>",
      "  <li><strong>Turnover on Downs: </strong>If the offensive ",
      "team doesn't achieve the required yardage on the fourth down ",
      "and chooses not to punt or kick a field goal, possession of ",
      "the ball is handed over to the opposing team at the spot ",
      "where the last play ended. The opposing team then begins ",
      "their offensive series of downs.",
      "</ol>",
      "<p>This system adds a strategic element to the game, as teams ",
      "must decide when to take risks and when to play it safe based ",
      "on the down and distance situation. It contributes to the ",
      "dynamic and strategic nature of American football.</p>"
    ))
  })
  output$explaining_scoring <- renderUI({
    text <- HTML(paste(
      "<p>In American football, scoring occurs through various ",
      "methods, and the primary objective is to accumulate points. ",
      "Here's an expansion on how teams can score:</p>",
      "<ol>",
      "     <li><strong>Touchdown (6 Points): </strong>The most ",
      "significant score in football is a touchdown. A team achieves ",
      "a touchdown by advancing the ball into the opponent's end zone, ",
      "which is the area between the goal line and the end line. When ",
      "a player carries the ball across the opponent's goal line or ",
      "catches a pass in the end zone, their team is awarded six points.",
      "         <ul><li><strong>Extra Points (1 or 2): </strong>After ",
      "scoring a touchdown, the scoring team has the option to attempt ",
      "an extra point. Traditionally, this involves kicking the ball ",
      "through the uprights from a short distance, earning the team an ",
      "additional point. However, teams can also opt for a two-point ",
      "conversion attempt, where they try to score from a closer ",
      "distance. A successful two-point conversion results in two ",
      "points instead of one. The decision between attempting an extra ",
      "point or a two-point conversion often depends on the game ",
      "situation and strategy.",
      "         </li></ul></li>",
      "     <li><strong>Field Goal (3 Points): </strong>When a team ",
      "is within a reasonable kicking distance from the opponent's ",
      "goalposts, they may attempt a field goal. A successful field ",
      "goal occurs when the kicker kicks the ball through the uprights. ",
      "This action earns the team three points. Field goals are ",
      "often attempted on fourth down if the team believes they can't ",
      "score a touchdown or if they are too far from the end zone for ",
      "a realistic scoring opportunity.",
      "     </li></ol>",
      "<p>These scoring methods add depth to the game, as teams ",
      "strategize on whether to go for a higher-risk, higher-reward ",
      "touchdown or a more conservative field goal. The variety of ",
      "scoring options contributes to the excitement and complexity ",
      "of American football.</p>"
    ))
    return(text)
  })
  output$explaining_defense <- renderUI({
    text <- HTML(paste(
      "<p>In American football, defense plays a crucial role in ",
      "preventing the opposing team from scoring. Here's a more ",
      "in-depth explanation of the defensive strategies and actions:</p>",
      "<ol>",
      "    <li><strong>Tackles: </strong>One of the fundamental ",
      "defensive maneuvers is the tackle. Defensive players, such as ",
      "linebackers, defensive linemen, and defensive backs, strive to ",
      "bring down the ball carrier from the opposing team. A successful ",
      "tackle involves stopping the player with possession of the ball ",
      "by physically bringing them to the ground. Tackling is a ",
      "fundamental skill in football and is essential for halting the ",
      "progress of the offensive team.</li>",
      "    <li><strong>Interceptions: </strong>Interceptions occur ",
      "when a defensive player catches a pass thrown by the quarterback ",
      "of the offensive team. This not only prevents the intended ",
      "receiver from making a play but also gives possession of the ",
      "ball to the defensive team. Interceptions can be game-changing ",
      "moments, as they can lead to a sudden shift in momentum and a ",
      "potential scoring opportunity for the defensive team.</li>",
      "    <li><strong>Fumble Recoveries: </strong>A fumble happens ",
      "when a player who has possession of the ball loses it due to ",
      "a mishandle or a defensive player's intervention. Defensive ",
      "players actively attempt to cause fumbles by tackling ",
      "aggressively or using strategic maneuvers to jar the ball loose. ",
      "Recovering a fumble involves securing possession of the loose ",
      "ball. Like interceptions, fumble recoveries give the defensive ",
      "team a chance to take control of the game and potentially ",
      "score.</li>",
      "    <li><strong>Sacks: </strong>A sack occurs when a defensive ",
      "player tackles the quarterback behind the line of scrimmage ",
      "before they can release a pass. Sacks are a significant ",
      "defensive achievement as they result in a loss of yards for the ",
      "offensive team and can disrupt their offensive rhythm.</li>",
      "    <li><strong>Pass Breakups and Deflections: </strong>Defensive ",
      "players in the secondary, such as cornerbacks and safeties, aim ",
      "to break up or deflect passes to prevent completions. This ",
      "disrupts the offensive flow and reduces the chances of the ",
      "opposing team moving the ball effectively.</li>",
      "    <li><strong>Goal Line Stands: </strong>When the opposing ",
      "team is near the end zone, the defensive team may engage in ",
      "a 'goal line stand' — a collective effort to prevent the ",
      "offensive team from scoring a touchdown in a critical ",
      "situation</li></ol>",
      "<p>Overall, defensive strategies involve a combination of ",
      "physicality, agility, and strategic decision-making to thwart ",
      "the opponent's scoring attempts and create opportunities for ",
      "the defensive team to take control of the game.</p>"
    ))
    return(text)
  })

  
  # ----------------------------------------------
  # INTERACTIVE MODEL DISPLAYS
  # ----------------------------------------------

}
