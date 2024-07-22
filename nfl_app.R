library(tidyverse)
library(nflfastR)
library(shiny)

# Hello and welcome to the code for my NFL player comparison shiny app!  
# You can use the app at https://kylestone225.shinyapps.io/nfl_player_comps/.
# Due to ram usage limits the 'shiny_position_car' summary tibbles
# were saved as separate .csv files and called with a read.csv() function in the 
# final version of this code used in the hosted shinyapps.io site. Below is the 
# full code used for wrangling and presenting the data. 
# I hope you find the app enjoyable!!



# First put our master data tables used for each tab:
# Pass Plays and Run Plays

pbp_pass_14_23 <- load_pbp(2014:2023) |>
  filter(play_type == "pass" & !is.na(down) & !is.na(passer_id) & !is.na(receiver_id)
         & season_type == "REG" & !is.na(pass_location)) |>
  mutate(passing_yards = ifelse(is.na(passing_yards), 0, passing_yards),
         air_yards = ifelse(is.na(air_yards), 0, air_yards),
         air_yards = ifelse(passing_yards == 0, 0, air_yards),
         receiving_yards = ifelse(is.na(receiving_yards), 0, receiving_yards),
         pyoe = resid(lm(passing_yards ~ 1 + down + ydstogo + down:ydstogo +
                           yardline_100 + pass_location + air_yards + score_differential)),
         rec_yoe = resid(lm(receiving_yards ~ 1 + down + ydstogo + down:ydstogo +
                              yardline_100 + pass_location + air_yards + score_differential)),
         ayoe = resid(lm(air_yards ~ 1 + down + ydstogo + down:ydstogo +
                           yardline_100 + pass_location + score_differential)))



pbp_rush_14_23 <- load_pbp(2014:2023) |>
  filter(play_type == "run" & !is.na(down) & !is.na(rusher_id) & season_type == "REG"
         & !is.na(run_location)) |>
  mutate(rushing_yards = ifelse(is.na(rushing_yards), 0, rushing_yards),
         ryoe = resid(lm(rushing_yards ~ 1 + down + ydstogo + down:ydstogo +
                           yardline_100 + run_location + score_differential)))

team_colors <- teams_colors_logos |>
  rename(posteam = team_abbr)

# Example Page

shiny_passers_ex <- pbp_pass_14_23 |>
  group_by(passer_id, passer, game_id) |>
  summarize(Atts = n(),
            Comps = sum(complete_pass),
            Comp_Pct = (sum(complete_pass)/n()),
            PYOE = sum(pyoe),
            TDs = sum(pass_touchdown)) |>
  mutate(cargame = ave(seq_along(passer_id), passer_id, FUN = seq_along))


pos_def_ex <- pbp_pass_14_23 |>
  group_by(passer_id, game_id, posteam, defteam, season) |>
  summarize()

shiny_passers_ex <- merge(shiny_passers_ex, pos_def_ex, by = c("passer_id", "game_id"))
shiny_passers_ex <- left_join(shiny_passers_ex, team_colors[,c(1,7:10)], by = "posteam")


# Create a stat summary tibble for each position group.  First QBS:
shiny_passers_car <- pbp_pass_14_23 |>
  group_by(passer_id, passer, game_id) |>
  summarize(Atts = n(),
            Comps = sum(complete_pass),
            Comp_Pct = (sum(complete_pass)/n()),
            YPC = mean(passing_yards),
            Pass_Yds = sum(passing_yards),
            TDs = sum(pass_touchdown),
            PYOE_Per = mean(pyoe),
            Total_PYOE = sum(pyoe),
            Air_Yds_Per = mean(air_yards),
            Total_AYs = sum(air_yards),
            AYOE_Per = mean(ayoe),
            Total_AYOE = sum(ayoe),
            EPA_Per = mean(epa),
            Total_EPA = sum(epa),
            Comp_Pct_OE = mean(cpoe),
            RecYds_OE_Per = mean(rec_yoe),
            Total_RecYds_OE = sum(rec_yoe))

# adding in relevent vectors
pos_def <- pbp_pass_14_23 |>
  group_by(passer_id, game_id, posteam, defteam, season) |>
  summarize()

# Joining and adding color vectors for plotting
shiny_passers_car <- merge(shiny_passers_car, pos_def, by = c("passer_id", "game_id"))
shiny_passers_car <- left_join(shiny_passers_car, team_colors[,c(1,7:10)], by = "posteam")




# Running Backs

shiny_backs <- pbp_rush_14_23 |>
  group_by(rusher_id, rusher, game_id) |>
  summarize(Carries = n(),
            YPC = mean(rushing_yards),
            Rush_Yds = sum(rushing_yards),
            TDs = sum(rush_touchdown),
            RYOE_Per = mean(ryoe),
            Total_RYOE = sum(ryoe),
            EPA_Per = mean(epa),
            Total_EPA = sum(epa))


rb_pos_def <- pbp_rush_14_23 |>
  group_by(rusher_id, game_id, posteam, defteam, season) |>
  summarise()

shiny_backs_car <- merge(shiny_backs, rb_pos_def, by = c("rusher_id", "game_id"))
shiny_backs_car <- left_join(shiny_backs_car, team_colors[,c(1,7:10)], by = "posteam")

# Creating a unique player name identifier to overcome duplicates in the 'rusher' vector.
# First create a unique vector combining player name with last digit of their ID.

shiny_backs_car <- shiny_backs_car |>
  mutate(pro_name = paste(rusher, substr(rusher_id, 10,10), sep = ""))

# Run an aggregation to find the most current team for each pro_name
pro_rb_name <- as.data.frame(aggregate(posteam ~ pro_name, data = shiny_backs_car, FUN = min))

# Clean and merge to create the name vector you will use to select players.
pro_rb_name <- pro_rb_name |>
  mutate(p_name = paste(sub("\\d", "", pro_name), posteam, sep = " "))

pro_rb_name$posteam <- NULL

shiny_backs_car <- left_join(shiny_backs_car, pro_rb_name, by = "pro_name")

# Wide Receivers

shiny_receivers_car <- pbp_pass_14_23 |>
  group_by(receiver_id, receiver, game_id) |>
  summarize(Receptions = n(),
            YPC = mean(receiving_yards),
            Rec_Yds = sum(receiving_yards),
            TDs = sum(touchdown),
            RECYOE_Per = mean(rec_yoe),
            Total_RECYOE = sum(rec_yoe),
            Air_Yds_Per = mean(air_yards),
            Total_AYs = sum(air_yards),
            AYOE_Per = mean(ayoe),
            Total_AYOE = sum(ayoe),
            EPA_Per = mean(epa),
            Total_Epa = sum(epa),
            CPOE = mean(cpoe))


rec_pos_def <- pbp_pass_14_23 |>
  group_by(receiver_id, game_id, posteam, defteam, season) |>
  summarise()

shiny_receivers_car <- merge(shiny_receivers_car, rec_pos_def, by = c("receiver_id", "game_id"))
shiny_receivers_car <- left_join(shiny_receivers_car, team_colors[,c(1,7:10)], by = "posteam")


# Repeat steps to handle duplicate receiver names.
shiny_receivers_car <- shiny_receivers_car |>
  mutate(pro_name = paste(receiver, substr(receiver_id, 10,10), sep = ""))

pro_wr_name <- as.data.frame(aggregate(posteam ~ pro_name, data = shiny_receivers_car, FUN = min))

pro_wr_name <- pro_wr_name |>
  mutate(p_name = paste(sub("\\d", "", pro_name), posteam, sep = " "))

pro_wr_name$posteam <- NULL

shiny_receivers_car <- left_join(shiny_receivers_car, pro_wr_name, by = "pro_name")



ui <- fluidPage(
  tabsetPanel(
    tabPanel("Welcome",
             tags$h3("Compare NFL Players With Paint Splatter Charts!"),
             tags$p("The purpose of this app is simple: For the user to quickly and easily intuit performance of
     one player in a statistical category, over a time series, relative to a player grouping.
     The data presented is comprised of NFL play by play data covering the 2014 - 2023 seasons. 
     Users can select a time series, one or multiple players, and a statistic.  The app will then render an interactive 
     plot like the one seen below."),
             tags$br(),
             tags$p("Time series can be called in two ways:"),
             tags$p("1. Select one or more seasons from the season selection box, click the 'SET SEASONS' button to 
        toggle the available players for those seasons, select your players and stat, and finally click the 'COMPARE' button.
        (click 'RESET' to reset available players to ALL)."),
             tags$b("OR"),
             tags$p("2. Simply select the players and statistic you would like compared, set your slider inputs, and 
        click the 'COMPARE' button to see career data unbound by seasons. If comparing full careers, it is NOT necessary to be precise on the right
                    slider input.  Simply max it, or casually set it in the far right, and the plot will render the correct x-axis."),
             tags$br(),
             tags$p("The produced plot will be a point chart.  The x-axis is your chosen time series of the player's career,
     the y-axis maps the statistical input you chose, and the size of the point is dependent on the 
     number of touchdowns thrown for, caught, or rushed for. Linear regression trend lines are present to give
     a sense of if a player is improving in the statistical category over the selected time series. 
     These are particularly useful for gauging young players."),
             tags$br(),
             tags$p("The plots are interactive! Click and drag your cursor around a grouping of points and a table will render 
     below the plot with full statistical detail on all of the games within your grouping."),
             tags$hr(),
             fluidRow(
               column(12,
                      plotOutput("explot", brush = "ex_plot_brush")
               ),
               column(12,
                      tableOutput("extable")
               )
             )
    ),
    
    tabPanel("Quarterbacks",
             column(width = 6,
                    selectInput("qb", "Select Passers", shiny_passers_car$passer, selected = "J.Hurts", multiple = TRUE),
                    selectInput("qbstat", "Stat To Compare", names(shiny_passers_car[,c(5:21)]), selected = "Pass_Yds"),
                    actionButton("qbcomp", "COMPARE")
             ),
             
             column(width = 6,
                    selectInput("qbseason", "Seasons", c("", 2014:2023), selected = "",  multiple = TRUE),
                    actionButton("setqb", "SET SEASONS"),
                    actionButton("clearqb", "RESET PASSERS"),
                    sliderInput("qbslider", "Number of Games", value = c(0,50), min = 0, max = 200)
             ),
             
             fluidRow(
               column(12,
                      plotOutput("qbsplat", brush = "plot_brush")
               ),
               column(12, 
                      tableOutput("qbtable")
               )
             )
             
    ),
    tabPanel("Receivers",
             column(6,
                    selectInput("wr", "Select Receivers", shiny_receivers_car$p_name, multiple = TRUE),
                    selectInput("wrstat", "Stat To Compare", names(shiny_receivers_car[, c(5:17)])),
                    actionButton("wrcomp", "COMPARE")
             ),
             
             column(6,
                    selectInput("wrseason", "Seasons", c("", 2014:2023), selected = "", multiple = TRUE),
                    actionButton("setwr", "SET SEASONS"),
                    actionButton("clearwr", "RESET RECEIVERS"),
                    sliderInput("wrslider", "Number of Games", value = c(0,50), min = 0, max = 200)
             ),
             
             fluidRow(
               column(12,
                      plotOutput("wrsplat", brush = "wr_plot_brush")
               ),
               column(12,
                      tableOutput("wrtable")
               )
             )
    ),
    tabPanel("Rushers",
             column(6,
                    selectInput("rb", "", shiny_backs_car$p_name, multiple = TRUE),
                    selectInput("rbstat", "Stat To Compare", names(shiny_backs_car[,c(5:12)])),
                    actionButton("rbcomp", "COMPARE")
             ),
             column(6,
                    selectInput("rbseason", "Seasons", c("", 2014:2023), selected = "", multiple = TRUE),
                    actionButton("setrb", "SET SEASONS"),
                    actionButton("clearrb", "RESET BACKS"),
                    sliderInput("rbslider", "Number of Games", value = c(0,50), min = 0, max = 200)
             ),
             
             fluidRow(
               column(12,
                      plotOutput("rbsplat", brush = "rb_plot_brush")
               ),
               column(12,
                      tableOutput("rbtable")
               )
             )
    )
  )
)


server <- function(input, output, session) {
  # Start with the homepage tab with the example plot and brush table.
  top_five <- c("J.Burrow", "T.Lawrence", "P.Mahomes", "J.Herbert", "L.Jackson")
  
  output$explot <- renderPlot({
    shiny_passers_ex |>
      filter(passer %in% top_five) |>
      ggplot(aes(cargame, PYOE, size = TDs, color = factor(passer))) +
      geom_point() +
      stat_smooth(method = "lm", show.legend = FALSE) +
      labs(title = "Career Comparison Top Five Highest Paid NFL QBs (Total Value)",
           subtitle = "Passing Yards Over Expected and Passing Touchdowns Per Game") +
      xlab("Career Game") + ylab("Passing Yards OE") +
      scale_color_manual(name = "QB",
                         values = qbcol) +
      scale_size_identity(name = "TDs",
                          guide = "legend") +
      theme_classic()
  })
  
  output$extable <- renderTable({
    brushedPoints(filter(shiny_passers_ex[,c(3:10)], passer %in% top_five), input$ex_plot_brush)
  })
  
  # Let's build out each tab in order starting with QBs:
  
  
  qbvals <- reactiveValues(
    df = data.frame()
  )
  qbvals$df <- as.data.frame(shiny_passers_car)
  
  
  qbs <- eventReactive(input$qbcomp,{
    if(any(c(2014:2023) %in% input$qbseason) == TRUE){
      qbvals$df |>
        filter(passer %in% input$qb & season %in% input$qbseason) |>
        mutate(cargame = ave(seq_along(passer_id), passer_id, FUN = seq_along)) 
      
      
    }else{
      qbvals$df |>
        mutate(cargame = ave(seq_along(passer_id), passer_id, FUN = seq_along)) |>
        filter(passer %in% input$qb & between(cargame, input$qbslider[1], input$qbslider[2]))
    }
  })
  
  observeEvent(input$clearqb,{
    updateSelectInput(session, "qb", choices = shiny_passers_car$passer)
    updateSelectInput(session, "qbseason", choices = c("", 2014:2023), selected = "")
  })
  
  
  qbszn <- eventReactive(input$setqb,{
    filter(qbvals$df, season %in% input$qbseason)
  })
  
  observeEvent(input$setqb,{
    updateSelectInput(session, "qb", choices = qbszn()$passer)
  })
  
  
  
  qbcol <- as.character(shiny_passers_car$team_color) 
  names(qbcol) <- as.character(shiny_passers_car$passer)
  
  # Build your plot
  # Making a choice to keep the input$stat non reactive so the plot changes by clicking a new stat
  # in the text input without having to click the action button each time.
  output$qbsplat <- renderPlot({
    qbs() |>
      ggplot(aes(x = cargame, y = .data[[input$qbstat]], size = TDs, color = factor(passer))) +
      geom_point() +
      stat_smooth(aes(cargame, .data[[input$qbstat]]), method = "lm", show.legend = FALSE) +
      labs(title = "Quarterback Paint Splatter Comparison",
           subtitle = paste0(input$qbstat, " and Passing Touchdowns Per Game")) +
      xlab("Week") + ylab(input$qbstat) +
      scale_color_manual(name = "Qbs",
                         values = qbcol) +
      scale_size_identity(name = "TDs",
                          guide = "legend") +
      theme_classic()
  })
  
  
  output$qbtable <- renderTable({
    brushedPoints(qbs()[,c(3:21,23,29)], input$plot_brush, yvar = input$qbstat)
  })
  
  
  
  # Wide Receivers
  wrvals <- reactiveValues(
    df = data.frame()
  )
  wrvals$df <- as.data.frame(shiny_receivers_car)
  
  wrs <- eventReactive(input$wrcomp,{
    if(any(c(2014:2023) %in% input$wrseason) == TRUE){
      wrvals$df |>
        filter(p_name %in% input$wr & season %in% input$wrseason) |>
        mutate(cargame = ave(seq_along(receiver_id), receiver_id, FUN = seq_along))
      
    }else{
      wrvals$df |>
        mutate(cargame = ave(seq_along(receiver_id), receiver_id, FUN = seq_along)) |>
        filter(p_name %in% input$wr & between(cargame, input$wrslider[1], input$wrslider[2]))
    }
  })
  
  observeEvent(input$clearwr,{
    updateSelectInput(session, "wr", choices = shiny_receivers_car$p_name)
    updateSelectInput(session, "wrseason", choices = c("", 2014:2023), selected = "")
  })
  
  
  wrszn <- eventReactive(input$setwr,{
    filter(wrvals$df, season %in% input$wrseason)
  })
  
  observeEvent(input$setwr,{
    updateSelectInput(session, "wr", choices = wrszn()$p_name)
  })
  
  wrcol <- as.character(shiny_receivers_car$team_color) 
  names(wrcol) <- as.character(shiny_receivers_car$p_name)
  
  output$wrsplat <- renderPlot(
    wrs() |>  
      ggplot(aes(x = cargame, y = .data[[input$wrstat]], size = TDs, color = factor(p_name))) +
      geom_point() +
      stat_smooth(aes(cargame, .data[[input$wrstat]]), method = "lm", show.legend = FALSE) +
      labs(title = "Wide Receiver Paint Splatter Comparison ",
           subtitle = paste0(str_to_title(input$wrstat), " and Receiving Touchdowns Per Game")) +
      xlab("Career Game") + ylab(str_to_title(input$wrstat)) +
      scale_color_manual(name = "WRs",
                         values = wrcol) +
      scale_size_identity(name = "TDs",
                          guide = "legend") +
      theme_classic()
  )
  
  output$wrtable <- renderTable({
    brushedPoints(wrs()[,c(3:17,27)], input$wr_plot_brush, yvar = input$wrstat)
  })
  
  # Running Backs 
  
  rbvals <- reactiveValues(
    df = data.frame()
  )
  rbvals$df <- as.data.frame(shiny_backs_car)
  
  rbs <- eventReactive(input$rbcomp,{
    if(any(c(2014:2023) %in% input$rbseason) == TRUE){
      rbvals$df |>
        filter(p_name %in% input$rb & season %in% input$rbseason) |>
        mutate(cargame = ave(seq_along(rusher_id), rusher_id, FUN = seq_along)) 
      
      
    }else{
      rbvals$df |>
        mutate(cargame = ave(seq_along(rusher_id), rusher_id, FUN = seq_along)) |>
        filter(p_name %in% input$rb & between(cargame, input$rbslider[1], input$rbslider[2]))
    }
  })
  
  observeEvent(input$clearrb,{
    updateSelectInput(session, "rb", choices = shiny_backs_car$p_name)
    updateSelectInput(session, "rbseason", choices = c("", 2014:2023), selected = "")
  })
  
  
  rbszn <- eventReactive(input$setrb,{
    filter(rbvals$df, season %in% input$rbseason)
  })
  
  observeEvent(input$setrb,{
    updateSelectInput(session, "rb", choices = rbszn()$p_name)
  })
  
  
  rbcol <- as.character(shiny_backs_car$team_color) 
  names(rbcol) <- as.character(shiny_backs_car$p_name)
  
  
  output$rbsplat <- renderPlot(
    rbs() |>  
      ggplot(aes(x = cargame, y = .data[[input$rbstat]], size = TDs, color = factor(p_name))) +
      geom_point() +
      stat_smooth(aes(cargame, .data[[input$rbstat]]), method = "lm", show.legend = FALSE) +
      labs(title = "Running Back Paint Splatter Comparison",
           subtitle = paste0(str_to_title(input$rbstat), " and Rushing Touchdowns Per Game")) +
      xlab("Career Game") + ylab(str_to_title(input$rbstat)) +
      scale_color_manual(name = "RBs",
                         values = rbcol) +
      scale_size_identity(name = "TDs",
                          guide = "legend") +
      theme_classic()
  )
  
  output$rbtable <- renderTable({
    brushedPoints(rbs()[, c(3:11,22)], input$rb_plot_brush, yvar = input$rbstat)
  })
}

