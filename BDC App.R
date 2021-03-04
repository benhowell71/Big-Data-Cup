library(shiny)
library(tidyverse)
library(DT)
library(plotly)

lb <- read.csv("leaders.csv") %>%
  mutate(All = "All",
         Team = ifelse(Team == "Olympic (Women) - Canada", "Canada",
                       ifelse(Team == "Olympic (Women) - United States", "USA",
                              ifelse(Team == "Olympic (Women) - Finland", "Finland",
                                     ifelse(Team == "Olympic (Women) - Olympic Athletes from Russia", "Russia", as.character(Team))))))

teams <- unique(as.character(lb$Team))

lb[is.na(lb)] <- 0

results <- read.csv("results.csv")

goal_x <- c(188, 190, 190, 188)
goal_y <- c(40, 40, 45, 45)

goal <- data.frame(goal_x, goal_y)

med <- median(results$xAV)

shots <- read.csv("shots.csv")
non_passes <- read.csv("non_passes_shots.csv")
passes <- read.csv("pass_cluster.csv")

data <- rbind(shots, non_passes, passes) %>%
  arrange(row_num)

zone <- data %>%
  filter(Event %in% c("Goal", "Shot", "Play", "Incomplete Play")) %>%
  #not considering dump in/outs b/c intentionally conceding possession
  group_by(id) %>%
  count(Event) %>%
  spread(Event, n, fill = 0) %>%
  mutate(Events = Goal + `Incomplete Play` + Shot + Play,
         Shots = Goal + Shot,
         #creating separate shots variable makes shooting% calculation easier
         Shot_pct = round((Shots / Events)*100, digits = 0),
         Move_pct = round(((`Incomplete Play` + Play) / Events)*100, digits = 0),
         #Shot_pct + Move_pct should equal 1
         Make_pct = round((Goal / Shots)*100, digits = 0),
         Make_pct = ifelse(Make_pct == "NaN", 0, Make_pct),
         #cleaning up the Make_pct variable so that we have 0s instead of NaNs
         AVxy = round(Shot_pct * Make_pct, digits = 5)) %>%
  dplyr::select(id, Shot_pct, Make_pct, Move_pct)

results <- right_join(results, zone, by = "id")

#movement between zones
movement <- data %>%
  filter(Event %in% c("Goal", "Shot", "Play", "Incomplete Play")) %>%
  #not considering dump in/outs b/c intentionally conceding possession
  group_by(id) %>%
  count(zone_next) %>%
  #using zone_next b/c it accounts for turnovers
  #since we want to know how often someone tries to pass from Zone X to Zone Y
  #knowing where they were aimed helps, even if the pass was taken away
  filter(! is.na(zone_next))

obs <- data %>%
  filter(! is.na(zone_next)) %>%
  ungroup() %>%
  count(id) %>%
  mutate(Obs = n) %>%
  dplyr::select(-n)

movement <- right_join(movement, obs, by = c("id"))
#enables us to find out how often someone went from Zone id to the next zone
#essentially: movement / total movements from that zone

movement <- movement %>%
  mutate(pct_of_moves = round(n / Obs, digits = 5)) %>%
  dplyr::select(id, zone_next, pct_of_moves)

df <- right_join(results, movement, by = "id")

next_res <- results %>%
  dplyr::select(id, x:y_max) %>%
  rename_all(paste0, "_next")

df <- right_join(df, next_res, by = c("zone_next" = "id_next"))

ui <- fluidPage(
  navbarPage("Big Data Cup Leaderboard",
             tabPanel("Movement Frequency and xAV Leaderboard",
                      sidebarLayout(
                        sidebarPanel(sliderInput("zone", "Select a Zone:",
                                                 min = 1, max = 70, value = 34, step = 1),
                                     wellPanel(
                                       p("Use slider above to toggle between zones. The zone outlined in black
                                        is the zone you have currently selected. The white-red gradient represents
                                        how frequently a player moves from your current zone to another zone."),
                                       p("The Move, Shot, Make numbers below represent how often a player moved, shot,
                                        or, given a shot, made the shot from your selected zone.")
                                     ),
                                     verbatimTextOutput("info"),
                                     width = 5),
                        mainPanel(plotOutput("new", width = 600, height = 400), 
                                  width = 7)),
                      hr(style = "border-color:black;"),
                      column(selectInput("league", "Select a League",
                                         choices = c("NWHL", "BDC"), selected = "NWHL"), width = 4),
                      column(selectInput("team", "Select a Team", 
                                         choices = c("All", teams),
                                         selected = "All"), width = 4),
                      column(sliderInput("min_events", "Minimum Number of Events",
                                         min = 0, max = max(lb$Events), value = 150, step = 10), width = 4),
                      DTOutput("leaderboard"),
                      hr(style = "border-color:black;")
             )#,
             #tabPanel("xAV Plot",
             #plotOutput("xav_plot"),
             #plotlyOutput("reactive_plot", width = 600, height = 400),
             
             #)
  )
)

server <- function(input, output, session) {
  
  updateSelectInput(session,
                    "league", choices = c("NWHL", "BDC"))
  
  observeEvent(input$league,
               {
                 use <- lb %>%
                   filter(League %in% c(input$league)) %>%
                   dplyr::select(All, Team)
                 
                 updateSelectInput(session,
                                   "team",
                                   choices = c("All", unique(use$Team)))
                 
               })
  
  output$leaderboard <- renderDT(
    lb %>%
      filter(League %in% c(input$league) & (Team %in% c(input$team) | All == input$team) &
               Events >= input$min_events) %>%
      dplyr::select(-League, -All, -Shot.) %>%
      mutate_at(vars(xAV, xAV_100, xG, xg_100), funs(round(., 2))) %>%
      dplyr::select(Team, Player, Events, Shot, Goal, xAV, xAV_100, xG, xg_100) %>%
      arrange(desc(xAV_100)) %>%
      datatable(extensions = "Responsive", 
                rownames = FALSE,
                colnames = c("Team", "Player", "Events", "Shots", "Goals", "xAV",
                             "xAV/100", "xG", "xG/100")) %>%
      formatStyle(c(1, 2, 5, 7), `border-right` = "solid 1px")
  )
  
  output$xav_plot <- renderPlot(
    results %>%
      ggplot() +
      geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, color = xAV, fill = xAV)) +
      scale_color_gradient2(low = "lightblue", mid = "white", midpoint = med, high = "red") +
      scale_fill_gradient2(low = "lightblue", mid = "white", midpoint = med, high = "red")  +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank()) +
      geom_segment(aes(x = 25 - 100, xend = 175 - 100, y = 0 - 42.5, yend = 0 - 42.5)) +
      geom_segment(aes(x = 25 - 100, xend = 175 - 100, y = 85 - 42.5, yend = 85 - 42.5)) +
      geom_segment(aes(x = 0 - 100, xend = 0 - 100, y = 20 - 42.5, yend = 60 - 42.5)) +
      geom_segment(aes(x = 200 - 100, xend = 200 - 100, y = 20 - 42.5, yend = 60 - 42.5)) +
      geom_curve(aes(x = 0 - 100, xend = 25 - 100, y = 60 - 42.5, yend = 85 - 42.5),
                 curvature = -0.32) +
      geom_curve(aes(x = 0 - 100, xend = 25 - 100, y = 20 - 42.5, yend = 0 - 42.5),
                 curvature = 0.32) +
      geom_curve(aes(x = 175 - 100, xend = 200 - 100, y = 0 - 42.5, yend = 20 - 42.5),
                 curvature = 0.32) +
      geom_curve(aes(x = 175 - 100, xend = 200 -100, y = 85 - 42.5, yend = 60 - 42.5),
                 curvature = -0.32)  +
      geom_path(data = goal, aes(x = goal_x - 100, y = goal_y - 42.5)) +
      geom_segment(aes(x = 100 - 100, xend = 100 - 100, y = 85 - 42.5, yend = 0 - 42.5), size = 2, color = "red")  +
      labs(title = "Average xAV by Location",
           caption = "Ben Howell | @benhowell71 | benhowell71.com",
           fill = "xAV") +
      geom_path(data = goal, aes(x = goal_x - 100, y = goal_y - 42.5), size = 2) +
      theme(panel.grid = element_blank(), 
            axis.text = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            plot.caption = element_text(hjust = 0.5, face = "italic", size = 10),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none"), 
    width = 600, height = 400)
  
  # output$val <- renderPrint(
  #     hover <- input$plot_hover
  #     
  #     x <- nearPoints(results, input$plot_hover)
  # )
  
  # output$reactive_plot <- renderPlotly(
  #     results %>%
  #         ggplot() +
  #         geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = xAV), color = "lightgrey") +
  #         scale_color_gradient2(low = "lightblue", mid = "white", midpoint = med, high = "red") +
  #         scale_fill_gradient2(low = "lightblue", mid = "white", midpoint = med, high = "red")  +
  #         theme_minimal() +
  #         theme(panel.grid = element_blank(),
  #               axis.text = element_blank(),
  #               axis.title = element_blank()) +
  #         geom_segment(aes(x = 25 - 100, xend = 175 - 100, y = 0 - 42.5, yend = 0 - 42.5)) +
  #         geom_segment(aes(x = 25 - 100, xend = 175 - 100, y = 85 - 42.5, yend = 85 - 42.5)) +
  #         geom_segment(aes(x = 0 - 100, xend = 0 - 100, y = 20 - 42.5, yend = 60 - 42.5)) +
  #         geom_segment(aes(x = 200 - 100, xend = 200 - 100, y = 20 - 42.5, yend = 60 - 42.5)) +
  #         geom_curve(aes(x = 0 - 100, xend = 25 - 100, y = 60 - 42.5, yend = 85 - 42.5),
  #                    curvature = -0.32) +
  #         geom_curve(aes(x = 0 - 100, xend = 25 - 100, y = 20 - 42.5, yend = 0 - 42.5),
  #                    curvature = 0.32) +
  #         geom_curve(aes(x = 175 - 100, xend = 200 - 100, y = 0 - 42.5, yend = 20 - 42.5),
  #                    curvature = 0.32) +
  #         geom_curve(aes(x = 175 - 100, xend = 200 -100, y = 85 - 42.5, yend = 60 - 42.5),
  #                    curvature = -0.32)  +
  #         geom_path(data = goal, aes(x = goal_x - 100, y = goal_y - 42.5)) +
  #         geom_segment(aes(x = 100 - 100, xend = 100 - 100, y = 85 - 42.5, yend = 0 - 42.5), size = 2, color = "red")  +
  #         labs(title = "Hover xAV by Location",
  #              caption = "Ben Howell | @benhowell71 | benhowell71.com",
  #              fill = "xAV") +
  #         geom_path(data = goal, aes(x = goal_x - 100, y = goal_y - 42.5), size = 2) +
  #         theme(panel.grid = element_blank(), 
  #               axis.text = element_blank(),
  #               axis.title = element_blank(),
  #               plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
  #               plot.caption = element_text(hjust = 0.5, face = "italic", size = 10),
  #               plot.subtitle = element_text(hjust = 0.5),
  #               legend.position = "none")#,
  #     
  #     # fig <- ggplotly(p), 
  #     # 
  #     # fig %>%
  #     #     layout(hoverinfo = 'text',
  #     #           text = ~paste('</br> Move: ', Move_pct, "%",
  #     #                         '</br> Shot: ', Shot_pct, "%",
  #     #                         '</br> Make: ', Make_pct, "%")))
  # )
  
  output$new <- renderPlot(
    df %>%
               filter(id == input$zone) %>%
               mutate(pct_of_moves = round(pct_of_moves * 100, digits = 1)) %>%
               ggplot() +
               geom_rect(aes(xmin = x_min_next, xmax = x_max_next, ymin = y_min_next, ymax = y_max_next, fill = pct_of_moves, color = pct_of_moves)) +
               geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max), size = 1, color = "black", fill= NA) +
               scale_color_gradient2(low = "lightblue", mid = "white", midpoint = med, high = "red") +
               scale_fill_gradient2(low = "lightblue", mid = "white", midpoint = med, high = "red")  +
               theme_minimal() +
               geom_segment(aes(x = 25 - 100, xend = 175 - 100, y = 0 - 42.5, yend = 0 - 42.5)) +
               geom_segment(aes(x = 25 - 100, xend = 175 - 100, y = 85 - 42.5, yend = 85 - 42.5)) +
               geom_segment(aes(x = 0 - 100, xend = 0 - 100, y = 20 - 42.5, yend = 60 - 42.5)) +
               geom_segment(aes(x = 200 - 100, xend = 200 - 100, y = 20 - 42.5, yend = 60 - 42.5)) +
               geom_curve(aes(x = 0 - 100, xend = 25 - 100, y = 60 - 42.5, yend = 85 - 42.5),
                          curvature = -0.32) +
               geom_curve(aes(x = 0 - 100, xend = 25 - 100, y = 20 - 42.5, yend = 0 - 42.5),
                          curvature = 0.32) +
               geom_curve(aes(x = 175 - 100, xend = 200 - 100, y = 0 - 42.5, yend = 20 - 42.5),
                          curvature = 0.32) +
               geom_curve(aes(x = 175 - 100, xend = 200 -100, y = 85 - 42.5, yend = 60 - 42.5),
                          curvature = -0.32)  +
               geom_path(data = goal, aes(x = goal_x - 100, y = goal_y - 42.5)) +
               geom_segment(aes(x = 100 - 100, xend = 100 - 100, y = 85 - 42.5, yend = 0 - 42.5), size = 2, color = "red")  +
               labs(title = "Movement Frequency from Zone to Zone", fill = "Movement%", color = "Movement%",
                    caption = "Ben Howell | @benhowell71 | benhowell71.com") +
               geom_path(data = goal, aes(x = goal_x - 100, y = goal_y - 42.5), size = 2) +
               theme(panel.grid = element_blank(), 
                     axis.text = element_blank(),
                     axis.title = element_blank(),
                     plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                     plot.caption = element_text(hjust = 0.5, face = "italic", size = 10),
                     plot.subtitle = element_text(hjust = 0.5))
  )
  
  output$info <- renderText(
    paste0(
      'Move: ', unique(subset(df, id == input$zone)$Move_pct), "%\n",
      'Shot: ', unique(subset(df, id == input$zone)$Shot_pct), "%\n",
      'Make: ', unique(subset(df, id == input$zone)$Make_pct), "%\n"  
    )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)