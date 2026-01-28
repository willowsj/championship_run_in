# Championship 2025-26 Run-in
# Last updated: 28 January 2026


# -------------------------
# 0️⃣ Install and load packages
# -------------------------
packages <- c(
  "shiny", "dplyr", "tidyr", "ggplot2",
  "readxl", "stringr", "showtext",
  "lubridate", "scales"
)

rsconnect::writeManifest()

for(p in packages){
  if(!require(p, character.only = TRUE)) install.packages(p, dependencies = TRUE)
  library(p, character.only = TRUE)
}

# -------------------------
# 1️⃣ Load font
# -------------------------
font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 96)

# -------------------------
# 2️⃣ Load fixture data
# -------------------------
january  <- read.csv("january.csv")
february <- read.csv("february.csv")
march    <- read.csv("march.csv")
april    <- read.csv("april.csv")
may      <- read.csv("may.csv")

all_fixtures <- bind_rows(march, april, may) %>% 
  select(-X) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  filter(Date >= Sys.Date()) %>%
  mutate(Home = str_trim(Home),
         Away = str_trim(Away))

# -------------------------
# 3️⃣ Load PPG tables
# -------------------------
home_ppg <- read_excel("league_table.xlsx", sheet = "Home") %>%
  rename(Home_PPG = PPG) %>%
  filter(!is.na(Team)) %>%
  mutate(Team = str_trim(Team))

away_ppg <- read_excel("league_table.xlsx", sheet = "Away") %>%
  rename(Away_PPG = PPG) %>%
  filter(!is.na(Team)) %>%
  mutate(Team = str_trim(Team))

team_name_map <- c(
  "Sheffield Utd" = "Sheffield United",
  "Blackburn"     = "Blackburn Rovers",
  "Sheffield Wed" = "Sheffield Wednesday",
  "QP Rangers"    = "Queens Park Rangers",
  "West Brom"     = "West Bromwich Albion",
  "Preston"       = "Preston North End",
  "Charlton"      = "Charlton Athletic",
  "Oxford Utd"    = "Oxford United"
)

home_ppg <- home_ppg %>%
  mutate(Team = ifelse(Team %in% names(team_name_map), team_name_map[Team], Team))

away_ppg <- away_ppg %>%
  mutate(Team = ifelse(Team %in% names(team_name_map), team_name_map[Team], Team))

# -------------------------
# 4️⃣ Static elements
# -------------------------
all_teams <- sort(unique(c(all_fixtures$Home, all_fixtures$Away)))
all_teams_list <- as.list(setNames(all_teams, all_teams)) # JSON-safe for Shiny

# -------------------------
# 5️⃣ Shiny UI
# -------------------------
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { background-color: #F5F1E8; font-family: 'Roboto', sans-serif; }
      .selectize-input, .selectize-dropdown { font-family: 'Roboto', sans-serif; }
    "))
  ),
  
  titlePanel(tags$span("EFL Championship 2025–26 Run-in",
                       style = "font-weight:700; font-size:28px;")),
  
  tags$div(style = "font-size:14px; margin-bottom:10px;",
           "Fixture difficulty based on weighted points per game (by home/away). ",
           "Last updated 27 January 2026."),
  
  fluidRow(
    column(12,
           selectizeInput(
             "teams_selected",
             "Select Teams:",
             choices = all_teams_list,
             selected = c("Oxford United"),
             multiple = TRUE
           )
    )
  ),
  
  fluidRow(
    column(12,
           div(style="overflow-y:auto; height:800px;", plotOutput("fixture_plot", width="100%"))
    )
  )
)

# -------------------------
# 6️⃣ Shiny server
# -------------------------
server <- function(input, output, session) {
  
  filtered_schedule <- reactive({
    req(input$teams_selected)
    
    # Expand fixtures so each team gets a row
    schedule_expanded <- bind_rows(
      all_fixtures %>% 
        mutate(Team = Home,
               `Home/Away` = "Home",
               Opposition = Away),
      all_fixtures %>% 
        mutate(Team = Away,
               `Home/Away` = "Away",
               Opposition = Home)
    ) %>%
      filter(Team %in% input$teams_selected) %>%
      left_join(home_ppg, by = c("Opposition"="Team")) %>% 
      rename(Opp_Home_PPG = Home_PPG) %>%
      left_join(away_ppg, by = c("Opposition"="Team")) %>% 
      rename(Opp_Away_PPG = Away_PPG) %>%
      mutate(
        Fixture_PPG = if_else(`Home/Away`=="Home", Opp_Away_PPG, Opp_Home_PPG),
        Opp_Display = if_else(`Home/Away`=="Home",
                              str_to_upper(Opposition),
                              Opposition)
      ) %>%
      arrange(Date)
    
    schedule_expanded
  })
  
  output$fixture_plot <- renderPlot({
    schedule <- filtered_schedule()
    req(nrow(schedule) > 0)
    
    # Respect dropdown order for plotting
    selected_teams <- input$teams_selected
    
    # Determine max number of fixtures across selected teams
    max_fixtures <- max(schedule %>% group_by(Team) %>% summarise(n=n()) %>% pull(n))
    
    # Dynamic text sizing
    text_size <- max(2.5, min(5, 600/max_fixtures))
    
    # Prepare table per team, include header, spacer, and FDR
    plot_table_full <- lapply(selected_teams, function(tm){
      team_sched <- schedule %>% filter(Team==tm)
      n_team <- nrow(team_sched)
      tibble(
        Team = tm,
        Row = 0, Opp_Display = tm, Fixture_PPG = NA, Bold = TRUE
      ) %>% bind_rows(
        tibble(Team=tm, Row=0.5, Opp_Display="", Fixture_PPG=NA, Bold=FALSE)
      ) %>% bind_rows(
        tibble(
          Team=tm,
          Row = 1:max_fixtures,
          Opp_Display = c(team_sched$Opp_Display, rep("", max_fixtures - n_team)),
          Fixture_PPG = c(team_sched$Fixture_PPG, rep(NA, max_fixtures - n_team)),
          Bold = FALSE
        )
      ) %>% bind_rows(
        tibble(
          Team=tm,
          Row = max_fixtures + 1,
          Opp_Display = "Fixture Difficulty",
          Fixture_PPG = round(mean(team_sched$Fixture_PPG, na.rm=TRUE),2),
          Bold = TRUE
        )
      )
    }) %>% bind_rows()
    
    # Scale PPG for tile coloring
    plot_table_full <- plot_table_full %>%
      mutate(
        ppg_scaled = scales::rescale(Fixture_PPG, to=c(0,1), na.rm=TRUE),
        text_colour = case_when(
          is.na(ppg_scaled) ~ "black",
          TRUE ~ "grey34"
        )
      )
    
    # Separator lines (header + FDR)
    separator_lines <- plot_table_full %>%
      filter(Row %in% c(0.5, max_fixtures + 0.5))
    
    # Plot
    ggplot(plot_table_full, aes(y=-Row)) +
      geom_tile(data=plot_table_full %>% filter(!is.na(Fixture_PPG)),
                aes(x=0.85, fill=Fixture_PPG),
                width=0.1, height=0.97) +
      geom_text(aes(x=0.05, label=Opp_Display, fontface=ifelse(Bold,"bold","plain")),
                hjust=0, size=text_size, family="roboto") +
      geom_text(aes(x=0.85, label=ifelse(is.na(Fixture_PPG),"",Fixture_PPG),
                    fontface=ifelse(Bold,"bold","plain"),
                    color=text_colour),
                size=text_size, family="roboto") +
      geom_hline(data=separator_lines, aes(yintercept=-Row),
                 color="grey85", size=0.4, inherit.aes=FALSE) +
      scale_fill_gradient2(low="#1B7837", mid="white", high="#B2182B", midpoint=1.5,
                           na.value="transparent") +
      scale_color_identity() +
      facet_wrap(~Team, scales="free_y", ncol=3) +
      coord_cartesian(clip="off", xlim=c(0,1)) +
      labs(x="", y="") +
      theme_minimal(base_family="roboto") +
      theme(axis.text=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            strip.text=element_blank(),
            legend.position="none",
            plot.background=element_rect(fill="#F5F1E8", color=NA),
            panel.background=element_rect(fill="#F5F1E8", color=NA),
            plot.margin=margin(t=8,r=4,b=2,l=4))
    
  }, height=function(){
    schedule <- filtered_schedule()
    if(nrow(schedule)==0) return(400)
    max_fixtures <- max(schedule %>% group_by(Team) %>% summarise(n=n()) %>% pull(n))
    max(400, 30*(max_fixtures + 2))
  })
  
}

# -------------------------
# 7️⃣ Run app
# -------------------------
shinyApp(ui, server)
