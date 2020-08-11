library(tidyverse)
library(shiny)
library(shinyWidgets)
library(leaflet)

#source("source/epl.R")
#source("source/laliga.R")
#source("source/serieA.R")
#source("source/bund.R")
#source("source/ligue.R")
#source("source/MLS.R")

EPL <- read_rds("data/rds/EPL.rds")
LaLiga <- read_rds("data/rds/LaLiga.rds")
SerieA <- read_rds("data/rds/SerieA.rds")
Bund <- read_rds("data/rds/Bund.rds")
Ligue1 <- read_rds("data/rds/Ligue1.rds")
MLS <- read_rds("data/rds/MLS.rds")

PL_data_app <- read_csv("data/csv/premier_league_data.csv")
LaLiga_data_app <- read_csv("data/csv/la_liga_data.csv")
serieA_data_app <- read_csv("data/csv/serie_A_data.csv")
Bund_data_app <- read_csv("data/csv/bundesliga_data.csv")
ligue1_data_app <- read_csv("data/csv/ligue1_data.csv")
MLS_data_app <- read_csv("data/csv/USA.csv") %>% 
  mutate(Season = paste0(Season-1, "-", Season))

colnames(LaLiga) <- colnames(EPL)
colnames(SerieA) <- colnames(EPL)
colnames(Bund) <- colnames(EPL)
colnames(Ligue1) <- colnames(EPL)
colnames(MLS) <- colnames(EPL)

all_stadiums_data = bind_rows(EPL, 
                              LaLiga, 
                              SerieA,
                              Bund, 
                              Ligue1,
                              MLS) %>% 
  mutate(`Capacity †`= str_remove_all(`Capacity †`, "\\[.*\\]")) %>% 
  filter(!is.na(team_logo))

all_teams_data = bind_rows(PL_data_app, 
                           LaLiga_data_app, 
                           serieA_data_app, 
                           Bund_data_app, 
                           ligue1_data_app,
                           MLS_data_app %>% 
                             select(Country, Home, Season) %>% 
                             rename(country = Country) %>% 
                             mutate(HomeTeam = Home %>% str_remove_all(" FC"))) %>% 
  arrange(desc(Season)) %>% 
  mutate(country = case_when(
    Div == "E0" ~ "England",
    Div == "SP1" ~ "Spain", 
    Div == "I1" ~ "Italy",
    Div == "D1" ~ "Germany",
    Div == "F1" ~ "France",
    TRUE ~ country
  ))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("stadium_plot", width = "100%", height = "100%"),
  absolutePanel(top = 20, right = 10,
                pickerInput("stadium_country", label = "Select a Country:",
                            choices = 
                              list("All Countries",
                                   "Europe" = c("England", "Spain", "Italy", "Germany", "France"),
                                   "North America"= c("USA")),
                            #   c("All Countries",
                            #             unique(all_stadiums_data$Country)
                            # ),
                            options = list(`live-search` = TRUE))
  ),
  absolutePanel(top = 90, right = 10,
                pickerInput("stadium_season", label = "Select a Season:",
                            choices = c("All Seasons", 
                                        unique(all_teams_data$Season)
                            ),
                            options = list(`live-search` = TRUE))
  )
)

server <- function(input, output) {
  
  filteredTeams <- reactive({
    if (input$stadium_season == "All Seasons") {
      if (input$stadium_country == "All Countries") {
        all_teams_data %>% pull(HomeTeam) %>% unique()
      } else {
        all_teams_data %>% filter(country == input$stadium_country) %>% pull(HomeTeam) %>% unique()
      }
      # else if (input$stadium_country == "England") {
      #   all_teams_data %>% filter(Div == "E0") %>% pull(HomeTeam) %>% unique()
      # } else{
      #   all_teams_data %>% filter(Div == "SP1") %>% pull(HomeTeam) %>% unique()
      # }
    } else {
      all_teams_data = all_teams_data %>% filter(Season == input$stadium_season)
      if (input$stadium_country == "All Countries") {
        all_teams_data %>% pull(HomeTeam) %>% unique()
      } else {
        all_teams_data %>% filter(country == input$stadium_country) %>% pull(HomeTeam) %>% unique()
      }
      # else if (input$stadium_country == "England") {
      #   all_teams_data %>% filter(Div == "E0") %>% pull(HomeTeam) %>% unique()
      # } else{
      #   all_teams_data %>% filter(Div == "SP1") %>% pull(HomeTeam) %>% unique()
      # }
    }
  })
  
  output$stadium_plot <- renderLeaflet({
    
    filtered_stadiums_data = all_stadiums_data %>% 
      filter(short_club %in% filteredTeams()) 
    
    Icons <- icons(
      
      iconUrl = ifelse(test = filtered_stadiums_data %>% 
                         pull(team_logo) %>% is.na,
                       yes = "", 
                       no = filtered_stadiums_data %>% 
                         pull(team_logo)),
      
      # filtered_stadiums_data %>% 
      # pull(team_logo),
      iconWidth = 35, iconHeight = 35
    )
    
    labels = sprintf("<strong>%s</strong><br/>%s",
                     filtered_stadiums_data %>% 
                       pull(Club),
                     filtered_stadiums_data %>% 
                       pull(Stadium)
    ) %>%
      lapply(htmltools::HTML)
    
    filtered_stadiums_data %>% 
      mutate(popup_text = paste0("<center>", #Setting up poopup info
                                 ifelse(!is.na(stadium_photo), paste0("<img src = ", stadium_photo, " width='300'>"), ""),
                                 "</br></br><b>Location: </b>",
                                 ifelse(!is.na(Location), Location, ""),
                                 "</br><b>Opened: </b>",
                                 ifelse(!is.na(Opened), Opened, ""),
                                 "</br><b>Capacity: </b>",
                                 ifelse(!is.na(`Capacity †`), `Capacity †`, ""),
                                 "</br></br>",
                                 actionButton("showmodal", "Show Data", onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())'))) %>% 
      leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>%            
      addMarkers(~Long, ~Lat, label = ~labels , 
                 icon = ~Icons, popup = ~popup_text, 
                 # crucial for the modal dialog to work. had to use the filtered_teams_data (all_teams_data failed)
                 layerId = ~(filtered_stadiums_data$short_club), 
                 labelOptions = labelOptions(textsize = "15px"))
    
  })
  
  # https://community.rstudio.com/t/click-on-leaflet-marker-and-get-info/11016
  # https://community.rstudio.com/t/shiny-using-multiple-exclusive-reactivevalues-for-filtering-dataset/40957
  
  # this identifies and "saves" which stadium the user is clicking on
  stadium_click <- reactive( { 
    
    input$stadium_plot_marker_click  # marker_click
    
  })
  
  observeEvent(input$button_click, {
    showModal(
      modalDialog(
        title = paste0(stadium_click()$id, "'s Home Games"),
        DT::renderDataTable(
          {
            all_teams_data %>% 
              filter(HomeTeam == stadium_click()$id) %>% # finds the club that the "stadium_click" refers to  
              select(Season, everything(), -Div, -country)
          },
          selection = "single",
          rownames = FALSE,
          options = list(scrollX = TRUE)
        ),
        easyClose = TRUE
      )
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

