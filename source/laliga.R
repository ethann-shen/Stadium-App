library(tidyverse)
library(rvest)
library(tidyr)
library(purrr)

LaLiga_data_app = read_csv("data/csv/la_liga_data.csv") 

source("source/source.R")
library(geojsonR)

laliga_stadiums_geo = json_to_tibble("data/geojson/la_liga.geojson")


laliga_stadiums_geo[laliga_stadiums_geo$team == "Athletico Madrid",]$team <- "Atlético Madrid"



laliga_stadium_wiki = read_html("https://en.wikipedia.org/wiki/List_of_stadiums_in_Spain") %>%
  html_table(fill=T) %>% 
  .[[1]] 


laliga_stadiums_geo = laliga_stadiums_geo %>% mutate(team = case_when(
  team == "Celta de Vigo" ~ "Celta Vigo",
  team == "Deportivo Alavés" ~ "Alavés",
  team == "Deportivo de La Coruna" ~ "Deportivo La Coruña",
  team == "Real Zaragoza" ~ "Zaragoza",
  team == "Sporting de Gijón" ~ "Sporting Gijón",
  TRUE ~ team
))

laliga_stadiums = fuzzyjoin::stringdist_left_join(laliga_stadiums_geo,
                                                  laliga_stadium_wiki,
                                                  by = c("team" = "Team"),
                                                  max_dist=2) 

laliga_years = paste0("https://en.wikipedia.org/wiki/20", 10:19,"%E2%80%93",11:20,"_La_Liga")

get_laliga_teams = function(x) {
  read_html(x) %>% 
    html_nodes("td:nth-child(1) a") %>% 
    html_attr("href") %>% 
    .[3:22] %>% 
    paste0("https://en.wikipedia.org", .) 
}

laliga_team_links = map(laliga_years[-10], get_laliga_teams)
laliga_team_links[[10]] = laliga_years[10] %>% 
  read_html() %>%
  html_nodes("td:nth-child(1) > a") %>%
  html_attr("href") %>% 
  .[1:23] %>% 
  .[c(-12, -13, -18)] %>% 
  paste0("https://en.wikipedia.org", .) 


get_laliga_tbls = function(x) {
  table = read_html(x) %>% 
    html_table(fill=T) %>% 
    .[[2]] 
  
  colnames(table) <- c("Club", "Location", "Venue", "Capacity")
  table = table %>% 
    distinct(Club, .keep_all = T)
  return(table)
  
}

laliga_table_list = map(laliga_years, get_laliga_tbls)

get_laliga_teams_stadiums = function(x) {
  read_html(x) %>% 
    html_nodes("td~ td+ td > a:nth-child(1)") %>% 
    html_attr("href") %>% 
    head(20) %>% 
    paste0("https://en.wikipedia.org", .) 
}

laliga_stadium_list = map(laliga_years, get_laliga_teams_stadiums) 


laliga_wiki = laliga_table_list %>% 
  bind_rows() %>% 
  mutate(team_links = laliga_team_links %>% unlist(),
         stadium_link = laliga_stadium_list  %>% unlist()) %>%
  distinct(Club, .keep_all = TRUE)

laliga_team_logos = map(laliga_wiki$team_links, get_logos_or_stadiums) %>% 
  paste0("https:", .)

laliga_wiki$team_logo = laliga_team_logos 

laliga_stadium_photo = map(laliga_wiki$stadium_link, get_logos_or_stadiums) %>% 
  paste0("https:", .) 

laliga_wiki$stadium_photo = laliga_stadium_photo %>% unlist()

laliga_wiki = laliga_wiki %>% 
  mutate(Club_join = case_when(
    Club == "Deportivo La Coruña" ~ "Deportivo de La Coruna",
    Club == "Alavés" ~ "Deportivo Alavés",
    Club == "Zaragoza" ~ "Real Zaragoza",
    TRUE ~ Club
  )) 

laliga_teams_and_logos = fuzzyjoin::stringdist_left_join(laliga_stadiums ,
                                                         laliga_wiki,
                                                         by=c("team" = "Club_join"),
                                                         max_dist = 2)
laliga_teams_and_logos$short_club <- laliga_teams_and_logos$team

LaLiga = laliga_teams_and_logos %>%
  dplyr::select(stadium, team, Location, Inaugurated, Capacity.x, Lat, Long, stadium_photo, short_club, leaguename, team_logo) 
