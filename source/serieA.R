library(tidyverse)
library(rvest)
library(tidyr)
library(purrr)

serieA_data_app = read_csv("data/csv/serie_A_data.csv") 

source("source/source.R")
library(geojsonR)

# geojson, stadiums with coordinates
serieA_stadiums_geo = json_to_tibble("data/geojson/serieA.geojson")

serieA_page = read_html("https://en.wikipedia.org/wiki/List_of_Serie_A_stadiums")
serieA_stadium_wiki = serieA_page %>%
  html_table(fill=TRUE) %>% 
  .[[1]]

serieA_stadium_wiki <- serieA_stadium_wiki %>%
  tidyr::separate_rows(Club, sep = "and ") %>% 
  mutate(Stadium = gsub("\\(.*\\)", "", Stadium) %>%
           str_trim()
         #   ,
         # Club = str_trim(Club) %>% str_remove_all("\\(|\\)| \\)|\\[[0-9]\\]") %>%
         #   str_trim()
  ) %>% 
  filter(is.na(Closed))

bad_rows = starts_with(match = "(", vars = serieA_stadium_wiki$Club)

serieA_stadium_wiki = serieA_stadium_wiki %>%
  filter(!(row_number() %in% bad_rows))


serieA_stadiums = fuzzyjoin::stringdist_left_join(serieA_stadiums_geo %>% 
                                                    mutate(team = str_trim(team), 
                                                           team_join = case_when(
                                                             team == "Sassuolo Calcio" ~ "Sassuolo",
                                                             TRUE ~ team
                                                           )),
                                                  serieA_stadium_wiki %>% 
                                                    mutate(Club = str_trim(Club)),
                                                  by = c("team_join" = "Club"),
                                                  max_dist=1)

# preparing main dataframe, with data from 2010-2011 season to 2019-2020 season
serieA_years = paste0("https://en.wikipedia.org/wiki/20", 10:19,"%E2%80%93",11:20,"_Serie_A")

# wiki links for all teams (was bad bc of inconsistent wiki formatting)
get_serieA_teams = function(x) {
  read_html(x) %>% 
    html_nodes("td:nth-child(1) a") %>% 
    html_attr("href") %>% 
    .[3:22] %>% 
    paste0("https://en.wikipedia.org", .) 
}

serieA_team_links = map(serieA_years, get_serieA_teams)

serieA_team_links[[3]] = serieA_years[3] %>% 
  read_html() %>% 
  html_nodes("td:nth-child(1) a") %>% 
  html_attr("href") %>% 
  paste0("https://en.wikipedia.org", .) %>% 
  .[15:34]

serieA_team_links[[4]] = serieA_years[4] %>% 
  read_html() %>% 
  html_nodes("td:nth-child(1) a") %>% 
  html_attr("href") %>% 
  paste0("https://en.wikipedia.org", .) %>% 
  .[11:30]

serieA_team_links[[5]] = serieA_years[5] %>% 
  read_html() %>% 
  html_nodes("td:nth-child(1) a") %>% 
  html_attr("href") %>% 
  paste0("https://en.wikipedia.org", .) %>% 
  .[11:30]

serieA_team_links[[7]] = serieA_years[7] %>% 
  read_html() %>% 
  html_nodes("td:nth-child(1) a") %>% 
  html_attr("href") %>% 
  paste0("https://en.wikipedia.org", .) %>% 
  .[4:23]


# wiki tables for all teams (club, location, stadium name, capacity)
get_serieA_tbls = function(x) {
  read_html(x) %>% 
    html_table(fill=T)
}

serieA_table_long_list = map(serieA_years, get_serieA_tbls)

serieA_wiki = tibble()
for (i in 1:length(serieA_table_long_list)) {
  
  tbl_list = serieA_table_long_list[[i]]
  
  for (j in 1:length(tbl_list)) {
    if (nrow(tbl_list[[j]]) == 20 & ncol(tbl_list[[j]]) >= 4 & ncol(tbl_list[[j]]) <= 6 & "Capacity" %in% colnames(tbl_list[[j]])) {
      tbl = tbl_list[[j]]
      if (ncol(tbl) == 4) {
        colnames(tbl) <- c("Club", "Location", "Venue", "Capacity")
      } else if(ncol(tbl) == 5)  {
        colnames(tbl) <- c("Club", "Location", "Venue", "Capacity", "Ref")
      } else {
        colnames(tbl) <- c("Club", "Location", "Region", "Venue", "Capacity", "Ref")
      }
      serieA_wiki = bind_rows(serieA_wiki, tbl %>% mutate(season=i))
      
      break
    }
  }
}

serieA_wiki = serieA_wiki %>% 
  mutate(team_links = serieA_team_links %>% unlist())

# wiki links for all stadiums
get_serieA_teams_stadiums = function(x) {
  read_html(x) %>% 
    html_nodes("td~ td+ td > a:nth-child(1)") %>% 
    html_attr("href") %>%
    paste0("https://en.wikipedia.org", .) 
}

# inconsistent wiki formatting
serieA_stadium_list = c(map(serieA_years[1], get_serieA_teams_stadiums) %>% .[[1]] %>% head(20),
                        map(serieA_years[2], get_serieA_teams_stadiums) %>% .[[1]] %>% head(20) ,
                        map(serieA_years[3], get_serieA_teams_stadiums) %>% .[[1]] %>% .[13:29],
                        map(serieA_years[4], get_serieA_teams_stadiums) %>% .[[1]] %>% .[4:19],
                        map(serieA_years[5], get_serieA_teams_stadiums) %>% .[[1]] %>% .[4:19],
                        map(serieA_years[6], get_serieA_teams_stadiums) %>% .[[1]] %>% .[1:17],
                        map(serieA_years[7], get_serieA_teams_stadiums) %>% .[[1]] %>% .[1:19],
                        map(serieA_years[8], get_serieA_teams_stadiums) %>% .[[1]] %>% .[1:20],
                        map(serieA_years[9], get_serieA_teams_stadiums) %>% .[[1]] %>% .[1:36],
                        map(serieA_years[10], get_serieA_teams_stadiums) %>% .[[1]] %>% .[1:59])

serieA_stadium_list = serieA_stadium_list[!(str_detect(serieA_stadium_list, "Serie_A|Serie_B"))]  %>% 
  str_replace_all("_", " ") %>% 
  str_trim()

serieA_stadium_list = serieA_stadium_list[str_detect(serieA_stadium_list, " ")] 

# stadiums that needed reformatting: 
#Stadio Atleti Azzurri d'Italia
#Stadio Adriatico – Giovanni Cornacchia
#Mapei Stadium – Città del Tricolore
#Stadio Artemio Franchi – Montepaschi Arena	


# final, large dataset
serieA_teams_and_logos = serieA_stadiums %>% 
  mutate(Stadium = str_remove_all(Stadium, "also known as the Stadio Giuseppe Meazza"))%>% 
  left_join(
    tibble(stadium_link = serieA_stadium_list %>% unique(),
           stadium = serieA_stadium_list %>% unique() %>% 
             str_remove_all("https://en.wikipedia.org/wiki/") %>% 
             str_replace("%27", "'") %>% 
             str_replace("%E2%80%93", "–") %>% 
             str_replace("%C3%A0", "à")),
    by = c("Stadium" = "stadium")
  ) %>% 
  left_join(serieA_wiki %>% 
              arrange(desc(season)) %>% 
              distinct(Club, .keep_all = TRUE),
            by=c("Club" = "Club")) %>% 
  filter(!is.na(team_links)) %>% 
  mutate(stadium_link = str_replace_all(stadium_link, " ", "_"))

# team logos
serieA_team_logos = map(serieA_teams_and_logos$team_links, get_logos_or_stadiums) %>% 
  paste0("https:", .)

serieA_teams_and_logos$team_logo = serieA_team_logos 

# stadium photo
serieA_stadium_photo = map(serieA_teams_and_logos$stadium_link, get_logos_or_stadiums) %>% 
  paste0("https:", .) 

serieA_teams_and_logos$stadium_photo = serieA_stadium_photo %>% unlist()

# necessary for Shiny app 
serieA_teams_and_logos$short_club <- serieA_teams_and_logos$Club

# changing short_club names to match larger football-data-co.uk dataset
SerieA = serieA_teams_and_logos %>%
  mutate(short_club = case_when(
    Club == "Internazionale" ~ "Inter",
    TRUE ~ as.character(Club)
  )) %>% 
  # selecting necessary columns for Shiny app
  dplyr::select(Stadium, Club, Location.x, Opened, Capacity.x, Lat, Long, stadium_photo, short_club, leaguename, team_logo) %>%
  arrange(Stadium)

# to see logos for teams that play together
for (i in 2:nrow(SerieA)) {
  if (SerieA[i, ]$Lat == SerieA[i-1, ]$Lat) {
    SerieA[i, ]$Lat = SerieA[i-1, ]$Lat + 0.01
    SerieA[i, ]$Long = SerieA[i-1, ]$Long + 0.01
  }
}

write_rds(SerieA, "SerieA.rds") 


# 
# serieA_page = read_html("https://en.wikipedia.org/wiki/List_of_football_stadiums_in_Italy")
# serieA_tbl = serieA_page %>% 
#   html_nodes("table")
# 
# 
# serieA_wiki = html_table(serieA_tbl[[2]], fill=T) 
# 
# serieA_wiki <- serieA_wiki %>% 
#   tidyr::separate_rows(`Home Team`, sep = "and") %>% 
#   dplyr::select(-Renovated, -Notes) %>% 
#   mutate(Stadium = gsub("\\(.*\\)", "", Stadium) %>% 
#            str_trim(),
#          `Home Team` = gsub("\\(.*\\)", "", `Home Team`) %>% 
#            str_trim()
#   )
# 
# 
# serieA_teams = serieA_stadiums$team %>% str_trim() %>% 
#   str_replace_all(" ", "_") 
# 
# serieA_club_links = read_html("https://en.wikipedia.org/wiki/Category:Serie_A_clubs") %>% 
#   html_nodes(".mw-category-group a") %>% 
#   html_attr("href") %>% 
#   paste0("https://en.wikipedia.org", .)
# 
# serieA_team_links = c()
# for (i in 1:length(serieA_teams)) {
#   j = serieA_club_links %>% 
#     str_detect(serieA_teams[i])
#   newlink = serieA_club_links[j]
#   serieA_team_links = c(serieA_team_links, newlink)
# }
# 
# get_serieA_logos = function(x){
#   read_html(x) %>% 
#     html_node(".vcard .image img") %>% 
#     html_attr("src") 
# }
# 
# serieA_logos = map(serieA_team_links, get_serieA_logos) %>% 
#   paste0("https:", .)
# 
# temp_teams = gsub("^.*wiki\\/", "", serieA_team_links) %>%
#   str_replace_all("_", " ") %>%  
#   str_replace_all(" B.C.| F.C.|A.C. |ACF | C.F.C.|S.S. |S.S.C. |S.S.D. |A.S. |U.C. |U.S. | [0-9]+", "") %>% 
#   str_trim()
# teams=c()
# for (i in 1:length(temp_teams)) {
#   if (!str_detect(temp_teams[i], "^S")) {
#     teams[i] <- str_remove_all(temp_teams[i], "Calcio") %>% str_trim()
#   } else {
#     teams[i] <- temp_teams[i]
#   }
#   
# }
# 
# teams[16] <- "Pescara"  
# 
# serieA_teams_and_logos = left_join(
#   serieA_stadiums %>% 
#     mutate(team = str_trim(team)),
#   tibble(
#     team = teams,
#     team_link = serieA_team_links,
#     team_logo = serieA_logos
#   ) %>% 
#     mutate(team = case_when(
#       team == "Inter Milan" ~ "Internazionale",
#       team == "ChievoVerona" ~ "Chievo",
#       team == "Robur Siena" ~ "Siena",
#       TRUE ~ as.character(team)
#     )),
#   by = "team"
# ) 
# 
# serieA = inner_join(serieA_wiki %>% 
#                       mutate(`Home Team` = case_when(
#                         `Home Team` == "Sassuolo" ~ "Sassuolo Calcio",
#                         TRUE ~ as.character(`Home Team`)
#                       ))
#                     , 
#                     serieA_teams_and_logos, by = c("Home Team"="team"))
# serieA$stadium_photo <- NA
# 
# serieA = serieA %>%
#   mutate(short_club = case_when(
#     `Home Team` == "Internazionale" ~ "Inter",
#     `Home Team` == "Sassuolo Calcio" ~ "Sassuolo",
#     TRUE ~ as.character(`Home Team`)
#   )) %>% 
#   dplyr::select(stadium, `Home Team`, Location, Opened, Capacity, 
#                 Lat, Long, stadium_photo, short_club, leaguename, team_logo) 
# 
# for (i in 2:nrow(serieA)) {
#   if (serieA[i, ]$Lat == serieA[i-1, ]$Lat) {
#     serieA[i, ]$Lat = serieA[i-1, ]$Lat + 0.1
#     serieA[i, ]$Long = serieA[i-1, ]$Long + 0.1
#   }
# }
