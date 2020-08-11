library(tidyverse)
library(rvest)
library(tidyr)
library(purrr)

MLS_data_app = read_csv("data/csv/USA.csv") %>% 
  mutate(Season = paste0(Season-1, "-", Season))

library(janitor)
library(sp)
MLS_page = "https://en.wikipedia.org/wiki/2019_Major_League_Soccer_season"
MLS_team_links = read_html(MLS_page) %>% 
  html_nodes(".center+ div .wikitable td:nth-child(1) a") %>% 
  html_attr("href") %>% 
  paste0("https://en.wikipedia.org", .)
MLS_stadium_links = read_html(MLS_page) %>% 
  html_nodes(".center+ div .wikitable td+ td a") %>% 
  html_attr("href") %>% 
  paste0("https://en.wikipedia.org", .)
MLS_stadium_links = MLS_stadium_links[!str_detect(MLS_stadium_links, "A_grp_1")]


library(janitor)
MLS_wiki = bind_rows(
  read_html(MLS_page) %>% 
    html_table(fill=T, header=T) %>% 
    .[[3]] %>% 
    janitor::row_to_names(row_number = 1)
  ,
  read_html(MLS_page) %>% 
    html_table(fill=T) %>% 
    .[[4]] %>% 
    janitor::row_to_names(row_number = 1)
) %>% 
  mutate(team_links = MLS_team_links,
         stadium_links = MLS_stadium_links,
         team_logo = map(MLS_team_links, get_logos_or_stadiums) %>% unlist() %>% paste0("https:", .))


get_MLS_stadium_photo = function(x) {
  read_html(x) %>%  
    html_nodes("tr+ tr td > .image img") %>% 
    html_attr("src")
  
}

MLS_stadium_photos = map(MLS_stadium_links, get_MLS_stadium_photo)

# some stadium wiki's have multiple pictures 
for (i in 1:length(MLS_stadium_photos)) {
  if (length(MLS_stadium_photos[[i]]) > 1) {
    MLS_stadium_photos[[i]] <- MLS_stadium_photos[[i]][2] %>% paste0("https:", .)
  } else {
    MLS_stadium_photos[[i]] <- MLS_stadium_photos[[i]] %>% paste0("https:", .)
  }
}

get_lat = function(x) {
  read_html(x) %>% 
    html_nodes("#coordinates .latitude") %>% 
    html_text()
}
get_long = function(x) {
  read_html(x) %>% 
    html_nodes("#coordinates .longitude") %>% 
    html_text()
}

lat = map(MLS_stadium_links, get_lat)
long = map(MLS_stadium_links, get_long)


library(sp)

convert_coord = function(x) {
  #https://gis.stackexchange.com/questions/293665/converting-dms-coordinates-to-decimal-in-r
  sp::char2dms(x, chd, chm, chs) %>% as.numeric()
}

coord <- lat %>% unlist() %>% .[1]

chd = substr(coord, 3, 3)[1]
chm = substr(coord, 6, 6)[1]
chs = substr(coord, 9, 9)[1]

converted_lat = map(lat %>% unlist(), convert_coord) %>% unlist()
converted_long = map(long %>% unlist(), convert_coord) %>% unlist()

get_location = function(x) {
  read_html(x) %>% 
    html_nodes(".label a") %>% 
    html_text()
}
MLS_locations = map(MLS_stadium_links, get_location)
MLS_locations_unlisted = map(MLS_locations, function(x) x %>% str_c(collapse = ", ")) %>%
  unlist() %>% 
  str_remove_all(", \\[.*\\]") # removing footnotes from Orlando

MLS_wiki = MLS_wiki %>% 
  mutate(Lat = converted_lat,
         Long = converted_long,
         stadium_photo = MLS_stadium_photos %>% unlist(),
         location = MLS_locations_unlisted)

MLS_wiki$Opened <- NA
MLS_wiki$short_club <- MLS_wiki$Team
MLS_wiki$Country <- "USA"

MLS = MLS_wiki %>% 
  select(Stadium, Team, location, Opened, Capacity, Lat, Long, stadium_photo, short_club, Country, team_logo) %>% 
  mutate(short_club = case_when(
    Team == "LA Galaxy" ~ "Los Angeles Galaxy",
    Team == "Los Angeles" ~ "Los Angeles FC",
    TRUE ~ Team
  ),
  short_club = short_club %>% str_remove_all(" FC| SC|\\."),
  Stadium = str_remove_all(Stadium, "\\[.*\\]"))

write_rds(MLS, "MLS.rds") 

