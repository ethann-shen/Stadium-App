library(tidyverse)
library(rvest)
library(tidyr)
library(purrr)

#=========================================================== EPL ===========================================================#

PL_data_app = read_csv("data/csv/premier_league_data.csv") 

#=========================================================== Stadiums ===========================================================#

page = read_html("https://en.wikipedia.org/wiki/List_of_Premier_League_stadiums")
tbl = page %>% 
  html_nodes("table")

stadiums = html_table(tbl[[1]]) %>% as.data.frame() %>% filter(is.na(Closed)) %>% 
  dplyr::select(-Image, -Closed, -Ref.)

stadium_df = stadiums %>% mutate(Coordinates = (str_extract(Coordinates, "[^/]+$")),
                                 Coordinates = str_replace(Coordinates, " \\(.*\\)", "")) %>% 
  separate(Coordinates, into = c("Lat", "Long"), sep="; ", remove = F) %>% 
  mutate(Lat = as.numeric(Lat),         
         Long = ifelse(str_detect(Long, "-"), 
                       as.numeric(str_extract(Long, "\\-\\d+\\.\\d+")), 
                       as.numeric(Long))
  )

stadium_df[stadium_df$Stadium == "Carrow Road", ]$Long = 1.30917
stadium_df[stadium_df$Stadium == "Portman Road", ]$Long = 1.14472
stadium_df[stadium_df$Stadium == "The Valley", ]$Long = 0.03639

remove_text = function(x) gsub("\\[.*\\]", "", x)

remove_commas = function(x) gsub(",", "", x)

stadium_df = stadium_df %>% 
  mutate(Stadium = Stadium %>% remove_text(.),
         Stadium = str_remove_all(Stadium, "Formerly.*"),
         Opened = Opened %>% remove_text(.) %>% as.numeric(),
         `Capacity †` = `Capacity †` %>% remove_text(.))

#=========================================================== Stadium Photos ===========================================================#

capital_let = function(x) {
  paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep="")
  
}

stadium_links = stadium_df$Stadium %>% str_trim() %>% capital_let() %>% 
  str_replace_all(" ", "_") %>% str_replace_all("'", "%27") %>% 
  paste0("https://en.wikipedia.org/wiki/", .)
stadium_links[40] <- "https://en.wikipedia.org/wiki/Stamford_Bridge_(stadium)"

stadium_images = function(x) { 
  read_html(x) %>% 
    html_nodes(".vcard .image img") %>%  
    html_attr("src") 
}

initial_mapped_images = map(stadium_links, stadium_images)
initial_length = initial_mapped_images %>% map_int(., length) %>% {. == 1}

image1 = function(x) { 
  read_html(x) %>% 
    html_nodes("tr~ tr+ tr .image img") %>% 
    html_attr("src") 
}
batch1 = map(stadium_links, image1) 
batch1_images_lengths = batch1 %>% map_int(., length) %>% {. == 1} 

image2 = function(x) {
  read_html(x) %>% 
    html_nodes("td > .image img") %>% 
    html_attr("src") 
}
batch2 = map(stadium_links, image2) 
batch2_images_lengths = batch2 %>% map_int(., length) %>% {. == 1} 

image3 = function(x) {
  read_html(x) %>% 
    html_nodes("tr+ tr .image img") %>% 
    html_attr("src") 
}
batch3 = map(stadium_links, image3) %>% map(.,1)
batch3_images_lengths = batch3 %>% map_int(., length) %>% {. == 1} 

stadium_df$stadium_photo = NA

for (i in 1:length(stadium_links)) {
  if (batch2_images_lengths[i] == TRUE & is.na(stadium_df[i, ]$stadium_photo)) {
    
    stadium_df[i, ]$stadium_photo = batch2[[i]] %>% paste0("https:", .)
    
  } else if (batch1_images_lengths[i] == TRUE & is.na(stadium_df[i, ]$stadium_photo)) {
    
    stadium_df[i, ]$stadium_photo = batch1[[i]] %>% paste0("https:", .)
    
  } else if (batch3_images_lengths[i] == TRUE & is.na(stadium_df[i, ]$stadium_photo)) {
    
    stadium_df[i, ]$stadium_photo = batch3[[i]] %>% paste0("https:", .)
    
  } else if (initial_length[i] == TRUE) {
      
      stadium_df[i, ]$stadium_photo = initial_mapped_images[[i]] %>% paste0("https:", .)
      
    } else {
    stadium_df[i, ]$stadium_photo = NA
  }
  
}
stadium_df[stadium_df$Club == "Swansea City",]$stadium_photo <- "https://upload.wikimedia.org/wikipedia/commons/thumb/6/68/New_Morfa_Stadium_-_geograph.org.uk_-_32243.jpg/285px-New_Morfa_Stadium_-_geograph.org.uk_-_32243.jpg"

stadium_df = stadium_df %>% 
  mutate(short_club = case_when(
    Club == "Birmingham City" ~ "Birmingham",
    Club == "Blackburn Rovers" ~ "Blackburn",
    Club == "Bolton Wanderers" ~ "Bolton",
    Club == "Brighton & Hove Albion" ~ "Brighton",
    Club == "Cardiff City" ~ "Cardiff",
    Club == "Crystal Palace & Wimbledon" ~ "Crystal Palace",
    Club == "Huddersfield Town" ~ "Huddersfield",
    Club == "Hull City" ~ "Hull", 
    Club == "Leicester City" ~ "Leicester",
    Club == "Manchester City" ~ "Man City",
    Club == "Manchester United" ~ "Man United",
    Club == "Newcastle United" ~ "Newcastle",
    Club == "Norwich City" ~ "Norwich",
    Club == "Queens Park Rangers& Fulham" ~ "QPR",
    Club == "Stoke City" ~ "Stoke", 
    Club == "Swansea City" ~ "Swansea",
    Club == "Tottenham Hotspur" ~ "Tottenham",
    Club == "West Bromwich Albion" ~ "West Brom",
    Club == "West Ham United" ~ "West Ham", 
    Club == "Wigan Athletic" ~ "Wigan",
    Club == "Wolverhampton Wanderers" ~ "Wolves",
    TRUE ~ as.character(Club)
  )) %>%
  filter(short_club %in% unique(PL_data_app$HomeTeam)) %>% 
  dplyr::select(-Coordinates) %>% 
  mutate(Country = "England")

#=========================================================== Team Logos ===========================================================#

logos = c(paste0("https://thefootballcrestindex.com/blogs/premier-league-clubs?page=", seq(1,5)))

get_logos = function(x) {
  read_html(x) %>%
    html_nodes("img") %>% 
    html_attr("src")
}

links = map(logos, get_logos) %>% 
  unlist()

# links = c()
# for (logo in logos) {
#   some_links = read_html(logo) %>%
#     html_nodes("img") %>% 
#     html_attr("src")
#   # html_nodes(".article__grid-left") %>%
#   # html_text() %>%
#   # str_trim()
#   
#   links = c(links, some_links)
# }
links = paste0("https:", links)


good_links = c(
  links[gsub("^.*articles/H_\\s*|\\s*_.*$", "", links) %>% 
          map_int(., str_length) %>% 
          {. < 20}],
  # gets clubs like Manchester_City
  links[gsub("^.*articles/H_\\s*|\\s*_compact.*$", "", links) %>% 
          map_int(., str_length) %>% 
          {. < 20}]
) %>% unique() 


team_names1 = gsub("^.*articles/H_\\s*|\\s*_.*$", "", good_links)
team_names2 = gsub("^.*articles/H_\\s*|\\s*_compact.*$", "", good_links)

team_names = c(
  team_names1,
  team_names2[team_names2 %>% map_int(., str_length) %>% {. < 20}]
) %>% 
  unique() 

team_names_tbl = team_names %>% 
  as_tibble() %>% 
  mutate(new_value = case_when(
    value == "AFCB" ~ "Bournemouth",
    value == "AstonVilla" ~ "Aston Villa",
    value == "Crystal" ~ "Crystal Palace", 
    value == "Manchester_City" ~ "Man City", 
    value == "Manchester_United" ~ "Man United", 
    value == "Sheffield_United" ~ "Sheffield United",
    value == "West" ~ "West Brom",
    value == "West_Ham" ~ "West Ham", 
    value == "Wolverhampton" ~ "Wolves",
    TRUE ~ as.character(value)
  )) %>% 
  filter(new_value %in% stadium_df$short_club) %>% 
  arrange(value)

EPL_team_logo = c()
for (i in 1:nrow(team_names_tbl)) {
  val = good_links[str_detect(good_links, team_names_tbl$value[i])]
  EPL_team_logo = c(EPL_team_logo, val)
}

team_names_tbl$EPL_team_logo = EPL_team_logo %>% unique() %>% sort()

team_names_tbl = team_names_tbl %>% 
  mutate(check_col = 
           str_detect(team_names_tbl$EPL_team_logo, team_names_tbl$value)
  )

#=========================================================== Stadiums & Team Logos ===========================================================#

EPL = inner_join(stadium_df, team_names_tbl,
                        by=c("short_club" = "new_value")) %>% 
  dplyr::select(-check_col, -value) %>% 
  rename(team_logo = EPL_team_logo)


write_rds(EPL, "EPL.rds") 

