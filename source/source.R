library(geojsonR)
library(dplyr)
library(purrr)


get_logos_or_stadiums = function(x){
  read_html(x) %>% 
    html_node(".vcard .image img") %>% 
    html_attr("src") 
}


json_to_tibble = function(x) {
  
  json = FROM_GeoJson(x)
  col_check = map_dfr(json$features, 
                      ~ map(., length)) %>%
    map_lgl(~ any(. > 1 | . == 0, na.rm = TRUE))
  
  list_cols = names(col_check)[col_check] 
  
  fix_list_cols = function(entry, name) {
    if (name %in% list_cols) {
      list(entry)
    } else {
      entry
    }
  }
  
  list_tbl = map_dfr(json$features,
                     function(char) {
                       map2(char, names(char), fix_list_cols) %>%
                         as_tibble()
                     }
  ) 
  
  col_check_geom = map_dfr(list_tbl$geometry, 
                           ~ map(., length)) %>%
    map_lgl(~ any(. > 1 | . == 0, na.rm = TRUE))
  
  list_cols_geom = names(col_check_geom)[col_check_geom] 
  
  fix_list_cols_geom = function(entry, name) {
    if (name %in% list_cols_geom) {
      list(entry)
    } else {
      entry
    }
  }
  
  coords = map_dfr(list_tbl$geometry,
                   function(char) {
                     map2(char, names(char), fix_list_cols_geom) %>%
                       as_tibble()
                   }
  ) 
  
  coords = coords %>% 
    separate(coordinates, sep = ",", into = c("Long", "Lat")) %>% 
    mutate(Long = gsub("^c\\(", "", Long) %>% as.numeric(),
           Lat = gsub("\\)$", "", Lat) %>% as.numeric())
  
  stadiums = map_dfr(list_tbl$properties, as_tibble) %>% 
    bind_cols(coords) 
  return(stadiums)
}
