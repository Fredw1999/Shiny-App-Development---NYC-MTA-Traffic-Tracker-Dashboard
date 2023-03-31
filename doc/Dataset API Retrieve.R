library(tidyverse) #For Data Manipulation and Plotting
library(janitor) #For cleaning up the variable names in the CSV Files
library(lubridate) #For date processing 
library(patchwork) # For combining multiple ggplots together
library(ggmap) # For producing a static map
library(ggtext) # For adding some flair to ggplot
library(leaflet) # For Making Interactive Plots
library(rvest) # For Web Scraping Links to Download

all_weeks_2019_2020 <- read_html("http://web.mta.info/developers/fare.html") %>%
  html_nodes("a") %>% 
  html_attr("href") %>% 
  keep(str_detect(., 'fares_(19)|(20)\\d{4}\\.csv')) %>% 
  map_dfr(., function(x){
    return(
      read_csv(paste0("http://web.mta.info/developers/", x), skip = 2) %>% 
        clean_names %>%
        #Drop Dead Columns
        select_if(~!all(is.na(.x))) %>%
        mutate(
          key = str_extract(x, '\\d+'),
          
          #The data in the files covers seven-day periods beginning on the Saturday 
          #two weeks prior to the posting date and ending on the following Friday. 
          #Thus, as an example, the file labeled Saturday, January 15, 2011, has data 
          #covering the period from Saturday, January 1, 2011, through Friday, January 7. 
          #The file labeled January 22 has data covering the period from 
          #Saturday, January 8, through Friday, January 14. And so on and so forth
          week_start = ymd(paste0('20',key)) - days(14),
          week_end = ymd(paste0('20',key)) - days(8)
        ) %>%
        mutate(across(c(-remote, -station, -week_start, -week_end, -key), parse_number)) %>% 
        pivot_longer(
          cols = c(-remote, -station, -week_start, -week_end, -key),
          names_to = "fare_type",
          values_to = "fares"
        )
    )
  }
  ) 

all_weeks_2021_2023 <- read_html("http://web.mta.info/developers/fare.html") %>%
  html_nodes("a") %>% 
  html_attr("href") %>% 
  keep(str_detect(., 'fares_(21)|(12)\\d{4}\\.csv')) %>% 
  map_dfr(., function(x){
    return(
      read_csv(paste0("http://web.mta.info/developers/", x), skip = 2) %>% 
        clean_names %>%
        #Drop Dead Columns
        select_if(~!all(is.na(.x))) %>%
        mutate(
          key = str_extract(x, '\\d+'),
          
          #The data in the files covers seven-day periods beginning on the Saturday 
          #two weeks prior to the posting date and ending on the following Friday. 
          #Thus, as an example, the file labeled Saturday, January 15, 2011, has data 
          #covering the period from Saturday, January 1, 2011, through Friday, January 7. 
          #The file labeled January 22 has data covering the period from 
          #Saturday, January 8, through Friday, January 14. And so on and so forth
          week_start = ymd(paste0('20',key)) - days(14),
          week_end = ymd(paste0('20',key)) - days(8)
        ) %>%
        mutate(across(c(-remote, -station, -week_start, -week_end, -key), parse_number)) %>% 
        pivot_longer(
          cols = c(-remote, -station, -week_start, -week_end, -key),
          names_to = "fare_type",
          values_to = "fares"
        )
    )
  }
  ) 

write.csv(all_weeks_2019_2020, "mta_2019_2020.csv", row.names=FALSE)
write.csv(all_weeks_2021_2023, "mta_2021_2022.csv", row.names=FALSE)
