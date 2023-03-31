library(tidyverse) #For Data Manipulation and Plotting
library(janitor) #For cleaning up the variable names in the CSV Files
library(lubridate) #For date processing 
library(patchwork) # For combining multiple ggplots together
library(ggmap) # For producing a static map
library(ggtext) # For adding some flair to ggplot
library(leaflet) # For Making Interactive Plots
library(rvest) # For Web Scraping Links to Download
library(dplyr)
library(data.table)
library(leaflet)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(xts)
library(rgdal)
require(rgdal)

#Gather dataset from MTA Website
dataset <- read_html("http://web.mta.info/developers/fare.html") %>%
  html_nodes("a") %>% 
  html_attr("href") %>% 
  keep(str_detect(., 'fares_(19)|(22)\\d{4}\\.csv')) %>% 
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

#Distribution of ridership through out the years
library(cowplot)
dataset %>% 
  group_by(key, week_start, week_end) %>% 
  summarize(fares = sum(fares, na.rm = T), .groups = 'drop') %>% 
  ggplot(aes(x = week_start, y = fares/1e6)) + 
  geom_line(color = '#0039A6') + 
  geom_vline(xintercept = ymd(20200322), lty = 2) + 
  geom_point(data = tibble(
    week_start = c(ymd(20200222), ymd(20200418)),
    fares = c(30768135, 2548002)
  ), color = 'red', size =3
  ) +
  geom_textbox(
    x = ymd(20191001),
    y = 15,
    label = "A ***<span style = 'color:red'>92% Decrease</span>*** \n in Subway Ridership \n 1 month before \n vs. 1 month after \n PAUSE order",
    fill = 'cornsilk',
    halign = 0.5,
  ) + 
  labs(x = "Week Beginning", y = "# of MTA Subway Fares (millions)",
       title = "<span style='color:#0039A6'>MTA</span> Ridership (Jan 2019 - Aug 2020)",
       subtitle = "PAUSE Order Begins on 3/22/2020") + 
  scale_y_continuous(labels = scales::comma) +
  cowplot::theme_cowplot() + 
  theme(
    plot.title = element_markdown()
  )

#Data Cleaning on Ridership
combined <- dataset %>% 
  mutate(
    fare_type = case_when(
      fare_type == 'ff' ~ 'Full Fare',
      fare_type == 'x30_d_unl' ~ '30-Day Unlimited',
      fare_type == 'x7_d_unl' ~ '7-Day Unlimited',
      fare_type == 'students' ~ 'Student',
      fare_type == 'sen_dis' ~ 'Senior Citizen/Disabled',
      fare_type == 'tcmc_annual_mc' ~ 'Annual Metrocard',
      fare_type == 'mr_ezpay_exp' ~ 'EasyPayXpress',
      TRUE ~ fare_type
    )
  ) %>% 
  #Remove SBS Bus Stations and PATH
  filter(!str_detect(station, "SBS-|PA-|AIRTRAIN"))

top_7 <- combined %>% 
  count(fare_type, sort = T) %>% 
  head(7) %>% 
  pull(fare_type)


station_level <- combined %>% 
  mutate(
    fare_type = fct_other(fare_type, keep = top_7, other_level = "Other Fares")
  ) %>% 
  group_by(remote, station, fare_type) 

geocodes <- read_csv('https://raw.githubusercontent.com/chriswhong/nycturnstiles/master/geocoded.csv', 
                     col_names = c('remote', 'zuh', 'station', 'line', 'system', 'lat', 'long'),
)
comb_geo <- station_level %>% 
  inner_join(geocodes %>% group_by(remote) %>% summarize(lat = max(lat), long = max(long)), by = "remote") %>%
  ungroup()

saveRDS(comb_geo, "comb_geo.RDS")
write.csv(comb_geo, "mta_dataset.csv", row.names=FALSE)



data <- as_tibble(fread('MTA_dataset.csv',header = T))



data <- as_tibble(fread('MTA_dataset.csv', header = T))

data <- data %>%
  mutate(dates = as.Date(data$week_start, format = "%m/%d/%Y"))

data <- data %>%
  select(lat, long, fares, dates) %>%
  group_by(lat, long, dates) %>% 
  summarise(sum_fares= sum(fares))

#data.expanded <- data[rep(row.names(data), data$sum_fares),]

data <- data %>%
  select(lat, long, fares, dates) %>%
  group_by(fare_type, lat, long, dates) %>% 
  summarise(sum_fares= sum(fares))

setDT(data)
data <- data[, .(
  Fares = sum(sum_fares)
), .(lat, long)]
subway_map <- readOGR(dsn = "/Users/denisesonia/ads-spring2023-project2-group_6/data/geo_export_1b991b55-24f5-4e63-9418-b365b941218f.shp")
leaflet(data) %>%
  addPolylines(data = subway_map, color = "blue", weight = 2) %>%
  addTiles(group = 'OSM') %>%
  addProviderTiles('Esri.WorldStreetMap', group = 'Esri') %>%
  addProviderTiles('CartoDB.Positron', group = 'CartoDB') %>%
  addMarkers(
    ~long, ~lat, label = ~Fares, 
    clusterOptions = markerClusterOptions(),
    labelOptions = labelOptions(noHide = TRUE)
  ) %>%
  addLayersControl(
    baseGroups = c('OSM', 'Esri', 'CartoDB'),
    options = layersControlOptions(collapsed = FALSE)
  )


