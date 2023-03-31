library(shiny)
library(leaflet)
library(shiny)
library(shinydashboard)
library(dygraphs)

type <- c("Full Fare" = "Full Fare", "30-Day Unlimited"= "30-Day Unlimited",
          "7-Day Unlimited" =  "7-Day Unlimited", "Student" = "Student",
          "Senior Citizen/Disabled" = "Senior Citizen/Disabled", "Annual Metrocard" = "Annual Metrocard",
          "EasyPayXpress" =  "EasyPayXpress", "Other Fares" = "Other Fares")
header <- dashboardHeader(title = p("MTA Subway Ridership App"),
                          titleWidth = 400)

sidebar <- dashboardSidebar(
  
  sidebarUserPanel("ADS Spring 2023 Group 6"),
  sidebarMenu(
    
    #menuItem("Hourly Collision Data", tabName = "data", icon = icon("bar-chart-o")),
    #menuItem("Hourly Borough Comparison", tabName = "data_by_borough", icon = icon("bar-chart-o")),
    #menuItem("Raw Fatality Map", tabName = "map", icon = icon("map")),
    menuItem("Interactive MTA Subway Ridership Map", tabName = "leafletmap", icon = icon("map"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  fluidRow(
    column(width =4,
           box(width = NULL, title =tagList(shiny::icon("filter",class = 'fa-lg'), "Filter Data") ,
               solidHeader = T, collapsible = T, status = 'primary',
               #selectizeInput('type','Fare Type', choices = type, width = 380,
                              #selected = c('Full Fare', '30-Day Unlimited',"7-Day Unlimited",
                                           #"Student","Senior Citizen/Disabled","Annual Metrocard",
                                           #"EasyPayXpress","Other Fares"),multiple = T),
               dateRangeInput('dates', label = "Date Range",width = 380,
                              start = '2019-11-01', end = '2022-01-01',
                              min = "2019-11-01", max = "2022-01-01"
               ),
               submitButton(text = "Submit",icon =icon('filter'))
           ),
           box(width = NULL,title = tagList(shiny::icon("info-circle",class = 'fa-lg'), "About MTA Subway Dashboard"), solidHeader = T, collapsible = T, status = 'info',
               strong("MTA Subway Dashboard"),"is an interactive map built on shiny which allows you to cutomize
                         time range and fare type to filter out MTA Subway Ridership of a certain location you want",
               "The data is from", a('here.', href = 'https://new.mta.info/open-data', target = "_blank")
           ),
           
    ),
    column(width =8,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput('subwayMap',height = 500))
           )
    )
  )



ui <- dashboardPage(skin = 'black',
                    header,
                    dashboardSidebar(disable = T),
                    body
)