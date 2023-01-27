############################################################################## #
### Leaflet Map
############################################################################## #  

# install.packages("sf", type = "source", configure.args = c("--with-sqlite3-lib=/usr/local/opt/sqlite/lib", "--with-proj-lib=/usr/local/opt/proj/lib"))

# Packages
library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)
library(sf)
library(shinycssloaders)

# load the daata
dat.map.fips <- readRDS("dat.leaflet.fips.rds")
dat.map.cbsa <- readRDS("dat.leaflet.cbsa.rds")
# color palette 
pal.map <- colorFactor(c("#c92f24", "#c92f24", "#0998eb"), 
                       levels = c("Central", "Outlying", "Neighboring"), 
                       na.color = "white"
                       )

# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(htmltools::HTML("&nbsp<i>Is Covid-19 to Blame?</i> Web App!"), "Is Covid-19 to Blame? Web App!"),
  sidebarLayout(
    sidebarPanel(style = "height: 91vh; overflow-y: scroll",
                 htmltools::HTML(
                 "This web application combines the Covid-19 and game-day data from 
                 the sports economics working paper \"<i>Is Covid-19 to Blame?</i>\" 
                 onto a single interactive map. The paper is written by Giancarlo 
                 Arcese (me) and professor Benjamin Anderson at Colgate University. 
                 We are investigating whether local Covid-19 activity, measured 
                 by the weekly total of new cases or new deaths in a city, impacted
                 attendance to NBA and NHL games during the 2021-22 season.</br></br>
                 
                 Select a week from the dropdown below to update the map's data. 
                 <b>Hover over</b> a county to display its Covid-19 statistics, 
                 as well as the Covid-19 statistics for the correspnding city. Counties that 
                 are directly in a city are colored <span style = \"color: #c92f24\">red</span>, 
                 while counties neighboring a city are colored <span style = \"color: #0998eb\">blue</span>. 
                 <b>Click on</b> any county in a city to display statistics about 
                 the NBA and NHL games played there for the selected week.</br></br>"
                 ),
      selectInput("week", label = "Select a week:", 
                  choices = dat.map.fips %>% pull(floor_monday) %>% unique()
                  ),
      htmltools::HTML(
      "We employ a linear regression as our primary model. To avoid endogeneity, 
      we use data from neighboring counties as an instrument for the weekly 
      total number of cases and deaths in each respective city. Additional variables 
      such as the home team's win probability and the local time when the game was 
      played are included in the regression but omitted from this web app. This 
      working paper won first place at the undergraduate paper competition at the 
      2022 New York State Economics Association conference at Suny Old Westbury.</br></br>
      
      Sports data is provided by ESPN.com, Covid-19 data from <a href='https://github.com/Garcese/sports_econ_project'>TheNewYorkTimes Covid-19 Tracker</a>,
      and county shape files from the R Tigris package (which comes from the 2020 census). 
      The source code used to create this project, as well as the R code to collect 
      and clean all the raw data, can be found at this project's <a href='https://github.com/Garcese/sports_econ_project'>GitHub page</a>.
      With any questions, please reach out to GTArcese@gmail.com."
        ),
    ),
    mainPanel(
      tags$div(style = "position: relative; left: 0px; height: 65vh; 
               background-color: #f5f5f5; border-radius: 2px; border: 1px solid #e3e3e3;",
               id = "myid",
               leafletOutput("map", height = "65vh", width = "100%") %>% 
                 withSpinner(type = 3, size = 2, color.background = "#f5f5f5")
               ),
      conditionalPanel(
        condition = "output.exists != 'yes'",
        column(style = "left: 0px; height: 25vh; width: 100%; margin-top: 10px; 
               overflow-y: scroll; overflow-x: scroll; background-color: #f5f5f5; border-radius: 
               2px; border: 1px solid #e3e3e3;",
               width = 6,
               textOutput("exists")
               )
      ),
      conditionalPanel(
        condition = "output.exists == 'yes'",
        column(style = "left: 0px; height: 25vh; width: 100%; margin-top: 10px; 
               scroll; background-color: #f5f5f5; border-radius: 2px; border: 1px solid #e3e3e3;",
               width = 6,
               div(style = 'height: 100%; overflow-y: scroll; overflow-x: scroll;', tableOutput('table'))
               )
      )
    )
  )
)

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  # static map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.VoyagerNoLabels, 
                       options = providerTileOptions(minZoom = 4, maxZoom = 8)
      ) %>%
      addPolygons(data = dat.map.fips %>% filter(floor_monday == "2021-10-11" & central_outlying != "Neighboring"),
                  fillColor = ~pal.map(central_outlying),
                  fillOpacity = 0.5, weight = 0.5, color = "#80091f"
      ) %>%
      addPolygons(data = dat.map.fips %>% filter(floor_monday == "2021-10-11" & central_outlying == "Neighboring"),
                  fillColor = ~pal.map(central_outlying),
                  fillOpacity = 0.5, weight = 0.5, color = "#053f63"
      ) %>%
      addPolylines(data = dat.map.cbsa %>% pull(cbsa_geometry),
                   weight = 0.75, color = "#000000"
      ) %>% 
      setMaxBounds(lng1 = -45, lat1 = 8, lng2 = -142, lat2 = 61) # box you can't drag out of
  })
  # reactive leaflet data
  dat.map.react <- reactive({
      dat.map.fips %>%
      filter(floor_monday == as.character(input$week))
  })
  # Dynamic leaflet elements
  observe({
    leafletProxy("map") %>%
      clearGroup(c("cbsa", "neighboring")) %>%
      addPolygons(data = dat.map.react() %>% filter(central_outlying != "Neighboring"),
                  fillOpacity = 0, weight = 0,
                  label = ~lapply(fip_label, htmltools::HTML),
                  group = "cbsa", layerId = ~fips
      ) %>%
      addPolygons(data = dat.map.react() %>% filter(central_outlying == "Neighboring"),
                  fillOpacity = 0, weight = 0,
                  label = ~lapply(fip_label, htmltools::HTML),
                  group = "neighboring", layerId = ~fips
      )
  })
  # sets default table value
  output$exists <- reactive({"Click on a city to display the NBA and NHL games played there for the current week."})
  # table
  observeEvent(input$map_shape_click, {
    # table existence
    output$exists <- renderText({
      case_when(
        dat.map.react() %>% 
          filter(fips == input$map_shape_click$id) %>%
          pull(game_data_nest) %>% .[[1]] %>% 
          is.null() == T ~ paste0("No games played in ", dat.map.react() %>% 
                                    filter(fips == input$map_shape_click$id) %>% 
                                    pull(cbsa_title) %>% first()," the week of ",
                                  as.character(input$week)),
        T ~ "yes"
      )
    })
    # table itself
    output$table <- renderTable({
      dat.map.react() %>% filter(fips == input$map_shape_click$id) %>% 
        pull(game_data_nest) %>% 
        .[[1]]
    })
  })
  outputOptions(output, 'exists', suspendWhenHidden = F) # I forget why I needed this
}

shinyApp(ui, server)



