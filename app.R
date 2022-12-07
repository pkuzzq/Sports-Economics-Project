############################################################################## #
### Leaflet Map
############################################################################## #  

# Packages
library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)
library(sf)

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
  titlePanel(htmltools::HTML(" <i>Is Covid-19 to Blame?</i> Web App!")),
  sidebarLayout(
    sidebarPanel(style = "height: 91vh",
                 htmltools::HTML(
                 "This web app serves to visualize a subset of the data used 
                 in the economics working paper <i>Is Covid-19 to Blame?</i> by Giancarlo Arcese (me)
                 and professor Benjamin Anderson at Colgate University. We are interested in determining whether 
                 local Covid-19 activity, measured by the weekly total of new cases or new deaths in a city, impacted
                 professional sports attendance in the NBA and NHL.</br></br>
                 Select a week from the dropdown below to update the map's data. <b>Hover over</b> a county to display
                 its Covid-19 statistics, as well as the statistics for the entire city. Counties that are a part
                 of a city are colored red, while counties directly neighboring a city ar colored blue. <b>Click on</b> any 
                 county in a city to display statistics about the NBA and NHL games played in the selected week.</br></br>
                 "),
      selectInput("week", label = "Select as week:", 
                  choices = dat.map.fips %>% pull(floor_monday) %>% unique()
                  ),
      htmltools::HTML(
      "This data will be used in a linear regression model to determine whether fans reacted to 
        local-Covid-19 activity when deciding to attend NBA and NHL games. To avoid endogeneity, we plan
        to use data from the counties directly neighboring each city as an instrument for the weekly total
        of cases and deaths in each respective city. Additional variables such as the home team's win probability 
        and the local time when the game was played are omitted from this web app but will be used in the regression.</br></br>
        Sports data is provided by ESPN.com and Covid-19 data from <a href='https://github.com/Garcese/sports_econ_project'>TheNewYorkTimes Covid-19 Tracker</a>,
        and county shape files from the R Tigris package. The source code used to create this project, 
        as well as the code to collect and clean all the raw data, can be found
        at this project's <a href='https://github.com/Garcese/sports_econ_project'>Github page</a>.
        With any questions, please reach out to GTArcese@gmail.com, I will glady answer any questions!
      "
        ),
    ),
    mainPanel(
      leafletOutput("map", height = "65vh"),
      conditionalPanel(
        condition = "output.exists != 'yes'",
        column(style = 
"height:25vh; width: 100%; margin-top: 10px; overflow-y: scroll; overflow-x: scroll; background-color: #f5f5f5; border-radius: 2px; border: 1px solid #e3e3e3;",
               width = 6,
               textOutput("exists")
               )
      ),
      conditionalPanel(
        condition = "output.exists == 'yes'",
        column(style = 
"height:25vh; width: 100%; margin-top: 10px; scroll; background-color: #f5f5f5; border-radius: 2px; border: 1px solid #e3e3e3;",
               width = 6,
               # tableOutput("table")
               # )
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
  outputOptions(output, 'exists', suspendWhenHidden = F)
}

shinyApp(ui, server)



