# packages ----
library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

summing <- function(cbsa) {
  
  dat <- dat.target.counties %>% 
    filter(`CBSA` == cbsa) %>% 
    select(cases, deaths, CBSA_title)
  
  cbsa.title <- dat %>% 
    pull(CBSA_title) %>% 
    first()
  
  sum.cases <- dat %>% 
    pull(cases) %>% 
    sum()
  
  sum.deaths <- dat %>% 
    pull(deaths) %>% 
    sum()
  
  paste0("<b>CBSA:</b> ", cbsa.title, "<br>",
         "<b>Cases:</b> ", sum.cases, 
         "<br>",
         "<b>Deaths:</b> ", sum.deaths) %>% 
    htmltools::HTML()
}

test <- data.frame(
  lng = -98.4934,
  lat = 33.9137,
  cbsa = "19100",
  lol = "Dallas",
  icon_size = "small"
) %>% 
  st_as_sf(coords = c("lng", "lat"))

icons <- iconList(
  large = makeIcon("input/icon.png", iconWidth = 18, iconHeight = 18*1.227273),
  small = makeIcon("input/icon.png", iconWidth = 14, iconHeight = 14*1.227273)
)

# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  # App title ----
  titlePanel("Hello Shiny!"),
  uiOutput("blah"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      # adding the new div tag to the sidebar            
      tags$div(class="header", checked = NA,
               tags$p("Ready to take the Shiny tutorial? If so"),
               tags$a(href="shiny.rstudio.com/tutorial", "Click Here!")
      )
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      leafletOutput("mymap", height = "90vh")
    )
  )
)

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {

  # Static objects
  map.pal <- colorQuantile("YlOrRd", NULL, n = 20)
  
  dat.target.counties <- counties(cb = TRUE, resolution = "500k") %>% # 500k is best resolution
    filter(`GEOID` %in% vec.fips) %>% # filter for target fips
    mutate(cases = rnorm(n(), 500, 150) %>% ceiling(),
           deaths = rnorm(n(), 50, 10) %>% ceiling()) %>% # placeholder
    mutate(label = paste0("<b>County:</b> ", NAME, "<br>",
                          "<b>Cases:</b> ", cases, "<br>",
                          "<b>Deaths:</b> ", deaths)) %>% # label
    left_join(dat.cbsa %>% select(CBSA, CBSA_title, fips) %>% rename("GEOID" = `fips`),
              by = "GEOID")
  
  # Reactive objects
  map.test <- reactive({
      dat.target.counties %>% 
      mutate(across(cases:deaths, ~ row_number() + input$bins)) %>% 
      mutate(label = paste0("<b>County:</b> ", NAME, "<br>",
                            "<b>Cases:</b> ", cases, "<br>",
                            "<b>Deaths:</b> ", deaths))
  })

  # Static map
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.VoyagerNoLabels,
                       options = providerTileOptions(
                         minZoom = 4, 
                         maxZoom = 8)) %>% 
      setMaxBounds(lng1 = -45, lat1 = 8, lng2 = -142, lat2 = 61) # box you can't drag out of

  })
  
  output$blah <- renderUI({
    input$bins
  })
  
  # Dynamic leaflet elements
  observe({
    click <- input$map_marker_click
    print(click)
    
    leafletProxy("mymap", data = map.test()) %>%
      addPolygons(data = map.test(),
                  fillColor = ~map.pal(cases), # Inputs counties w/covid data
                  fillOpacity = 0.65, 
                  color = "#565656", 
                  weight = 0.25,
                  label = ~lapply(label, htmltools::HTML)) %>% # I hate using lapply%>% 
      addMarkers(data = test, icon = ~icons[icon_size], 
                 label = ~summing(cbsa))  
  })
}

shinyApp(ui, server)