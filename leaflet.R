############################################################################## #
### Leaflet Map
############################################################################## #  

# Read 2020 county shape files from tigris package
dat.leaflet <- counties(cb = TRUE, resolution = "500k") %>% # 500k is best resolution
  select(GEOID, NAME, geometry) %>% 
  set_colnames(c("fips", "county", "geometry")) %>% 
  left_join(dat.cbsa, by = "fips") %>% 
  mutate(label = paste0(
    "<b>CBSA:</b> ", cbsa, "<br>",
    "<b>County:</b> ", county, "<br>",
    "<b>Fip:</b> ", fips, "<br>",
    "<b>CO:</b> ", central_outlying
  )) %>% 
  distinct(fips, .keep_all = T)

map.pal <- colorFactor("YlOrRd", levels = c("Central", "Outlying", "Bordering"), na.color = "white")

leaflet(dat.leaflet) %>% 
  addProviderTiles(
    providers$CartoDB.VoyagerNoLabels,
    options = providerTileOptions(
      minZoom = 4, 
      maxZoom = 10
      )) %>% 
  addPolygons(
    data = dat.leaflet,
    fillColor = ~map.pal(central_outlying),
    fillOpacity = 0.65, 
    color = "#565656", 
    weight = 0.25,
    label = ~lapply(label, htmltools::HTML)
    ) %>%
  setMaxBounds(lng1 = -45, lat1 = 8, lng2 = -142, lat2 = 61) # box you can't drag out of

  
  
  
  
  

  filter(`GEOID` %in% vec.fips) %>% # filter for target fips
  mutate(cases = rnorm(n(), 500, 150) %>% ceiling(),
         deaths = rnorm(n(), 50, 10) %>% ceiling()) %>% # placeholder
  mutate(label = paste0("<b>County:</b> ", NAME, "<br>",
                        "<b>Cases:</b> ", cases, "<br>",
                        "<b>Deaths:</b> ", deaths)) %>% # label
  left_join(dat.cbsa %>% select(CBSA, CBSA_title, fips) %>% rename("GEOID" = `fips`),
            by = "GEOID")


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

marker.style <- list("font-family" = "serif",
                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                     "font-size" = "10px",
                     "border-color" = "rgba(0,0,0,0.5)")

# create map
leaflet(dat.target.counties) %>% 
  addProviderTiles(providers$CartoDB.VoyagerNoLabels,
           options = providerTileOptions(
             minZoom = 4, 
             maxZoom = 8)) %>% 
  addPolygons(data = dat.target.counties,
              fillColor = ~map.pal(cases), # Inputs counties w/covid data
              fillOpacity = 0.65, 
              color = "#565656", 
              weight = 0.25,
              label = ~lapply(label, htmltools::HTML)) %>% # I hate using lapply%>% 
  setMaxBounds(lng1 = -45, lat1 = 8, lng2 = -142, lat2 = 61) %>% # box you can't drag out of
  addMarkers(data = test, icon = ~icons[icon_size], 
             label = ~summing(cbsa))  
  # addStaticLabels(data = test, label = test$lol,
  #                 labelOptions(
  #                              offset = c(-1000,1000),
  #                              style = marker.style))









