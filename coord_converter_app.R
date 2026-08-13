library(shiny)
library(sf)
library(leaflet)
library(stringr)

sf_use_s2(FALSE)

# -------------------------------------------------------------------
# 1. PATH DEFINITIONS & LOAD SHAPEFILE
# -------------------------------------------------------------------
shp_path  <- here::here('shapefiles/groundfish_stat_areas.shp')


if (file.exists(shp_path)) {
  gdf_shapes <- st_read(shp_path, quiet = TRUE) %>% st_make_valid()
  if (is.na(st_crs(gdf_shapes))) {
    st_crs(gdf_shapes) <- 4326
  } else {
    gdf_shapes <- st_transform(gdf_shapes, crs = 4326)
  }
  gdf_shapes$Id <- str_trim(as.character(gdf_shapes$Id))
} else {
  stop(paste("Statistical_Areas_2010.shp not found in:", desktop_folder))
}

# -------------------------------------------------------------------
# 2. COORDINATE CONVERSION HELPER FUNCTIONS (FIXED FOR LENGTH-ZERO)
# -------------------------------------------------------------------
dms_to_dd <- function(deg, min, sec, direction) {
  d <- as.numeric(deg)
  m <- as.numeric(min)
  s <- as.numeric(sec)
  
  if (is.na(d) || is.na(m) || is.na(s)) return(NA)
  
  dd <- d + (m / 60) + (s / 3600)
  
  # Safe check against NULL/empty input
  if (length(direction) > 0 && toupper(direction) %in% c("S", "W")) {
    dd <- -dd
  }
  return(dd)
}

ddm_to_dd <- function(deg, dec_min, direction) {
  d <- as.numeric(deg)
  m <- as.numeric(dec_min)
  
  if (is.na(d) || is.na(m)) return(NA)
  
  dd <- d + (m / 60)
  
  # Safe check against NULL/empty input
  if (length(direction) > 0 && toupper(direction) %in% c("S", "W")) {
    dd <- -dd
  }
  return(dd)
}

# -------------------------------------------------------------------
# 3. USER INTERFACE (UI)
# -------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("NOAA Statistical Area Coordinate Lookup"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("coord_format", "Select Coordinate Input Format:",
                  choices = c("Decimal Degrees (DD)" = "DD",
                              "Degrees Decimal Minutes (DDM)" = "DDM",
                              "Degrees Minutes Seconds (DMS)" = "DMS")),
      
      hr(),
      
      # Dynamic Input UI using textInput
      uiOutput("coord_inputs"),
      
      actionButton("lookup_btn", "Find Statistical Area", class = "btn-primary", width = "100%")
    ),
    
    mainPanel(
      wellPanel(
        h3("Lookup Results"),
        htmlOutput("result_text")
      ),
      leafletOutput("map", height = "500px")
    )
  )
)

# -------------------------------------------------------------------
# 4. SERVER LOGIC
# -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Dynamic Input UI
  output$coord_inputs <- renderUI({
    req(input$coord_format)
    
    if (input$coord_format == "DD") {
      tagList(
        textInput("dd_lat", "Latitude (Decimal Degrees):", value = "41.5"),
        textInput("dd_lon", "Longitude (Decimal Degrees, e.g. -70.5):", value = "-70.5")
      )
    } else if (input$coord_format == "DDM") {
      tagList(
        h5("Latitude"),
        textInput("ddm_lat_d", "Degrees:", value = "41"),
        textInput("ddm_lat_m", "Decimal Minutes:", value = "30.0"),
        selectInput("ddm_lat_dir", "Direction:", choices = c("N", "S")),
        
        h5("Longitude"),
        textInput("ddm_lon_d", "Degrees:", value = "70"),
        textInput("ddm_lon_m", "Decimal Minutes:", value = "30.0"),
        selectInput("ddm_lon_dir", "Direction:", choices = c("W", "E"))
      )
    } else if (input$coord_format == "DMS") {
      tagList(
        h5("Latitude"),
        textInput("dms_lat_d", "Degrees:", value = "41"),
        textInput("dms_lat_m", "Minutes:", value = "30"),
        textInput("dms_lat_s", "Seconds:", value = "0"),
        selectInput("dms_lat_dir", "Direction:", choices = c("N", "S")),
        
        h5("Longitude"),
        textInput("dms_lon_d", "Degrees:", value = "70"),
        textInput("dms_lon_m", "Minutes:", value = "30"),
        textInput("dms_lon_s", "Seconds:", value = "0"),
        selectInput("dms_lon_dir", "Direction:", choices = c("W", "E"))
      )
    }
  })
  
  # Reactive values to hold converted coordinates
  coords <- reactiveVal(NULL)
  
  observeEvent(input$lookup_btn, {
    req(input$coord_format)
    
    lat_val <- NULL
    lon_val <- NULL
    
    if (input$coord_format == "DD") {
      req(input$dd_lat, input$dd_lon)
      lat_val <- as.numeric(input$dd_lat)
      lon_val <- as.numeric(input$dd_lon)
      
    } else if (input$coord_format == "DDM") {
      req(input$ddm_lat_d, input$ddm_lat_m, input$ddm_lat_dir,
          input$ddm_lon_d, input$ddm_lon_m, input$ddm_lon_dir)
      
      lat_val <- ddm_to_dd(input$ddm_lat_d, input$ddm_lat_m, input$ddm_lat_dir)
      lon_val <- ddm_to_dd(input$ddm_lon_d, input$ddm_lon_m, input$ddm_lon_dir)
      
    } else if (input$coord_format == "DMS") {
      req(input$dms_lat_d, input$dms_lat_m, input$dms_lat_s, input$dms_lat_dir,
          input$dms_lon_d, input$dms_lon_m, input$dms_lon_s, input$dms_lon_dir)
      
      lat_val <- dms_to_dd(input$dms_lat_d, input$dms_lat_m, input$dms_lat_s, input$dms_lat_dir)
      lon_val <- dms_to_dd(input$dms_lon_d, input$dms_lon_m, input$dms_lon_s, input$dms_lon_dir)
    }
    
    # Input validation check
    if (is.null(lat_val) || is.null(lon_val) || is.na(lat_val) || is.na(lon_val)) {
      showNotification("Please enter valid numeric coordinates.", type = "error")
      return()
    }
    
    coords(c(lat = lat_val, lon = lon_val))
  })
  
  # Point spatial lookup
  lookup_result <- reactive({
    req(coords())
    pt <- st_as_sf(data.frame(lon = coords()["lon"], lat = coords()["lat"]), 
                   coords = c("lon", "lat"), crs = 4326)
    joined <- st_join(pt, gdf_shapes, join = st_intersects, left = TRUE)
    
    stat_area <- joined$Id
    if (is.na(stat_area)) stat_area <- "Outside All Statistical Area Boundaries"
    
    list(stat_area = stat_area, lat = coords()["lat"], lon = coords()["lon"])
  })
  
  # Output Result Text
  output$result_text <- renderUI({
    if (is.null(coords())) {
      return(HTML("<p>Enter coordinates on the left and click <b>Find Statistical Area</b>.</p>"))
    }
    res <- lookup_result()
    HTML(paste0(
      "<b>Converted Latitude (DD):</b> ", round(res$lat, 5), "<br>",
      "<b>Converted Longitude (DD):</b> ", round(res$lon, 5), "<br>",
      "<h4 style='color: #1F497D;'><b>Statistical Area:</b> ", res$stat_area, "</h4>"
    ))
  })
  
  # Render Base Leaflet Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addPolygons(
        data = gdf_shapes,
        fillColor = "transparent",
        color = "#444444",
        weight = 1.5,
        label = ~paste("Stat Area:", Id)
      )
  })
  
  # Update Leaflet Map with Pinned Location on Lookup
  observe({
    req(lookup_result())
    res <- lookup_result()
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      setView(lng = res$lon, lat = res$lat, zoom = 8) %>%
      addMarkers(
        lng = res$lon, 
        lat = res$lat,
        popup = paste0("<b>Stat Area:</b> ", res$stat_area, "<br>",
                       "<b>Lat/Lon:</b> ", round(res$lat, 4), ", ", round(res$lon, 4))
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)