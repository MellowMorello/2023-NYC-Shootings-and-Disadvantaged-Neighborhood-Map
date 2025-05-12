message("🚀 App starting...")

# ---- Load Required Libraries ----
tryCatch({
  library(shiny)
  library(leaflet)
  library(dplyr)
  library(lubridate)
  library(sf)
  library(htmltools)
  message("✅ Packages loaded")
}, error = function(e) {
  message("❌ Package load failed: ", e$message)
  stop(e)
})

# ---- Load CSVs from Google Drive ----
tryCatch({
  shootings <- read.csv("https://drive.google.com/uc?export=download&id=1-5P3Y_08vQcuDOg_3sv7XfMbr_0S0Yc7", stringsAsFactors = FALSE)
  neighborhoods <- read.csv("https://drive.google.com/uc?export=download&id=19NBmFM_xJ2hw9lreGc5dMSXQQ9xr27zM", stringsAsFactors = FALSE)
  message("✅ Data loaded successfully")
}, error = function(e) {
  message("❌ Data load failed: ", e$message)
  stop(e)
})

# ---- UI ----
ui <- fluidPage(
  titlePanel("NYC 2023 Shootings & Disadvantaged Neighborhoods"),
  leafletOutput("nycMap", height = 700)
)

# ---- Server ----
server <- function(input, output, session) {
  message("🧠 Inside server function")
  
  neighborhoods_sf <- st_as_sf(neighborhoods, wkt = "the_geom", crs = 4326)
  neighborhoods_sf <- st_make_valid(neighborhoods_sf)
  
  nyc_sf <- neighborhoods_sf %>%
    st_filter(st_as_sfc(st_bbox(c(
      xmin = -74.2591, xmax = -73.7004,
      ymin = 40.4774, ymax = 40.9176
    ), crs = 4326)))
  
  nyc_disadvantaged_sf <- nyc_sf %>%
    filter(DAC_Designation == "Designated as DAC")
  
  shootings_clean <- shootings %>%
    mutate(
      OCCUR_DATE = if (!inherits(OCCUR_DATE, "Date")) mdy(OCCUR_DATE) else OCCUR_DATE,
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude)
    ) %>%
    filter(
      year(OCCUR_DATE) == 2023,
      !is.na(Latitude) & !is.na(Longitude)
    ) %>%
    mutate(
      OCCUR_DATE_FORMATTED = format(OCCUR_DATE, "%B %d, %Y"),
      STATISTICAL_MURDER_FLAG = ifelse(STATISTICAL_MURDER_FLAG, "Fatal", "Non-Fatal")
    )
  
  output$nycMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = nyc_disadvantaged_sf,
        group = "Disadvantaged Neighborhoods",
        fillColor = "lightblue",
        color = "blue",
        weight = 1,
        opacity = 0.7,
        fillOpacity = 0.6,
        highlightOptions = highlightOptions(color = "yellow", weight = 2, bringToFront = TRUE),
        label = lapply(
          paste0(
            "<div style='font-size:14px; background-color:white; padding:5px; border-radius:5px; border:1px solid #ccc;'>",
            "<b>GEOID:</b> ", nyc_disadvantaged_sf$GEOID, "<br>",
            "<b>County:</b> ", nyc_disadvantaged_sf$County, "<br>",
            "<b>Percentile Rank Combined NYC:</b> ", nyc_disadvantaged_sf$Percentile_Rank_Combined_NYC,
            "</div>"
          ),
          htmltools::HTML
        ),
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "12px",
          style = list(
            "color" = "black",
            "background-color" = "white",
            "border" = "1px solid gray",
            "padding" = "6px",
            "border-radius" = "6px"
          )
        )
      ) %>%
      addCircleMarkers(
        data = shootings_clean,
        group = "2023 Shootings",
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 4,
        color = "red",
        fillColor = "red",
        fillOpacity = 0.5,
        stroke = FALSE,
        popup = ~paste0(
          "<b>Date:</b> ", OCCUR_DATE_FORMATTED, "<br>",
          "<b>Time:</b> ", OCCUR_TIME, "<br>",
          "<b>Precinct:</b> ", PRECINCT, "<br>",
          "<b>Outcome:</b> ", STATISTICAL_MURDER_FLAG, "<br>",
          "<b>Victim Age Group:</b> ", VIC_AGE_GROUP, "<br>",
          "<b>Victim Sex:</b> ", VIC_SEX, "<br>",
          "<b>Victim Race:</b> ", VIC_RACE
        )
      ) %>%
      addLayersControl(
        overlayGroups = c("Disadvantaged Neighborhoods", "2023 Shootings"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomleft"
      ) %>%
      setView(lng = -74.0060, lat = 40.7128, zoom = 11)
  })
}

message("🚦 Launching shinyApp()")
shinyApp(ui = ui, server = server)
