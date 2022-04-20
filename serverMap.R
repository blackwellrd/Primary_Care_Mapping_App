# ------------------------- #
# Desc: Map Tab Server File #
# File: serverMap.R         #
# ------------------------- #

fnScale <- function(data_min, data_max, radius_min, radius_max, value){
  return(((radius_max - radius_min) * (value - data_min))/(data_max - data_min))
}

rvMap <- reactiveValues(map = leaflet())

output$tab02_lftMap <- renderLeaflet({rvMap$map})

observeEvent(
  input$tab01_radGeocodeLevel, 
  {
    updateRadioButtons(session, inputId = 'tab02_radGeocodeLevel', selected = input$tab01_radGeocodeLevel)
  }
)

observeEvent(
  input$tab02_btnRefreshMap,
  {
    england_bounds <- st_bbox(sf_country %>% filter(CTRY20CD == 'E92000001'))
    
    map <- leaflet()
    map <- map %>% addTiles()
    map <- map %>% addPolylines(
      data = sf_country,
      color = '#000000',
      weight = 2
    )
    map <- map %>% fitBounds(
      lng1 = unname(england_bounds$xmin),
      lat1 = unname(england_bounds$ymin),
      lng2 = unname(england_bounds$xmax),
      lat2 = unname(england_bounds$ymax))
    if(input$tab02_radGeocodeLevel=='Lat/Long'){
      min_value <- min(sqrt(rvData$data$value))
      max_value <- max(sqrt(rvData$data$value))

      data_labels <- sprintf(
        "Postcode: %s<br>Value: %.1f",
        rvData$data$postcode,
        rvData$data$value) %>%
        lapply(htmltools::HTML)
      
      map <- map %>% 
        addCircles(
          data = rvData$data,
          lng = ~longitude,
          lat = ~latitude,
          label = data_labels,
          radius = ~fnScale(min_value, max_value, 1, 10, value),
          weight = as.numeric(ini_file_settings$data_circles$weight),
          color = ini_file_settings$data_circles$color,
          fillColor = ini_file_settings$data_circles$fillColor,
          fillOpacity = as.numeric(ini_file_settings$data_circles$fillOpacity)
      )
    }else if(input$tab02_radGeocodeLevel=='OA'){
      data_labels <- sprintf(
        "Output Area: %s<br>Value: %.1f",
        rvData$data$oa11cd,
        rvData$data$value) %>%
        lapply(htmltools::HTML)

      data_palette <- colorNumeric(
        palette = ini_file_settings$data_shapefiles$fillPalette,
        domain = rvData$data$value, 
        na.color = ini_file_settings$data_shapefiles$na.color,
        reverse = as.logical(ini_file_settings$data_shapefiles$reverse)
      )
      
      map <- map %>% 
        addPolygons(
          data = sf_oa %>% left_join(rvData$data, by = c('OA11CD' = 'oa11cd')),
          label = data_labels,
          weight = as.numeric(ini_file_settings$data_shapefiles$weight),
          stroke = as.logical(ini_file_settings$data_shapefiles$stroke),
          color = ini_file_settings$data_shapefiles$color,
          fillColor = ~data_palette(value),
          fillOpacity = as.numeric(ini_file_settings$data_shapefiles$fillOpacity),
          highlightOptions = list(
            stroke = as.logical(ini_file_settings$data_highlightOptions$stroke),
            color = ini_file_settings$data_highlightOptions$color,
            weight = as.numeric(ini_file_settings$data_highlightOptions$weight),
            fill = as.logical(ini_file_settings$data_highlightOptions$fill),
            fillColor = ini_file_settings$data_highlightOptions$fillColor,
            fillOpacity = as.numeric(ini_file_settings$data_highlightOptions$fillOpacity),
            bringToFront = as.logical(ini_file_settings$data_highlightOptions$bringToFront)
          )
        )
      map <- map %>% 
        addLegend(
          data = sf_oa %>% left_join(rvData$data, by = c('OA11CD' = 'oa11cd')), 
          position = 'bottomright', 
          pal = data_palette, 
          values = ~value, 
          opacity = as.numeric(ini_file_settings$polygons$fill_opacity)
        )
    }else if(input$tab02_radGeocodeLevel=='LSOA'){
      data_labels <- sprintf(
        "Lower-layer Super Output Area: %s<br>Value: %.1f",
        rvData$data$lsoa11cd,
        rvData$data$value) %>%
        lapply(htmltools::HTML)
      
      data_palette <- colorNumeric(
        palette = ini_file_settings$data_shapefiles$fillPalette, 
        domain = rvData$data$value, 
        na.color = ini_file_settings$data_shapefiles$na.color, 
        reverse = as.logical(ini_file_settings$data_shapefiles$reverse)
      )
      
      map <- map %>% 
        addPolygons(
          data = sf_lsoa %>% left_join(rvData$data, by = c('LSOA11CD' = 'lsoa11cd')),
          label = data_labels,
          weight = as.numeric(ini_file_settings$data_shapefiles$weight),
          stroke = as.logical(ini_file_settings$data_shapefiles$stroke),
          color = ini_file_settings$data_shapefiles$color,
          fillColor = ~data_palette(value),
          fillOpacity = as.numeric(ini_file_settings$data_shapefiles$fillOpacity),
          highlightOptions = list(
            stroke = as.logical(ini_file_settings$data_highlightOptions$stroke),
            color = ini_file_settings$data_highlightOptions$color,
            weight = as.numeric(ini_file_settings$data_highlightOptions$weight),
            fill = as.logical(ini_file_settings$data_highlightOptions$fill),
            fillColor = ini_file_settings$data_highlightOptions$fillColor,
            fillOpacity = as.numeric(ini_file_settings$data_highlightOptions$fillOpacity),
            bringToFront = as.logical(ini_file_settings$data_highlightOptions$bringToFront)
          )
        )
      map <- map %>% 
        addLegend(
          data = sf_lsoa %>% left_join(rvData$data, by = c('LSOA11CD' = 'lsoa11cd')), 
          position = 'bottomright', 
          pal = data_palette, 
          values = ~value, 
          opacity = as.numeric(ini_file_settings$polygons$fill_opacity)
        )
    }
    if(input$tab02_radFootprint=='Practice'){
      map <- map %>% 
        addPolylines(
          data = sf_practice,
          color = ini_file_settings$boundaries$colour,
          weight = as.numeric(ini_file_settings$boundaries$weight),
          highlightOptions = list(
            stroke = TRUE,
            color = ini_file_settings$boundaries$highlight_colour,
            weight = as.numeric(ini_file_settings$boundaries$highlight_weight)
          )
        )
    } else if(input$tab02_radFootprint=='PCN'){
      map <- map %>% 
        addPolylines(
          data = sf_pcn,
          color = ini_file_settings$boundaries$colour,
          weight = as.numeric(ini_file_settings$boundaries$weight),
          highlightOptions = list(
            stroke = TRUE,
            color = ini_file_settings$boundaries$highlight_colour,
            weight = as.numeric(ini_file_settings$boundaries$highlight_weight)
          )
        )
    }
    map <- map %>%
      addControl(
        html = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0<br>Contains OS data © Crown copyright and database right [2022]',
        position = "bottomleft",
        layerId = NULL,
        className = "info legend"
      )
    rvMap$map <- map
  }
)

output$tab02_saveInteractiveMap <- downloadHandler(
  filename = 'map.html',
  content = function(file) {
    htmlwidgets::saveWidget(rvMap$map, file)  
  }
)

output$tab02_savePNGMap <- downloadHandler(
  filename = 'map.png',
  content = function(file) {
    mapshot(rvMap$map, file, vwidth = 1600, vheight = 1080)
  },
  contentType = 'image/png'
)

  