# --------------------------- #
# Desc: Setup Tab Server File #
# File: serverImportData.R                 #
# ---------------------------------------- #

# rvIniFileSections <- reactive(
#   {
#     req(input$tab01_filIni)
#     shinyjs::show(id = 'tab01_filData')
#     return(read.ini(input$tab01_filIni$datapath))
#   }
# )
# 
# observeEvent(input$tab01_filIni, shinyjs::show(id = 'tab01_filData'))

rvData <- reactiveValues(data = data.frame())

observeEvent(
  input$tab01_filImport,
  {
    req(input$tab01_filImport)
    showTab(inputId = 'tabPanel', target = 'tab02')
    df <- read.csv(input$tab01_filImport$datapath)
    updateSelectInput(session, inputId = 'tab01_fldPostcode', choices = names(df), selected = NULL)
    updateSelectInput(session, inputId = 'tab01_fldGroupBy', choices = names(df), selected = NULL)
    updateSelectInput(session, inputId = 'tab01_fldAggregate', choices = names(df), selected = NULL)
    rvData$data <- df
  }
)

observeEvent(input$tab01_btnGeocode, {
  if(input$tab01_radGeocodeLevel=='Lat/Long') {
    df_postcode_lookup <- df_postcode_lookup %>% select(postcode, latitude, longitude)
  } else if(input$tab01_radGeocodeLevel=='OA'){
    df_postcode_lookup <- df_postcode_lookup %>% select(postcode, oa11cd)
  } else if(input$tab01_radGeocodeLevel=='LSOA'){
    df_postcode_lookup <- df_postcode_lookup %>% select(postcode, lsoa11cd)
  }
  rvData$data <- rvData$data %>% left_join(df_postcode_lookup, by = structure(names = input$tab01_fldPostcode, .Data = 'postcode'))
  rvData$data <- rvData$data %>% rename(postcode = input$tab01_fldPostcode)
  updateSelectInput(session, inputId = 'tab01_fldPostcode', choices = names(rvData$data), selected = NULL)
  updateSelectInput(session, inputId = 'tab01_fldGroupBy', choices = names(rvData$data), selected = NULL)
  updateSelectInput(session, inputId = 'tab01_fldAggregate', choices = names(rvData$data), selected = NULL)
})

observeEvent(input$tab01_btnAggregate, {
  if(input$tab01_radFunction=='Count') {
    rvData$data <- rvData$data %>% group_by_at(input$tab01_fldGroupBy) %>% summarise(value = n()) %>% ungroup()
  } else if(input$tab01_radFunction=='Sum'){
    rvData$data <- rvData$data %>% group_by_at(input$tab01_fldGroupBy) %>% summarise_at(.vars = input$tab01_fldAggregate, .funs = sum) %>% ungroup()
    names(rvData$data) <- gsub(input$tab01_fldAggregate, 'value', names(rvData$data))
  } else if(input$tab01_radFunction=='Mean'){
    rvData$data <- rvData$data %>% group_by_at(input$tab01_fldGroupBy) %>% summarise_at(.vars = input$tab01_fldAggregate, .funs = mean) %>% ungroup()
    names(rvData$data) <- gsub(input$tab01_fldAggregate, 'value', names(rvData$data))
  } else if(input$tab01_radFunction=='Median'){
    rvData$data <- rvData$data %>% group_by_at(input$tab01_fldGroupBy) %>% summarise_at(.vars = input$tab01_fldAggregate, .funs = median) %>% ungroup()
    names(rvData$data) <- gsub(input$tab01_fldAggregate, 'value', names(rvData$data))
  }
  
  updateSelectInput(session, inputId = 'tab01_fldPostcode', choices = names(rvData$data), selected = NULL)
  updateSelectInput(session, inputId = 'tab01_fldGroupBy', choices = names(rvData$data), selected = NULL)
  updateSelectInput(session, inputId = 'tab01_fldAggregate', choices = names(rvData$data), selected = NULL)
  #write.csv(rvData$data, 'temp.csv')
})

output$tab01_dtProcessedData <- renderDataTable(
  rvData$data,
  filter = list(position = 'top'), 
  options = list(
    search = list(regex = TRUE, smart = TRUE, caseInsensitive = TRUE)
  )
)


