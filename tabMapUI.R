# ------------------------ #
# Desc: Map User Interface #
# File: tabMapUI.R         #
# ------------------------ #

tabPanel(
  title = 'Map',
  value = 'tab02',
  sidebarLayout(
    sidebarPanel(
      tags$hr(),
#      tags$h5(tags$strong('Boundaries')),
      radioButtons(inputId = 'tab02_radFootprint', label = 'Boundary', choices = c('Practice', 'PCN'), selected = 'PCN'),
      radioButtons(inputId = 'tab02_radGeocodeLevel', label = 'Geography', choices = c('Lat/Long', 'OA', 'LSOA')),
      actionButton(inputId = 'tab02_btnRefreshMap', label = 'Refresh Map'),
      tags$hr(),
      tags$h5(tags$strong('Downloads')),
      downloadButton(outputId = 'tab02_saveInteractiveMap', label = 'Save Interactive Map'),
      downloadButton(outputId = 'tab02_savePNGMap', label = 'Save PNG Map'),
      width = 2),
    mainPanel(
      tags$h3('Map'),
      leafletOutput(outputId = 'tab02_lftMap', width = '1400px', height = '900px'),
      tags$hr(),
      width = 10)
  )
)
