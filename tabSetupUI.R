# -------------------------- #
# Desc: Setup User Interface #
# File: tabSetupUI.R         #
# -------------------------- #

tabPanel(
  title = 'Setup',
  value = 'tab01',
  sidebarLayout(
    sidebarPanel(
      tags$hr(),
      tags$h5(tags$strong('Import Data')),
      fileInput(inputId = 'tab01_filImport', label = '', multiple = FALSE, accept = '.csv'),
      tags$hr(),
      tags$h5(tags$strong('Geocode Postcode Data')),
      selectInput(inputId = 'tab01_fldPostcode', label = 'Postcode Field', choices = 'No Fields Available', selected = 'No Fields Available'),
      radioButtons(inputId = 'tab01_radGeocodeLevel', label = 'Level', choices = c('Lat/Long', 'OA', 'LSOA'), selected = 'Lat/Long'),
      actionButton(inputId = 'tab01_btnGeocode', label = 'Geocode'),
      tags$hr(),
      tags$h5(tags$strong('Aggegrate Data')),
      selectInput(inputId = 'tab01_fldGroupBy', label = 'Grouping Fields', multiple = TRUE, choices = 'No Fields Available', selected = 'No Fields Available'),
      selectInput(inputId = 'tab01_fldAggregate', label = 'Aggegrate Field', choices = 'No Fields Available', selected = 'No Fields Available'),
      radioButtons(inputId = 'tab01_radFunction', label = 'Function', choices = c('Count', 'Sum', 'Mean', 'Median'), selected = 'Count'),
      actionButton(inputId = 'tab01_btnAggregate', label = 'Aggregate'),
      width = 2),
    mainPanel(
#      tags$h5(tags$strong('Raw Data')),
#      tableOutput(outputId = 'tab01_tblRawData'),
      tags$h5(tags$strong('Processed Data')),
      tags$hr(),
      tags$head(tags$style('#tab01_dtProcessedData {white-space: nowrap;}')),
      DT::dataTableOutput(outputId = 'tab01_dtProcessedData'),
      tags$hr(),
      width = 10)
  )
)
