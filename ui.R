# ------------------------------------------------------ #
# Desc: User Interface File for Primary Care Mapping App #
# File: ui.R                                             #
# ------------------------------------------------------ #
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel('Primary Care Mapping'),
  tabsetPanel(
    id = 'tabPanel',
    type = 'tabs',
    source('tabSetupUI.R', local = TRUE)$value,
    source('tabMapUI.R', local = TRUE)$value
  )
)
