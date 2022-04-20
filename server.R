# ---------------------------------------------- #
# Desc: Server File for Primary Care Mapping App #
# File: server.R                                 #
# ---------------------------------------------- #
shinyServer(function(session, input, output) {
  source('serverSetup.R', local = TRUE, encoding = "UTF-8")
  source('serverMap.R', local = TRUE, encoding = "UTF-8")
  hideTab(inputId = 'tabPanel', target = 'tab02')
#  shinyjs::hide(id = 'tab01_filData') 
  }
)

