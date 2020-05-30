# Server.R ####
library(shiny)
library(shinydashboard)
library(DT)
server <- shinyServer(function(input, output, session) {
  output$Table1 <- DT::renderDT(metrics_display, filter = 'top',
                                options = list(rowCallback = JS('
                       function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                       if (parseFloat(aData[3]) < aData[7])
                       $("td:eq(1)", nRow).css("color", "red");
                       if (parseFloat(aData[3]) >= aData[7])
                       $("td:eq(1)", nRow).css("color", "green");
                       }') ))
  }
)


