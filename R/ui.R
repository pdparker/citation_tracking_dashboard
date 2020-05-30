

## ui.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
load("../data/metrics_display.RData")

ui <- dashboardPage(
  dashboardHeader(title = "Publication Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Publications",
      tabName = "Publications",
      icon = NULL,
      
      switchInput(
        inputId = "long1",
        onLabel = "Go",
        offLabel = "NoGo",
        value = T
      ),
      actionButton("Gobtn", "Get data")
    )
  )),
  dashboardBody(tags$div(
    tabName = "Publications",
    fluidRow(box(DT::dataTableOutput("Table1"))), class = "tab-content"
  ))
)