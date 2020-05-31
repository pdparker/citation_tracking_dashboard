

## ui.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Publication Dashboard"),
  dashboardSidebar(
    sliderInput("year_input", "Years:",
                min = 2008, max = 2020, value = c(2008, 2020), step = 1,
                sep = "", ticks = FALSE),
    checkboxGroupInput("var_select", "Variables:",
                       choices = list("Alt Metrics" = "Alt", 
                                      "Abstract Views" = "Views", 
                                      "Downloads" = "Downloads",
                                      "Social Media" = "Social"),
                       selected = "Social"),
    checkboxInput("log", "Log Variables", FALSE),
    actionButton("Button", "Update")
    ),
  dashboardBody(tags$div(
    tabName = "Publications",
    fluidRow(
      column(12, plotOutput('Plot', height = 500))
    ),
    fluidRow(
      column(12, DT::dataTableOutput('Table1'))
      )))
)