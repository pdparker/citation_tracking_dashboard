# Server.R ####
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(scales)

z <- function(x){round((x-mean(x))/sd(x),3)}
server <- shinyServer(function(input, output, session) {
  
  new_data <- reactive({
    input$Button
    isolate({
      datadata <-  metrics_display %>%
        filter(Year %in% input$year_input[1]:input$year_input[2]) %>%
        select(Title, Year, Citations, input$var_select) %>%
        mutate_at(vars(Citations,input$var_select), as.numeric) %>%
        group_by(Year) %>%
        mutate(baseline_cite = z(Citations)) %>%
        ungroup()
    })
    return(datadata)
  })
  
  long_data <- reactive({
    input$Button
    isolate({
      datadata <-  metrics_display %>%
        filter(Year %in% input$year_input[1]:input$year_input[2]) %>%
        select(Title, Year,Citations, input$var_select) %>%
        pivot_longer(cols = input$var_select, names_to = "Variable",
                     values_to = "Values") %>%
        mutate(Year = factor(as.character(Year)))
    })
  })
  
  log <- reactive({
    input$Button
    isolate({inp <- input$log})
  })
  
  output$Table1 <- DT::renderDataTable(
    datatable(new_data(), 
                                filter = 'top',
                                extensions = 'Buttons', options = list(
                                  dom = 'Bfrtip',
                                  style = 'bootstrap',
                                  buttons = 
                                    list('copy', 'print', list(
                                      extend = 'collection',
                                      buttons = c('csv', 'excel', 'pdf'),
                                      text = 'Download'
                                    ))) 
  ))
  
  output$Plot = renderPlot({
    p <- long_data() %>%
      mutate(
        Citations = as.numeric(Citations)+.001,
        Values = as.numeric(Values)+.001
      ) %>%
      ggplot(aes(x = Citations, y = Values, colour = Year)) +
      geom_point() +
      facet_grid(~Variable)
    if(log()==FALSE){
      p
    }else{
      p +
        scale_x_continuous(trans = 'log10',
                           breaks = trans_breaks("log10", function(x) 10^x),
                           labels = trans_format("log10", math_format(10^.x))) +
        scale_y_continuous(trans = 'log10',
                           breaks = trans_breaks("log10", function(x) 10^x),
                           labels = trans_format("log10", math_format(10^.x)))
    }
    
  })
  
  }
)


