# Server.R ####
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(scales)
library(plotly)
load("../data/metrics_display.RData")


z <- function(x){round((x-mean(x))/sd(x),3)}
server <- shinyServer(function(input, output, session) {
  
value_data <- reactive({
    input$Button
    isolate({
      datadata <-  metrics_display %>%
        filter(Year %in% input$year_input[1]:input$year_input[2]) %>%
        mutate(SNIP = as.numeric(SNIP), SJR = as.numeric(SJR)) %>%
        summarise(SNIP = round(mean(SNIP, na.rm=TRUE),2),
                  SJR = round(mean(SJR, na.rm=TRUE),2))
    })
    return(datadata)
  })
  
  new_data <- reactive({
    input$Button
    isolate({
      datadata <-  metrics_display %>%
        mutate(Title = paste0("<a href='https://www.doi.org/", DOI,"' target='_blank'>", Title,"</a>")) %>%
        select(-DOI) %>%
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
      s = input$Table1_rows_selected
      datadata <-  metrics_display %>%
        select(-DOI) %>%
        filter(Year %in% input$year_input[1]:input$year_input[2]) %>%
        select(Title, Year,Citations, input$var_select) %>%
        rownames_to_column() %>%
        mutate(selector = ifelse(rowname %in% s,"selected","not selected") ) %>%
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
    datatable(new_data(), escape=FALSE,
                                filter = 'top',
                                extensions = 'Buttons', options = list(
                                  dom = 'Bfrtip',
                                  style = 'bootstrap',
                                  buttons = 
                                    list('copy', 'print', list(
                                      extend = 'collection',
                                      buttons = c('csv', 'excel', 'pdf'),
                                      text = 'Download'
                                    ))) ) %>%
      formatStyle(
        columns = 'Title',
        valueColumns = 'baseline_cite',
        background = styleColorBar(range(new_data()$baseline_cite, na.rm=TRUE),
                                   'lightblue'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center')
    )
  
  output$Plot = renderPlotly({
    p <- long_data() %>%
      mutate(
        Citations = as.numeric(Citations)+.001,
        Values = as.numeric(Values)+.001
      ) %>%
      ggplot(aes(x = Citations, y = Values, colour = Year, label=Title)) +
      geom_point(aes(size = selector)) +
      facet_grid(~Variable) +
      theme(legend.position = "none") 
    if(log()==TRUE){
      p <- p +
        scale_x_continuous(trans = 'log10',
                           breaks = trans_breaks("log10", function(x) 10^x),
                           labels = trans_format("log10", math_format(10^.x))) +
        scale_y_continuous(trans = 'log10',
                           breaks = trans_breaks("log10", function(x) 10^x),
                           labels = trans_format("log10", math_format(10^.x)))
      
      
    }
    ggplotly(p,tooltip = c("label", "colour") )
  })
  

  output$box_sjr <- renderValueBox({
    valueBox(
      subtitle = "SJR Mean",
      value = value_data()$SJR,
      #icon = "fa-area-chart"
      color = case_when(
        value_data()$SJR < 2 ~ 'red',
        value_data()$SJR >= 2.5 ~ 'green',
        TRUE ~ 'orange'
        )
    )
  })
  
  output$box_snip <- renderValueBox({
    valueBox(
      subtitle = "SNIP Mean",
      value = value_data()$SNIP,
      #icon = "fa-area-chart"
      color = case_when(
        value_data()$SJR < 1.5 ~ 'red',
        value_data()$SJR >= 2 ~ 'green',
        TRUE ~ 'orange'
      )
    )
  })
  
  
  }
)


