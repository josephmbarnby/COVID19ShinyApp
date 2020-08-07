#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages(c('ggplot2', 'shiny', 'plotly', 'sf'))

library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

df <- read.csv("shinyappdata.csv")
df$Date <- as.Date(df$Date)
df$Cases <- as.numeric(df$Cases)

ui <- fluidPage(
    
    tags$head(
        tags$style("label{font-family: Helvetica;}")
    ),
    
    titlePanel("COVID-19 cases in London by borough between March and August 2020"), 
        
    mainPanel(
        br(),
        div("Data from https://data.london.gov.uk/dataset/coronavirus--covid-19--cases", style = "color:grey"),
        br(),
        
        tabsetPanel(
            tabPanel("Interactive Plot", plotlyOutput("plot")), 
            tabPanel("Data Table", DT::dataTableOutput("ProgressTable", height = 1000, width = 1000))
        )
    )
)


server <- function(input, output) {
    
    set.seed(122)
    
    output$plot <- renderPlotly({
        
        
        plot1 <- ggplot(df) +
            
            geom_line(aes(x=Date, y=Cases, colour=Area.Name)) +
            
            #labs(
            #    title = "COVID-19 cases in London by borough between March and April 2020",
            #    subtitle = 'Data from https://data.london.gov.uk/dataset/coronavirus--covid-19--cases')+
            
            bbplot::bbc_style() 
            #theme(plot.subtitle = element_text(family = "Helvetica", face = 'italic', size = 12, color = "#696969"),
            #      plot.title = element_text(family = "Helvetica", size = 14))
        
        plot1 %>%
            ggplotly(`Area Name` = "tooltip") %>% 
            layout(dragmode = "select",
                   legend = list(title = list(text = '<b> Borough </b>'),
                                 font = list(size = 14)))
    })
    
    output$ProgressTable <- DT::renderDataTable(
        
        df %>% 
            as.data.frame() %>% 
            select(Area.Name, Date, Cases) %>% 
            distinct() 
    )
}

shinyApp(ui, server)

