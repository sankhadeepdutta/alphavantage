# Libraries
library(shiny)
library(hrbrthemes)
library(alphavantager)
library(xts)
library(dygraphs)
library(shinythemes)
library(shinycssloaders)

#UI
ui <- fluidPage(theme = shinytheme("cerulean"),
    
    # App title ----
    titlePanel("Realtime Stock Prices"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            textInput(inputId = "comName", label = "Enter company name", value = "IBM"),
            
            selectInput(inputId = "outputSize", label = "Output Size", choices = c("compact" ,"full"), selected = c("compact"))),
    
        # Main panel for displaying outputs ----
        mainPanel(
            
            
            withSpinner(dygraphOutput(outputId = "tsPlot", width = "95%"), type = 5, color = "#16697a", size = 0.5)
            
        )
    )
)
#Server
server <- function(input, output){
    
    # Data Preparation Steps
    
    av_api_key("ESMLXJQO643EOF4S")
    
        output$tsPlot <- renderDygraph({
            
        data <- av_get(symbol = "IBM", av_fun = "TIME_SERIES_DAILY", outputsize = 
                               "compact", datatype = "csv")
        
        data$timestamp = as.Date(data$timestamp)
            
        don <- xts(x = data$open, order.by = data$timestamp)
        
        names(don) = "Data"
            
        dygraph(don) %>%
            
            dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.2, drawGrid = T, colors="#db6400") %>%
            
            dyRangeSelector() %>%
            
            dyCrosshair(direction = "vertical") %>%
            
            dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, 
                        hideOnMouseOut = T)  %>%
            
            dyRoller(rollPeriod = 1)
        })

}
shinyApp(ui = ui, server = server)