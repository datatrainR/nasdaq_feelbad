#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(quantmod)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        # Function reading inputs + calculating returns
        getStock <- reactive({ 
                #-------------------------------------
                # Inputs
                #-------------------------------------
                
                # Initial capital
                start.capital = input$start.capital
                
                # Date Range
                date.range = input$date.range
                
                # Stock selection
                select.stock = input$select.stock
                
                #-------------------------------------
                # Data Retrieval
                #-------------------------------------
 
                stockseries = getSymbols(select.stock, src = "yahoo", from = date.range[1], to = date.range[2])
                
                #-------------------------------------
                # Calculate Return
                #-------------------------------------
                
                return <- (as.numeric(tail(eval(parse(text = stockseries))[,4],1))/as.numeric(head(eval(parse(text = stockseries))[,4],1))-1)
                end.capital <- round(start.capital*(1+return), digits = 0)
                
                #-------------------------------------
                # Return Output
                #-------------------------------------
                               
                returnlist <- list(eval(parse(text = stockseries)), start.capital, end.capital)
                names(returnlist) <- c("stock.name", "start.capital", "end.capital")
                
                return(returnlist)  
        })
        
        # Using the quantmod plotter
        output$quantplot <- renderPlot({
                stock <- getStock()
                chartSeries(stock$stock.name,theme="white")
                
        })
        
        # Printing the price return 
        output$return <- renderText({ 
                stock <- getStock()
                paste0("Your investment of USD ",stock$start.capital," on ", input$date.range[1], " would have turned into USD ",stock$end.capital," by ", input$date.range[2])
        })
        
})
