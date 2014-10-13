library(shiny)
library(ggplot2)
library(plyr)
library(reshape2)

# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)

dados <-readRDS(paste(getwd(),"/data/listaQtAbsolutaDiscursosPorPartidos.Rda",sep = ''))



# Define a server for the Shiny app
shinyServer(function(input, output) {
    
    result <- reactive({
        
        
        result<-dados[dados$Ano==input$ano,]
        
      
    })
    
    
    
    # Fill in the spot we created for a plot
    output$phonePlot <- renderPlot({
        
        # Render a barplot
       # barplot(result2)
         
        ggplot(data=result(), aes(
            
            x=Partido
            
           , y=Quantidade, fill=Partido)) + geom_bar(stat="identity") + 
             theme(axis.text.x = element_text(angle = 90, hjust = 1))
        

        
    })
})