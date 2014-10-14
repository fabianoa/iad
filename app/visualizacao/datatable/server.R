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
        result$Partido1 <- factor(result$Partido, as.character(result$Partido))
        result
    })
    
    
    
    # Fill in the spot we created for a plot
    output$quantitativoDiscursos <- renderPlot({
        
        
        
        
        ggplot(data=result(), aes(x=Partido1,y=Quantidade,fill=Partido1)) + geom_bar(aes(fill = factor(Quantidade)),colour = "black",stat="identity") +coord_flip()
        
        
        
      
        
    })
    
    
    
    
})