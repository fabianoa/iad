library(shiny)

# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)







# Define a server for the Shiny app
shinyServer(function(input, output) {

    dados <-readRDS("C:/R/iad/app/dados/processados/listaQtAbsolutaDiscursosPorPartidos.Rda")
    result<-dados[dados$Ano==2012,]
    
    deputados<- obterDadosDeputados()
 
    dt1<-data.table(as.data.frame(table(deputados$Partido))) 
    
    setkeyv(dt1, 'Nome')
    dt1 <- dt1[order(Nome),] 
    
    d<-merge(result,t, by='Partido')
    
    
    factor(result$Partido, levels = result$Partido[order(result$Quantidade)])
 
    
    
    
    class(result)    
    # Fill in the spot we created for a plot
    output$phonePlot <- renderPlot({
        
        # Render a barplot
       # barplot(result2)
        
        library(ggplot2)
        library(plyr)
        library(reshape2)
        
        ggplot(data=result, aes(
            
            x=Partido
            
           , y=Quantidade, fill=Partido)) + geom_bar(stat="identity") + 
             theme(axis.text.x = element_text(angle = 90, hjust = 1))
        

        
    })
})