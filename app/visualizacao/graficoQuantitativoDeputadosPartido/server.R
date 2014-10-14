
quantitativo <- readRDS("app/dados/processados/quantitativoDeputadorPartido.Rda")


shinyServer(
    function(input, output) {
        output$map <- renderPlot({
            c <- ggplot(quantitativo, aes(x=factor(nrow(quantitativo)),fill=graphX),environment=environment()) + geom_bar(width = 1)
           
            print(c + coord_polar(theta = "y")+ xlab("xLabel")+ylab("yLabel")+ labs(fill="legendTitle")+ theme(
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                panel.background = element_blank(),
                plot.background = element_blank()
            ))
            
            
            
            })
    }
)