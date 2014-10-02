library(shiny)

shinyServer(function(input, output) {
    
    # a large table, reative to input$show_vars
    output$mytable1 <- renderDataTable({
        library(ggplot2)
        dataset[, input$show_vars, drop = FALSE]
    })
    
       
})