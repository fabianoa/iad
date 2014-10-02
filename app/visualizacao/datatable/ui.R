library(shiny)
library(ggplot2)  # for the diamonds dataset

shinyUI(fluidPage(
    title = 'Examples of DataTables',
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                'input.dataset === "diamonds"',
                checkboxGroupInput('show_vars', 'Columns in diamonds to show:',
                                   names(dataset), selected = names(dataset))
            )
        ),
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel('Lista de Discursos', dataTableOutput('mytable1'))
            )
        )
    )
))