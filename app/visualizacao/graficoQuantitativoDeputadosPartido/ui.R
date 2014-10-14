# ui.R

shinyUI(fluidPage(
    titlePanel("censusVis"),
    
    sidebarLayout(
        mainPanel(plotOutput("map"))
    )
))