library(shiny)

# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)


# Define the overall UI
shinyUI(
    
    
    # Use a fluid Bootstrap layout
    fluidPage(    
        
        # Give the page a title
        titlePanel("Discrusos por partidos "),
        
        # Generate a row with a sidebar
        sidebarLayout(      
            
            # Define the sidebar with one input
            sidebarPanel(
                selectInput("ano", "Ano:", 
                            choices=c(2011,2012,2013,2014)),
                hr(),
                helpText("Fonte: Dados abertos da câmara de deputados.")
            ),
            
            # Create a spot for the barplot
            mainPanel(
                plotOutput("phonePlot")  
            )
            
        )
    )
)


