library(shiny)
library(rmarkdown)




shinyUI(fluidPage(theme = "bootstrap3.css",
                  tags$head(includeScript("http://www.google-analytics.com/analytics.js")),
                  navbarPage("O que estamos falando?",
                             tabPanel("Visão Geral",h3("Bem Vindo")
                                      
                                 ),
                             navbarMenu("Análise de Dados Demográficos",           
                                        tabPanel("Composição Partidária",
                                                 h3("US Labor Market Indicators: Current Values, Relative to the Worst Point This Cycle"),
                                                 h5("The large charts show the longer-term evolution since 2000; the small charts show progress among different labor market indicators during this recovery/expansion. The cycle is defined as starting in 2008;
                                                    current values are shown relative to the best and worst readings of this indicator during this cycle."),
                                                 plotOutput("composicaoPartidariaDashboard", height="1000px")
                                        )
                                        
                                        
                                        ),
                             navbarMenu("Analise Conteúdo Discursos",
                                        tabPanel("Quantitivo de Discursos porPartido", sidebarLayout(      
                                            
                                            # Define the sidebar with one input
                                            sidebarPanel(
                                                selectInput("legislatura", "Legislatura:", 
                                                            choices=c(54)),
                                                
                                                selectInput("ano", "Ano:", 
                                                            choices=c(2011,2012,2013,2014)),
                                                hr(),
                                                helpText("Fonte: Dados abertos da câmara de deputados.")
                                            ),
                                            
                                            # Create a spot for the barplot
                                            mainPanel(
                                                plotOutput("phonePlot")  
                                            )))
                                       
                             ),
                             navbarMenu("Analise de Resultados",
                                        tabPanel("Analise 1111",
                                                 tableOutput("Data.Realtime")
                                        )
                             ),
                             tabPanel("Sobre",
                                      includeMarkdown("About.md")
                             )                   
                                                 )))