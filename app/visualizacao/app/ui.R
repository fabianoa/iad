library(shiny)
library(rmarkdown)




shinyUI(fluidPage(theme = "bootstrap3.css",
                  tags$head(includeScript("http://www.google-analytics.com/analytics.js")),
                  navbarPage("O que estamos falando?",
                             tabPanel("Visão Geral",
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
                                          ))),
                             navbarMenu("Análise de Dados Demográficos",           
                                        tabPanel("Composição Partidária",
                                                 h3("US Labor Market Indicators: Current Values, Relative to the Worst Point This Cycle"),
                                                 h5("The large charts show the longer-term evolution since 2000; the small charts show progress among different labor market indicators during this recovery/expansion. The cycle is defined as starting in 2008;
                                                    current values are shown relative to the best and worst readings of this indicator during this cycle."),
                                                 plotOutput("composicaoPartidariaDashboard", height="1000px")
                                        ),
                                        tabPanel("US Labor Market",
                                                 h3("US Labor Market Indicators: Current Values, Relative to the Worst Point This Cycle"),
                                                 h5("The large charts show the longer-term evolution since 2000; the small charts show progress among different labor market indicators during this recovery/expansion. The cycle is defined as starting in 2008;
                                                    current values are shown relative to the best and worst readings of this indicator during this cycle."),
                                                 plotOutput("US.LaborMarket.Dashboard", height="1000px")
                                                 ),
                                        tabPanel("US Housing Market",
                                                 h3("US Housing Market Indicators"),
                                                 plotOutput("US.HousingMarket.Dashboard", height="1000px")
                                        ),
                                        tabPanel("US Vehicle Sales",
                                                 h3("US Vehicles Sales and Auto Market Indicators"),
                                                 plotOutput("US.AutoMarket.Dashboard", height="1000px")
                                        )
                                        
                                        
                                        ),
                             navbarMenu("Forecasting",
                                        tabPanel("Time Series Forecasting",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         uiOutput("UI.Macro.Control"),
                                                         uiOutput("UI.Variable.Control")
                                                         #checkboxInput("RegressionXREGControlChoice", "Add additional regressors", FALSE)
                                                     ),
                                                     mainPanel(
                                                         HTML("<h3>Time Series Forecasting: Comparison of Different Approaches</h3>"),
                                                         htmlOutput("Macro.Regression.Commentary"),
                                                         plotOutput("Macro.Chart"),
                                                         HTML("Regression specification (Arima model)"),
                                                         checkboxInput("UIRegressionSpecControl", "Show Regression Output", value=FALSE),
                                                         conditionalPanel(condition = "input.UIRegressionSpecControl",
                                                                          verbatimTextOutput("Macro.Regression")
                                                         )
                                                     )
                                                 )
                                        ),
                                        tabPanel("Ensemble Forecasting",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         selectInput("ForecastPooling.Selection",
                                                                     "Select A Variable To Forecast",
                                                                     c("Nonfarm Payrolls", "GDP") ),
                                                         htmlOutput("EnsembleForecast.Commentary")
                                                     ),
                                                     mainPanel(
                                                         checkboxInput("EnsembleForecastingDescChoice", "Show Model Description", FALSE),
                                                         conditionalPanel(condition = "input.EnsembleForecastingDescChoice",
                                                                          includeMarkdown("Description.EnsembleForecasting.md")),
                                                         textOutput("Forecast.Tracking"),
                                                         plotOutput("EnsembleForecast.Plot")
                                                     )))
                             ),
                             navbarMenu("Stock Market",
                                        tabPanel("Real-Time Data",
                                                 tableOutput("Data.Realtime")
                                        ),
                                        tabPanel("Historical Data",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         uiOutput("StockSelector")
                                                     ),
                                                     mainPanel(
                                                         plotOutput("TestPlot"),
                                                         
                                                         tableOutput("LatestValue")
                                                     )
                                                 )
                                        )
                             ),
                             tabPanel("Sobre",
                                      includeMarkdown("About.md")
                             )                   
                                                 )))