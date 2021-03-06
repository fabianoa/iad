library(shiny)

library(rmarkdown)

shinyUI(fluidPage(theme = "bootstrap3.css",
                  tags$head(includeScript("http://www.google-analytics.com/analytics.js")),
                  
                  navbarPage("Economic Dashboard",
                             tabPanel("Overview",
                                      sidebarLayout(
                                          sidebarPanel(    
                                              htmlOutput("MarketUpdate.Commentary")
                                          ),
                                          mainPanel(
                                              HTML("<h3>GDP Growth: History And Projections For Major Economies</h3> 
                                                   Charts show naive time-series forecasts for the next 4 quarters. All data shown as q/q growth rates (annualized).<p>Note: This page will take a few seconds to download the data."),
                                              plotOutput("Overview.Charts"),
                                              htmlOutput("UI.Date")
                                              ))),
                             tabPanel("Country View",
                                      sidebarLayout(
                                          sidebarPanel(
                                              uiOutput("UI.Country.Analysis"),
                                              htmlOutput("Commentary.Country.Analysis")
                                          ),
                                          mainPanel(
                                              HTML("<h3>Latest developments</h3>"),
                                              plotOutput("Charts.Country.Analysis")
                                          )
                                      )
                             ),
                             navbarMenu("Detailed Analysis",           
                                        tabPanel("US Activity Surveys",
                                                 h3("High-Frequency Surveys of US Economic Activity"),
                                                 HTML("High-frequency indicators have some interesting properties. <ul><li>They provide an early snapshot of economic activity in different sectors of the US economy. 
                                                      <li>Unlike GDP, which is only published quarterly and can be heavily revised,
                                                      these indicators are released monthly (and data revisions are small). <li>They are survey-based, and may thus exaggerate positive or negative sentiment.</ul><p>We have re-scaled the San Fransisco Fed Tech Pulse by multiplying it by 10. 
                                                      You can zoom into the charts using the scroll wheel of the mouse or using the date range selector underneath the chart."),
                                                 htmlOutput("US.ActivityMeasures.Dashboard")
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
                                                         conditionalPanel(condition = "input.EnsembleForecastingDescChoice"),
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
                             tabPanel("About"
                             )                   
                                                 )))