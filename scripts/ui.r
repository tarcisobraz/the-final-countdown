library(shiny)

# Define UI 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Contagem regressiva"),
  
  # Sidebar with controls to select the dataset and forecast ahead duration
  sidebarPanel(
    selectInput("variable", "Açude:",
                list("Açude Epitácio Pessoa - Boqueirão" = "boqueirao", 
                     "Coremas" = "coremas",
                     "Mãe d'agua" = "maedagua")),
    numericInput("ahead", "Quantidade de meses na previsão:", 12),
    
    submitButton("Atualizar")
  ),
  

  
  # Show the caption and forecast plots
  mainPanel(
    h3(textOutput("caption")),
    
    tabsetPanel(
      tabPanel("Exponetial Smoothing (ETS) Forecast", plotOutput("etsForecastPlot")), 
      tabPanel("Arima Forecast", plotOutput("arimaForecastPlot")),
      tabPanel("Timeseries Decomposition", plotOutput("dcompPlot"))
    )
  )
))
