library(shiny)

shinyUI(fluidPage(
  titlePanel("Outlier detection for Surrender benefit"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("The plot shows the most common surrender benefit patterns(Medoids, Blue colour) and the most outlying patterns(Outliers, Red colour)"),
      
      sliderInput("NumOutliers", label = h3("Number of Outliers"),
                  min = 1, max = 30, value = 5)
    ),
    
    mainPanel(plotOutput("plot")
    )
  )
))




