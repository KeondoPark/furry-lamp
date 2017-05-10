#Input data; the pre-processing of data by clustering is done in the preliminary step.
#This script focuses only on the visualisation fo the data.
library(reshape)
library(ggplot2)

Outliers_Join2 <- read.csv("data/WL12447_Ordered_Data.csv", header=T)
Medoids_Join2 <- read.csv("data/WL12447_Medoids.csv",header=T)


shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
      
      NumOutliers     <- input$NumOutliers    
      
      Outliers_Join3 <- Outliers_Join2[1:NumOutliers,2:53]
      colnames(Outliers_Join3)[3:52] <- c(1:50)
      
      Medoids_Join3 <- Medoids_Join2[,2:53]
      colnames(Medoids_Join3)[3:52] <- c(1:50)
      
      ggplot_Data   <- rbind(melt(Outliers_Join3, id = c("ID","Class")), melt(Medoids_Join3, id = c("ID","Class")))
      
      #ggplot
      ggplot_print <- ggplot(ggplot_Data, aes(x = variable, y = value, group = ID, color = Class, label = ID)) 
      ggplot_print <- ggplot_print + geom_line() 
      ggplot_print <- ggplot_print + geom_label(data = subset(ggplot_Data, variable == "40"))
    
      return(ggplot_print)
    
  })
})

