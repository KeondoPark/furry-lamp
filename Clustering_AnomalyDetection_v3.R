#Data processing
# This script detects abnormal SURRENDER BENEFIT cash outflows from WL products.
#Input data
RawData <- read.delim("LiabCF_Surr_WL12447.tsv",header=T, sep = "\t")
MPFile <- read.csv("MP_12447.csv",header=T)

Join_Data <- cbind(MPFile, RawData)

columnPP <- Join_Data$PP

RowNum <- nrow(Join_Data)

mydata <- data.frame(matrix(rep(0,RowNum * 100), nrow = RowNum, ncol = 100))

for (i in 1:RowNum){
  PP <- columnPP[i]
  
  if (PP < 10 || PP >=50){
    mydata[i,] <- cbind(Join_Data[i,19:68], t(rep(0,50)))
  } else {
    mydata[i,] <- cbind(Join_Data[i,19:(19+PP-1)], t(rep(Join_Data[i,19+PP-1],(50-PP))), Join_Data[i,(19+PP):68], t(rep(Join_Data[i,68],PP)))
  }
}

# # Determine the number of clusters; kmeans algorithm
# # This step can be ignored
# wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")
# 

#-----------Sohyun Test: Start
#reference page: http://www.sthda.com/english/wiki/partitioning-cluster-analysis-quick-start-guide-unsupervised-machine-learning#pam-partitioning-around-medoids
#k-means 
#library(cluster)
#library(factoextra)
fit2 <- kmeans(mydata, centers = 2, iter.max =10)
fit2$cluster
fit2$size
fit2$centers
fviz_cluster(fit2, data=mydata)

fviz_nbclust(mydata, kmeans, method = "wss") +  geom_vline(xintercept = 4, linetype = 2) 
#wss (within sum of square) is drawn according to the number of clusters
#can choose k=3~4 with the location of a bend in the plot
graphics.off()

fviz_nbclust(mydata, pam, method ="silhouette" ) 
#can choose k=2 with the location of a bend in the plot

#check cluster plot
fviz_cluster(fit2, data = mydata)



#pam
fit.pam <- pam(mydata, 2)
head(fit.pam$medoids)
clusplot(fit.pam, main = "Cluster plot, k = 2", color = TRUE)
fviz_nbclust(mydata, pam, method = "wss") +  geom_vline(xintercept = 2, linetype = 2)
fviz_cluster(fit.pam, data = mydata)
fviz_silhouette(silhouette(fit.pam))
#-----------Sohyun Test: End



# #Import library
# library(fpc)
# 
# #Do Clustering, k-medoids algorithm
fit <- pamk(mydata) 
fit2 <- kmeans(mydata, centers = 2)

dist_mydata <- dist(mydata)
hclust_mydata <- hclust(dist_mydata)
mydata_order <- mydata[hclust_mydata$order,]
mydata_outliers <- mydata_order[1:5,]
mydata$hCluster <- cutree(hclust_mydata,5)

# 
# #Plot the result, two plots in one window
# layout(matrix(c(1,2),1,2))
# plot(fit$pamobject, labels = 1)


##kmeans clustering
##Determine the number of Clusters from the plot
#nCluster <- 3
#fit <- kmeans(mydata, nCluster) 

##append cluster assignment
#mydata <- data.frame(mydata, fit$cluster)

##Plot the result
## vary parameters for most readable graph
#library(cluster)
#clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#Outlier detection
Cluster_Order <- fit$pamobject$silinfo$widths

mydata_Order <- cbind(mydata, Cluster_Order)
mydata_Order_Rev <- mydata_Order[order(mydata_Order$sil_width),]

Outliers_Join <- Join_Data[rownames(mydata_Order_Rev),19:68]
Outliers_Join2 <- cbind(rownames(Outliers_Join), rep("Outliers",RowNum), Outliers_Join)
colnames(Outliers_Join2)[1:2] <- c("ID", "Class")
colnames(Outliers_Join2)[3:52] <- c(1:50)


#Medoids data 
NumMedoids <- nrow(fit$pamobject$medoids)
mydata_Medoids <- mydata[fit$pamobject$id.med,]
Medoids_Join <- Join_Data[rownames(mydata_Medoids),19:68]
colnames(Medoids_Join) <- c(1:50)

Medoids_Join2 <- cbind(rownames(Medoids_Join), rep("Medoids",NumMedoids), Medoids_Join)
colnames(Medoids_Join2)[1:2] <- c("ID", "Class")


#Export the clustering result to CSV files
write.csv(Medoids_Join2, file = "WL12447_Medoids.csv")
write.csv(Outliers_Join2, file = "WL12447_Ordered_Data.csv")


#Plot using ggplot

Outliers_Join3 <- Outliers_Join2[1:5,]

ggplot_Data <- rbind(melt(Outliers_Join3, id = c("ID","Class")), melt(Medoids_Join2, id = c("ID","Class")))

ggplot(ggplot_Data, aes(x = variable, y = value, group = ID, color = Class, label = ID)) + geom_line() + geom_label(data = subset(ggplot_Data, variable == "40"))


# plot using default plot method
# layout(matrix(c(1,1),1,1))
# 
# #plot outliers
# for(i in 1:5){
#   plot(c(1:100),mydata_Outliers[i,1:100], ylim = c(0,1),col = 10, pch  = i)
#   lines(c(1:100),mydata_Outliers[i,1:100], ylim = c(0,1),col = 10, pch = i)
#   par(new = TRUE)
# }
# legend(90,1,legend = rownames(mydata_Outliers), pch = c(1:5), col = 10, cex= 0.7, title = "Outliers")
# par(new = TRUE)
# 
# #plot Medoids
# NumMedoids <- nrow(fit$pamobject$medoids)
# mydata_Medoids <- mydata[fit$pamobject$id.med,]
# 
# for(i in 1:NumMedoids){
#   plot(c(1:100),mydata_Medoids[i,1:100], ylim = c(0,1), pch = 5+i)
#   lines(c(1:100),mydata_Medoids[i,1:100], ylim = c(0,1), pch = 5+i)
#   par(new = TRUE)
# }
# 
# legend(90,0.5,legend = rownames(mydata_Medoids), col = 1, pch = c(6:(5+NumMedoids)), cex=0.7, title = "Medoids")
# 
# par(new = FALSE)

