#Problem 2
library(rcrimeanalysis)
library(dplyr)
data("crimes")
crimes.NAR <- subset(crimes,crimes$primary_type =="NARCOTICS")
data<-as.data.frame(as.numeric(unlist(crimes.NAR[,20])))
LatitudeSD <- as.numeric(lapply(data,sd))
LatitudeSE <- LatitudeSD/sqrt(nrow(data))
lapply(data,IQR)
latitude <- hist(data[,1],breaks="FD",col="red",xlab = "Latitude",
                 main="Histogram of Latitude")
d <- density(data[,1],bw=0.0267)
plot(d, main= "Kernel Density of Latitude")
polygon(d,col="red", border = "red")
#xfit<-seq(min(data[,1]),max(data[,1]),length=40)
#yfit<-dnorm(xfit,mean = mean(data[,1]),sd=sd(data[,1]))
#yfit<-yfit*diff(latitude$mids[1:2])
#lines(xfit,yfit, col="blue", lwd=2)
data<-as.data.frame(as.numeric(unlist(crimes.NAR[,21])))
LongitudeSD <- as.numeric(lapply(data,sd))
LongitudeSE <- LongitudeSD/sqrt(nrow(data))
lapply(data,IQR)
longitude <- hist(data[,1],breaks="FD",col="red",xlab = "Longitude",
                  main="Histogram of Longitude")
d <- density(data[,1],bw = 0.0217)
plot(d, main= "Kernel Density of Longitude")
polygon(d,col="red", border = "blue")
#Problem 3
#KMeans
#We load our packages
library(factoextra)
library(cluster)
#Set our directory
setwd("\\Users\\adfig\\Desktop")
#Clean the workspace
rm(list=ls())
#Load our data
Data = read.csv("Kmeans.csv", header=T)
#perform k-means clustering with k = 3 clusters
#setting seed
#set.seed(123)
km <- kmeans(Data, centers = 3, nstart = 1)
#plot results of final k-means model
fviz_cluster(km, data = Data,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "Converged"
)


#Problem 4
library(mclust)
data(iris)
data = (iris[,-5])
class= (iris[,5])
BIC <- mclustBIC(data)
plot(BIC)
x<-clPairs(data,class)

summary(BIC)

mod1 <- Mclust(data, x = BIC)
summary(mod1, parameters = TRUE)
