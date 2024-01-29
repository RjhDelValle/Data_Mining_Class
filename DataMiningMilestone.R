#The Data processing for the Data Mining Project
#We call our packages that we have used so far in our semester
library(dplyr)
library(GGally)
library(MASS)
library(hrbrthemes)
library(viridis)
library(mclust)
library(factoextra)
library(cluster)
#Set up the working directory
setwd("\\Users\\adfig\\Desktop\\DataMining")
#CLEAN WORKSPACE
rm(list = ls())
#We upload our dataset
Data = read.csv("data.csv", header=T)
Data=Data[,c(4:9,16)]
#We create a function to separate our variables with the output.
VarOut <- function(x){
 y=NULL
 i=1
 repeat{
  y[[i]]=cbind(x[[i]],x[,ncol(x)])
  assign(paste0(toString(colnames(x[i])),"_",colnames(x[ncol(x)])), as.data.frame(y[[i]]),envir = globalenv())
  i=i+1
  if(i>(ncol(x)-1))break()
 }
 }



#We apply VarOut to our Dataset
VarOut(Data)
#We apply elbow method
set.seed(125)
fviz_nbclust(density_ZT, kmeans, method = "wss")
fviz_nbclust(e_total_ZT, kmeans, method = "wss")
fviz_nbclust(efermi_ZT, kmeans, method = "wss")
fviz_nbclust(energy_above_hull_ZT, kmeans, method = "wss")
fviz_nbclust(energy_per_atom_ZT, kmeans, method = "wss")
fviz_nbclust(band_gap_ZT, kmeans, method = "wss")
#We now apply the KMeans Clustering for each component
#V1 is our input and V2 our Output
Km<- function(data,input_output, centers){
km <- kmeans(data, centers = centers, nstart = 10)
fviz_cluster(km, data = data,
             palette = "Set1", 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = paste0(toString(input_output))
)
}

Km(e_total_ZT,"e_total_ZT",4)
Km(density_ZT,"Density_ZT",4)
Km(energy_above_hull_ZT,"Energy_above_hull_ZT",4)
Km(energy_per_atom_ZT,"Energy_per_atom_ZT",4)
Km(efermi_ZT,"EFermi_ZT",4)
Km(band_gap_ZT,"Band_Gap_ZT",4)
#We now begin to apply Gaussian mixture modeling
#Lets reload our data
Data = read.csv("data.csv", header=T)
Data=Data[,c(4:9,16)]
#First However
#I want to organize these ZT values into different categories
summary(Data[,7])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.03725 0.06693 0.10139 0.10076 1.94577
#We create our peformance measure function
ZTClass<-function(x){
  if (0.0000<=x && x<=0.03725) { 
  print("Low")
} else if (0.03725<=x && x<=0.10076) {
  print("Medium")
} else if  (0.10076 <=x) {
  print("High")
} else {
  print("N/A")
}}

Class <- sapply(Data[,7],ZTClass)
Data <- cbind(Data,Class)

#We now have both our class and numerical values
#We begin working with some the McLust Package
GMMdata = (Data[,1:5])
BIC <- mclustBIC(GMMdata)
plot(BIC)
x<-clPairs(GMMdata,Class)

summary(BIC)

mod1 <- Mclust(data, x = BIC)
summary(mod1, parameters = TRUE)
