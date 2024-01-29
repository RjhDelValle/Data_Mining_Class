#Third Milestone code
#Roberto J. Herrera del Valle
#We upload our data and library again
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
#We perform our GMM modeling
#We organize our data into categories
ZTClass<-function(x){
  if (0.0000<=x && x<0.4) { 
    print("Low")
  } else if  (0.4 <=x) {
    print("High")
  } else {
    print("N/A")
  }}

Class <- sapply(Data[,7],ZTClass)
Data <- cbind(Data,Class)
#We now have both our class and numerical values
#We begin working with some the McLust Package
GMMdata = (Data[,1:6])
x<-clPairs(GMMdata,Class)

summary(BIC)

mod2 <- MclustDA(GMMdata, Class, modelType = "EDDA")
summary(mod2)
plot(mod2, what = "scatterplot")

##We perform a histogram of our ZT value for the PCA and TCA
ZT <- Data$ZT
ZT<-hist(ZT,main="ZT Histogram",xlab="ZT"
         ,col = "blue")
ZT<-hist(ZT,main="ZT Histogram",breaks="FD",xlab="ZT"
         ,col = "blue")
frequency <- ZT$counts
frequency

#We perform a correlation analysis
cormat <- round(cor(GMMdata),2)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
#We normalize our data
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

GMMdata <- as.data.frame(lapply(GMMdata, min_max_norm))
#We perform our PCA analysis
library(ggfortify)
pc <- prcomp(GMMdata,
             center = TRUE,
             scale. = TRUE)
#We create our Scree plot
#calculate total variance explained by each principal component
var_explained = pc$sdev^2 / sum(pc$sdev^2)
print(var_explained)

attributes(pc)
autoplot(pc, data = GMMdata,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

#We now implement ICA analysis
#We find our ZT Data
library(fastICA)
x <- as.data.frame(lapply(Data[,-8], min_max_norm))
x=x[,1:7]


ICA <- fastICA(x, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1, 
             method = "C", row.norm = FALSE, maxit = 200, 
             tol = 0.0001, verbose = TRUE)
par(mfrow = c(1, 3))
plot(ICA$X, main = "Pre-processed data")
plot(ICA$X %*% ICA$K, main = "PCA component")
plot(ICA$S, main = "ICA component")




