#Problem 1
#We create our gaussian mixture model with the specified conditions in the problem 1
n = 1000
y1 <- rnorm(n, mean = 0, sd = 1)
y2 <- rnorm(n, mean = 10, sd =1)
y3 <- rnorm(n, mean = 20, sd = 1)
y <- 0.25 * y1 + (0.5 * y2)+(0.25 * y3)
hist(y)
plot(density(y))
#write.csv(y, file = "GMM.csv", row.names = FALSE)
# initial value 
pi = 0.5
mu1 = mean(y) - 0.5
mu2 = mean(y) + 0.5
sigma1 = sd(y)^2 
sigma2 = sd(y)^2
diff = 1
while(diff > 0.0001) {
  # E step 
  r10 = pi * dnorm(y, mean = mu1, sd = sqrt(sigma1))
  r20 = (1 - pi) * dnorm(y, mean = mu2, sd = sqrt(sigma2))
  r1 = r10/(r10 + r20)
  r2 = r20/(r10 + r20)
  
  pi.0 = pi
  mu1.0 = mu1
  mu2.0 = mu2
  sigma1.0 = sigma1 
  sigma2.0 = sigma2
  
  # M step
  pi = mean(r1)
  mu1 = sum(r1*y)/sum(r1)
  mu2 = sum(r2*y)/sum(r2)
  sigma1 = sum(r1*(y-mu1)^2)/sum(r1)
  sigma2 = sum(r2*(y-mu2)^2)/sum(r2)
  
  # calculate difference
  theta.0 = c(pi.0, mu1.0, mu2.0, sigma1.0, sigma2.0)
  theta = c(pi, mu1, mu2, sigma1, sigma2)
  diff = sum((theta.0 - theta)^2)
  
  # print(pi)
}
theta




#We create our gaussian mixture model with the specified conditions in the problem 1
n = 1000
y1 <- rnorm(n, mean = 0, sd = 1)
y2 <- rnorm(n, mean = 1, sd =1)
y3 <- rnorm(n, mean = 3, sd = 1)
y <- 0.25 * y1 + (0.5 * y2)+(0.25 * y3)
hist(y)
plot(density(y))
#write.csv(y, file = "GMM.csv", row.names = FALSE)
# initial value 
pi = 0.5
mu1 = mean(y) - 0.5
mu2 = mean(y) + 0.5
sigma1 = sd(y)^2 
sigma2 = sd(y)^2
diff = 1
while(diff > 0.0001) {
  # E step 
  r10 = pi * dnorm(y, mean = mu1, sd = sqrt(sigma1))
  r20 = (1 - pi) * dnorm(y, mean = mu2, sd = sqrt(sigma2))
  r1 = r10/(r10 + r20)
  r2 = r20/(r10 + r20)
  
  pi.0 = pi
  mu1.0 = mu1
  mu2.0 = mu2
  sigma1.0 = sigma1 
  sigma2.0 = sigma2
  
  # M step
  pi = mean(r1)
  mu1 = sum(r1*y)/sum(r1)
  mu2 = sum(r2*y)/sum(r2)
  sigma1 = sum(r1*(y-mu1)^2)/sum(r1)
  sigma2 = sum(r2*(y-mu2)^2)/sum(r2)
  
  # calculate difference
  theta.0 = c(pi.0, mu1.0, mu2.0, sigma1.0, sigma2.0)
  theta = c(pi, mu1, mu2, sigma1, sigma2)
  diff = sum((theta.0 - theta)^2)
  
  # print(pi)
}
theta

#Problem 2
rm(list = ls())
data(iris)
library(kernlab)
library(devtools)
library(ggbiplot)
library(ggfortify)
ggbiplot(pc, obs.scale = 1, var.scale = 1,
         groups = train$Species, ellipse = TRUE, circle = TRUE,ellipse.prob = 0.68) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
#Lets apply KPCA
kpc <- kpca(~.,data=iris[,-5],kernel="rbfdot",
            kpar=list(sigma=0.2),features=2)
#Lets plot it
plot(rotated(kpc),col=as.integer(iris[,5]),
     xlab="1st Principal Component",ylab="2nd Principal Component")
#Let us log transform the data
#Collect our classes
iris.species <- iris[, 5]
ir.pca <- prcomp(iris[,-5], center = TRUE, scale = TRUE)
print(ir.pca)
#We plot our PCA
library(ggfortify)
autoplot(ir.pca, data = iris, colour = 'Species')

#Problem 3
library(dslabs)
mnist <- read_mnist()
dim(mnist$train$images)
dim(mnist$test$images)
image(1:28, 1:28, matrix(mnist$test$images[3,], nrow=28)[ , 28:1],
      col = gray(seq(0, 1, 0.05)), xlab = "", ylab="")
#We apply PCA on our set
pca_result <- prcomp(mnist$train$images[,-1], rank = 16)
summary(pca_result)
#We attempt to predict with this PCA model
mnist_test_pred <- predict(pca_result, newdata = mnist$test$images[,-1])
image(1:28, 1:28, matrix(mnist_test_pred, nrow=28)[ , 28:1],
      col = gray(seq(0, 1, 0.05)), xlab = "", ylab="")


