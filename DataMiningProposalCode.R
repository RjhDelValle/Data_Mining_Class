#Data Mining Proposal preprocessing
#Roberto J. Herrera
#HW #1
#Reproduction of Figure 2
library(GGally)
library(MASS)
library(hrbrthemes)
library(viridis)
#We create the a parallel coordinate
#We set our working directory
#setwd("/home/robertoherrera/Desktop/Informatics_hw/")
#Set our directory
setwd("\\Users\\adfig\\Desktop")
#Clean the workspace
rm(list=ls())
#Load our data
Data = read.csv("dataset.csv", header=T)

#Correlation Plot
library(corrplot)
corrplot(cor(Data[,4:9]), method="circle")

d <- Data$density
hist(d,main="Density Histogram",xlab="Density(g/cm3)"
     ,col = "red")

energy_per_atom <- Data$energy_per_atom
hist(energy_per_atom,main="Energy per Atom Histogram",xlab="Energy per Atom (eV/atom)"
     ,col = "blue")

efermi <- Data$efermi
hist(efermi,main="Fermi Energy Histogram",xlab="Energy per Atom (eV)"
     ,col = "green")

energy_above_hull <- Data$energy_above_hull
hist(energy_above_hull,main="Energy above Hull Histogram",xlab="Energy above Hull (eV/atom)"
     ,col = "yellow")

band_gap <- Data$band_gap
hist(band_gap,main="Band Gap Histogram",xlab="Band Gap (eV)"
     ,col = "violet")

e_total <- Data$e_total
hist(e_total,main="Total Energy Histogram",xlab="Total energy (eV)"
     ,col = "orange")
