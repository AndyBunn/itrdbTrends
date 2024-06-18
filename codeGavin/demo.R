# AGB April 5, 2024

# This is an example for Gavin of look at. Shows how we can extract  
# a rwl object and its meta info from the data in the dataVault folder. 
# Specifically, from dataVault/meta.rds snf dataVault/rwls.rds

library(dplR)
meta <- readRDS("dataVault/meta.rds")
rwls <- readRDS("dataVault/rwls.rds")

nrow(meta)
length(rwls)

# the first study
meta[1,]
aRWL <- rwls[[1]]

summary(aRWL)
plot(aRWL)

aRWL




