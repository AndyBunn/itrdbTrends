# example for Gavin of look at a rwl object and it's meta info from 
# dataVault/meta.rds snf dataVault/rwls.rds

library(dplR)
meta <- readRDS("dataVault/meta.rds")
rwls <- readRDS("dataVault/rwls.rds")

# the first study
meta[1,]
aRWL <- rwls[[1]]

summary(aRWL)
plot(aRWL)
