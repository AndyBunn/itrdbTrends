# AGB April 5, 2024

# This is an example for Gavin of look at. Shows how we can extract  
# use a logical mask to check if a given rwl meets the time window for the
# 20th century
# Inouts are from dataVault/meta.rds and dataVault/rwls.rds
rm(list=ls())
library(dplR)
meta <- readRDS("dataVault/meta.rds")
rwls <- readRDS("dataVault/rwls.rds")

nStudies <- length(rwls)

yrCheckFunction <- function(aRWLObject) {
  rwlSummary <- summary(aRWLObject)
  any(rwlSummary$last >= 2000 & rwlSummary$first <= 1900)
}

# replace this loop with sapply or lapply
keepers <- logical()
for(i in 1:nStudies){
  keepers[i] <- yrCheckFunction(rwls[[i]])
}
keepers
summary(keepers)

studies2keep <- (1:nStudies)[keepers] 

meta2keep <- meta[studies2keep,]
rwl2keep <- rwls[studies2keep]
length(rwl2keep)
