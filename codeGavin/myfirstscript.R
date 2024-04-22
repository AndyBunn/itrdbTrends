#here is an analysis
rm(list=ls())
library(dplR)
library(tidyverse)

meta <- readRDS("dataVault/meta.rds")
rwls <- readRDS("dataVault/rwls.rds")

nrow(meta)
length(rwls)


'______________________________________________________________________________
some funcitons I will be calling on:
_______________________________________________________________________________'
#to grab values from a model
extract_values <- function(model) {
  Estimates<-summary(model)$coefficients[, "Estimate"]
  year_effect<-Estimates["year"]
  SEs<-summary(model)$coefficients[, "Std. Error"]
  SE<-SEs["year"]
  Pvalues<-summary(model)$coefficients[, "Pr(>|t|)"]
  Pvalue<- Pvalues["year"]
  return(c(year_effect = as.numeric(year_effect), SE = as.numeric(SE), Pvalue = as.numeric(Pvalue)))
}
#to make a vector of the linear regression coefficients from each model
find_recent_trend <-function(df) {
  if (!is.data.frame(df)) {  # Trying to use more Validation 
    stop("Input must be a data frame, and should be a RWI dataset")
  }
  
  chronology <- chron(df)
  recent_chron <- chronology[as.numeric(rownames(df)) > 1950, ]
  recent_chron$year<- as.numeric(rownames(recent_chron))
  model<- lm(std~year,data=recent_chron)
  results<-extract_values(model)
  return(results)
}

#to identify the columns that arent trees 
tree_columns <- function(df) {
  if (!is.data.frame(df)) {  # Trying to use more Validation 
    stop("Input must be a data frame, and should be a RWI dataset")
  }
  cols <- setdiff(names(df), "year")
  return(cols)
}
#to extract plm model values
extract_plm_values <- function(model) {
  if (!inherits(model, "plm")) {
    stop("The provided model is not a valid 'plm' model.")
  }
  model_summary <- summary(model)
  coefficients_summary <- model_summary$coefficients
  Estimate<-coefficients_summary[, "Estimate"]
  SE<-coefficients_summary[, "Std. Error"]
  Pvalue<-coefficients_summary[, "Pr(>|t|)"]
  return(c(year_effect = as.numeric(Estimate), SE = as.numeric(SE), Pvalue = as.numeric(Pvalue)))
}

#To make an estimate of the Fixed Effects Regression
library(plm)

find_recent_trend_plm <-function(data.frame) {
  rwi<-data.frame
  rwi$year <- as.numeric(rownames(rwi))
  recent_rwi <- rwi[as.numeric(rownames(rwi)) > 1950, ]
  long_recent_rwi <- pivot_longer(
    recent_rwi, 
    cols = tree_columns(recent_rwi),
    names_to = "tree", 
    values_to = "std"
  )
  p_recent_rwi<-pdata.frame(long_recent_rwi, index=c("tree"))
  model_fe<-plm(std~year, data=p_recent_rwi, model="within")
  results<-extract_plm_values(model_fe)
  return(results)
}

rwi_ModNegExp<- detrend(rwl = aRWL, method = "ModNegExp")
find_recent_trend_plm(rwi_ModNegExp)

