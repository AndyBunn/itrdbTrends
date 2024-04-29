#here is an analysis
rm(list=ls())
library(dplR)
library(tidyverse)

'______________________________________________________________________________
to mask the observations 
_______________________________________________________________________________'

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

meta21c <- meta[studies2keep,]
rwl21c <- rwls[studies2keep]

saveRDS(meta21c, file = "~/Desktop/DENDRO PROJECT/itrdbTrends/dataVault/meta21c.rds")
saveRDS(rwl21c, file = "~/Desktop/DENDRO PROJECT/itrdbTrends/dataVault/rwl21c.rds")


'______________________________________________________________________________
some funcitons I will be calling on:
_______________________________________________________________________________'

#I need to clean data for last value being after 2000
#sapply

#to grab values from a model
extract_values <- function(model) {
model_summary<-summary(model)
  Estimates<-model_summary$coefficients[, "Estimate"]
  year_effect<-Estimates["year"]
  SEs<-model_summary$coefficients[, "Std. Error"]
  SE<-SEs["year"]
  Pvalues<-model_summary$coefficients[, "Pr(>|t|)"]
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
  #need to submit only NA's if there are no observations after 1950
  if (nrow(recent_chron) == 0 || all(is.na(recent_chron$std))) {
    return(c(NA,NA,NA))
  }else
  
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
find_recent_trend_plm <-function(df) {
  #need to submit only NA's if there are no observations after 1950

  rwi<-df
  rwi$year <- as.numeric(rownames(rwi))
  recent_rwi <- rwi[as.numeric(rownames(rwi)) > 1950, ]
 if (nrow(recent_rwi) == 0) {
    return(c(NA,NA,NA))
  }else

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

rwi_ModNegExp<- detrend(rwl = rwls[[1]], method = "ModNegExp")
find_recent_trend_plm(rwi_ModNegExp)

'______________________________________________________________________________
Processing some data to find trends:
_______________________________________________________________________________'
stand_names <- rownames(rwl2k)
list_of_estimates<-list()
for(i in 1340:length(rwls)){
  df<-rwls[[i]]
  #making three estimates: 
  rwi_ModNegExp<- detrend(rwl = df, method = "ModNegExp")
  rwi_Mean<- detrend(rwl = df, method = "Mean")
  rwi_AgeDepSpline <- detrend(rwl = df, method = "AgeDepSpline")
  rwis<-list(ModNegExp = rwi_ModNegExp, Mean = rwi_Mean, AgeDepSpline = rwi_AgeDepSpline)
  newrow<-c(stand_names[i])
  for(rwi in rwis){
    trend<-find_recent_trend(rwi)
    newrow<-c(newrow, trend )
  }
  list_of_estimates[[i]]<-newrow
}
table_of_estimates<-t(data.frame(list_of_estimates))

colnames(table_of_estimates) <- c("Stand", 
                                  "ModNegExp",
                                  "SE_ModNegExp",
                                  "P_ModNegExp",
                                  "Mean",
                                  "SE_Mean",
                                  "P_Mean",
                                  "AgeDepSpline",
                                  "SE_AgeDepSpline",
                                  "P_AgeDepSpline"
)


