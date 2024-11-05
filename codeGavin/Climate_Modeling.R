'______________________________________________________________________________
Loading data:
_______________________________________________________________________________'
#Here I estimate the climate trends using fixed effects regression analysis 
rm(list=ls())
library(dplR)
library(tidyverse)
library(plm) #panel data

meta <- readRDS("dataVault/meta.rds")#note: this is not a truncated version of the data 
rwis_Mean <- readRDS("dataVault/rwis_Mean.rds")
rwis_ModNegExp <- readRDS("dataVault/rwis_ModNegExp.rds")
rwis_AgeDepSpline <- readRDS("dataVault/rwis_AgeDepSpline.rds")

cutoff<- 1971 #year we start analyzing at 1971-2000 climate normal 
data_standard = 20 #the minimum number of years of data after 1971 to be used in analysis. 

'______________________________________________________________________________
Fixed Effects Regression:
_______________________________________________________________________________'
#This helps format all of the NA's into the version of the detrended 
find_recent_trend_plm <- function(df,cutoff) {
  rwi <- data.frame(df)
  if (length(rwi) == 1){
    print("Model is empty")
    return(c(NA,NA,NA,NA))
  } else{
  rwi$year <- as.numeric(rownames(rwi))
  recent_rwi <- rwi[as.numeric(rownames(rwi)) > cutoff, ]
  
  long_recent_rwi <- pivot_longer(
    recent_rwi, 
    cols = setdiff(names(rwi), "year"),#returns the names of the dataset that don't include the set: c("year)
    names_to = "tree", 
    values_to = "std"
  )
  p_recent_rwi <- pdata.frame(long_recent_rwi, index = "tree")
  model_fe <- plm(std ~ year, data = p_recent_rwi, model = "within")
  
  if (all(model_fe$coefficients == 0)) {
    print("Model is empty")
    return(c(NA,NA,NA,NA))
  }else{
    results <- extract_plm_values(model_fe)
    return(results)
  }
  }
}

#this is just a function that can grab all the parts of model we want, so we don't have to store a thousand models
extract_plm_values <- function(model) {
  if (!inherits(model, "plm")) {
    stop("The provided model is not a valid plm model.")
  }
  model_summary <- summary(model)
  coefficients_summary <- model_summary$coefficients
  Estimate<-coefficients_summary[, "Estimate"]
  SE<-coefficients_summary[, "Std. Error"]
  Pvalue<-coefficients_summary[, "Pr(>|t|)"]
  Rsq_summary <- model_summary$r.squared
  Rsq<-Rsq_summary["rsq"]
  return(c(year_effect = as.numeric(Estimate), SE = as.numeric(SE), Pvalue = as.numeric(Pvalue), Rsq = as.numeric(Rsq_summary["rsq"])))
  
}
# Then we apply the function to each stand:

climate_effects_AgeDepSpline <-lapply(seq_along(rwis_AgeDepSpline), function(i) {
  print(paste("Fitting climate trend for Stand #:", i))
  find_recent_trend_plm(rwis_AgeDepSpline[[i]],cutoff)
})
climate_effects_AgeDepSpline<-data.frame(t(data.frame(climate_effects_AgeDepSpline)))
rownames(climate_effects_AgeDepSpline)<-seq_along(rwis_AgeDepSpline)
saveRDS(climate_effects_AgeDepSpline, file = "dataVault/climate_effects_AgeDepSpline.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

#now doing the same with the mean estimate:
climate_effects_Mean<- lapply(seq_along(rwis_Mean), function(i) {
  print(paste("Fitting climate trend for Stand #:", i))
  find_recent_trend_plm(rwis_Mean[[i]],cutoff)
})
climate_effects_Mean<-data.frame(t(data.frame(climate_effects_Mean)))
rownames(climate_effects_Mean)<-seq_along(rwis_Mean)
saveRDS(climate_effects_Mean, file = "dataVault/climate_effects_Mean.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)


climate_effects_ModNegExp<- lapply(seq_along(rwis_ModNegExp), function(i) {
  print(paste("Fitting climate trend for Stand #:", i))
  find_recent_trend_plm(rwis_ModNegExp[[i]],cutoff)
})
climate_effects_ModNegExp<-data.frame(t(data.frame(climate_effects_ModNegExp)))
rownames(climate_effects_ModNegExp)<-seq_along(rwis_ModNegExp)

saveRDS(climate_effects_ModNegExp, file = "dataVault/climate_effects_ModNegExp.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
