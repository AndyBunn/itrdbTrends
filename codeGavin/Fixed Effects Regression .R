#Here I find trends using fixed effects regression analysis 
rm(list=ls())
library(dplR)
library(tidyverse)
library(plm) #panel data

'______________________________________________________________________________
Loading data:
_______________________________________________________________________________'

meta <- readRDS("dataVault/meta21c.rds")
rwls <- readRDS("dataVault/rwl21c.rds")

'______________________________________________________________________________
some funcitons I will be calling on:
_______________________________________________________________________________'

#to identify the columns that are trees, inorder to pivot_longer
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
find_recent_trend_plm <- function(df) {
  rwi <- df
  rwi$year <- as.numeric(rownames(rwi))
  recent_rwi <- rwi[as.numeric(rownames(rwi)) > 2000, ]
  
  if (nrow(recent_rwi) <= 2) {
    print("Recent data is near empty")
    return(c(NA,NA,NA))
  } else {
    long_recent_rwi <- pivot_longer(
      recent_rwi, 
      cols = tree_columns(recent_rwi),
      names_to = "tree", 
      values_to = "std"
    )
    p_recent_rwi <- pdata.frame(long_recent_rwi, index = "tree")
    
    if (nrow(p_recent_rwi) == 0) {
      print("pdata.frame is empty")
      return(c(NA,NA,NA))
    } else {
      model_fe <- plm(std ~ year, data = p_recent_rwi, model = "within")
      
      if (all(model_fe$coefficients == 0)) {
        print("Model is empty")
        return(c(NA,NA,NA))
      } else {
        results <- extract_plm_values(model_fe)
        return(results)
      }
    }
  }
}
#to wrap it all together into something you can vapply:
climate_effects<-function(df){
  rwi_ModNegExp<- detrend(rwl = df, method = "ModNegExp")#makes 3 different estimate (to show robustness)
  rwi_Mean<- detrend(rwl = df, method = "Mean")
  rwi_AgeDepSpline <- detrend(rwl = df, method = "AgeDepSpline")
  rwis<-list(ModNegExp = rwi_ModNegExp, Mean = rwi_Mean, AgeDepSpline = rwi_AgeDepSpline)
  trends<-vapply(rwis, find_recent_trend_plm, FUN.VALUE = numeric(3),USE.NAMES = TRUE)#applying plm to all three
  flattened_trends <- as.vector(trends)
  return(flattened_trends)
}

df<-detrend(rwl= rwls[[1]],method = "Mean")
find_recent_trend_plm(df)

'______________________________________________________________________________
Applying the functions to the data:
_______________________________________________________________________________'

list_of_estimates<-data.frame()
for( i in 1:10){#for dubugging purposes 
  df<-rwls[[i]]
  print(paste("stand #", i, sep=""))
  stand_climate_effects<-climate_effects(df)
  list_of_estimates[i,]<-t(stand_climate_effects)
}


#for real use purposes 
list_of_estimates<-vapply(rwls, climate_effects, FUN.VALUE = numeric(9), USE.NAMES = TRUE)

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
