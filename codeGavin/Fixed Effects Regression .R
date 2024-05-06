
'______________________________________________________________________________
Loading data:
_______________________________________________________________________________'

#Here I find trends using fixed effects regression analysis 
rm(list=ls())
library(dplR)
library(tidyverse)
library(plm) #panel data

meta <- readRDS("dataVault/meta21c.rds")#note: this in a truncated verion of the data 
rwls <- readRDS("dataVault/rwl21c.rds")

'______________________________________________________________________________
some funcitons I will be calling on:
_______________________________________________________________________________'

cutoff<- 2000 #year we start analyzing at 
#I want to run CV to see what year is best to start with for best predictive power of the year trend? 

#to identify the columns that are trees, in order to pivot_longer
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
#To estimate the Fixed Effects Regression
find_recent_trend_plm <- function(df) {
  rwi <- df
  rwi$year <- as.numeric(rownames(rwi))
  recent_rwi <- rwi[as.numeric(rownames(rwi)) > cutoff, ]

    long_recent_rwi <- pivot_longer(
      recent_rwi, 
      cols = tree_columns(recent_rwi),
      names_to = "tree", 
      values_to = "std"
    )
    p_recent_rwi <- pdata.frame(long_recent_rwi, index = "tree")
    
      model_fe <- plm(std ~ year, data = p_recent_rwi, model = "within")
      
      if (all(model_fe$coefficients == 0)) {
        print("Model is empty")
        return(c(NA,NA,NA))
      } else {
        results <- extract_plm_values(model_fe)
        return(results)
      }
  }
#to wrap it all together into something you can vapply:
climate_effects<-function(df){
  # here i provide a stop for if there arn't enough years in the 21st century 
  recent_years <- df[as.numeric(rownames(df)) > cutoff, ]
  if (nrow(recent_years) <= 2) {
    print("Recent data is near empty")
    return(rep(NA,10))#here I remove stands with very few observations 
  } else {
    #makes 3 different estimate (to show robustness)
    rwi_ModNegExp<- detrend(rwl = df, method = "ModNegExp", pos.slope= T, constrain.nls= "when.fail")
    rwi_Mean<- detrend(rwl = df, method = "Mean")
    rwi_AgeDepSpline <- detrend(rwl = df, method = "AgeDepSpline", pos.slope= T)
    rwis<-list(ModNegExp = rwi_ModNegExp, Mean = rwi_Mean, AgeDepSpline = rwi_AgeDepSpline)
    trends<-vapply(rwis, find_recent_trend_plm, FUN.VALUE = numeric(3),USE.NAMES = TRUE)#applying plm to all three
    flattened_trends <- as.vector(trends)
    flattened_trends<-c(flattened_trends, nrow(recent_years))#here I add the number of years in the dataset
    return(flattened_trends)
  }
}

#testing functions:
df<-detrend(rwl= rwls[[10]], method = "Mean")
find_recent_trend_plm(df)
climate_effects(rwls[[1]])
climate_effects(rwls[[341]])
df<-rwls[[10]]
head(meta[10,])

'______________________________________________________________________________
Applying the functions to the data:
_______________________________________________________________________________'
# Note: by using "microbenchmark" 
#I found that vapply is ~.341 seconds a stand, 
# while a forloop is ~.358 seconds a stand

t_mat_of_estimates <- vapply(seq_along(rwls), function(i) {
  print(paste("Modeling Stand #:", i))
  climate_effects(rwls[[i]])
}, FUN.VALUE = numeric(10), USE.NAMES = TRUE)


climate_estimates<-t(data.frame(t_mat_of_estimates))#need to transpose the matrix
colnames(climate_estimates) <- c( 
                                  "ModNegExp",
                                  "SE_ModNegExp",
                                  "P_ModNegExp",
                                  "Mean",
                                  "SE_Mean",
                                  "P_Mean",
                                  "AgeDepSpline",
                                  "SE_AgeDepSpline",
                                  "P_AgeDepSpline",
                                  "N_years"
)

climate_estimates_with_meta<-cbind(meta,climate_estimates)
'______________________________________________________________________________
Analysing the resuls:
_______________________________________________________________________________'

#next up:
for(i in 0:23){
  
  print(paste("the number of stands with >= ", i, " years of observarions is", sum(climate_estimates_with_meta$N_years > i, na.rm = TRUE)))
  }




