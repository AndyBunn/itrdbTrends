#Beta-testing Code:

'This code de-trends the cores, and models them, step-by-step, in order to produce 
Ring width indexes (RWIs), 
Chronolgoies, 
Fixed Effect Linear Regression of RWI~Time 
The Residuals (for robustness testing)
The slope and SE of each stands growth
'

'______________________________________________________________________________
Loading data:
_______________________________________________________________________________'

#Here I find trends using fixed effects regression analysis 
rm(list=ls())
library(dplR)
library(tidyverse)
library(plm) #panel data

meta <- readRDS("dataVault/meta.rds")#note: this is not a truncated version of the data 
rwls <- readRDS("dataVault/rwls.rds")

'______________________________________________________________________________
some funcitons  I will be calling on:
_______________________________________________________________________________'
#setting up data standards 
cutoff<- 1971 #year we start analyzing at 1971-2000 climate normal 
data_standard = 20
#thinking about adding a min number of trees to the data standard threshhold

#here I need a vector of stand names to keep all of the data organised 
stand_id<-rownames(meta)

#impoving, so that i can specify which detrending technique I would like to do just one type at a time. 
suf_data_check<-function(df, cutoff,data_standard) {
  years<-as.numeric(rownames(df))
  recent_years<-df[years>cutoff,]#making sure there are enough years
  return(nrow(recent_years)>data_standard)#True if there is sufficient data 
}

suf_data<-sapply(rwls,function(df) suf_data_check(df, cutoff,data_standard))%>%unlist()
summary(suf_data)
rwls[1]

rm_bio_signal<-function(df, cutoff, method) {
  # here i provide a stop for if there arn't enough years in the 21st century 
  if (suf_data_check(df,cutoff,data_standard)== F) {
    print("Recent data is insuffient")#here I remove stands without relevant observations
    return((F)) 
  } else {
    #makes 3 different estimate (to show robustness)
    rwi<- detrend(df, method=method)
    return(rwi)
  }
}
rm_bio_signal(rwls[[494]], cutoff,"Mean")


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
find_recent_trend_plm <- function(df,cutoff) {
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

#to extract plm model residuals
#this returns residuals in a long format. This is easier to do dplyr stuff to 
extract_plm_residuals <- function(model) {
  if (!inherits(model, "plm")) {
    stop("The provided model is not a valid 'plm' model.")
  }
  model_summary <- summary(model)
  st <- model_summary$residuals
  return(st)
}

#here I want to have an aditional function that returns the residuals
find_recent_residuals<- function(df) {
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
    return(NA)
  } else {
    residuals <- extract_plm_residuals
    return(residuals)
  }
}

#detrend in method specifically specified:
rm_bio_signal<-function(df, cutoff, method){
  # here i provide a stop for if there arn't enough years in the 21st century 
  recent_years <- df[as.numeric(rownames(df)) > cutoff, ]
  if ((nrow(recent_years) <= 29) {
    print("Recent data is insuffient")
    return(rwis = c(NA))#here I remove stands without relevant observations 
  } else {
    #makes 3 different estimate (to show robustness)
    rwis <- detrend(rwl = df, method = method, pos.slope= T)
    return(rwis)
  }
}

#to detrend in three different methods:

rm_bio_signal_1<-function(df, cutoff, method){
  # here i provide a stop for if there arn't enough years in the 21st century 
  recent_years <- df[as.numeric(rownames(df)) > cutoff, ]
  if ((nrow(recent_years) <= 29) | (recent_years==numeric(0))){
    print("Recent data is insuffient")
    return(c(ModNegExp = c(NA), Mean = c(NA), AgeDepSpline = c(NA)))#here I remove stands without relevant observations 
  } else {
    #makes 3 different estimate (to show robustness)
    rwi_ModNegExp<- detrend(rwl = df, method = "ModNegExp", pos.slope= T, constrain.nls= "when.fail")
    rwi_Mean<- detrend(rwl = df, method = "Mean")
    rwi_AgeDepSpline <- detrend(rwl = df, method = "AgeDepSpline", pos.slope= T)
    rwis<-list(ModNegExp = rwi_ModNegExp, Mean = rwi_Mean, AgeDepSpline = rwi_AgeDepSpline)
    return(rwis)
  }
}


#this function will output three dataframes for each rwl. you can find thm with rwis[1]$Mean 

#to wrap it all together into something you can vapply:
#this is the fastest, but does not allow us to decompose each step to look for robustness 
climate_effects<-function(df,cutoff){
  # here i provide a stop for if there arn't enough years in the 21st century 
  recent_years <- df[as.numeric(rownames(df)) > cutoff, ]
  if (nrow(recent_years) <= 23) {
    print("Recent data is near empty")
    return(rep(NA,10))#here I remove stands with very few observations 
  } else {
    #makes 3 different estimate (to show robustness)
    rwi_ModNegExp<- detrend(rwl = df, method = "ModNegExp", pos.slope= T, constrain.nls= "when.fail")
    rwi_Mean<- detrend(rwl = df, method = "Mean")
    rwi_AgeDepSpline <- detrend(rwl = df, method = "AgeDepSpline", pos.slope= T)
    rwis<-list(ModNegExp = rwi_ModNegExp, Mean = rwi_Mean, AgeDepSpline = rwi_AgeDepSpline)
    trends <- lapply(rwis, function(df) find_recent_trend_plm(df, cutoff = cutoff))#applying plm to all three
    flattened_trends <- as.vector(trends)
    flattened_trends<-c(flattened_trends, nrow(recent_years))#here I add the number of years in the dataset
    return(flattened_trends)
  }
}
#testing functions:
df<-detrend(rwl= rwls[[14]], method = "Mean")
find_recent_trend_plm(df,cutoff)# I have cuttof set to 1971 here, but this could easily be changed around 
climate_effects(rwls[[1]],cutoff)
climate_effects(rwls[[341]],cutoff)

'______________________________________________________________________________
Applying the functions to the data:
_______________________________________________________________________________'
#first, I remove the biological signal, three ways: 
rwis<- lapply(seq_along(rwls), function(i) {
  print(paste("Detrending Stand #:", i))
  rm_bio_signal(rwls[[i]],cutoff,"AgeDepSpline")
})
#woot woot! so vectorised so fast 

# here i provide a stop for if there arn't enough years in the 21st century 
recent_years <- df[as.numeric(rownames(df)) > cutoff, ]
if (nrow(recent_years) <= 29) {
  print("Recent data is insuffient")
  return(rwis = c(NA))#here I remove stands without relevant observations 
} else {
  #makes 3 different estimate (to show robustness)
  rwis <- detrend(rwl = df, method = method, pos.slope= T)
  return(rwis)
  
  #Now I save the chronologies, for now I will just be using the mean detrending method 
  chronologies<- lapply(seq_along(rwis), function(i) {
    print(paste("Building Chronology #:", i))
    #I remove stands with insufficient data.
    if (is.atomic(rwis[[i]]) == T) {
      print("Recent data is insufficient")
      return(NA)
    }
    rwi <- rwis[[i]]#$Mean
    #here I don't use bieweight but I do use prewiten. This is an arbitrary choice
    chron(rwi, biweight = F, prewhiten = TRUE)
  })
  
  
  
  #now, I fit a linear trend to the data since 1971, and save the models in a large list
  #I leverge the panel data structure by using a fixed effect model, 
  #where its the divergence from an individual's mean rwi, not the actual rwi that is the predicted varb,
  #to maximize the signal:noise ratio
  fe_models<- lapply(seq_along(rwis), function(i) {
    print(paste("Climate Modeling Stand #:", i))
    #dealing with the stands with not enough data
    if (is.atomic(rwis[[i]]) == T) {
      print("Recent data is insufficient")
      return(NA)
    }
    rwi <- rwis[[i]]$Mean
    ifelse(rwi == c(NA), model_fe<- c(NA))
    rwi$year <- as.numeric(rownames(rwi))
    recent_rwi <- rwi[as.numeric(rownames(rwi)) > cutoff, ]
    
    long_recent_rwi <- pivot_longer( #need to pivot longer to use plm 
      recent_rwi, 
      cols = tree_columns(recent_rwi),
      names_to = "tree", 
      values_to = "std"
    )
    p_recent_rwi <- pdata.frame(long_recent_rwi, index = "tree")
    model_fe <- plm(std ~ year, data = p_recent_rwi, model = "within")
  })
  
  #now I grab the residuals from the whole thing
  climate_residuals<-sapply(seq_along(fe_models), function(i) {
    print(paste("Climate Modeling Stand #:", i))
    model <- fe_models[[i]]
    extract_plm_residuals(model)
  },USE.NAMES = TRUE)
  
  #now I grab the climate effects form the whole thing. 
  climate_effects<-sapply(seq_along(fe_models), function(i) {
    print(paste("Climate Modeling Stand #:", i))
    #dealing with the stands with not enough data
    if (is.atomic(rwis[[i]]) == T) {
      print("Recent data is insufficient")
      return(NA)
    }
    model <- fe_models[[i]]
    extract_plm_residuals(model)
  },USE.NAMES = TRUE)
  
  
  