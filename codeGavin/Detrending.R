
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

#Here I detrend the tree ring lengths using three different techniques
rm(list=ls())
library(dplR)
library(tidyverse)
library(plm) #panel data

meta <- readRDS("dataVault/meta.rds")#note: this is not a truncated version of the data 
rwls <- readRDS("dataVault/rwls.rds")

'______________________________________________________________________________
Cleaning the data:
_______________________________________________________________________________'

cutoff<- 1971 #year we start analyzing at 1971-2000 climate normal 
data_standard = 20 #the minimum number of years of data after 1971 to be used in analysis. 

suf_data_check<-function(df, cutoff,data_standard) {
  years<-as.numeric(rownames(df))
  recent_years<-years[years>cutoff]#making sure there are enough years
  return(length(recent_years)>data_standard)#True if there is sufficient data 
  #so the function would return F if there are no or less then 20 years after 1971
}

suf_data <- lapply(seq_along(rwls), function(i) {
  print(paste("Challenging Stand #:", i))
  suf_data_check(rwls[[i]],cutoff,data_standard)
})
suf_data<-unlist(suf_data)

summary(suf_data)

'______________________________________________________________________________
Detrending the data:
_______________________________________________________________________________
'

#making a function that detrends each tree, with as much vectorisation as possible. 
rm_bio_signal<-function(df, cutoff, data_check, method){
  # here I skip all the stands that didnt pass the data check. This way we can later go back and see if we are biasing the dataset while filtering the data
  if (data_check == F) {
    return(rwis = data.frame(NA))#here I remove stands without relevant observations 
    print("recent data is insuficient")
  } else {
    #makes 3 different estimate (to show robustness)
    rwis <- detrend(rwl = df, method = method, pos.slope= T)
    return(rwis)
  }
}

rwis<-list() #making an empty list
#so previously I had all of these stored in a list.
rwis_AgeDepSpline<- lapply(seq_along(rwls), function(i) {
  print(paste("Detrending Stand #:", i))
  rm_bio_signal(rwls[[i]],cutoff,suf_data[i],"AgeDepSpline")
})
saveRDS(rwis_AgeDepSpline, file = "dataVault/rwis_AgeDepSpline.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

rwis_Mean<- lapply(seq_along(rwls), function(i) {
  print(paste("Detrending Stand #:", i))
  rm_bio_signal(rwls[[i]],cutoff,suf_data[i],"Mean")
})
saveRDS(rwis_Mean, file = "dataVault/rwis_Mean.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

rwis_ModNegExp<- lapply(seq_along(rwls), function(i) {
  print(paste("Detrending Stand #:", i))
  rm_bio_signal(rwls[[i]],cutoff,suf_data[i],"ModNegExp")
})

saveRDS(rwis_ModNegExp, file = "dataVault/rwis_ModNegExp.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)


