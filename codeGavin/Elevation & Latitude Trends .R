# here my goal is to look for trends in elevaltion and latitude 
rm(list=ls())
df <- read.csv("dataVault/Fixed_Effects.csv")
str(df)
#here I pivot the data longer:
str(df)
long_df<-pivot_longer(df, 
                      cols = c(14,17,20),
                      values_to = "estimate", 
                      names_to = "type"
)
long_df$SE<-ifelse(long_df$type=="ModNegExp",long_df$SE_ModNegExp,
                   ifelse(long_df$type=="Mean",long_df$SE_Mean,
                          ifelse(long_df$type=="AgeDepSpline",long_df$SE_AgeDepSpline,
                                 "NA")))
long_df<-long_df[-c(14:19)]
str(long_df)
long_df$SE<-as.numeric(long_df$SE)

library(PNWColors)
pal <- pnw_palette("Bay",3)
names(df)

fitting_model<-lm(AgeDepSpline~Lat+Altitude, data=df)

summary(fitting_model)


#goofing around looking at data:
meta <- readRDS("~/Desktop/dendroProject/itrdbTrends/dataVault/meta.rds")
rwls <- readRDS("~/Desktop/dendroProject/itrdbTrends/dataVault/rwls.rds")
albi<-vector()
for( i in 1:nrow(meta)){
  tf<-meta$GenusSpp[i] == "Pinus albicaulis"
  albi[i]<-tf
}
length(albi)
sum(albi)

meta[albi,]

rwlsWBP<-rwls[albi]
rwl<-rwlsWBP[[4]]
str(rwl)
class(rwl)
plot(rwl, plot.type="spag")#shows RWI
rwi<-detrend.series(rwl[, 21])#shows possible detrending techniques on one tree
rwi<-detrend(rwl, method = "AgeDepSpline")  #detrends all the trees 
crn<-chron(rwi)
plot(crn)


ca533.crn <- chron(ca533.rwi)#this will describe how many observations are in each year 
dim(ca533.rwi)
dim(ca533.crn) 
plot(ca533.crn, add.spline=TRUE, nyrs=20)
