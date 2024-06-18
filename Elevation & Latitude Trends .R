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
