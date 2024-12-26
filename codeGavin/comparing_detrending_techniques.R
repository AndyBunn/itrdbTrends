#Here I do some exploritory analysis of the data
#goals: 1 to find the correlation between the RWI measures 
#to plot chnage in RWI vs elevation vs latitude
#to look for clters in the data 

rm(list=ls())
library(tidyverse)
library(PNWColors)
library(gridExtra)

ModNegExp<-readRDS("dataVault/climate_effects_ModNegExp.rds")
Mean<-readRDS("dataVault/climate_effects_Mean.rds")
AgeDepSpline<-readRDS("dataVault/climate_effects_AgeDepSpline.rds")
head(AgeDepSpline)
#df is the estimates 
df<-data.frame(ModNegExp$year_effect,Mean$year_effect,AgeDepSpline$year_effect) 
names(df)<-c("ModNegExp","Mean","AgeDepSpline")
#plotting this relationship:
pal2<-pnw_palette(name="Cascades",n=3)

p1<-ggplot(df,aes(x=Mean, y=ModNegExp))+
  geom_point(shape=1)+
  geom_smooth(method = "lm",color=pal2[1], fill=pal2[2])+
  geom_abline(intercept = 0, slope=1, color=1)+
  labs(title="Climate effect on in Tree RWI",
       subtitle = "Mean vs ModNegExp")+
    scale_x_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025), labels = scales::percent) +
    scale_y_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025), labels = scales::percent)+
  theme_bw() #haveto make both sides scale the same 

p1
p2<-ggplot(df,aes(x=Mean, y=AgeDepSpline))+
  geom_point(shape=1)+
  labs(title="Climate effect on in Tree RWI",
       subtitle = "Mean vs AgeDepSpline")+
  geom_smooth(method = "lm",color=pal2[1], fill=pal2[2])+
  geom_abline(intercept = 0, slope=1, color=1)+
  scale_x_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025), labels = scales::percent) +
  scale_y_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025), labels = scales::percent)+
  theme_bw() #haveto make both sides scale the same 

p3<-ggplot(df,aes(x=ModNegExp, y=AgeDepSpline))+
  geom_point(shape=1)+
  geom_smooth(method = "lm",color=pal2[1], fill=pal2[2])+
  geom_abline(intercept = 0, slope=1, color=1)+
  labs(title="Climate effect on in Tree RWI",
       subtitle = "ModNegExp vs AgeDepSpline")+
  scale_x_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025), labels = scales::percent) +
  scale_y_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025), labels = scales::percent)+
  theme_bw()

#there has got to be a way to look at this more in depth, but I am not sure how
Pairwise_Method_Comparisons<-grid.arrange(p1, p2, p3, nrow = 1, widths= c(1,1,1), heights= 1)
ggsave("Plots/Pairwise_Method_Comparisons.png",plot = Pairwise_Method_Comparisons, width = 10, height = 4, units = "in", dpi = 600)


p1<-ggplot(df,aes(x=Mean, y=ModNegExp))+
  geom_point(shape=1)+
  geom_smooth(method = "lm",color=pal2[1], fill=pal2[2])+
  geom_abline(intercept = 0, slope=1, color=1)+
  labs(title="Change in Tree RWI pr. Year",
       subtitle = "Mean vs ModNegExp")+
  scale_x_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025)) +
  scale_y_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025))+
  theme_bw() #haveto make both sides scale the same 


p2<-ggplot(df,aes(x=Mean, y=AgeDepSpline))+
  geom_point(shape=1)+
  labs(title="Change in Tree RWI pr. Year",
       subtitle = "Mean vs AgeDepSpline")+
  geom_smooth(method = "lm",color=pal2[1], fill=pal2[2])+
  geom_abline(intercept = 0, slope=1, color=1)+
  scale_x_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025)) +
  scale_y_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025))+
  theme_bw() #haveto make both sides scale the same 

p3<-ggplot(df,aes(x=ModNegExp, y=AgeDepSpline))+
  geom_point(shape=1)+
  geom_smooth(method = "lm",color=pal2[1], fill=pal2[2])+
  geom_abline(intercept = 0, slope=1, color=1)+
  labs(title="Change in Tree RWI pr. Year",
       subtitle = "ModNegExp vs AgeDepSpline")+
  scale_x_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025)) +
  scale_y_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025))+
  theme_bw()

#Lets find these outliers! 
sd(df$e_mean_modneg)
df$e_mean_modneg<-df$Mean-df$ModNegExp #normaized error to have sd of 1 
df$e_mean_modneg<-(df$Mean-df$ModNegExp)/sd((df$Mean-df$ModNegExp), na.rm = T)
ggplot(df,aes(x=e_mean_modneg,y=ModNegExp))+
  geom_point(type=1)+
  theme_bw()


outliers<-vector()
for(i in 1:nrow(df)){
ifelse(is.na(df[i,4]),
outlier==F,
outlier<-abs(df[i,4])>=2
)
outliers[i]<-outlier
}
sum(outliers, na.rm=T)

#now we can investigate these trees: 

meta <- readRDS("~/Desktop/dendroProject/itrdbTrends/dataVault/meta.rds")
climate_effects <- readRDS("~/Desktop/dendroProject/itrdbTrends/dataVault/climate_effects_ModNegExp.rds")
meta[outliers,]
climate_effects[outliers,]
climate_effects$outliers<-outliers

#lets look at our data quality
ggplot(climate_effects,aes(x=year_effect,y=SE,color=outliers))+
  geom_point(size=2)+
  scale_color_manual(values=c("black","red"))+
  labs(title="Looking for Heterskadisticity: Uncertianty of Climate Estimate, vs Estimated Climate Effect" ,
       x="Climate Effect",
       y="Standard Error of Recent Chronology Slope",
       subtitle="Noisey Stands More Likely to Cause Models Disagreement, But not Only Reason",
         color="High Model Disagreement?")+
  theme_bw()
#When the Climate Effect is very high, there will be model disagreement, and the Chronology will be Uncertian. Removing THose pitier stands would create a more homoskadstic estimate. 

