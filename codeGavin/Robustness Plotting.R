#Here I do some exploritory analysis of the data
#goals: 1 to find the correlation between the RWI measures 
#to plot chnage in RWI vs elevation vs latitude
#to look for clters in the data 
rm(list=ls())
library(tidyverse)
library(PNWColors)
library(gridExtra)

df <- read.csv("dataVault/Fixed_Effects.csv")

str(df)
cor(df[c("ModNegExp","Mean","AgeDepSpline")])#whoa, lower then expected 

#plotting this relationship:
pal2<-pnw_palette(name="Cascades",n=3)
pal2
help(geom_abline)

p1<-ggplot(df,aes(x=Mean, y=ModNegExp))+
  geom_point(shape=1)+
  geom_smooth(method = "lm",color=pal2[1], fill=pal2[2])+
  geom_abline(intercept = 0, slope=1, color=1)+
  labs(title="Percent Change in Tree Growth",
       subtitle = "Mean vs ModNegExp")+
    scale_x_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025), labels = scales::percent) +
    scale_y_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025), labels = scales::percent)+
  theme_bw() #haveto make both sides scale the same 


p2<-ggplot(df,aes(x=Mean, y=AgeDepSpline))+
  geom_point(shape=1)+
  labs(title="Percent Change in Tree Growth",
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
  labs(title="Percent Change in Tree Growth",
       subtitle = "ModNegExp vs AgeDepSpline")+
  scale_x_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025), labels = scales::percent) +
  scale_y_continuous(limits = c(-.03, .055), breaks = seq(-0.025, 0.05, 0.025), labels = scales::percent)+
  theme_bw()

#there has got to be a way to look at this more in depth, but I am not sure how
Pairwise_Method_Comparisons<-grid.arrange(p1, p2, p3, nrow = 1, widths= c(1,1,1), heights= 1)
ggsave("Plots/Pairwise_Method_Comparisons.png",plot = Pairwise_Method_Comparisons, width = 10, height = 4, units = "in", dpi = 600)





