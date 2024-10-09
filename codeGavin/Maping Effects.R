#Mapping the effects
rm(list=ls())
library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(PNWColors)

df <- read.csv("dataVault/Fixed_Effects.csv")
ModNegExp<-c("ModNegExp","SE_ModNegExp","Lat","Long")
df<-df[,ModNegExp]
names(df)<-c("change","SE","Lat","Long")
df <- st_as_sf(df, coords = c("Long","Lat"), crs = 4326)#WGS 84

str(df)
world <- ne_countries(scale = "medium", returnclass = "sf")
world<-st_as_sf(world, crs = 4326)
str(world)

st_crs(world)
st_bbox(world)
st_crs(df)
st_bbox(df)

#for some polar projections:
world_north_pole <- st_transform(world, crs = "+proj=laea +lat_0=90 +lon_0=0")
world_south_pole <- st_transform(world, crs = "+proj=laea +lat_0=-90 +lon_0=0")

str(df)#now I am making a factor variable that just indicates if a stand has a stat. sig increase or not. 
df <- df %>%
  mutate(changes2 = case_when(
    abs(change)-SE > 0 & change >= 0 ~ "Fertilized",
    abs(change)-SE <= 0 ~ "Constant",
    abs(change)-SE > 0 & change < 0 ~ "Inhibited"
  ))
df$changes2 <- factor(df$changes2, levels = c("Inhibited","Constant","Fertilized"  ))

pal3<-pnw_palette("Bay",3)

ModNegExp1<-ggplot(df)+
  geom_sf(data = world, fill="white")+
  geom_sf(data = df, alpha=.5, aes(color = changes2, size=abs(change))) +
  scale_color_manual(values= rev(pal3))+
  labs(title= "Mapping Effect of Climate Change on Tree Growth",
       subtitle = "Detrended with the ModNegExp Function",
       color = "Climate Effect",
       size = "Yearly Change in RWI") +
  guides(size = "none")+
  coord_sf()+
  theme_bw()
ModNegExp1
ggsave("Plots/ModNegExp1.png",ModNegExp1)




pal=pnw_palette("Bay",100)
pal
#this one in continious, and I think it is less helpfull:
ggplot(df)+
  geom_sf(data = world, fill="white")+
  geom_sf(data = df, size=2, aes(color = change, alpha=1/SE)) +
  scale_color_gradientn(colors = rev(pal)) +
  labs(Title= "Mapping changes in growth rate") +
  coord_sf()+
  theme_bw()

ModNegExp2<-ggplot() +
  geom_sf(data = world_north_pole, fill = "white", color = "black") +
  geom_sf(data = df, alpha=.5, aes(color = changes2, size=abs(change))) +
  scale_color_manual(values= rev(pal3))+
  labs(title= "Mapping Effect of Climate Change on Tree Growth",
       subtitle = "Detrended with the ModNegExp Function",
       color = "Climate Effect",
       size = "Yearly Change in RWI") +
  coord_sf()+
  guides(size = "none")+
  theme_bw()+
  theme(
    plot.background = element_rect(fill = "white", color = NA),   # Change plot background color
    panel.background = element_rect(fill = "black", color = NA),       # Change panel background color
    panel.grid.major = element_line(color = "black", size = 0.5, linetype = "dashed"), # Customize major grid lines
    panel.grid.minor = element_line(color = "black", size = 0.25, linetype = "dotted") # Customize minor grid lines
  )
ModNegExp2
ggsave("Plots/ModNegExp2.png",ModNegExp2)


ggplot() +
  geom_sf(data = world_south_pole, fill = "white", color = "black") +
  geom_sf(data = df, alpha=.5, aes(color = changes2, size=abs(change))) +
  scale_color_manual(values= rev(pal3))+
  labs(title= "Mapping Climate Change Effect on Tree Growth",
       subtitle = "Detrended with the ModNegExp Function",
       color = "Climate Effect",
       size = "Yearly Change in RWI") +
  coord_sf()+
  theme_bw()+
  theme(
    plot.background = element_rect(fill = "white", color = NA),   # Change plot background color
    panel.background = element_rect(fill = "black", color = NA),       # Change panel background color
    panel.grid.major = element_line(color = "darkgrey", size = 0.5, linetype = "dashed"), # Customize major grid lines
    panel.grid.minor = element_line(color = "darkgrey", size = 0.25, linetype = "dotted") # Customize minor grid lines
  )



