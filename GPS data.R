


## 2021 movement data ##

 
rm(list=ls())
setwd("/Users/aliyacaldwell/Box/Furey Lab Shared Materials/TERNS/Movement Analyses/Data/2021 GPS data")

library(ggplot2)
library(ggmap)
library(cowplot)
library(googleway)
library(ggrepel)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggsn)
library(lubridate)
library(palettetown)
library(tidyr)
library(lubridate)

movdat<-read.csv("2021GPSDataMaster.csv")

#extract tags, locations, hours, and date
movdat<-movdat %>% 
  dplyr::select(TagID, Latitude, Longitude, DateTime, DateHour, Hour, Day,Month,Sex)
movdat<-as_tibble(movdat)

movdat$DateTime <- as.POSIXct(movdat$DateTime, format="%m/%d/%Y %H:%M:%S")
movdat$DateHour <- as.POSIXct(movdat$DateHour, format="%Y-%m-%d %H")

#okay this is such a dumb way to do this UGH but it wasnt working when I tried to lump them together
movdat <- movdat %>% 
  mutate(DayTime = ifelse(Hour == "6", "morning",
                          ifelse(Hour  == "7", "morning",
                                 ifelse(Hour  == "8", "morning",
                                        ifelse(Hour  == "9", "morning",
                                               ifelse(Hour  == "10", "morning",
                                                      ifelse(Hour  == "11", "morning",
                                                             ifelse(Hour  == "12", "midday",
                                                                    ifelse(Hour  == "13", "midday",
                                                                           ifelse(Hour  == "14", "midday",
                                                                                  ifelse(Hour  == "15", "midday",
                                                                                         ifelse(Hour  == "16", "midday",
                                                                                                ifelse(Hour  == "17", "midday", "evening")))))))))))))


#visualize
register_google(key = "AIzaSyAkZZRBsRjxtw_PCaAidAVCdzKY5VVwDf4")

#all points sorted by tag zoomed
mapGB<-get_googlemap(c(-70.703,43.066), maptype = "satellite", zoom=10)
ggmap(mapGB)+
  geom_point(data=movdat, aes(x=Longitude, y=Latitude, fill=TagID, color=TagID))

#all points sorted by tag full extent
depMap<-get_googlemap(c(-70.01763,42.49268), maptype = "satellite", zoom=8)
ggmap(depMap)+
  geom_point(data=movdat, aes(x=Longitude, y=Latitude, fill=TagID, color=TagID),alpha=0.5)+theme(legend.position = "none")+
  geom_point(aes(x=-70.62523, y=42.96828), color="gray30", size=2)


#all points sorted by hour
mapGB<-get_googlemap(c(-70.703,43.05), maptype = "terrain", zoom=13)
ggmap(mapGB)+
  geom_point(data=movdatRecent, aes(x=Longitude, y=Latitude, fill=as.character(Hour), color=as.character(Hour)))#+
scale_color_continuous(type="viridis")

#all points sorted by sex and tagID
movdatSex<-subset(movdat, !Sex == "Pending")
depMap<-get_googlemap(c(-70.71763,42.49268), maptype = "satellite", zoom=8)
ggmap(depMap)+
  geom_point(data=movdatSex, aes(x=Longitude, y=Latitude, fill=TagID, color=TagID, shape=Sex),alpha=0.5)



## KERNEL DENSITY ANALYSIS ##

#using kernsmooth as per: http://www.samuelbosch.com/2014/02/creating-kernel-density-estimate-map-in.html
library(KernSmooth)
  #extract coordinates
records<-movdat
coordinates<-movdat[,2:3]
coordinates<-as.data.frame(coordinates)

  #compute 2D binned KD estimate
est <- bkde2D(coordinates, 
              bandwidth=c(3,3), 
              gridsize=c(4320,2160),
              range.x=list(c(-180,180),c(-90,90)))
est$fhat[est$fhat<0.00001] <- 0

  #next step: plot est 

#using ggplot which implemends bkde2d on the backend as per: https://www.earthdatascience.org/tutorials/visualize-2d-point-density-ggmap/

ggplot(coordinates, aes(x=Latitude, y=Longitude)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 h = 1, n = 200,
                 geom = "polygon", data = coordinates) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')


#using ggmap as per: https://www.supplychaindataanalytics.com/map-based-point-and-density-plots-in-r-using-ggmap/

library(ggmap)
library(ggplot2)
library(dplyr)
library(gridExtra)

  # plot a ggmap basemap
mapGB<-get_googlemap(c(-70.703,43.066), maptype = "satellite", zoom=10)
plot(mapGB)
densityplot_gpspoints <- qmplot(x=Longitude, y=Latitude, 
                             data = movdat, 
                             geom = "blank",
                             maptype = "toner-background", 
                             darken = .7, 
                             legend = "topright",
                             zoom=12) + stat_density_2d(aes(fill = ..level..), geom = "polygon",alpha = .5, color = NA) + scale_fill_gradient2(low = "blue", mid = "green", high = "red")
plot(densityplot_gpspoints)
