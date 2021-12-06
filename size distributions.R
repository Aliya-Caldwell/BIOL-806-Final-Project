
## SIZE DISTRIBUTIONS ##

rm(list=ls())

# packages etc
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(grid)
library(palettetown)
library(colorspace)
library(RColorBrewer)
library(pals)
library(wesanderson)

#===========================#
## CLEAN UP MAIN DATAFRAME ##
#===========================#
getwd()
setwd("/Users/aliyacaldwell/Box/SML Tern Program/Raw data/Feeding data")

df<-read.csv("T-Feeding up to 2020.csv",fileEncoding="UTF-8-BOM")

dfclean<-df %>%
  dplyr::rename(size='PreySizeMM') %>%
  dplyr::rename(prey='PreyFamily') %>%
  dplyr::rename(method='ObservationType') %>%
  dplyr::rename(year='Year') %>%
  dplyr::rename(day='DayOfYear') %>%
  dplyr::rename(week='WeekOf.Year') %>%
  dplyr::rename(month='Month') %>%
  dplyr::rename(watchtime='WatchDurationMM') %>%
  dplyr::rename(nestswatched='NumberNestsObserved') %>%
  dplyr::rename(event='Event')



# size distribution top 5 species from TERNS across all years
#------------------------------------------------------------#

dfcleansub<-dfclean %>%
  filter(prey %in% c("herring","hake","butterfish","sandlance","mackerel"))

#multiple panes by species
ggplot(dfcleansub)+theme_bw()+
  geom_density(aes(x=size, color=prey, fill=prey),
               adjust=3,alpha=0.7,size=1)+
  facet_wrap(~prey)+
  scale_fill_poke(pokemon="surskit", spread=5)+
  scale_color_poke(pokemon="surskit", spread=5)+
  xlab("Length (mm)")

#single pane
ggplot(dfcleansub)+theme_bw()+
  geom_density(aes(x=size, color=prey, fill=prey),
               adjust=3,alpha=0.3,size=1)+
  scale_fill_poke(pokemon="surskit", spread=5)+
  scale_color_poke(pokemon="surskit", spread=5)+
  xlab("Length (mm)")

# size distribution of species TERNS throughout years using HERRING as example
#----------------------------------------------------------------------------#
#-just replace herring sub with whatever species sub you want
#-need to use unsummarized dataset (dfclean)

herringsub<-dfclean %>%
  filter(! year %in% c(2003, 2004, 2010, 2012, 2006, 2017)) %>%  #2006 and 2017 have <15 herring samples
  filter(prey %in% "herring")
#TO DO: decide how low a sample size needs to be to exclude the species/year combo from analysis

nb.cols<-15
mycolors <- colorRampPalette(brewer.pal(8, "BuGn"))(nb.cols)
#TO DO: create a color palette for each family that matches with colors from other plots

#multiple panes
ggplot(herringsub)+theme_bw()+
  geom_density(aes(x=as.numeric(size), color=as.character(year), fill=as.character(year)),
               adjust=3,alpha=0.7,size=1)+
  scale_fill_manual(values=mycolors,name="year")+
  scale_color_manual(values=mycolors,name="year")+
  xlab("Herring Length (mm)")+
  guides(color=guide_legend("year"))+
  facet_wrap(~year)

#single pane
ggplot(herringsub)+theme_bw()+
  geom_density(aes(x=size, color=as.character(year), fill=as.character(year)),
               adjust=3,alpha=0.7,size=1.5)+
  scale_fill_manual(values=mycolors, name="year")+
  scale_color_manual(values=mycolors, name="year")+
  xlab("Herring Length (mm)")+
  guides(color=guide_legend("year"))

#mean and median size of the top 5 species across time
#-----------------------------------------------------#

tprovsubsize<-tprov %>% 
  filter(prey %in% c("herring","hake","butterfish","sandlance","mackerel"))

ggplot(tprovsubsize)+theme_bw()+
  geom_point(aes(x=year,y=meansize, color=prey))+
  geom_line(aes(x=year,y=meansize, color=prey))+
  geom_point(aes(x=year,y=mediansize, color=prey), pch=21)+
  geom_line(aes(x=year,y=mediansize, color=prey), linetype="dashed")

#herring
#-------
tprovsubsizeherr<-tprov %>% 
  filter(prey %in% c("herring"))



ggplot(tprovsubsizeherr)+theme_bw()+
  geom_point(aes(x=year,y=meansize, color=prey))+
  geom_line(aes(x=year,y=meansize, color=prey))+
  geom_point(aes(x=year,y=mediansize, color=prey), pch=21)+
  geom_line(aes(x=year,y=mediansize, color=prey), linetype="dashed")



#hake
#-------
tprovsubsizehake<-tprov %>% 
  filter(prey %in% c("hake"))

ggplot(tprovsubsizehake)+theme_bw()+
  geom_point(aes(x=year,y=meansize, color=prey))+
  geom_line(aes(x=year,y=meansize, color=prey))+
  geom_point(aes(x=year,y=mediansize, color=prey), pch=21)+
  geom_line(aes(x=year,y=mediansize, color=prey), linetype="dashed")





##================================================##
## SIZE DISTRIBUTION of FISH in TERN FEEDING DATA ##
##================================================##

tprovsize0 <- dfclean %>% 
  drop_na(size) %>% 
  filter(prey %in% c("herring","hake","butterfish","mackerel","sandlance","pollock")) %>% 
  select(year,prey,size)

tprovsizebyyear<-tprovsize0 %>% 
  group_by(prey,year) %>% 
  summarise(meansize=mean(as.numefstric(size)),minsize=min(as.numeric(size)),maxsize=max(as.numeric(size))) 

tprovsizeallyrs<-tprovsize0 %>% 
  group_by(prey) %>% 
  summarise(meansize=mean(as.numeric(size)),minsize=min(as.numeric(size)),maxsize=max(as.numeric(size))) 

