
## SHANNON DIVERSITY ##




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





#=========================================================#
## GENERAL USE DF with PROPORTION and RELATIVE ABUNDANCE ##
#=========================================================#

#-- this df will have a row for each family/year combinations
#-- families will include all fish species along with invertebrate, unknown, and unknown fish categories
#-- columns generated will include an intermediate sampling effort column and final relative abundance (in fish per nest minute)
#   and proportion (proportion of family from total) columns
#-- uses dfclean from above

#sampling effort
effort<-dfclean %>%
  filter(!IncludeForRegression %in% c("no")) %>% 
  dplyr::mutate(`sampling effort`=(watchtime*as.numeric(nestswatched))) %>%
  group_by(year) %>%
  dplyr::summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T))

#diet data
fish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("")) %>% 
  group_by(year,prey) %>%
  dplyr::summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T), mediansize=median(as.numeric(size),na.rm=T)) %>%
  dplyr::mutate(prop=n/sum(n))

#combine and generate relative abundance
fisheffort<-merge(effort,fish) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

#FINAL DATAFRAME (remove intermediate effort column, rename columns for easy use)
tprov<-fisheffort %>%
  dplyr::select(year, prey, n, meansize, mediansize, `Effort (nest mins)`, prop, `relative abundance (fish per nest min)`) %>%
  dplyr::rename(proportion=prop) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)


#remove non-fish species and bad years
tprovfish<-tprov %>%
  filter(! year %in% c(2003, 2004, 2010, 2012, 2016)) %>% 
  filter(! prey %in% c("unknown", "unknown fish", "other"))


## calculate shannon diversity
tprovshannon<-tprov %>% 
  select(year,prey,proportion) %>% 
  group_by(year) %>% 
  summarise(H=sum((proportion)*log10(proportion)*-1))


## plot shannon diversity over time

ggplot(tprovshannon)+theme_classic()+
  geom_point(aes(x=year,y=H))+
  geom_line(aes(x=year,y=H))+
  ylab("Shannon Diversity")+xlab("")+
  scale_x_continuous(breaks = seq(1999, 2020, by = 1))
  
