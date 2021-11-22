
## playground and misc code ##


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






#====================================================================================#
## GENERAL USE DF with PROPORTION and RELATIVE ABUNDANCE for each DAY and WEEK of each YEAR ##
#====================================================================================#

#-- this df will have a row for each family/year combinations
#-- families will include all fish species along invertebrate, unknown, and unknown fish categories
#-- columns generated will include an intermediate sampling effort column and final relative abundance (in fish per nest minute)
#   and proportion (proportion of family from total) columns
#-- uses dfclean from above

#sampling effort
dailyweeklyeffort<-dfclean %>%
  dplyr::mutate(`sampling effort`=(watchtime*as.numeric(nestswatched)))%>%
  group_by(year, week, day) %>%
  dplyr::summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T))

#diet data
dailyweeklyfish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("")) %>% 
  group_by(year, week, day, prey) %>%
  dplyr:: summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T)) %>%
  dplyr:: mutate(prop=n/sum(n))

#combine and generate relative abundance
dailyweeklyfisheffort<-merge(dailyweeklyeffort,dailyweeklyfish) %>%
  dplyr::mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

#FINAL DATAFRAME (remove intermediate effort column, rename columns for easy use)
dailyweeklytprov<-dailyweeklyfisheffort %>%
  dplyr::select(year, week, day, prey, n, meansize, `Effort (nest mins)`,prop, `relative abundance (fish per nest min)`) %>%
  dplyr::rename(proportion=prop) %>%
  dplyr::rename(size=meansize) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)






#====================================================================================#
## GENERAL USE DF with PROPORTION and RELATIVE ABUNDANCE for each WEEK of each YEAR ##
#====================================================================================#

#-- this df will have a row for each family/year combinations
#-- families will include all fish species along invertebrate, unknown, and unknown fish categories
#-- columns generated will include an intermediate sampling effort column and final relative abundance (in fish per nest minute)
#   and proportion (proportion of family from total) columns
#-- uses dfclean from above

#sampling effort
weeklyeffort<-dfclean %>%
  mutate(`sampling effort`=(watchtime*as.numeric(nestswatched)))%>%
  group_by(year, week) %>%
  summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T))

#diet data
weeklyfish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("")) %>% 
  group_by(year, week, prey) %>%
  dplyr:: summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T)) %>%
  dplyr::mutate(prop=n/sum(n))

#combine and generate relative abundance
weeklyfisheffort<-merge(weeklyeffort,weeklyfish) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

#FINAL DATAFRAME (remove intermediate effort column, rename columns for easy use)
weeklytprov<-weeklyfisheffort %>%
  select(year, week, prey, n, meansize, prop, `relative abundance (fish per nest min)`) %>%
  dplyr::rename(proportion=prop) %>%
  dplyr::rename(size=meansize) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)







#================================================================================================#
## GENERAL USE DF with PROPORTION and RELATIVE ABUNDANCE for each WEEK and MONTH of each YEAR ##
#================================================================================================#
#-- uses dfclean from above

#sampling effort
weeklymonthlyeffort<-dfclean %>%
  mutate(`sampling effort`=(watchtime*as.numeric(nestswatched)))%>%
  group_by(year, week, month) %>%
  summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T))

#diet data
weeklymonthlyfish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("")) %>% 
  dplyr:: group_by(year, week, month, prey) %>%
  dplyr:: summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T)) %>%
  dplyr::mutate(prop=n/sum(n))

#combine and generate relative abundance
weeklymonthlyfisheffort<-merge(weeklymonthlyeffort,weeklymonthlyfish) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

#FINAL DATAFRAME (remove intermediate effort column, rename columns for easy use)
weeklymonthlytprov<-weeklymonthlyfisheffort %>%
  select(year, week, month, prey, n, meansize, `Effort (nest mins)`,prop, `relative abundance (fish per nest min)`) %>%
  dplyr::rename(proportion=prop) %>%
  dplyr::rename(size=meansize) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)








#====================================================================================================#
## GENERAL USE DF with PROPORTION and RELATIVE ABUNDANCE for each DAY of WEEK of MONTH of each YEAR ##
#====================================================================================================#
#-- uses dfclean from above

#sampling effort
alltimeeffort<-dfclean %>%
  mutate(`sampling effort`=(watchtime*as.numeric(nestswatched)))%>%
  group_by(year, week, month, day) %>%
  summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T))

#diet data
alltimefish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("")) %>% 
  group_by(year, week, month, day, prey) %>%
  dplyr::summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T)) %>%
  dplyr::mutate(prop=n/sum(n))

#combine and generate relative abundance
alltimeeffort<-merge(alltimeeffort,alltimefish) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

#FINAL DATAFRAME (remove intermediate effort column, rename columns for easy use)
alltimetprov<-alltimeeffort %>%
  select(year, week, month, day, prey, n, meansize, `Effort (nest mins)`,prop, `relative abundance (fish per nest min)`) %>%
  dplyr:: rename(proportion=prop) %>%
  dplyr::rename(size=meansize) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)






#==========================================================#
## ASSESSING WHICH FISH SPECIES MAKE UP >1% of TERN DIET  ##
#==========================================================#

tprovprey<-tprov %>% 
  group_by(prey) %>% 
  summarise('tot_n'=sum(n)) 

tprovpercent<-tprovprey %>% 
  mutate('percent_tot_diet'=(100*(tot_n/sum(tot_n))))

#bluefish, butterfish, hake, herring, mackerel, sandlance, and ALMOST lumpfish



#=========================================#
## PROPORTIONS of PREY ITEMS in THE DIET  ##
#=========================================#

ggplot(tprov)+
  geom_col(aes(x=year, y=proportion, fill=prey))+theme_bw()+
  ylab("Proportion of Total Observations")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))

#main five prey categories, good sample size years tern data
#----------------------------------------------------------#
tprovsub<-tprov %>%
  mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","lumpfish","hake","butterfish","sandlance","mackerel","bluefish","cunner","pollock","mummichog","stickleback"),
                                             collapse="|"), prey, ignore.case=T), prey, "other/unknown")) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

tprovsub[,9]=toupper(tprovsub[,9]) #make prey uppercase


speciesprop<-ggplot(tprovsub)+
  geom_col(aes(x=year, y=proportion, fill=preysub))+theme_bw()+
  ylab("Proportion of Total Observations")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_poke(pokemon="surskit", spread=12)








#===============================================#
## SAMPLING DISTRIBUTION ACROSS TIME TERN DATA ##
#===============================================#
#- uses unsummarized df (dfclean)

#distribution using number of observations#
#-----------------------------------------#

#total observations across days across years
ggplot(dfclean)+theme_bw()+
  geom_density(aes(x=day),
               adjust=3,alpha=0.6)+xlab("day of year")

#total observations (i.e. sampling effort) across days between years
ggplot(dfclean)+theme_bw()+
  geom_density(aes(x=day, color=as.character(year), fill=as.character(year)),
               adjust=3,alpha=0.6)+xlab("day of year")+
  facet_wrap(~year)

#total observations of each species across all years
dfcleansub<-dfclean %>%
  filter(prey %in% c("herring","hake","butterfish","sandlance","mackerel"))

ggplot(dfcleansub)+theme_bw()+
  geom_density(aes(x=day, fill=prey, color=prey),
               adjust=3,alpha=0.4,size=1)+xlab("day of year")+
  scale_fill_poke(pokemon="surskit", spread=5)+
  scale_color_poke(pokemon="surskit", spread=5)

#distribution using weekly sampling effort
#-----------------------------------------#

#total observations across days across all years single pane
ggplot(dailyweeklytprov)+theme_bw()+
  geom_point(aes(x=day, y=`Effort (nest mins)`, color=as.character(year)), na.rm=T)
#total observations across days across all years multiple panes
ggplot(dailyweeklytprov)+theme_bw()+
  geom_point(aes(x=day, y=`Effort (nest mins)`, color=as.character(year)), na.rm=T)+
  geom_line(aes(x=day, y=`Effort (nest mins)`, color=as.character(year)), na.rm=T)+
  facet_wrap(~year)

#plot number of samples and sampling effort together to compare
dailyweeklytprovsub<-dailyweeklytprov %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))
dfcleansub<-dfclean %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

ggplot()+theme_bw()+
  geom_density(data=dfcleansub,aes(x=day, color=as.character(year), fill=as.character(year)),
               adjust=3,alpha=0.3)+xlab("day of year")+
  geom_line(data=dailyweeklytprovsub, aes(x=day, y=(`Effort (nest mins)`/20000), color=as.character(year)), na.rm=T, size=1)+
  scale_y_continuous(expand=c(0,0.2),
                     name="sampling density", sec.axis=sec_axis(~.*20000, name="sampling effort (nest min)"))+
  facet_wrap(~year)







#===========================================#
## PROPORTIONS OF PREY ITEMS in TERN DIET  ##
#========================================#

#all prey categories all years tern data
#--------------------------------------#

ggplot(tprov)+
  geom_col(aes(x=year, y=proportion, fill=prey))+theme_bw()+
  ylab("Proportion of Total Observations")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))


#main five prey categories, good sample size years tern data
#----------------------------------------------------------#
tprovsub<-tprov %>%
  mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","lumpfish","hake","butterfish","sandlance","mackerel","bluefish","cunner","pollock","mummichog","stickleback"),
                                             collapse="|"), prey, ignore.case=T), prey, "other/unknown")) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

tprovsub[,9]=toupper(tprovsub[,9]) #make prey uppercase


ggplot(tprovsub)+
  geom_col(aes(x=year, y=proportion, fill=preysub))+theme_bw()+
  ylab("Proportion of Total Observations")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_poke(pokemon="surskit", spread=12)



#====================================================#
## RELATIVE ABUNDANCE of PREY ITEMS in TERN DIEt ##
#====================================================#

# all species all years TERNS
#---------------------------#
ggplot(tprov)+theme_bw()+
  geom_point(aes(x=year, y=abundance, color=prey))+ylim(0,0.015)+
  geom_line(aes(x=year, y=abundance, color=prey))+xlab("Year")+ylab("relative abundance (fish/nest min)")

# top 5 species, good sample size years TERNS
#-------------------------------------------#
tprovsub<-tprov %>%
  mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","hake","butterfish","sandlance","mackerel","unknown fish"),
                                             collapse="|"), prey, ignore.case=T), prey, "other")) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

ggplot(tprovsub)+theme_bw()+
  geom_point(aes(x=year, y=abundance, color=preysub))+ylim(0,0.015)+
  geom_line(aes(x=year, y=abundance, color=preysub))+xlab("Year")+ylab("relative abundance (fish/nest min)")+
  scale_color_poke(pokemon="surskit", spread=8)

#total relative abundance, good sample years (i.e. "total relative landings") TERNS
#----------------------------------------------------------------------------------#
tprovyrs<-tprov %>%
  group_by(year) %>%
  summarise(totalabundance=sum(abundance)) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

ggplot(tprovyrs)+theme_bw()+
  geom_point(aes(x=year, y=totalabundance))+ylim(0,0.05)+
  geom_line(aes(x=year, y=totalabundance))+xlab("Year")+ylab("total abundance (fish/nest min)")

#landings plotted with proportions TERNS
#---------------------------------------#
ggplot()+
  geom_col(data=tprovsub, aes(x=year, y=proportion, fill=preysub))+theme_bw()+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_poke(pokemon="surskit", spread=8)+
  geom_point(data=tprovyrs, aes(x=year, y=totalabundance*20),size=2)+
  geom_line(data=tprovyrs, aes(x=year, y=totalabundance*20), size=1)+
  scale_x_continuous(expand=c(0,0.01),name="Year")+
  scale_y_continuous(expand=c(0,0.001),
                     name="Proportion in diet", sec.axis=sec_axis(~./20, name="Total abundance (fish/nest min)"))
#TO DO: make the line cut off for years without samples?

# relative abundanc`e of certain species throughout the season for each year #
#---------------------------------------------------------------------------#
#--using herring as an example
herringtprov<-dailyweeklytprov %>%
  group_by(year, week) %>%
  filter(prey %in% c("herring")) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

#good sample years multiple panes
ggplot(herringtprov)+theme_bw()+
  geom_point(aes(x=day, y=abundance, color=as.character(year)))+
  geom_line(aes(x=day, y=abundance, color=as.character(year)))+
  facet_wrap(~year)

#good sample years multiple panes
ggplot(herringtprov)+theme_bw()+
  geom_point(aes(x=day, y=abundance))


# relative abundance of certain species throughout the season across years #
#--------------------------------------------------------------------------#
ggplot(herringtprov)+theme_bw()+
  geom_point(aes(x=day, y=abundance))








#====================================#
## BLIND vs VIDEO METHOD COMPARISON ##
#====================================#
#-- proportion of species in blind vs video

#copied from sampling effort from earler:
effort<-dfclean %>%
  mutate(`sampling effort`=(watchtime*nestswatched))%>%
  group_by(method) %>%
  summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T))

methodfish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("")) %>% 
  group_by(method, prey) %>%
  summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T)) %>%
  mutate(prop=n/sum(n))

methodfisheffort<-merge(effort,methodfish) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

methodtprov<-methodfisheffort %>%
  select(prey, method, meansize, prop, `relative abundance (fish per nest min)`) %>%
  rename(proportion=prop) %>%
  rename(size=meansize) %>%
  rename(abundance=`relative abundance (fish per nest min)`)

ggplot(methodtprov)+theme_bw()+
  geom_col(aes(x=method, y=proportion, fill=prey))+theme_bw()+
  ylab("Proportion of Total Observations")+xlab("Method")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_y_continuous(expand=c(0,0.001))

#subset to top 5
methodtprovsub<-methodtprov %>%
  mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","hake","butterfish","sandlance","mackerel","unknown fish"),
                                             collapse="|"), prey, ignore.case=T), prey, "other"))
ggplot(methodtprovsub)+ theme_bw()+
  geom_col(aes(x=method, y=proportion, fill=preysub))+
  ylab("Proportion of Total Observations")+xlab("Method")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_poke(pokemon="surskit", spread=8)







#====================================#
# IN PROCESS: MAP of GOM STUDY SITE ##
#====================================#

library(ggmap)
library(cowplot)
library(googleway)
library(ggrepel)
library(ggspatial)
library(libwgeom)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggsn)

# by the way, white island is at c(42.966,-70.625)

register_google(key = "AIzaSyAkZZRBsRjxtw_PCaAidAVCdzKY5VVwDf4")

map<-get_googlemap(c(-70.625,42.968), maptype = "satellite", zoom=9)

ggmap(map)

