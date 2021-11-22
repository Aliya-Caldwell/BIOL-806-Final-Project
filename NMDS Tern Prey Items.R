
#============================#
## NMDS for TERN PREY ITEMS ##
#============================#
library(vegan)
library(moments)
library(dplyr)

#start with "dailyweeklytprov" df from line 109 ish

#subset
tprovsub<-tprov %>%
  mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","hake","butterfish","sandlance","mackerel","pollock","unknown fish"),
                                             collapse="|"), prey, ignore.case=T), prey, "other")) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

dailyweeklytprovsub<-dailyweeklytprov %>%
  filter(!year %in% c(2003, 2004, 2010, 2012)) %>%
  filter(prey %in% c("herring","hake","butterfish","mackerel","sandlance", "bluefish","cunner","lumpfish",
                     "mummichog","pollock","silverside","stickleback")) %>%
  filter(!abundance>300000000) #im fucking annoying for fixing an issue this way but there were weird infinity values in the abundance column that needed to come out

dfNMDS0<-dailyweeklytprovsub %>%
  dplyr::mutate(uniqueID=group_indices(.,year,day)) %>%
  dplyr::select(uniqueID, prey, abundance, year, week) %>%
  spread(prey, abundance) %>%
  dplyr::mutate_all(~replace(., is.na(.),0))

dfNMDS1<-dfNMDS0 %>%
  dplyr::select(uniqueID, year, week) %>%
  group_by(uniqueID) %>%
  dplyr::summarise_all(funs(max))

dfNMDS2<-dfNMDS0 %>%
  dplyr::select(uniqueID,butterfish, hake, herring, mackerel, sandlance, bluefish, cunner, lumpfish,
                mummichog, pollock, silverside, stickleback) %>%
  group_by(uniqueID) %>%
  dplyr::summarise_all(funs(sum))

dfNMDS<-bind_cols(dfNMDS1, dfNMDS2) %>% 
  dplyr::select(-uniqueID...4)


NMDS_1ut<-dfNMDS[,4:12]
NMDS_2<-dfNMDS[,1:3]

#For the all diets NMDS
original.dist<-vegdist(NMDS_1ut) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r2<-numeric(4)

for (n in 1:4) {
  nmds.resu <- metaMDS(NMDS_1ut, k=n, distance = "bray", try=250, autotransform=F) #Runs an NMDS at each dimension with 250 random starts
  stress_values[n]<-nmds.resu$stress*100 #Extract the ideal stress at that dimension and put it into the vector
  nmds.scores<-scores(nmds.resu)
  nmds.dist<-dist(nmds.scores)
  r2[n]<-summary(lm(original.dist~nmds.dist))[[8]] #Calculate the r-squared of that n-dim ordination
}
plot(stress_values, xlab="Number of axes", ylab="Stress",type="b")
abline(h=20,col="red") #Rule of thumb, a good ordination will have stress below 20 at least

View(stress_values) #Second rule of thumb is stress should be <5 more than the stress at n+1-dimension

#Go back and create the output for the 2 dimensions NMDS--chosen based on rules of thumb (1) below 20 and (2) drop <5 to next n-dimension
terndiet_NMDS<-metaMDS(NMDS_1ut, distance = "bray", k = 2, try=250, autotransform=F)
r2<-summary(lm(original.dist~dist(vegan::scores(terndiet_NMDS))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress<-terndiet_NMDS$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


#Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns<-as.data.frame(terndiet_NMDS$species)
allNMDS_samples_terns<-as.data.frame(terndiet_NMDS$points)
allNMDS_env_terns<-as.data.frame(NMDS_2) %>% 
  dplyr:: select(year,week, uniqueID...1) %>% 
  mutate(binyear=cut(year, breaks=c(-Inf, 2003, 2007, 2011, 2015, Inf), labels=c("1999-2003","2004-2007","2008-2011","2011-2015","2016-2020"))) %>% 
  mutate(binyeardecs=cut(year, breaks=c(-Inf, 2006, 2013, Inf), labels=c("1999-2006","2007-2013","2014-2020"))) %>% 
  mutate(month=cut(week, breaks=c(-Inf, 22, 26, 30, Inf), labels=c("May","June","July","August")))



#Plot filled by year
myord<-ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns,aes(MDS1,MDS2, fill=allNMDS_env_terns$binyeardecs),shape=21,size=2,alpha=0.5)+ #Each point is a sample (unique Id i.e. watch day)
  geom_segment(data=allNMDS_species_terns,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.2, y= 1.45, aes(-.65,1.27,label=paste("Stress =",round(actualStress,digits=4)*100)),size=6)+
  scale_fill_manual(values=c("red","green","yellow"))+
  guides(fill=guide_legend(title=""))

#plot filled by week
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns,aes(MDS1,MDS2, fill=allNMDS_env_terns$week),shape=21,size=2,alpha=0.5)+ #Each point is a sample (unique Id i.e. watch day)
  geom_segment(data=allNMDS_species_terns,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.5, y= 1.4, aes(-.65,1.27,label=paste("Stress =",round(actualStress,digits=4)*100)),size=6)

#plot filled by month different method
#went back and used the alltimetprov df that has months in it
#TO DO: go back and use this df for the other NMDS above so that I dont have to run multiple models to look at different timescales
alltimetprovsub<-subset(alltimetprov, prey=="hake"|prey=="herring"|prey=="butterfish"|prey=="sandlance"|prey=="mackerel")

alltimetprovsub<-alltimetprov %>%
  filter(!year %in% c(2003, 2004, 2010, 2012)) %>%
  filter(prey %in% c("herring","hake","butterfish","mackerel","sandlance")) %>%
  filter(!abundance>300000000) #im fucking annoying for fixing an issue this way but there were weird infinity values in the abundance column that needed to come out

dfNMDS01<-alltimetprovsub %>%
  mutate(uniqueID=group_indices(.,year,day)) %>%
  select(uniqueID, prey, abundance, n, year, week, month) %>%
  spread(prey, abundance) %>%
  mutate_all(~replace(., is.na(.),0))

dfNMDS11<-dfNMDS01 %>%
  select(uniqueID, year, week, month) %>%
  group_by(uniqueID) %>%
  summarise_all(funs(max))

dfNMDS21<-dfNMDS01 %>%
  select(uniqueID, n, butterfish, hake, herring, mackerel, sandlance) %>%
  group_by(uniqueID) %>%
  summarise_all(funs(sum))

dfNMDS1<-bind_cols(dfNMDS11, dfNMDS21) %>%
  select(!uniqueID...5)

NMDS_1ut1<-dfNMDS1[,6:10]
NMDS_21<-dfNMDS1[,1:5]

#For the all diets NMDS
original.dist1<-vegdist(NMDS_1ut1) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values1<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r21<-numeric(4)

#run NMDS
terndiet_NMDS1<-metaMDS(NMDS_1ut1, distance = "bray", k = 2, try=250, autotransform=F)
r21<-summary(lm(original.dist1~dist(vegan::scores(terndiet_NMDS1))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress1<-terndiet_NMDS1$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS1) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions

#Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns1<-as.data.frame(terndiet_NMDS1$species)
allNMDS_samples_terns1<-as.data.frame(terndiet_NMDS1$points)
allNMDS_env_terns1<-as.data.frame(NMDS_21)

#plot filled by month
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns1,aes(MDS1,MDS2, fill=allNMDS_env_terns1$month),shape=21,size=2,alpha=0.5)+
  stat_ellipse(data=allNMDS_samples_terns1,aes(MDS1,MDS2,color=allNMDS_env_terns1$month),lwd=1.0)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  geom_segment(data=allNMDS_species_terns1,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns1,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns1))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.5, y= 1.7, aes(-.65,1.27,label=paste("Stress =",round(actualStress1,digits=4)*100)),size=6)


#IN PROCESS: NMDS between decades
#-------------------------------#

# so to start I am just going to run and visualize multiple NMDSs
# gonna do this in 5 year increments (1999-2004, 2005-2009, 2010-2014, 2015-2019)

alltimetprovsub<-subset(alltimetprov, prey=="hake"|prey=="herring"|prey=="butterfish"|prey=="sandlance"|prey=="mackerel")

alltimetprovsub<-alltimetprov %>%
  filter(!year %in% c(2003, 2004, 2010, 2012)) %>%
  filter(prey %in% c("herring","hake","butterfish","mackerel","sandlance")) %>%
  filter(!abundance>300000000) #im fucking annoying for fixing an issue this way but there were weird infinity values in the abundance column that needed to come out

dfNMDS01<-alltimetprovsub %>%
  mutate(uniqueID=group_indices(.,year,day)) %>%
  select(uniqueID, prey, abundance, n, year, week, month) %>%
  spread(prey, abundance) %>%
  mutate_all(~replace(., is.na(.),0))

dfNMDS11<-dfNMDS01 %>%
  select(uniqueID, year, week, month) %>%
  group_by(uniqueID) %>%
  summarise_all(funs(max))

dfNMDS21<-dfNMDS01 %>%
  select(uniqueID, n, butterfish, hake, herring, mackerel, sandlance) %>%
  group_by(uniqueID) %>%
  summarise_all(funs(sum))

dfNMDS1<-bind_cols(dfNMDS11, dfNMDS21) %>%
  select(!uniqueID1)

# FOR 1999-2004 #

dfNMDS1999<-dfNMDS1 %>%
  filter(year %in% c(1999:2004)) #sample size is 115

NMDS_1ut1<-dfNMDS1999[,6:10]
NMDS_21<-dfNMDS1999[,1:5]

#For the all diets NMDS
original.dist1<-vegdist(NMDS_1ut1) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values1<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r21<-numeric(4)

#run NMDS
terndiet_NMDS1<-metaMDS(NMDS_1ut1, distance = "bray", k = 2, try=250, autotransform=F)
r21<-summary(lm(original.dist1~dist(vegan::scores(terndiet_NMDS1))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress1<-terndiet_NMDS1$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS1) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


#Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns1<-as.data.frame(terndiet_NMDS1$species)
allNMDS_samples_terns1<-as.data.frame(terndiet_NMDS1$points)
allNMDS_env_terns1<-as.data.frame(NMDS_21)

#plot filled by month
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns1,aes(MDS1,MDS2, fill=allNMDS_env_terns1$month),shape=21,size=2,alpha=0.5)+
  stat_ellipse(data=allNMDS_samples_terns1,aes(MDS1,MDS2,color=allNMDS_env_terns1$month),lwd=1.0)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  geom_segment(data=allNMDS_species_terns1,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns1,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns1))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.8, y= 3, aes(-.65,1.27,label=paste("Stress =",round(actualStress1,digits=4)*100)),size=6)+
  scale_x_continuous(limits=c(-2.5,3.5))+
  scale_y_continuous(limits=c(-2.5,3))

# FOR 2005-2009 #

dfNMDS2005<-dfNMDS1 %>%
  filter(year %in% c(2005:2009)) #sample size is 70

NMDS_1ut1<-dfNMDS2005[,6:10]
NMDS_21<-dfNMDS2005[,1:5]

#For the all diets NMDS
original.dist1<-vegdist(NMDS_1ut1) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values1<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r21<-numeric(4)

#run NMDS
terndiet_NMDS1<-metaMDS(NMDS_1ut1, distance = "bray", k = 2, try=250, autotransform=F)
r21<-summary(lm(original.dist1~dist(vegan::scores(terndiet_NMDS1))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress1<-terndiet_NMDS1$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS1) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


#Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns1<-as.data.frame(terndiet_NMDS1$species)
allNMDS_samples_terns1<-as.data.frame(terndiet_NMDS1$points)
allNMDS_env_terns1<-as.data.frame(NMDS_21)

#plot filled by month
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns1,aes(MDS1,MDS2, fill=allNMDS_env_terns1$month),shape=21,size=2,alpha=0.5)+
  stat_ellipse(data=allNMDS_samples_terns1,aes(MDS1,MDS2,color=allNMDS_env_terns1$month),lwd=1.0)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  geom_segment(data=allNMDS_species_terns1,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns1,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns1))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.8, y= 3, aes(-.65,1.27,label=paste("Stress =",round(actualStress1,digits=4)*100)),size=6)+
  scale_x_continuous(limits=c(-2.5,3.5))+
  scale_y_continuous(limits=c(-2.5,3))

# FOR 2010-2020 #

dfNMDS2019<-dfNMDS1 %>%
  filter(year %in% c(2019:2020)) #sample size is 74

NMDS_1ut1<-dfNMDS2019[,6:10]
NMDS_21<-dfNMDS2019[,1:5]

#For the all diets NMDS
original.dist1<-vegdist(NMDS_1ut1) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values1<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r21<-numeric(4)

#run NMDS
terndiet_NMDS1<-metaMDS(NMDS_1ut1, distance = "bray", k = 2, try=250, autotransform=F)
r21<-summary(lm(original.dist1~dist(vegan::scores(terndiet_NMDS1))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress1<-terndiet_NMDS1$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS1) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


#Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns1<-as.data.frame(terndiet_NMDS1$species)
allNMDS_samples_terns1<-as.data.frame(terndiet_NMDS1$points)
allNMDS_env_terns1<-as.data.frame(NMDS_21)

#plot filled by month
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns1,aes(MDS1,MDS2, fill=allNMDS_env_terns1$month),shape=21,size=2,alpha=0.5)+
  stat_ellipse(data=allNMDS_samples_terns1,aes(MDS1,MDS2,color=allNMDS_env_terns1$month),lwd=1.0)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  geom_segment(data=allNMDS_species_terns1,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns1,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns1))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.8, y= 3, aes(-.65,1.27,label=paste("Stress =",round(actualStress1,digits=4)*100)),size=6)+
  scale_x_continuous(limits=c(-2.5,3.5))+
  scale_y_continuous(limits=c(-2.5,3))

# FOR FIRST DECADE ISH (i.e. 1999-2009) #

dfNMDSdec1<-dfNMDS1 %>%
  filter(year %in% c(1999:2009)) #sample size is 185

NMDS_1ut1<-dfNMDSdec1[,6:10]
NMDS_21<-dfNMDSdec1[,1:5]

#For the all diets NMDS
original.dist1<-vegdist(NMDS_1ut1) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values1<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r21<-numeric(4)

#run NMDS
terndiet_NMDS1<-metaMDS(NMDS_1ut1, distance = "bray", k = 2, try=250, autotransform=F)
r21<-summary(lm(original.dist1~dist(vegan::scores(terndiet_NMDS1))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress1<-terndiet_NMDS1$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS1) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


#Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns1<-as.data.frame(terndiet_NMDS1$species)
allNMDS_samples_terns1<-as.data.frame(terndiet_NMDS1$points)
allNMDS_env_terns1<-as.data.frame(NMDS_21)

#plot filled by month
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns1,aes(MDS1,MDS2, fill=allNMDS_env_terns1$month),shape=21,size=2,alpha=0.5)+
  stat_ellipse(data=allNMDS_samples_terns1,aes(MDS1,MDS2,color=allNMDS_env_terns1$month),lwd=1.0)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  geom_segment(data=allNMDS_species_terns1,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns1,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns1))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.8, y= 3, aes(-.65,1.27,label=paste("Stress =",round(actualStress1,digits=4)*100)),size=6)+
  scale_x_continuous(limits=c(-2.5,3.5))+
  scale_y_continuous(limits=c(-2.5,3))

# FOR SECOND DECADE ISH (i.e. 2010-2020) #

dfNMDSdec2<-dfNMDS1 %>%
  filter(year %in% c(2010:2020)) #sample size is 208

NMDS_1ut1<-dfNMDSdec2[,6:10]
NMDS_21<-dfNMDSdec2[,1:5]

#For the all diets NMDS
original.dist1<-vegdist(NMDS_1ut1) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values1<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r21<-numeric(4)

#run NMDS
terndiet_NMDS1<-metaMDS(NMDS_1ut1, distance = "bray", k = 2, try=250, autotransform=F)
r21<-summary(lm(original.dist1~dist(vegan::scores(terndiet_NMDS1))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress1<-terndiet_NMDS1$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS1) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


#Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns1<-as.data.frame(terndiet_NMDS1$species)
allNMDS_samples_terns1<-as.data.frame(terndiet_NMDS1$points)
allNMDS_env_terns1<-as.data.frame(NMDS_21)

#plot filled by month
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns1,aes(MDS1,MDS2, fill=allNMDS_env_terns1$month),shape=21,size=2,alpha=0.5)+
  stat_ellipse(data=allNMDS_samples_terns1,aes(MDS1,MDS2,color=allNMDS_env_terns1$month),lwd=1.0)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  geom_segment(data=allNMDS_species_terns1,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns1,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns1))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.8, y= 3, aes(-.65,1.27,label=paste("Stress =",round(actualStress1,digits=4)*100)),size=6)+
  scale_x_continuous(limits=c(-2.5,3.5))+
  scale_y_continuous(limits=c(-2.5,3))


#IN PROCESS: NMDS using watch ID as sample as opposed to day
#----------------------------------------------------------#









##=====================================================================#
# IN PROCESS: ##NMDS comparing tern diet community and NMFS community ##
##=====================================================================#


library(vegan)

#NMDS with pres abs on only tern diet data #
#------------------------------------------#

dailyweeklytprovsub<-subset(dailyweeklytprov, prey=="hake"|prey=="herring"|prey=="butterfish"|prey=="sandlance"|prey=="mackerel")

dailyweeklytprovsub<-dailyweeklytprov %>%
  filter(!year %in% c(2003, 2004, 2010, 2012)) %>%
  filter(prey %in% c("herring","hake","butterfish","mackerel","sandlance"))

dfNMDS<-dailyweeklytprovsub %>%
  mutate(uniqueID=group_indices(.,year,day)) %>%
  select(uniqueID, prey, abundance, n) %>%
  spread(prey, abundance) %>%
  mutate_all(~replace(., is.na(.),0)) %>%
  group_by(uniqueID) %>%
  summarise_each(funs(sum)) %>%
  select(n, uniqueID, butterfish, herring, hake, sandlance, mackerel) #%>%
#filter(n>20)

NMDS_1ut<-dfNMDS[,3:7]
NMDS_2<-dfNMDS[,1:2]

NMDS_1utpa<-decostand(x=NMDS_1ut, method="pa")

#For the all diets NMDS
original.dist<-vegdist(NMDS_1utpa) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r2<-numeric(4)

for (n in 1:4) {
  nmds.resu <- metaMDS(NMDS_1utpa, k=n, distance = "bray", try=250, autotransform=F) #Runs an NMDS at each dimension with 250 random starts
  stress_values[n]<-nmds.resu$stress*100 #Extract the ideal stress at that dimension and put it into the vector
  nmds.scores<-scores(nmds.resu)
  nmds.dist<-dist(nmds.scores)
  r2[n]<-summary(lm(original.dist~nmds.dist))[[8]] #Calculate the r-squared of that n-dim ordination
}
plot(stress_values, xlab="Number of axes", ylab="Stress",type="b")
abline(h=20,col="red") #Rule of thumb, a good ordination will have stress below 20 at least

View(stress_values) #Second rule of thumb is stress should be <5 more than the stress at n+1-dimension

#Go back and create the output for the 2 dimensions NMDS--chosen based on rules of thumb (1) below 20 and (2) drop <5 to next n-dimension
terndiet_NMDS<-metaMDS(NMDS_1utpa, distance = "bray", k = 2, try=250, autotransform=F)
r2<-summary(lm(original.dist~dist(vegan::scores(terndiet_NMDS))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress<-terndiet_NMDS$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


#Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns<-as.data.frame(terndiet_NMDS$species)
allNMDS_samples_terns<-as.data.frame(terndiet_NMDS$points)
allNMDS_env_terns<-as.data.frame(NMDS_2)


#Plot
ggplot()+
  geom_point(data=allNMDS_samples_terns,aes(MDS1,MDS2),shape=21,size=2,alpha=0.5)+ #Each point is a sample (trawl)
  geom_segment(data=allNMDS_species_terns,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns))),size=8)+ #Label the lines with the species
  #stat_ellipse(data=allNMDS_samples_terns,aes(MDS1,MDS2),lwd=1.1)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  #scale_x_continuous(limits=c(-0.7,0.7))+
  #scale_y_continuous(limits=c(-0.301,0.455))+
  geom_text(aes(-.65,1.27,label=paste("Stress =",round(actualStress,digits=4)*100)),size=10,fontface="bold") #Always provide ordination stress

#  NMDS with pres abs on only trawl data #
#----------------------------------------#

setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
trawl<-read.csv("buffer_pa_tern.MV.csv")

trawldfNMDS<-trawl %>%
  mutate(herring=(ATLANTIC_HERRING+BLUEBACK_HERRING+ALEWIFE),
         hake=(RED_HAKE+SILVER_HAKE+WHITE_HAKE)) %>%
  rename(uniqueID=ID,sandlance=NORTHERN_SAND_LANCE, mackerel=ATLANTIC_MACKEREL, butterfish=BUTTERFISH) %>%
  select(uniqueID, butterfish, herring, hake, sandlance, mackerel)


NMDS_1trawl<-trawldfNMDS[,2:6]
NMDS_2trawl<-trawldfNMDS[,1]

NMDS_1trawlpa<-decostand(x=NMDS_1trawl, method="pa")
original.dist<-vegdist(NMDS_1trawlpa) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r2<-numeric(4)

for (n in 1:4) {
  nmds.resu <- metaMDS(NMDS_1trawlpa, k=n, distance = "bray", try=250, autotransform=F) #Runs an NMDS at each dimension with 250 random starts
  stress_values[n]<-nmds.resu$stress*100 #Extract the ideal stress at that dimension and put it into the vector
  nmds.scores<-scores(nmds.resu)
  nmds.dist<-dist(nmds.scores)
  r2[n]<-summary(lm(original.dist~nmds.dist))[[8]] #Calculate the r-squared of that n-dim ordination
}
plot(stress_values, xlab="Number of axes", ylab="Stress",type="b")
abline(h=20,col="red") #Rule of thumb, a good ordination will have stress below 20 at least

View(stress_values) #Second rule of thumb is stress should be <5 more than the stress at n+1-dimension

#Go back and create the output for the 2 dimensions NMDS--chosen based on rules of thumb (1) below 20 and (2) drop <5 to next n-dimension
trawl_NMDS<-metaMDS(NMDS_1trawlpa, distance = "bray", k = 2, try=250, autotransform=F)
r2<-summary(lm(original.dist~dist(vegan::scores(trawl_NMDS))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress<-trawlt_NMDS$stress #calculate the stress of this chosen NMDS ord
stressplot(trawl_NMDS) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


#Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_trawl<-as.data.frame(trawl_NMDS$species)
allNMDS_samples_trawl<-as.data.frame(trawl_NMDS$points)
allNMDS_env_trawl<-as.data.frame(NMDS_2trawl)


#Plot
ggplot()+
  geom_point(data=allNMDS_samples_trawl,aes(MDS1,MDS2),shape=21,size=2,alpha=0.5)+ #Each point is a sample (trawl)
  geom_segment(data=allNMDS_species_trawls,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_trawl,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns))),size=8)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  #scale_x_continuous(limits=c(-0.7,0.7))+
  #scale_y_continuous(limits=c(-0.301,0.455))+
  geom_text(aes(-.65,1.27,label=paste("Stress =",round(actualStress,digits=4)*100)),size=10,fontface="bold") #Always provide ordination stress