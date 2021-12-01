
#============================#
## NMDS for TERN PREY ITEMS ##
#============================#
library(vegan)
library(moments)
library(dplyr)

#====================================================================================#
## GENERAL USE DF with PROPORTION and RELATIVE ABUNDANCE for each DAY and WEEK of each YEAR ##
#====================================================================================#

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

## start creating ordination

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

#Go back and create the output for the 2 dimensions NMDS--chosen based on rules of thumb (1) below 20 and (2) drop <5 to next n-dimension
terndiet_NMDS<-metaMDS(NMDS_1ut, distance = "bray", k = 2, try=250, autotransform=F)
r2<-summary(lm(original.dist~dist(vegan::scores(terndiet_NMDS))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress<-terndiet_NMDS$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions

#Ordination plots for the NMDS 2D results--put them into dfs for ggplot
allNMDS_species_terns<-as.data.frame(terndiet_NMDS$species)
allNMDS_samples_terns<-as.data.frame(terndiet_NMDS$points)
allNMDS_env_terns<-as.data.frame(NMDS_2) %>% 
  dplyr:: select(year,week, uniqueID...1) %>% 
  mutate(binyear=cut(year, breaks=c(-Inf, 2003, 2007, 2011, 2015, Inf), labels=c("1999-2003","2004-2007","2008-2011","2011-2015","2016-2020"))) %>% 
  mutate(binyeardecs=cut(year, breaks=c(-Inf, 2006, 2013, Inf), labels=c("1999-2006","2007-2013","2014-2020"))) %>% 
  mutate(month=cut(week, breaks=c(-Inf, 22, 26, 30, Inf), labels=c("May","June","July","August")))

#Plot filled by year
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns,aes(MDS1,MDS2, fill=allNMDS_env_terns$binyeardecs),shape=21,size=3,alpha=0.75)+ 
  stat_ellipse(data=allNMDS_samples_terns,aes(MDS1,MDS2,color=allNMDS_env_terns$binyeardecs),lwd=1.1, linetype=2)+
  geom_segment(data=allNMDS_species_terns,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1,alpha=0.7)+ 
  geom_label(data=allNMDS_species_terns,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns))),fill="white",size=6)+
  xlab("Axis 1")+ylab("Axis 2")+ 
  geom_text(x=-1.6, y= 1.4, aes(-.65,1.27,label=paste("Stress =",round(actualStress,digits=4)*100)),size=4)+
  scale_fill_manual(values=c("brown3","darkolivegreen","darkgoldenrod1"))+scale_color_manual(values=c("brown3","darkolivegreen","darkgoldenrod1"))+
  guides(fill=guide_legend(title=""))
  
 

#plot filled by month different method
#went back and used the alltimetprov df that has months in it
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
