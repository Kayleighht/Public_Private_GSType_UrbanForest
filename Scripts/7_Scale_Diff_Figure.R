#### Load Packages
Packages.7 <- c("plyr", "ggplot2", "dplyr","ggthemes", "tidyr")
lapply(Packages.7, library, character.only = TRUE)

#### Plotting Scale (Site vs. Parcel) Differences in Tree Richness
meta.sum<- read.csv("LMM_META_FINAL.csv")

###Site-scale matrices and plotting
#calculate site-scale species richness and standard error
library(dplyr)
sr.gs.type <- meta.sum %>%
  group_by(GS.Type) %>%
  summarise(
    sd = sd(SR),
    S.R = mean(SR))
sr.gs.type<- as.data.frame(sr.gs.type)

####Figure
##creating axis labels
scale.labs<- labs(x= "Green Space Type", y= "Species Richness")
#creating colour table
colour.table.site<- tibble(
  GS.Type= c("Inst", "Park", "Private", "Row"),
  Colour= c("gray82", "gray82","gray46", "gray46"))

#plot commands
site.scale.bar<- ggplot(sr.gs.type, aes(x= reorder(GS.Type, -S.R), y=S.R, fill= GS.Type))
site.scale.bar+ geom_bar(stat = 'identity', position = position_dodge(width=0.9)) + 
  geom_errorbar(data = sr.gs.type, mapping= aes(x= GS.Type, y= S.R, ymin= S.R - sd, ymax= S.R + sd), size= 0.4, width= 0.03) + 
  scale.labs + theme_classic() + scale_fill_manual(values= colour.table.site$Colour) + 
  theme(axis.title = element_text(face= "bold", size= 45), axis.text.x = element_text(size= 40), axis.text.y = element_text(size= 40))

###Parcel-scale matrices and plotting
#read in institutional data
in.data<- read.csv("Clean_Inst_Cu.csv")
#create table with species abundances sorted by site code
wide.in<- table(in.data$Species.Code., in.data$Site.Code.)
wide.in<- as.data.frame(wide.in)
wide.in<- spread(wide.in, "Var1", "Freq")

#Calculate species richness per parcel (per institution)
in.r<-ddply(wide.in,~Var2,function(x) {
  data.frame(RICHNESS=sum(x[-1]>0))
})
in.r

#read in park data
park.data<- read.csv("Clean_Park_Cu.csv")
#create table with species abundances sorted by site code
wide.park<- table(park.data$Species.Code, park.data$Site.Code)
wide.park<- as.data.frame(wide.park)
wide.park<- spread(wide.park, "Var1", "Freq")

park.r<-ddply(wide.park,~Var2,function(x) {
  data.frame(RICHNESS=sum(x[-1]>0))
})

####For Plotting
#calculate park means and standard deviation for plotting
park.mean<-mean(park.r$RICHNESS) 
park.sd<-sd(park.r$RICHNESS)

##Creating a dataframe with parcel data
parcel.means<- data.frame(mean(in.r$RICHNESS))
parcel.means$sd<- sd(in.r$RICHNESS)
parcel.means[nrow(parcel.means) +1,] = c(park.mean, as.numeric(park.sd))
parcel.means$GS.Type<- c("Inst", "Park")
parcel.means$sd<- as.numeric(parcel.means$sd)
parcel.means
colnames(parcel.means)<- c("SR", "sd", "GS.Type")

#Subset Private and Institutional means from matrix
Pr.Row.Parcel.Means<-sr.gs.type[-c(1,2), ]
colnames(Pr.Row.Parcel.Means)<- c("GS.Type", "sd", "SR")

#add new rows with private and institutional data
parcel.means<-parcel.means %>% add_row(SR= 4.123596, sd= 2.674884, GS.Type= "Private")
parcel.means<-parcel.means %>% add_row(SR= 6.073394, sd= 2.455932, GS.Type= "Row")

#Creating axis labels
parcel.labs<- labs(x= "Green Space Type", y= "Species Richness")
#Adding colour scale for bars
colour.table.parcel<- tibble(
  GS.Type= c("Inst", "Park", "Private", "Row"),
  Colour= c("gray82", "gray82", "gray46", "gray46"))

#plotting
parcel.scale.bar<-  ggplot(parcel.means, aes(x= reorder(GS.Type, -SR), y= SR, fill= GS.Type))
parcel.scale.bar + geom_bar(stat = 'identity') + 
  geom_errorbar(data= parcel.means, mapping= aes(x= GS.Type, y= SR, ymin= SR - sd, ymax= SR + sd), size= 0.4, width= 0.03) + 
  theme_classic() + parcel.labs + scale_fill_manual(values= colour.table.parcel$Colour)+ 
  theme(axis.title = element_text(face= "bold", size= 45), axis.text.x = element_text(size= 40), axis.text.y = element_text(size= 40))

