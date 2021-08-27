#load packages
Packages.5 <- c("ggplot2", "tidyr", "ggpubr", "tibble", "reshape2", "plyr", "dplyr", "janitor")
lapply(Packages.5, library, character.only = TRUE)
setwd("/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Input")

#load metadata file and max dbh file
metacsv<- read.csv("Metadata_GS_CU .csv")
max.dbh<- read.csv("Max.DBH.csv")
LMM.Meta<- read.csv("LMM_META_FINAL.csv")

####DBH Distribution
#create kernel density plot to show the distribution of DBH across the four green space types 
#### DBH Distribution ####
##Scaled for all green space types
gs.dbh<- ggplot(data= metacsv,
                mapping = aes(x= DBH.Round, group = GS.Type, col= GS.Type))

gs.dbh<-gs.dbh + geom_density(alpha= 0.02, mapping = aes(y= ..count..)) +
        theme_classic() + xlim(c(0,125)) + xlab("DBH (cm)")+ ylab("Density")+ 
        theme(legend.position = "top", axis.title = element_text(face= "bold", size= 18), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
gs.dbh

#formatting max DBH file and combining it with tree data file
#combine two dataframes to include max DBH values
gs.max.dbh<- left_join(metacsv, max.dbh, by= c("Species.Code" = "Species.Code"))
#converting columns to numerics rather than factors and removing rows that have empty values 
gs.max.dbh$Max.DBH <- as.numeric(as.character(gs.max.dbh$Max.DBH))  
#removing NA values from dataframe for plotting
gs.max.dbh<- gs.max.dbh %>% drop_na()
gs.max.dbh

gs.maxdbh<- ggplot(data= gs.max.dbh,
                mapping = aes(x= Max.DBH, group = GS.Type, col= GS.Type))

gs.maxdbh.plot<-gs.maxdbh + geom_density(alpha= 0.02, mapping = aes(y= ..count..), bw=5) +
  theme_classic() + xlim(0,110) + xlab("Maximum DBH (cm)")+ ylab("Density")+ 
  theme(legend.position = "none", axis.title = element_text(face= "bold", size= 18), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
gs.maxdbh.plot

#arranging two figures together
FinalDBH.Fig<-ggarrange(gs.dbh, gs.maxdbh.plot,
                        nrow = 2, ncol = 1,
                        labels = c("A", "B"), font.label = list(size= 15))
FinalDBH.Fig


##wrapping to create 4 plots 
#gs.dbh + geom_density(alpha= 0.02, mapping = aes(y= ..count..)) +theme_classic() + xlim(c(0,125)) + xlab("DBH (cm)")+ ylab("Density") + facet_grid(cols = vars(GS.Type)) 

####Species Abundances 
#calculate species abundances for each green space type or public/private land for plotting

####Public vs. Private Land 
#First, use metadata to compare public vs. private green space types
#use metadata to create a table of abundances according to land classification
meta.sp.table<- table(metacsv$Species.Code, metacsv$Classification)
#create dataframe with species abundances based on species code and land-use classification (public/private)
meta.sp.table<- as.data.frame(meta.sp.table)
meta.sp.table

#Private
#Create private land classification only file by subsetting only private
all.private.ab<- meta.sp.table[-c(156:310),]
#remove zeros since these species are not present 
all.private.ab<-all.private.ab[apply(all.private.ab!=0, 1, all),]
all.private.ab<-all.private.ab[order(all.private.ab$Freq, decreasing = TRUE),]
#calculate sum of trees to calculate percent abundance
sum(all.private.ab$Freq)

#Calculate the percent abundance of each species based on total private trees
#create a new column with percent abundance value
all.private.ab$per.abund<- (all.private.ab$Freq/1389)*100
all.private.ab
#subet only the top five most abundant species 
top.5.private<- all.private.ab %>% slice(1:5)

#Public
#Create public file by subsetting only public land classification
all.public.ab<- meta.sp.table[-c(1:155),]
#removing zeros so that trees that are not present are not included in calculation 
all.public.ab<- all.public.ab[apply(all.public.ab!=0, 1, all),]
all.public.ab<- all.public.ab[order(all.public.ab$Freq, decreasing = TRUE),]
#finding total number of trees for percent abundance calculation
sum(all.public.ab$Freq)

#calculate percent abundance of each species and create a new column 
all.public.ab$per.abund<- (all.public.ab$Freq/2883)*100
all.public.ab
#subset only the top five most abundant species
top.5.public<- all.public.ab %>% slice(1:5)

####Plotting Figure
##plotting 5 Most Abundant Species in PUBLIC 
##create colour palette for figure and abundance labels
colour.table.publicab<- tibble(
  GS.Type= c("ACPL", "ACSA", "FRPE", "GLTR", "TICO"),
  Colour= c("gray76", "gray76","gray76", "gray76", "gray76"))
abundance.labs<- labs(x= "Species Code", y= "Percent Abundance")

Public5<- ggplot(data = top.5.public,
                     mapping = aes(x= reorder(Var1, -per.abund), y= per.abund, fill= Var1))
Public5.final<- Public5 + geom_col(alpha= 0.7, width= 0.7) + theme_classic() +
                abundance.labs + scale_fill_manual(values = colour.table.publicab$Colour) +
                theme(legend.position = "none", axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
Public5.final

##plotting 5 Most Abundant Species in PRIVATE
#creating a table of colour 
colour.table.privateab<- tibble(
  GS.Type= c("ACNE", "ACPL", "ACSA", "SYVU", "THOC"),
  Colour= c("gray76", "gray76","gray76", "gray76", "gray46"))

Private5<- ggplot(data= top.5.private,
                      mapping = aes(x= reorder(Var1, -per.abund), y= per.abund, fill= Var1))
Private5.final<-Private5 + geom_col(alpha= 0.7, width = 0.7) + theme_classic()+ 
                abundance.labs + scale_fill_manual(values= colour.table.privateab$Colour) + 
                theme(legend.position = "none", axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
Private5.final

##Arrange public and private plots into one figures
Public.Private5<-ggarrange(Public5.final, Private5.final,
                            labels = c("A", "B"), font.label = list(size= 18), hjust = -1, vjust= 1.2)
Public.Private5

#### Genus Abundances
meta.genus<- table(metacsv$Genus)
#create dataframe with species abundances based on species code and land-use classification (public/private)
meta.genus<- as.data.frame(meta.genus)
meta.genus
#find total number trees for percent abundance
sum(meta.genus$Freq)
#sort dataframe based on frequency
meta.genus<- meta.genus[order(meta.genus$Freq, decreasing = TRUE),]

#Calculate the percent abundance of each species based on total private trees
#create a new column with percent abundance value
meta.genus$per.abund<- (meta.genus$Freq/4272)*100
meta.genus
#subet only the top five most abundant species 
top.5.genus<- meta.genus %>% slice(1:5)

#assign axis labels
genus.labs<- labs(x= "Genus", y= "Percent Abundance")

Genus5<- ggplot(data = top.5.genus,
                 mapping = aes(x= reorder(Var1, -per.abund), y= per.abund, fill= Var1))
Genus5.final<- Genus5 + geom_col(alpha= 0.7, width= 0.7) + theme_classic() +
  genus.labs + scale_fill_manual(values = colour.table.publicab$Colour) +
  theme(legend.position = "none", axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
Genus5.final

#### BASAL AREA PER SITE ####
#not used for manuscript
Site.Means.BA <- metacsv %>%
  group_by(Subsite.Code) %>%
  summarise(
    Basal.Area = mean(Basal.Area))
Site.Means.BA

GS.BA.means<- metacsv %>%
  group_by(GS.Type) %>%
  summarise(
    mean = mean(Basal.Area),
    sd= sd(Basal.Area))

write.csv(Site.Means.BA, "/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output/site.means.ba.csv")
basal.area.labs<- labs(x= "Green Space Type", y= "Basal Area (m2)")

base.basal<-ggplot(LMM.Meta, aes(GS.Type, Basal.Area))
basal.final<- base.basal +theme_classic() +geom_jitter(width= 0.3, height= 0.1, alpha= 0.8, colour= "grey") + basal.area.labs + 
  geom_errorbar(data= GS.BA.means, aes(y= mean, ymax= mean+sd, ymin= mean-sd), color= "red", width= 0.05)
basal.final












