#### Load Packages
Packages.2 <- c("tidyr", "dplyr")
lapply(Packages.2, library, character.only = TRUE)

#For missing Maximum DBH values I followed the same procedure as the original public tree dataset
#Any tree species with at least 10 trees in my dataset we compiled and the 95 percentile was calculated based on measured DBH values to create a maximum value
#Below, I calculate maximum DBH values for the necessary tree species

#read in and format datafile
metacsv<- read.csv("Input/Metadata_GS_CU .csv")
site.sp.table<- table(metacsv$Species.Code, metacsv$Subsite.Code)
site.sp.table<- as.data.frame(site.sp.table)
site.sp.table

##formatting data for calculations
#here I am using an OUTPUT file that I created in script one. This file is sourced in the folder "output" rather than "input"
total.sp.table<-read.csv("Output/allgs.matrix.csv")
sp.tot.ab<- as.data.frame(t(total.sp.table))
colnames(sp.tot.ab)<- c("Inst", "Park", "Private", "Row")
sp.tot.ab<-sp.tot.ab[-1,]
#convert columns to numerics rather than factor
i <- c(1:4)      
sp.tot.ab[ , i] <- apply(sp.tot.ab[ , i], 2,        
                         function(x) as.numeric(as.character(x)))
sapply(sp.tot.ab, class)
#summing across rows
sp.tot.ab<-sp.tot.ab %>% mutate(sumrow= Inst + Park + Private + Row)

#subset for taxus canadensis
taxus<-filter(metacsv, Species.Code == "TACA")
taxus
#calculating the 95 percentile based on DBH data on species 
quantile(taxus$DBH..2020.., 0.95)

#subset acer palmatum 
palmatum<- filter(metacsv, Species.Code == "ACPA")
quantile(palmatum$DBH..2020.., 0.95)

#subset caragana aborescens 
caragana<- filter(metacsv, Species.Code == "CAAB")
quantile(caragana$DBH..2020.., 0.95)

#subset smoketree
smoketree<- filter(metacsv, Species.Code == "CTCO")
quantile(smoketree$DBH..2020.., 0.95)

#subset euonymous alatus
euonal<- filter(metacsv, Species.Code == "EUAL")
quantile(euonal$DBH..2020.., 0.95)

#subset lonicera xylosteum
loxy<- filter(metacsv, Species.Code == "LOXY")
quantile(loxy$DBH..2020.., 0.95)

#subset viburnum letago
nannyberry<- filter(metacsv, Species.Code == "VILE")
quantile(nannyberry$DBH..2020.., 0.95)
