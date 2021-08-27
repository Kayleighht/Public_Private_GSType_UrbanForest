#### Load Packages
Packages.1 <- c("readr", "vegan", "plyr", "tidyr", "psych", "benthos", "mobr", "dplyr")
lapply(Packages.1, library, character.only = TRUE)

#### Loading Data and Formatting Species/Site Matrixes 
### Creating csv files for each green space type matrix for future analysis

#### Green Space Type: PRIVATE YARDS
pr.data<- read.csv("Clean_Private_Cu.csv")
#creating a data table that sorts species codes and site code
wide.pr<- table(pr.data$Species.Code, pr.data$Yard.Code)
wide.pr<-as.data.frame(wide.pr)
#spreading data table by species code to create a matrix 
wide.pr<-spread(wide.pr, "Var1", "Freq")
#write as csv so that I can pull it for other scripts/analysis
write.csv(wide.pr, "/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output/yards.sp.wide.csv", row.names = FALSE)

#write another matrix without column of sitecodes for data matrix
#pr.matrix<-row.names(wide.pr)<-wide.pr$Var2
#pr.matrix<- subset(wide.pr, select = -c(Var2))
#creating presence absence matrix
#pa.pr.matrix<- pr.matrix
#pa.pr.matrix[pa.pr.matrix>0]<-1

#### Green Space Type: STREET RIGHT OF WAY (ROW)
row.data<- read.csv("Clean_ROW_Cu.csv")
#creating a data table that sorts species codes and site code
wide.row<- table(row.data$Species.Code, row.data$Site.Code)
wide.row<- as.data.frame(wide.row)
#spreading data table by species code to create a matrix 
wide.row<- spread(wide.row, "Var1", "Freq")
#write as csv so that I can pull it for other scripts/analysis
write.csv(wide.row, "/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output/row.wide.matrix.csv", row.names = FALSE)
#creating a atrix where site codes are rows
#row.matrix<-row.names(wide.row)<- wide.st$Var2
#row.matrix<- subset(wide.row, select = -c(Var2))
#presence absence matrix
#pa.st.matrix<- row.matrix
#pa.st.matrix[pa.st.matrix>0]<-1

#### Green Space Type: PARKS 
##First I will use the parcel-scale data which divides parks into sites at the scale of management (an entire park)
park.data<- read.csv("Clean_Park_Cu.csv")
#creating a data table that sorts species codes and site code
wide.park<- table(park.data$Species.Code, park.data$Site.Code)
wide.park<- as.data.frame(wide.park)
#spreading data table by species code to create a matrix 
wide.park<- spread(wide.park, "Var1", "Freq")
#write as csv so that I can pull it for other scripts/analysis
write.csv(wide.park,"/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output/parks.wide.matrix.csv", row.names = FALSE)

#park.matrix<-row.names(wide.park)<- wide.park$Var2
#park.matrix<- subset(wide.park, select = -c(Var2))
#PRES/ABS
#pa.park.tot.matrix<- park.matrix
#pa.park.tot.matrix[pa.park.tot.matrix>0]<-1

#### Site Scale (subsites)
##Next I used site-scale data which are divided into subsites of equal area (0.04 hectares), each code represents a park site which is a subsampled unit within an individual park
park.ar<- read.csv("Clean_Subsite_Park_Cu.csv")
#creating a data table that sorts species codes and site code
ar.park.w<- table(park.ar$Species.Code, park.ar$layer)
ar.park.w<- as.data.frame(ar.park.w)
#spreading data table by species code to create a matrix 
ar.park.w<- spread(ar.park.w, "Var1", "Freq")
#write as csv so that I can pull it for other scripts/analysis
write.csv(ar.park.w, "/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output/site.park.wide.matrix.csv", row.names = FALSE)

#park.sub.matrix<-row.names(ar.park.w)<- ar.park.w$Var2
#park.sub.matrix<- subset(ar.park.w, select = -c(Var2))
#pa.park.matrix<- park.sub.matrix
#pa.park.matrix[pa.park.matrix>0]<-1

#### Green Space Type: INSTITUTIONS
##First I will use the parcel-scale data which divides parks into sites at the scale of management (an entire place of worship or schoolyard)
in.data<- read.csv("Clean_Inst_Cu.csv")
#creating a data table that sorts species codes and site code
wide.in<- table(in.data$Species.Code., in.data$Site.Code.)
wide.in<- as.data.frame(wide.in)
#spreading data table by species code to create a matrix 
wide.in<- spread(wide.in, "Var1", "Freq")
#write as csv so that I can pull it for other scripts/analysis
write.csv(wide.in, "/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output/in.wide.matrix.csv", row.names = FALSE)

#in.matrix<-row.names(wide.in)<- wide.in$Var2
#in.matrix<- subset(wide.in, select = -c(Var2))
#pa.in.tot.matrix<- in.matrix
#pa.in.tot.matrix[pa.in.tot.matrix>0]<-1

## Site-Scale (subsites)
##Next I used site-scale data which are divided into subsites of equal area (0.04 hectares), each code represents an insitutional site which is a subsampled unit within an individual insitutional (school or place of worship)
in.ar<- read.csv("Clean_Subsite_Inst_Cu.csv")
#creating a data table that sorts species codes and site code
in.ar.w<- table(in.ar$Species.Code., in.ar$layer)
in.ar.w<- as.data.frame(in.ar.w)
#spreading data table by species code to create a matrix 
in.ar.w<- spread(in.ar.w, "Var1", "Freq")
#write as csv so that I can pull it for other scripts/analysis
write.csv(in.ar.w, "/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output/site.in.wide.matrix.csv", row.names = FALSE)

#in.sub.matrix<- row.names(in.ar.w)<- in.ar.w$Var2
#in.sub.matrix<- subset(in.ar.w, select = -c(Var2))
#pa.in.matrix<- in.sub.matrix
#pa.in.matrix[pa.in.matrix>0]<-1

#### Green Space Type: ALL GREEN SPACES METADATA
metacsv<- read.csv("Metadata_GS_CU .csv")
#create a data table that summarizes each entire green space type by species code
total.sp.table<- table(metacsv$Species.Code, metacsv$GS.Type)
list.total.sp.table<- as.data.frame(total.sp.table)
#spreading data table by species code to create a matrix 
total.sp.table<- spread(list.total.sp.table, "Var1", "Freq")
#write as csv so that I can pull it for other scripts/analysis
write.csv(total.sp.table, "/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output/allgs.matrix.csv", row.names = FALSE)

#Creating a data matrix that summarizes ALL site codes by species code for linear mixed models
#create a table that summarizes ALL site codes by species code
site.sp.table<- table(metacsv$Species.Code, metacsv$Subsite.Code)
site.sp.table<- as.data.frame(site.sp.table)
#spread data by site code to create a matrix
site.sp.table<- spread(site.sp.table, "Var1", "Freq")
write.csv(site.sp.table, "/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output/site.sp.table.csv", row.names = FALSE)
#assign site codes as rownames
rownames(site.sp.table)<- site.sp.table$Var2
site.sp.tot.mat<- site.sp.table
#push out as csv for future use (raw manipulation)
#write.csv(site.sp.tot.mat, "/home/kayleighhutttaylor/Ch. 2 Thesis Analysis/sitecomm.csv", row.names = TRUE)
#removed column with site codes since they are now the rownames
site.sp.tot.mat<-subset(site.sp.tot.mat, select = -c(Var2))
write.csv(site.sp.tot.mat, "/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Input/site.sp.tot.mat.csv", row.names = FALSE)

#### Summarizing Site/Parcel Scale Metrics for Linear Mixed Models
## here I am creating summaries of metrics: species evenness (PIE), tree abundance and species richness at the site-scale to plot with linear mixed models 

#### Species Evenness (PIE)
#read in LMM datafile
LMM.Meta<- read.csv("LMM_META_FINAL.csv")
##create a table that summarizes PIE evenness by site code from metadata
pie.site.means <- LMM.Meta %>%
  group_by(Site.Code) %>%
  summarise(
    PIE.site = mean(PIE))
pie.site.means
write.csv(pie.site.means, "/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output/pie.site.means.csv", row.names = FALSE)

#### Tree Abundance
##create a table that summarizes tree abundance by site code from metadata
ab.site.means<- LMM.Meta %>%
  group_by(Site.Code) %>%
  summarise(
    ab.site = mean(Abund))
ab.site.means
write.csv(ab.site.means, "/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output/ab.site.means.csv", row.names = FALSE)

#### Species Richness
##create a table that summarizes tree species richness by site code from metadata
sr.site.means<- LMM.Meta %>%
  group_by(Site.Code) %>%
  summarise(
    sr.site= mean(SR))
sr.site.means
write.csv(sr.site.means, "/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output/sr.site.means.csv", row.names = FALSE)


