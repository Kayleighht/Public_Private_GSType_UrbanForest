#### Load Packages
Packages.3 <- c("dplyr", "plyr", "tidyr", "psych", "mobr", "benthos")
lapply(Packages.3, library, character.only = TRUE)

### In this script I calculate three metrics based on urban tree data: species richness, tree abundance and species evenness (PIE)
## I do this for metadata and each green space type (Institutional, Park, Private Yard, Street ROW)

#### 1. Species Richness
total.sp.table<- read.csv("Output/allgs.matrix.csv")

####Calculate species richness of all site codes in metadata file
#calculate species richness per green space type (totals)
#need output files created in script 1
pr.total<-ddply(total.sp.table, ~Var2, function(x) {
  data.frame(RICHNESS= sum((x[-1]>0)))
})
pr.total
#summarize results of the richness values (min/max, mean and sd)
sum(pr.total$RICHNESS)

#calculate species richness for each site using metadata 
site.sp.table<- read.csv("Output/site.sp.table.csv")
per.site.total<-ddply(site.sp.table, ~Var2, function(x) {
  data.frame(RICHNESS= sum((x[-1]>0)))
})
per.site.total
##push out to csv
write.csv(per.site.total, "Output/sr.sitetotals.csv")

#### Calculating Species Richness for each green space type and summarizing (max/min, mean, sd)
wide.pr<-read.csv("Output/yards.sp.wide.csv")

####Private Yards
pr.r<-ddply(wide.pr,~Var2,function(x) {
  data.frame(RICHNESS=sum(x[-1]>0))
})
pr.r
#summarize results of the richness values (min/max, mean and sd)
describe(pr.r$RICHNESS)

####Street ROW 
wide.row<- read.csv("Output/row.wide.matrix.csv")
row.r<-ddply(wide.row,~Var2,function(x) {
  data.frame(RICHNESS=sum(x[-1]>0))
})
#summarize results of the richness values (min/max, mean and sd)
describe(row.r$RICHNESS)

####Parks
##Park parcel-scale
wide.park<- read.csv("Output/parks.wide.matrix.csv")
park.r<-ddply(wide.park,~Var2,function(x) {
  data.frame(RICHNESS=sum(x[-1]>0))
})
#summarize results of the richness values (min/max, mean and sd)
describe(park.r$RICHNESS)

#Park site-scale
ar.park.w<- read.csv("Output/site.park.wide.matrix.csv")
park.s.r<-ddply(ar.park.w,~Var2,function(x) {
  data.frame(RICHNESS=sum(x[-1]>0))
})
#summarize results of the richness values (min/max, mean and sd)
describe(park.s.r$RICHNESS)

####Institutional
##Insitutional parcel-scale
wide.in<- read.csv("Output/in.wide.matrix.csv")
in.r<-ddply(wide.in,~Var2,function(x) {
  data.frame(RICHNESS=sum(x[-1]>0))
})
in.r
#summarize results of the richness values (min/max, mean and sd)
describe(in.r$RICHNESS)

#Institutional site-scale
in.ar.w<- read.csv("Output/site.in.wide.matrix.csv")
in.s.r<-ddply(in.ar.w,~Var2,function(x) {
  data.frame(RICHNESS=sum(x[-1]>0))
})
#summarize results of the richness values (min/max, mean and sd)
describe(in.s.r$RICHNESS)

#### 2.Tree Abundances

####Calculate tree abundance of all site codes in metadata file
#calculate tree abundance per green space type (totals)
total.sp.table<- read.csv("Output/allgs.matrix.csv")
abund.total<-ddply(total.sp.table, ~Var2, function(x) {
  data.frame(ABUNDANCE=sum(x[-1]))  
})
abund.total

#calculate tree abundance for each site using metadata 
site.sp.total<- read.csv("Output/site.sp.table.csv")
per.site.total.ab<-ddply(site.sp.table, ~Var2, function(x) {
  data.frame(ABUNDANCE= sum(x[-1]))
})
per.site.total.ab<-as.data.frame(per.site.total.ab)
#create csv file
write.csv(per.site.total.ab, "Output/sr.sitetotals.ab.csv")

####Park
##Park parcel-scale
abund.park<-ddply(wide.park,~Var2,function(x) {
  data.frame(ABUNDANCE=sum(x[-1]))
})
#summarize results of the richness values (min/max, mean and sd)
describe(abund.park$ABUNDANCE)

#Park site-scale
abund.park.ar<-ddply(ar.park.w,~Var2,function(x) {
  data.frame(ABUNDANCE=sum(x[-1]))
})
#summarize results of the richness values (min/max, mean and sd)
describe(abund.park.ar$ABUNDANCE)

####Institutional
##Institutional parcel-scale
abund.in<-ddply(wide.in,~Var2,function(x) {
  data.frame(ABUNDANCE=sum(x[-1]))
})
abund.in
#summarize results of the richness values (min/max, mean and sd)
describe(abund.in$ABUNDANCE)
sum(abund.in$ABUNDANCE)

##Institutional site-scale
abund.in.ar<-ddply(in.ar.w,~Var2,function(x) {
  data.frame(ABUNDANCE=sum(x[-1]))
})
abund.in.ar
#summarize results of the richness values (min/max, mean and sd)
describe(abund.in.ar$ABUNDANCE)
sum(abund.in.ar$ABUNDANCE)

####Street ROW
abund.row<-ddply(wide.row,~Var2,function(x) {
  data.frame(ABUNDANCE=sum(x[-1]))
})
#summarize results of the richness values (min/max, mean and sd)
describe(abund.row$ABUNDANCE)
sum(abund.row$ABUNDANCE)

####Private Yards
abund.pr<-ddply(wide.pr,~Var2,function(x) {
  data.frame(ABUNDANCE=sum(x[-1]))
})
#summarize results of the richness values (min/max, mean and sd)
describe(abund.pr$ABUNDANCE)
sum(abund.pr$ABUNDANCE)

#### 3. Species Evenness (Hurlberts PIE)

####Institution
##Institutional site-scale

#load and format datafile
in.site.data<- read.csv("Output/site.in.wide.matrix.csv")
#replace site column as rownames
rownames(in.site.data)<- in.site.data$Var2
in.site.data<- subset(in.site.data, select = -c(Var2))

#calculate PIE evennness using calc_PIE
pie.site.in<-as.data.frame(calc_PIE(in.site.data))
##for any sites that only had one species in the matrix the function will input an NA
#replace NAs with 1.0 (because they have one species and are completely even)
pie.site.s.in<-pie.site.in$'calc_PIE(in.site.data)' %>% replace_na(1.000000)
pie.site.s.in<- as.data.frame(pie.site.s.in)
#summarize pie values (min/max, mean, sd)
describe(pie.site.s.in)

#calculate the mean value across all institutional sites
mean(pie.site.s.in$pie.site.s.in)
##Institutional parcel-scale
#load and format datafile
in.data<- read.csv("Output/in.wide.matrix.csv")
#replace parcel column as rownames
rownames(in.data)<- in.data$Var2
in.data<- subset(in.data, select = -c(Var2))
#calculate PIE evenness at parcel scale using calc-PIE
pie.in<- as.data.frame(calc_PIE(in.data))

####Park
##Parks site-scale
#load and format datafile
park.site.data<- read.csv("Output/site.park.wide.matrix.csv")
#replace site column as rownames
rownames(park.site.data)<- park.site.data$Var2
park.site.data<- subset(park.site.data, select = -c(Var2))

#calculate PIE evennness using calc_PIE
pie.site.park<-as.data.frame(calc_PIE(park.site.data))
##for any sites that only had one species in the matrix the function will input an NA
#replace NAs with 1.0 (because they have one species and are completely even)
pie.site.park<-pie.site.park$'calc_PIE(park.site.data)' %>% replace_na(1.000000)
pie.site.park<- as.data.frame(pie.site.park)
#summarize pie values (min/max, mean, sd)
describe(pie.site.park)

#### Private Yards 
#load and format datafile
private.data.raw<- read.csv("Output/yards.sp.wide.csv")
#replace site column as rownames
rownames(private.data.raw)<- private.data.raw$Var2
private.data<- subset(private.data.raw, select = -c(Var2))

pie.site.pr<- as.data.frame(calc_PIE(private.data))
#replace NAs with 1.0 (because they have one species and are completely even)
pie.site.pr<-pie.site.pr$'calc_PIE(private.data)' %>% replace_na(1.000000)
pie.site.pr<-as.data.frame(pie.site.pr)
describe(pie.site.pr)

#### Street ROW
#load and format datafile
row.data<- read.csv("Output/row.wide.matrix.csv")
#replace site column as rownames
rownames(row.data)<- row.data$Var2
row.data<- subset(row.data, select = -c(Var2))

pie.site.row<- as.data.frame(calc_PIE(row.data))
#replace NAs with 1.0 (because they have one species and are completely even)
pie.site.row<-pie.site.row$'calc_PIE(row.data)' %>% replace_na(1.000000)
pie.site.row<-as.data.frame(pie.site.row)
describe(pie.site.row)
sd(pie.site.row$pie.site.row)

#### creating a summary for all sites from metadata for linear mixed models
#load and format datafile
site.sp.tot.mat<- read.csv("Output/site.sp.tot.mat.csv")

pie.tot.sites<- as.data.frame(calc_PIE(site.sp.tot.mat))
pie.tot.sites.nona<-pie.tot.sites$'calc_PIE(site.sp.tot.mat)' %>% replace_na(1.000000)
pie.tot.sites<- cbind(pie.tot.sites, pie.tot.sites.nona)
#create a new column with NA replaced with 1.0
pie.tot.sites$site_code<-site.sp.table$Var2
pie.tot.sites
##push out to csv
write.csv(pie.tot.sites, "Output/pie.site.totals.csv")

#### Calculating PIE Species Evenness at the scale of the neighbourhood
## Each calculation uses the species pool of the entire green space type (e.g. all species in all parks)
#load metadata
total.sp.table<-read.csv("Output/allgs.matrix.csv")

####Private Yards
#remove other green space types from matrix
pie.mat.pr<- total.sp.table[-c(1,2,4), -1]
pie.mat.pr
#calculate evenness using calc_PIE
calc_PIE(pie.mat.pr)

#shows the same value using the function (quality check)
#load private data file
#pr.data<-read.csv("Clean_Private_Cu.csv")
#create table with species abundances for the entire dataset
#even.matrix.pr<-table(pr.data$Species.Code)
#even.matrix.pr<- as.data.frame(even.matrix.pr)
#calculate pie evenness using hpie function according to species abundances 
#hpie(even.matrix.pr, taxon = Var1, count = Freq)

####Parks
#remove other green space types from matrix
park.n.mat<- total.sp.table[-c(1,3,4),-1]
park.n.mat
#calculate evenness using calc_PIE
calc_PIE(park.n.mat)

####Institutions
#remove other green space types from matrix
inst.n.matrix<- total.sp.table[-c(2,3,4), -1]
#calculate evenness using calc_PIE
calc_PIE(inst.n.matrix)

####Street ROW
#remove other green space types from matrix
row.n.matrix<- total.sp.table[-c(1,2,3), -1]
#calculate evenness using calc_PIE
calc_PIE(row.n.matrix)

#### Calculating PIE Species Evenness at the scale of the parcel (scale of management)

#load metadata file with parcel-scale data
metacsv<-read.csv("Input/Metadata_GS_CU .csv")
#created a table to create a matrix 
parcel.sp.table<- table(metacsv$Site.Code, metacsv$Species.Code)
parcel.sp.table<- as.data.frame(parcel.sp.table)
parcel.sp.table<- spread(parcel.sp.table, "Var2", "Freq")
parcel.sp.table

####Institutions
#remove other green space types
parcel.inst.matrix<- parcel.sp.table[-c(17:221),-1]
inst.parcel.pie<-calc_PIE(parcel.inst.matrix)
#calculate mean across each institution for reporting
mean(inst.parcel.pie)
sd(inst.parcel.pie)

####Parks
#remove other green space types
parcel.park.matrix<- parcel.sp.table[-c(1:16,24:221), -1]
park.parcel.pie<- calc_PIE(parcel.park.matrix)
#calculate mean across each institution for reporting
mean(park.parcel.pie)
sd(park.parcel.pie)

