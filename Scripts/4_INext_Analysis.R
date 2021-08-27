### Load Packages
Packages.4 <- c("dplyr", "iNEXT", "ggplot2", "ggthemes", "grid")
lapply(Packages.4, library, character.only = TRUE)
setwd("/home/kayleighhutttaylor/Ch. 1 Thesis Analysis/Output")

#### INEXT RAREFACTION CURVES ####
#read in data matrixes
park.matrix<- read.csv("site.park.wide.matrix.csv")
inst.matrix<- read.csv("site.in.wide.matrix.csv")
row.matrix<- read.csv("row.wide.matrix.csv")
private.matrix<- read.csv("yards.sp.wide.csv")

#creating presence/absence matrixes based on green space type for INEXT formatting/conversion
row.names(park.matrix)<-park.matrix$Var2
park.matrix<- park.matrix[ ,-1]
park.matrix[park.matrix > 0] <- 1
#transpose matrix so that it is formatted for INEXT freq conversion
park.matrix<-as.data.frame(t(park.matrix))
park.freq<-as.incfreq(park.matrix)

#do the same for each green space type
#insitutions
row.names(inst.matrix)<- inst.matrix$Var2
inst.matrix<- inst.matrix[ ,-1]
inst.matrix[inst.matrix > 0] <- 1
inst.matrix<- as.data.frame(t(inst.matrix))
inst.freq<- as.incfreq(inst.matrix)

#street row
row.names(row.matrix)<- row.matrix$Var2
row.matrix<- row.matrix[ ,-1]
row.matrix[row.matrix > 0] <- 1
row.matrix<- as.data.frame(t(row.matrix))
row.freq<- as.incfreq(row.matrix)

#private
row.names(private.matrix)<- private.matrix$Var2
private.matrix<- private.matrix[ ,-1]
private.matrix[private.matrix > 0] <- 1
private.matrix<- as.data.frame(t(private.matrix))
private.freq<- as.incfreq(private.matrix)

#labelling each green space type for input
metadata<-list(private.freq, row.freq, inst.freq, park.freq)
names(metadata)<-c("Private", "Street", "Institutional", "Park")

#### PLOTTING ####
#adding theme for figures
mytheme.in<-theme(axis.text = element_text(face = "bold", size = 10), axis.title = element_text(size= 20, face = "bold"), plot.title = element_text(face= "bold", hjust= 1, size = 15))
inext.lab<- labs(x= "Sampling Units", y= "Species Richness")

out<-iNEXT(metadata, datatype = "incidence_freq", q=0)
rarefaction1<-ggiNEXT(out, type = 1)+ xlim(c(0,400)) + inext.lab  + theme_classic(base_size = 15) + scale_fill_grey(start = 0.1, end = 1) +
  scale_colour_grey(start = 0.2, end = 0.2) + theme(text= element_text(family= "Arial"),
    legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 35), legend.box = "vertical", axis.title.x = element_text(size= 45, face= "bold", family = "Arial"), 
    axis.title.y = element_text(size= 45, face = "bold", family = "Arial"), axis.text = element_text(size= 35))
#changing the size of the symbols
rarefaction2<- ggplot_build(rarefaction1)
rarefaction2$data[[1]]$size<- 10
rarefaction3<- ggplot_gtable(rarefaction2)
grid.draw(rarefaction3)
