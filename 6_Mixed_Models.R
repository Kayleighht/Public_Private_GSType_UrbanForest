#### LINEAR MIXED MODELS #### 

#load packages
Packages <- c("broom.mixed", "ggplot2", "lme4", "ggpubr", "lmerTest", "sjPlot", "car", "effects", "multcomp", "dplyr", "glmmTMB")
lapply(Packages, library, character.only = TRUE)

#read in metadata file
LMM.Meta<- read.csv(file.choose())
#convert species richness values to numeric (rather than factor) for models
LMM.Meta$SR<-as.numeric(LMM.Meta$SR)
class(LMM.Meta$SR)

#### GREEN SPACE TYPE AND SPECIES RICHNESS #### 

### runnning a linear mixed model to test the effect of green space type on species richness, including logged sample area as a covariate 
model.gs.sr<- glmer(SR ~ GS.Type + log(Area) + (1|Site.Code), data= LMM.Meta, family = "negative.binomial"(theta = 1))
print(model.gs.sr)
summary(model.gs.sr)

#model diagnostics
plot(model.gs.sr, which = 1)
shapiro.test(resid(model.gs.sr))
hist(residuals(model.gs.sr))
qqnorm(residuals(model.gs.sr))
qqline(residuals(model.gs.sr))

#check effect of green space type on species richness using model outputs
Anova(model.gs.sr)
#tukey test to check pairwise comparisons of species richness of each green space type
tukey.test<-Pairwise.sr<-glht(model.gs.sr, mcp(GS.Type = "Tukey")) 
#summarizing pairwise differences across green space types
summary(Pairwise.sr) 

#### GREEN SPACE TYPE AND TREE ABUNDANCE ####
### runnning a linear mixed model to test the effect of green space type on tree abundance, including logged sample area as a covariate 
model.gs.log<- lmer(log(Abund) ~ GS.Type + log(Area) + (1|Site.Code), data = LMM.Meta)
plot(model.gs.log)

#model diagnostics
shapiro.test(resid(model.gs.log))
qqnorm(residuals(model.gs.log))
qqline(residuals(model.gs.log))
hist(residuals(model.gs.log))

##summary
summary(model.gs.log)
##Anova to test the effect of green space type on tree abundance
Anova(model.gs.log)

#pairwise comparisons
Pairwise.ab<-glht(model.gs.log, mcp(GS.Type = "Tukey")) 
summary(Pairwise.ab)

#### GS TYPE AND EVENNESS (PIE) ####
### runnning a linear mixed model to test the effect of green space type on species evenness (PIE), including logged sample area as a covariate 
model.gs.ev <- lmer(PIE ~ GS.Type + log(Area) + (1|Site.Code), data= LMM.Meta)
summary(model.gs.ev)

#Model Diagnostics
qqnorm(residuals(model.gs.ev))
qqline(residuals(model.gs.ev))
hist(residuals(model.gs.ev))
plot(residuals(model.gs.ev))
shapiro.test(resid(model.gs.ev))

##Anova to test the effect of green space type on species evenness
Anova(model.gs.ev)

#pairwise comparison
Pairwise.ev<-glht(model.gs.ev, mcp(GS.Type = "Tukey")) 
summary(Pairwise.ev) 

#### PLOTTING MODEL ESTIMATES AND ERRORS IN ONE FIGURE ####

#read in parcel-scale means csv files (tree abundance, species richness and PIE)
#each of these files contain calculated mean values and standard error for parcel-scale (the scale of management/an individual park or place of worship)
ab.means<- read.csv("ab.site.means.csv")
pie.means<- read.csv("pie.site.means.csv")
sr.means<- read.csv("sr.site.means.csv")

##read in files with model estimates from model run above for plotting
est.ab<-read.csv("AB.ModelEst.csv")
est.sr<- read.csv("SR.ModelEst.csv")
est.pie<- read.csv("PIE.ModelEst.csv")

#assigning a jitter value for all following plots
jitter <- position_jitter(width = 0.2, height = 0.01)

#### TREE ABUNDANCE PLOT ####

#assigning axis labels for plot
ab.labs<- labs(x= "", y= "Tree Abundance")
#logging raw abundance values and adding as a new column since model used logged transformed data
LMM.Meta$loggedabund<- log(LMM.Meta$Abund)
LMM.Meta

#adding column of logged abundance means
ab.means$logged.ab.site<-log(ab.means$ab.site)
##subsetting only parks and institutional values to show parcel scale values (one park, one school etc.)
parcel.ab.means<- ab.means[-c(24:221),]
##adding a column that shows green space type (either inst or park)
parcel.ab.means$GS.Type<- c("Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Park","Park","Park","Park","Park","Park","Park")

##PLOT
model.ab.plot<-ggplot(LMM.Meta, aes(GS.Type, loggedabund)) 
final.ab<-model.ab.plot + theme_minimal() + geom_point(data= parcel.ab.means, aes(x= GS.Type, y= logged.ab.site), colour= "black", position= jitter, size= 3) + geom_jitter(width= 0.4, height = 0.4, alpha= 0.5, colour= "grey", size= 3) +
  ab.labs + geom_errorbar(data = est.ab, aes(x= GS.Type, y= Estimated.Mean, ymin= Estimated.Mean-SE, ymax= Estimated.Mean+SE), color= "red", width= 0.08, size= 1.5) + 
  theme(axis.text.x = element_blank(), axis.title.y = element_text(face = "bold", size = 16))
final.ab

#### SPECIES RICHNESS PLOT ####

#assigning axis labels
sr.labs<- labs(x= "", y= "Richness")

#subsetting parcel scale species richness means for parks and institutions
sr.parcel.means<- sr.means[-c(24:221),]
#adding GS type to parcel scale means for plotting
sr.parcel.means$GS.Type<- c("Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Park","Park","Park","Park","Park","Park","Park")

#plotting
model.sr.plot <- ggplot(LMM.Meta, aes(GS.Type, SR))
final.sr<-model.sr.plot + theme_minimal() + geom_point(data= sr.parcel.means, aes(x= GS.Type, y= sr.site), colour= "black", position = jitter, size= 3) + geom_jitter(width= 0.4, height = 0.4, alpha= 0.5, colour= "grey", size= 3) + sr.labs +
  geom_errorbar(data = est.sr, aes(x= GS.Type, y= Estimated.Mean, ymin= Estimated.Mean - SE, ymax= Estimated.Mean + SE), size= 1.5, color= "red", width= 0.08) + theme(
    axis.text.x = element_blank(), axis.title.y = element_text(face = "bold", size = 16))
final.sr

#### SPECIES EVENNESS (PIE) PLOT ####

#assigning axis labels
pie.labs<- labs(x= "", y= "Evenness")

#subsetting parcel scale species evenness (PIE) means for parks and institutions
pie.parcel.means<- pie.site.means[-c(24:221),]
#adding GS type to parcel scale means for plotting
pie.parcel.means$GS.Type<- c("Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Inst","Park","Park","Park","Park","Park","Park","Park")

#plotting
model.pie.plot<-ggplot(LMM.Meta, aes(GS.Type,PIE)) 
final.ev<-model.pie.plot + geom_errorbar(data = est.pie, mapping = aes(x = GS.Type, y = Estimated.Mean, ymin = Estimated.Mean - SE, ymax = Estimated.Mean + SE), size=1.5, color="red", width=.08) + 
  theme_minimal() + geom_point(data= pie.parcel.means, mapping = aes(x= GS.Type, y= PIE.site), colour= "black", position= jitter, size= 3) + geom_jitter(width= 0.2, height = 0.01, alpha= 0.5, colour= "grey", size= 3) + pie.labs +
  theme(axis.text.x = element_text(size= 16, colour= "black", face= "bold"), axis.title.y = element_text(face = "bold", size = 16))
final.ev

###WRAPPING INTO ONE FIGURE
model.figure<- ggarrange(final.ab, final.sr, final.ev,
                         nrow= 3, ncol= 1,
                         labels = c("A", "B", "C"), font.label =  list(size =9), hjust = -1, vjust = 1.5)
model.figure
