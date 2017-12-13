# R teaching Workshops.
# Lotte Meteyard, 2017
# School of Psychology and Clinical Language Sciences,University of Reading

################## ASSESSMENT #############

#Model answer

#read in data
dat <- read.table(file="AssessmentData.txt",header=TRUE)

#check number of list orders
unique(dat$List.Order)  #there are six different list orders
length(unique(dat$List.Order))

#Check structure of data
str(dat)  #Subject and MeanRT are integer.
# List.Order, Congruence, ImpPerf and ObjInstr are Factors.

#Check labelling
unique(dat$Congruence)  #each one has two levels
unique(dat$ImpPerf)
unique(dat$ObjInstr)

#Relabelling
library(plyr) 
dat$ImpPerf<-revalue(dat$ImpPerf, c("Perfect" = "Perfective", "Imperfect" = "Imperfective"))
unique(dat$ImpPerf)

#Summary data / descriptive statistics
require(psych)
names(dat)  #check names
describeBy(dat$MeanRT,list(dat$Congruence,dat$ImpPerf,dat$ObjInstr))

#interaction plots
#Congruence by Imperfect/Perfect
with(dat, interaction.plot(Congruence, ImpPerf, MeanRT, fun = mean))
#Interaction here.
#Incongruent RTs are high for Perfect/Imperfect. Congruent RTs are lower in Perfect sentences, 
#but similar to Incongruent sentences for Imperfective.

#Congruence by Object/Instrument
with(dat, interaction.plot(Congruence, ObjInstr, MeanRT, fun = mean))
#Main effect with Object taking longer than Instrument
#Main effect of incongruent, with higher RTs for incongruent than congruent
#but note axes only shows very small change

#Imperfect/Perfect by Object/Instrument
with(dat, interaction.plot(ImpPerf, ObjInstr, MeanRT, fun = mean))
#Object higher RT than instrument
#This difference is bigger for Perfective sentences, smaller for Imperfective


#Within subjects ANOVA 2x2x2
require(ez)
names(dat) # check names
dat.anova <- ezANOVA(data=dat, dv=MeanRT, wid = Subject, within = .(Congruence,ImpPerf,ObjInstr),
                     type = 3)
dat.anova
#Significant main effect of Congruence
#Significant main effect of Object / Instrument
##Significant interaction between Sentence (perfect/imperfect) and Object/Instrument

#Follow up comparisons
#need to re-do ANOVA as can't use output from EZ anova
require(lsmeans)
require(afex)

dat.anova <- aov_car(MeanRT ~ (Congruence*ImpPerf*ObjInstr) + Error(Subject/(Congruence*ImpPerf*ObjInstr)), 
                     data=dat, return = "aov")
(ref1 <- lsmeans(dat.anova, c("Congruence", "ImpPerf", "ObjInstr")))
#2 x 2 x 2 ANOVA, have 8 unique levels

c_list <- list(
  "Cong.Imp.Instr - Incong.Imp.Instr" = c(1, -1, 0, 0, 0, 0, 0, 0),
  "Imp.Object - Imp.Instr" = c(-0.5, -0.5, 0, 0, 0.5, 0.5, 0, 0),
  "Perf.Obj - Perf.Instr" = c(0, 0, -0.5, -0.5, 0, 0, 0.5, 0.5))
   
summary(contrast(ref1, c_list), adjust = "bonferroni")

#no significant difference for first comparison
#for ImpObject vs ImpInstr, just sig difference
#for Perf Obj vs Perf Instr, highly sig difference





