# R teaching Workshops.
# Lotte Meteyard, 2017
# School of Psychology and Clinical Language Sciences,University of Reading

################## ASSESSMENT #############

#Model answer

#Part 1

#Load in data
dat <- read.table(file="AssessmentData.txt",header=TRUE)

#check number of list orders
unique(dat$List.Order)  #there are six different list orders
length(unique(dat$List.Order))

#Check structure of data
str(dat)  #Subject and MeanRT are integer.
# List.Order, Congruence, ImpPerf and ObjInstr are Factors.

#Check labelling
unique(dat$Congruence)  
unique(dat$ImpPerf)
unique(dat$ObjInstr)  #each one has two levels

#Relabelling
#load package plyr
library(plyr) 
#relabel
dat$ImpPerf<-revalue(dat$ImpPerf, c("Perfect" = "Perfective", "Imperfect" = "Imperfective"))
#check relabelling
unique(dat$ImpPerf)


#detach plyr and activate dplyr
detach("package:plyr", unload=TRUE)
library(dplyr)

#select out data for Subject, Congruence and MeanRT
temp<-select(dat,Subject,Congruence,MeanRT)

#melt and case to get two columns for congruence
#load reshape2
library(reshape2)
#melt data
x<-melt(temp,id.vars=c("Subject","Congruence"),
           variable.name = "RT",
           variable.value = "RT_value"
           )
#check melt has worked
head(x)

#case data
temp <- dcast(x,Subject + RT ~ Congruence,
              fun.aggregate = mean, na.rm = TRUE)
#check cast has worked
head(temp)

#check normality
#set up plotting window
par(mfrow=c(2,2))
#histogram
hist(temp$Congruent)
#qq plot
qqnorm(temp$Congruent)
qqline(temp$Congruent)
#histogram
hist(temp$Incongruent)
#qq plot
qqnorm(temp$Incongruent)
qqline(temp$Incongruent)
#looks OK - data for qq plots falls along the line reasonably well, histograms are alright

#test to compare RTs for Congruent vs Incongruent conditions
t.test(temp$Congruent,temp$Incongruent,paired = TRUE)

#boxplot, with labelled titles and axes
#set up one plotting window
par(mfrow=c(1,1))
#boxplot with titles and axes labelled
boxplot(temp$Congruent,temp$Incongruent, main = "Congruent vs Incongruent RTs", ylab = "Mean RT",
        names = c("Congruent", "Incongruent"), xlab = "Condition")

#descriptive statistics
#use psych and original data to get means, sd etc.
#load psych package
library(psych)
#look at orginal data
head(dat)
#get descriptive statistics for MeanRT by levels/conditions in Congruence
#this code takes descriptive statistics from the raw data, not the subject average data
describeBy(dat$MeanRT,list(dat$Congruence))
#this code would give descriptive stats for the averaged data
mean(temp$Congruent)
mean(temp$Incongruent)
sd(temp$Congruent)
sd(temp$Incongruent)
min(temp$Congruent)
min(temp$Incongruent)
max(temp$Congruent)
max(temp$Incongruent)




