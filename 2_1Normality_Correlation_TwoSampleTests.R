# R teaching Workshops.
# Lotte Meteyard, 2017
# School of Psychology and Clinical Language Sciences,University of Reading

########################### some basic statistics ###########################


########################### checking for normality ###########################

#check normality

#use visual checks - often better than using formal tests, as you can see what 
#the deviation from normality looks like
hist(CongTemp$RT)
qqnorm(CongTemp$RT)
qqline(CongTemp$RT)

#plot them in the same window
par(mfrow=c(1,2))
hist(CongTemp$RT)
qqnorm(CongTemp$RT)
qqline(CongTemp$RT)

#return plotting window to single pane
par(mfrow=c(1,1))

#### INDEPENDENT PRACTICE ####
# Complete question 1



################## Correlation ##############
# http://www.statmethods.net/stats/correlations.html


#Generate some summary data so we can correlate participant data




library(dplyr)
Participant.Mydata <- group_by(Mydata,SubNo)
# summary RT by participant, overwriting Participant.Mydata

Participant.Mydata <- summarise(Participant.Mydata,
                      MeanRT = mean(RT),
                      SDRT = sd(RT),
                      MeanTwitch = mean(Twitches),
                      number = length(RT))

#This code is the same as that on lines 81-96 of 1_2Manipulating Data.R
#with minor changes 

head(Participant.Mydata)
dim(Participant.Mydata)

#eyeball data with a scatterplot
plot(Participant.Mydata$MeanRT,Participant.Mydata$MeanTwitch)

# get correlation coefficients
# spearman correlation coefficient for data that is not normally distributed
cor(Participant.Mydata$MeanRT,Participant.Mydata$MeanTwitch, method="spearman", use="pairwise.complete.obs")
# pearson correlation coefficient for data that is normally distributed
cor(Participant.Mydata$MeanRT,Participant.Mydata$MeanTwitch, method="pearson", use="pairwise.complete.obs")
# note the 'use' to specify what to do with missing data

#Lets look at data for one participant, and correlate Trial number, RT and Twitches
#going to use the subset function here
temp <- subset(Mydata, SubNo == 1, select = c(Trial, RT, Twitches))
head(temp)

# Install package to help plot results for multiple correlations
library(corrplot)

# Asking for correlation between columns 1 to 3
# note that I have specified to 'pairwise complete observations'
M <- cor(temp[,1:3],method="spearman",use="pairwise.complete.obs")
M

#repeat those correlations, but plot with labelled axes
#set up names
names(temp)
rownames(M)<-names(temp)[1:3]
colnames(M)<-names(temp)[1:3]
#plot
corrplot(M, method = "number")

# make things a bit more jazzy
corrplot(M, order="AOE", addCoef.col="grey")

# Get significance levels for correlations
library(Hmisc)
# for this function we need to use a numeric matrix not a dataframe
temp<-data.matrix(temp)

#NB: rcorr() uses pairwise deletion for missing values
rcorr(temp[,c(1:3)], type="spearman") 
#gives correlation coefficient, n and p values


#the psych package has a lot of nice functions for dealing with large data sets
#and data sets where you need to check lots of variables - e.g. with correlation
#and to look at relationships across variables
require(psych)
#recreate data with three columns
temp <- subset(Mydata, SubNo == 1, select = c(Trial, RT, Twitches))
#this function plots variables (from different columns), their distributions
#and their correlations
pairs.panels(temp[,1:3])



#### INDEPENDENT PRACTICE  ####
# Complete question 2




################## Repeated measures t-test ##############

#let's tidy up a bit first
rm(x,temp)  #remove objects x and temp from the Environment


#Let's return to our large data set
#here's another way to look at the first few columns and rows
print(tbl_df(Mydata), n = 3)

names(Mydata)

#let's extract data for corret trials only
#two ways to get this data.. filter (dplyr) or subset

library(dplyr)
Mydata.corr<-filter(Mydata, ACC == 1)
#check this has worked
unique(Mydata.corr$ACC)
unique(Mydata$ACC)

#OR

temp <- subset(Mydata, ACC == 1, select = c(Trial, RT, Twitches, ACC))
unique(temp$ACC)


# prepare data for paired test

#check levels / conditions to be compared
levels(Mydata.corr$Congruence)

library(reshape2)
# using reshape to make paired columns
congruence<-dcast(Mydata.corr, SubNo ~ Congruence, value.var = "RTms",
                  fun.aggregate = mean, na.rm = TRUE)
# this averages across other conditions for each subject
head(congruence)

# complete a t test for two levels of congruence
t.test(congruence$Cong,congruence$Incong,paired=TRUE)


#non-parametric paired test
help(wilcox.test)


#### INDEPENDENT PRACTICE  ####
# Complete question 3


##### graphing data

#simple boxplot of data 
#this uses raw data, unaveraged
boxplot(Mydata.corr$RT ~ Mydata.corr$Congruence)
#OR with data averaged across participants
#this is what we analysed
boxplot(congruence$Cong, congruence$Incong)



################## Between subject t-test ##############
# http://www.statmethods.net/stats/ttest.html

str(Mydata)
#Nice way to look at number of data points in each condition
table(Mydata$Task,Mydata$SubNo)
#can see here that Task (CRT/Flanker) is distributed between subjects


#Averaging across subjects first
temp<-dcast(Mydata.corr, SubNo ~ Task, value.var = "RTms",
                  fun.aggregate = mean, na.rm = TRUE)
Task<-melt(temp,id.vars = c("SubNo"))
head(Task)

#tidy up data and rename
Task<-na.omit(Task)
head(Task)

names(Task)[2:3]<-c("Experiment","RTms")
head(Task)
tail(Task)

t.test(Task$RTms~Task$Experiment) 


#### INDEPENDENT PRACTICE  ####
# Complete question 4





################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")







