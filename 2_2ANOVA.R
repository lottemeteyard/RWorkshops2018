# R teaching Workshops.
# Lotte Meteyard, 2017
# School of Psychology and Clinical Language Sciences,University of Reading

################## 1 way ANOVA #########

# We will compare RT differences for different TMS coil orientations

names(Mydata.corr)
levels(Mydata.corr$Axes) # 4 levels of this variable

# Let's first check the data meets assumptions for ANOVA

##### Homogeneity of variance tests ####

# Two tests come in the base package

# (1) Bartlett Test
# checks for k samples having equal variances
# sensitive to departues from normality
# so may just show non-normality
#check for each condition level in Axes
bartlett.test(RTms ~ Axes,data=Mydata.corr)
#check by subject
bartlett.test(RTms ~ SubNo,data=Mydata.corr)

# (2) Figner-Killeen Test of Homogeneity of Variances
# Very robust against departures from normality
# non-parametric test
fligner.test(RTms ~ Axes,data=Mydata.corr)

# One familiar from SPSS:
# (3) Levene's test can be found in the package car
# so install car
require(car)
leveneTest(RTms ~ Axes,data=Mydata.corr)



##### Normality ####

# plot normality formally
qqnorm(Mydata.corr$RTms)
qqline(Mydata.corr$RTms)

hist(Mydata.corr$RTms) #can see the long tail / skew that is typical for RT data

# shapiro-wilk test
# example adapted from: 
# www.dummies.com/how-to/content/how-to-test-data-normality-in-a-formal-way-in-r.html
# tests null of whether sample comes from normal distribution

# test whole set of data
shapiro.test(Mydata.corr$RTms)
# data set is too big, so we can apply the analysis to each condition instead

# test each cell (i.e. split by factor levels)
with(Mydata.corr,tapply(RTms,Axes,shapiro.test))
help("tapply")
help("with")




############# Transform data, remove outliers #########

# Log transform usually good to normalise RTs
par(mfrow=c(1,2))
hist(Mydata.corr$RTms)
hist(log(Mydata.corr$RTms))

# quantile plots show less deviation
qqnorm(Mydata.corr$RTms)
qqline(Mydata.corr$RTms)
qqnorm(log(Mydata.corr$RTms))
qqline(log(Mydata.corr$RTms))

par(mfrow=c(1,1)) #put plot window back to single pane




# Still have some outliers at the extreme end

#Look for outliers in DV

#Boxplot by Subject for outliers, store results
mybp <- boxplot(RTms ~ SubNo, data=Mydata.corr)

#identify outliers row number in original data set
x<-which(Mydata.corr$RTms %in% mybp$out, arr.in=TRUE)
head(x)

#mark outliers in new column
Mydata.corr$Out<-rep("FALSE")
#loop to identify rows and put 'TRUE' for each outlier
for (i in 1:length(x))
{Mydata.corr$Out[x[i]]<-"TRUE"}
#make new column a factor/category
Mydata.corr$Out<-as.factor(Mydata.corr$Out)
head(Mydata.corr)

#look at number of outliers per subject
table(Mydata.corr$SubNo,Mydata.corr$Out)
#new data set with outliers removed 
Mydata.corr.a<-filter(Mydata.corr,Out==FALSE)

#Calculate percentage of data lost
1-dim(Mydata.corr.a)/dim(Mydata.corr) #is about 4%


# finally, plot log transform of data with outliers removed
par(mfrow=c(1,2))
hist(log(Mydata.corr.a$RTms))
qqnorm(log(Mydata.corr.a$RTms))
qqline(log(Mydata.corr.a$RTms))
# much better

#save workspace here for backup
#session - save workspace
#or
save.image(file="Workshop_workspace.RData")



#### INDEPENDENT PRACTICE  ####
# Complete questions 1-4





############# One way ANOVA (between & within subjects) #########

# It's best to store output from tests into variables

# ANOVAs in R can be tricky.
# But, it will make you understand ANOVA better.
#http://myowelt.blogspot.co.uk/2008/05/obtaining-same-anova-results-in-r-as-in.html

# Between-subjects ANOVA is straightforward in R, performing repeated measures (within-subjects)
# ANOVA is not so obvious. 
#  e.g https://blog.gribblelab.org/2009/03/09/repeated-measures-anova-using-r/

# ANOVA is GLM / linear models with factorial IVs only, so in R it is coded like a regression
# hence the formuala entry format

#average across subjects so that ANOVA done on means, as is typical

temp<-dcast(Mydata.corr.a, SubNo ~ Axes, value.var = "RTms",
            fun.aggregate = mean, na.rm = TRUE)
head(temp)
data.summary<-melt(temp, id.vars = "SubNo")
head(data.summary)
names(data.summary)[2:3]<-c("Axes","meanRT")
head(data.summary)

#look at the data by condition
with(data.summary, tapply(log(meanRT), Axes, mean))

#We can start with a linear model 
first.lm <-lm(log(meanRT) ~ Axes, data=data.summary)
summary(first.lm)

#note that by default R has used a reference level (E.W. - usually decided by alphabetic order)
# so the intercept is the mean value for E.W. and the other model estimates
# are the difference between other conditions and E.W.

# We can then do an ANOVA on this linear model NB: this is treating the 
# conditions as between subjects (will come back to this below)
anova(first.lm)

# To do ANOVA correctly for repeated measures, we tell R to include a within subjects error term
# for each condition. This reflects that we have conditions nested within subjects
# That is, in principle, we can see the condition effect in every subject

# We will now use the aov function
help(aov)  # read through this help file, note the warnings about unbalanced data

aov.1 <-aov(log(meanRT) ~ Axes + Error(SubNo/Axes), data.summary)
#"SubNo" and "Axes" are our sources of variability. 
#The treatment we are interested in is "Axes" (that's what we want to see the effect of), 
#and this treatment effect is visible within each subject (i.e., nested within each subject). 

#The proper Error term is "SubNo/Axes", which is read as "Axes within subjects" 

summary(aov.1)





############# Post-hoc pairwise tests (within & between subjects) #########

#Within subjects
#pairwise t tests for post-hoc comparisons
pairwise.t.test(log(data.summary$meanRT), data.summary$Axes, p.adjust.method="none", paired=T)
# the output is the p value

help(p.adjust)
pairwise.t.test(log(data.summary$meanRT), data.summary$Axes, p.adjust.method="bonferroni", paired=T)


# The package 'multcomp' has other methods for multiple comparisons
# nice summary here: www.pitt.edu/~njc23/Lecture10.pdf
require(multcomp)



#Between subjects
# TukeyHSD for between subjects (pretending Axes is between subjects here)
aov(first.lm)
glht(aov(first.lm), linfct = mcp(Axes = "Tukey"))

#you can also specify the contrast matrix directly
levels(data.summary$Axes) #note the order of output for labels
contr <- rbind("E.W - N.S" = c(-1, 1, 0, 0),
               "NE.SW - NW.SE" = c(0, 0, 1, -1), 
               "NW.SE - NE.SW" = c(0, 0, -1, 1))
glht(aov(first.lm), linfct = mcp(Axes = contr))

#to get significance, confidence internvals etc.
temp<-glht(aov(first.lm), linfct = mcp(Axes = contr))
summary(temp)
confint(temp)


#### INDEPENDENT PRACTICE  ####
# Complete questions 5-8




