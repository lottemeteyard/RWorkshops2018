# R teaching Workshops.
# Lotte Meteyard, 2016, University of Reading

# Regression & modelling

# Reference reading:
# www.zoology.ubc.ca/~schluter/R/fit-model/
# personality-project.org/r/r.lm.html
# www.pitt.edu/~njc23/Lecture10.pdf
# cran.r-project.org/doc/contrib/Faraway-PRA.pdf (ANOVA & Regression ind detail)

# The following books assume a fair level of statistical knowledge already:
# Crawley, M.J. (2005) Statistics: An Introduction using R. Wiley.
# Baayen, R.H. (2008) Analyzing Linguistic Data: A Practical Introduction to Statistics using R. Cambridge UP.

################ Regression & Modelling in R ########################

# Regression is where you'll see more explicit data modelling
# you saw this already with ANOVA, and it continues with LMM

# Model fitting = finding the 'least worst model'
# and optimising the fit of the model to the data
# you want the model that makes the data most likely

# NB: I'm not really covering how to fix problems with fitted models, just how to check for them
# See this webpage for a nice summary of assumptions and possible fixes
# people.duke.edu/~rnau/testing.htm
# note that the same assumptions apply for LMMs

##########  Better coding practice ########################
# so far we have been loading packages as we need them
# much better to have them all at the top of your script...
library(car)
library(effects)



##########  Model formulae in R ########################

#check structure with our data with outliers removed
str(Mydata.corr.a)

# read through help to get an idea of options 
# and what is returned
help(lm)

# example with DV ~ IV
# predict RT by strength of muscle twitches
lm.1<-lm(RTms ~ Twitches, data=Mydata.corr.a)  
#note here that we have just put all data points through
#as though they are all independent (they are not)
summary(lm.1)

# see what else is in the output
names(lm.1)
# get fitted values
lm.1$fit
fitted.values(lm.1)
# get residuals (these are the differences between actual and fitted values)
head(lm.1$res)
head(residuals(lm.1))
# get confidence intervals for parameters (IVs) fitted in model
confint(lm.1)

#look at how R has built the regression model
#we saw this with ANOVA in the model matrices / contrast matrices
str(model.matrix(lm.1))



##########  Model checks ########################
# see also: www.pitt.edu/~njc23/Lecture10.pdf

# produce diagnostic plots for model
plot(lm.1)
plot.lm(lm.1)
# Histograms of residuals from model (should be normal / evenly dispersed)
hist(resid(lm.1))

# tabulate influential/problematic data points
influence.measures(lm.1)
# this places an asterisk next to each influential data point

# a way of selecting out the important bits / outliers / influential data points
inf<-influence.measures(lm.1)
names(inf)  # let's look at what this function gives you
inf$is.inf
head(inf$is.inf)
# select those that are influential
which(apply(inf$is.inf, 1, any))     
# compare to graphs / influence.measures full table



# We know that RTms is skewed, so let's model with log RT
# predict log RT by strength of muscle twitches
lm.1log<-lm(log(RTms) ~ Twitches, data=Mydata.corr.a)
summary(lm.1log)
# note the esimates are now in log units
plot(lm.1log)
# fewer influential points on residual plots

# overwrite initial model and remove other one from workspace to tidy up
lm.1<-lm(log(RTms) ~ Twitches, data=Mydata.corr.a)
rm(lm.1log)



##########  Building more complex models ########################
# Lets work with this data and build a more complex model

#### INDEPENDENT PRACTICE ####
# Complete questions 1-5


# From the questions above we now have a set of nested models
# that is, models that are related and increase (or decrease in complexity)
# we can now compare these models to see which is a better fit to the data

#Is a model with Twitches a better fit that one with Task and Congruence?
anova(lm.2,lm.3)
#this one throws up an error - we know that we some problems with the Twitches
#data as not all participants have data for this variable
#this means that when we include it in the model, we loose data
summary(lm.2)
summary(lm.3)  #look at the DF (degress of freedom) to see this


#### INDEPENDENT PRACTICE ####
# Complete questions 6


####Now let's compare our models to see which fits the data best

anova(lm.2, lm.3) #compare Task + Congruence to Twitchs + Task + Congruence
#i.e. does including Twitches make a better model of the data?

anova(lm.3,lm.4) #does the model imprve if we add an interaction between Task & Congruence?

anova(lm.4, lm.5) #does the model change if we remove main effects for Task & Congruence?


#### INDEPENDENT PRACTICE ####
# Complete questions 7




########## Multicollinearity and auto-correlation ########################

# Multi-collinearity
# When predictor variables are correlated, the estimated regression coefficient 
# of any one variable depends on which other predictor variables are 
# included in the model. That is, you can't get a true partial effect.
# see onlinecourses.science.psu.edu/stat501/node/82

# Taken from www.statmethods.net/stats/rdiagnostics.html
# From the car package
vif(lm.4) # variance inflation factors 
sqrt(vif(lm.4)) > 2  # problem?


# Temporal correlation in errors
# Autocorrelation in the residuals distorts the regression statistics, 
# (e.g. if positive, will inflate F statistics by fitting small SEs).
# It suggests the model is missing a useful predictor variable 
# or it needs a time series component.
durbinWatsonTest(lm.4)

#Looks like we have auto-correlation in the residuals
#Not that surprising for RT data
# We can add in a predictor for trial
names(Mydata.corr.a)
lm.5<-lm(log(RTms) ~ Trial + Twitches + (Task*Congruence), data=Mydata.corr.a)
anova(lm.4,lm.5)  #improves model fit

summary(lm.5)
durbinWatsonTest(lm.5)
# Has reduced a bit but not much
# remember that we have not accounted for the relationship between 
# responses from each participant - we would therefore expect correlations in the data set
# LMMs allow you to control for this whilst keeping the power of a regression analysis



############### Effects package #########################

# Now we'll see an example of how R becomes a real pleasure

# Using the effects package
# NB: nice intro to this package quantoid.net/IntroR/Handout6_2012.pdf
# some examples below taken from this document

eff.5<-allEffects(lm.5)

# make figure window bigger so you can see everything..
plot(eff.5)

#look at the components
names(eff.5)
#rename to make it easier to access
names(eff.5)[3]<-c("TaskXCongruence")
plot(eff.5$TaskXCongruence)
# Look at what else you get with this package output
names(eff.5$TaskXCongruence)

# we can look at Twitches and check we have the right scale
unique(Mydata.corr.a$Twitches)
# select Twitches effect, choose some sensible no. bins
eff<-effect("Twitches",lm.5,default.levels=10)


#modify the presentation
plot(eff.5$Twitches, xlab = "Twitch Rating", ylab = "log RT (model fit)", main = "Partial Effect for Twitch Rating", 
     lty = "dashed")
plot(eff.5$Trial, xlab = "Experimental Trial", ylab = "log RT (model fit)", main = "Partial Effect for Trial", 
     lty = "dashed")

###put the graphs on the same scale, and remove data markers on horizontal axis
plot(eff.5$Twitches, xlab = "Twitch Rating", ylab = "log RT (model fit)", main = "Partial Effect for Twitch Rating", 
     ylim = c(6.0,6.4),lty = "dashed",rug=FALSE)
plot(eff.5$Trial, xlab = "Experimental Trial", ylab = "log RT (model fit)", main = "Partial Effect for Trial", 
     ylim = c(6.0,6.4),lty = "dashed",rug=FALSE)

#these plots are ready to save and go into a document!




################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")
