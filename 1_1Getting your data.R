# R teaching Workshops.
# Norway, University of Agder
# Lotte Meteyard, 2017
# School of Psychology and Clinical Language Sciences,University of Reading



####################  Welcome to R ###################################################

# We are using R Studio, a much friendlier way to use R
# If we use R itself, all you get is the Console (the window below)
# Script files can be used and annotated
# Look at the bottom of this script file, you can navigate to different parts

#R Studio
#Using the command line in the console (bottom panel)
#Running code from the top panels/R Script files
#Environment
#History
#Bottom right panel: files, plots, packages, Help


#################### Housekeeping #######################

#(1) ALWAYS set working directory
getwd()

#store current directory as a filepath, useful for saving and loading
filepath<-getwd()
filepath

# Copy and paste the whole Workshops folder somewhere sensible
filepath<-"C:/FolderPathHere/WorkshopsFolder"  
#write your preferred file path in here

setwd(filepath)  #set current folder / location to work from

#a simpler way is to use the GUI in R Studio
#Bottom right, click the ... choose the file location
#then click 'More' and 'set as working directory'

# Create an R script file / analysis log #######
# File - New File - R Script (or Ctrl+Shift+N). 
# This acts as a record of what you have done and a way to store useful code
# Save this and call it something sensible.
# Using # marks writing as not code / text for annotating 
# adding ## text ## around something makes it a heading (see below) 


#################### Run some code #######################
1 + 1 #place cursor on line and click 'Run'

######### Store data in objects #######
temp <- c(1,3,5,6,17,8) #temp has now appeared in your Environment

######### Do things with those objects #######
str(temp) #structure of the data object temp
summary(temp)  #summarise data inside temp
(sum(temp))/length(temp)  #calculate mean
mean(temp)  #calculat mean
plot(temp)  #plot data in temp
order(temp)  #gives you the index of the cells in order of magnitude

# work with specific cells in the data
temp[6]
temp[6]<-20  #change cell 6 to hold the value 20
temp[6]

plot(temp)

############################ Importing Data #############################

# It is possible to import directly from excel, but this requires
# installing perl on the PCs (in mac, this is already done). So we won't cover 
# that in these workshops. For future reference, the package needed to do this
# is 'gdata'. NB: it works with .xls

### Importing text files #########

# R prefers to import data from .txt or .csv files

# With data in .txt or .csv format, there are two ways to get it into R.
# (1) Import dataset in the Environment (top right panel)
# (2) Read it in yourself
Mydata <- read.table(file="TMS_data.txt",header=TRUE)
# or 
Mydata <- read.delim("TMS_data.txt")
#This has appeared as dat1 (data) in your Environment

# work with specific cells in the data
# Now need [rows, columns], remember 'roman catholic'

Mydata[9,4]  #9th row, 4th column
Mydata[4862,11]  #4862nd row, 11th column

names(Mydata) #get column names
Mydata$HomLocation[4862]  #use column names to get cell value, 4862nd row, 11th column


### Importing SPSS files #########

# It is possible to import directly from SPSS
# From SPSS data sheet, install package 'foreign'. Then set it running in R.
library(foreign)
# or
require(foreign)

# Read in SPSS file
dat2<-read.spss("SPSS_Data1.sav")  #note the warnings in red...
str(dat2)

### Data Frames, Column names #########

# R works with different data structures. One particularly useful one is 
# the dataframe. A data frame is more general than a matrix, 
# in that different columns can have different modes (numeric, character, factor, etc.). 
# This is similar to SAS and SPSS datasets

# dat2 currently saved as a List object
dat2<-data.frame(dat2)
str(dat2)

# look at variable names and change them
names(dat2)
#rename second column
names(dat2)[2]<-"HeightCM"
head(dat2)
#rename all columns
names(dat2)[1:4]<-c("AgeYrs","HeightCM","ShoeSizeUK","HairColour")
names(dat2)
head(dat2)


#list objects in your Environment
ls()

# Remove objects
rm(dat2)
rm(filepath,temp)




#### INDEPENDENT PRACTICE ####
#complete activities 1-3 on the worksheet.



#########  Looking at data #########

# We are going to work with Mydata
# This is for a Flanker and Choice Reaction Time (CRT) Task
# whilst people had TMS across different scalp locations
# we recorded the reported strength of muscle twitches for each trial (0-10 in strength)
# (Meteyard and Holmes, submitted)

#Click on the blue arrow next to Mydata in the Environment panel
#Click on Mydata itself in the Environment panel

# Look at the data
str(Mydata) 
# Notice that R automatically turns character/nominal variables into factors 
# with levels   

names(Mydata)   # Look at all the variable names

head(Mydata$Axes)  # The first few elements in a variable
head(Mydata$SubNo)
tail(Mydata$Twitches) 



#### INDEPENDENT PRACTICE ####
# Complete question 4 



############## Indexing variables ##################

# Roman Catholic - rows, columns
Mydata[47,3]
Mydata[3,9]

# When you want to look at some data use []
Mydata[47,3:5]

# When you want to apply a function use ()
levels(Mydata$Axes)

# When data is stored in a dataframe, use $column_name
names(Mydata)
Mydata$HomLocation


######## Basic descriptive statistics ##########

#explore a variable

mean(Mydata$Twitches)
mean(Mydata$Twitches,na.rm=TRUE) #to get this to function you need to remove missing values

sd(Mydata$Twitches,na.rm=TRUE)
sqrt(var(Mydata$Twitches,na.rm=TRUE))

# frequency count for each value
# useful for checking for missing data / errors in computations
table(Mydata$Twitches)
#and useful if you want to do a chi-square analysis

# overall summary statistics
summary(Mydata$Twitches)


#### Basic plots to look at normality #######

#histograms
hist(Mydata$Twitches)
hist(Mydata$RT)

#combining plots
#http://www.statmethods.net/advgraphs/layout.html
par(mfrow=c(1,2))
hist(Mydata$RT)
hist(Mydata$Twitches)

# check distribution graphically
par(mfrow=c(1,2))
hist(Mydata$RT)
plot(density(Mydata$RT,na.rm=TRUE)) 

#return plotting space to single window (not two panels)
par(mfrow = c(1,1))


# quantile plots
qqnorm(Mydata$RT)        #plot quantiles against theoretical normal distribution
# normal distribution should fall along line
qqline(Mydata$RT)
#we can see the standard 'long tail' for reaction time data

#lets put these together to illustrate that
par(mfrow=c(2,2))
hist(Mydata$RT)
plot(density(Mydata$RT,na.rm=TRUE)) 
qqnorm(Mydata$RT)
qqline(Mydata$RT)


#Something a bit more complicated, but useful
#From http://www.r-bloggers.com/video-overlay-histogram-in-r-normal-density-another-series/
# Overlay normal distribution

par(mfrow = c(1,1)) #put plotting window back to single pane

#example of how to generate some normal data
dnorm(seq(min(1),max(10),length=100),mean=5,sd=2)

#create some data that is normally distributed, using the mean and sd from our RT data
d<-na.omit(Mydata$RT) #Remove NAs from data so R doesn't get upset
dnorm(seq(min(d),max(d),length=100),mean=mean(d),sd=sd(d))

hist(d,freq=FALSE)  #plot histogram with probability not frequency counts
#create normal distribution data and add to plot
#make sure to highlight both lines of code and run them
points(seq(min(d),max(d),length=100),
        dnorm(seq(min(d),max(d),length=100),mean=mean(d),sd=sd(d)),type="l",col="red")


## Plot frequency & cummulative frequency
# From http://stackoverflow.com/questions/13960896/add-density-lines-to-histogram-and-cumulative-histogram/13961565#13961565
hcum <- h <- hist(d, plot=FALSE)
hcum$counts <- cumsum(hcum$counts) #sums frequencies to get cummulative
plot(hcum, main="")
plot(h, add=T, col="grey")




#### INDEPENDENT PRACTICE ####
# Complete question 5




############ Descriptive statistics, using package 'Psych' 

# Install package 'Psych'
library(psych)

# Let's look at Psych
help(psych)
# List all the functions
help(package="psych")

# Find the function 'describe'
help(describe)

# This code generates descriptive statistics for all other variables by the levels of Axes
# Remind ourselves what Axes data is:
levels(Mydata$Axes)
# Axes is the orientation of the TMS coil used in the experiment
# So we should get 4 levels (for Axes), each with descriptive statistics

# We are going to store this in an object
Axes.dat<-describeBy(Mydata,group=Mydata$Axes)  

# notice that Axes.dat1 is under Values in the Environment window

head(Axes.dat)    #Check what it looks like
str(Axes.dat)     # Look at the nested structure

# Look at rownames 
# Try the following, note how sensitive R is to the double square brackets for lists
# This is because of that nested structure (found in Lists)
rownames(Axes.dat[1])
rownames(Axes.dat[[1]])
rownames(Axes.dat[[2]])

# Repeat the same function, but get a matrix, not a list as output
# We will just select the Reaction Time data this time
# We will overwrite the previous data
Axes.dat<-describeBy(Mydata$RT,group=Mydata$Axes,mat=TRUE)  
# we set mat=TRUE, so we get a matrix/dataframe as output, not a list

# notice that Axes.dat1 is now under data in the Environment window
head(Axes.dat)

# several functions have this flexibility with parameters you set to TRUE/FALSE etc.



#### INDEPENDENT PRACTICE ####
# Complete question 6 and 7




################# Saving work #################

# The simplest thing is to save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  #check the Files tab bottom right, it should appear

# Save specific data
# e.g. save our summary data from above
save(Axes.dat,file="RTxAxes.RData")

# Save a text file
write.table(Axes.dat, "Axes_RT.txt", sep="\t")

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")

#And this R script is already saved!



################# General principles for R #################

# R means coding
# R demands that you problem solve. And be transparent.

# Persevere
# Look for answers
# Trial and error (lots of this)
# Store useful code
# Intelligent copy paste

# This all = TIME. 

### Citing / referencing R packages ###########
# When reporting analyses, you need to cite the specific packages you have used

# To get information to cite the base package
citation()
#check if installed and then give citation
if(nchar(system.file(package="psych"))) citation("psych")

# For a specific package
citation(package = "psych")
citation(package = "foreign")


################  Crib sheets & R helpers ######

# Read help documentation (not always useful, examples usually good)
help(setwd)
??setwd
# For example, to find statistical tests
help.search("anova")
# Return all available functions with 'test' in the name
apropos("test")   
# Return all functions in package 'psych'.
objects(package:psych)

# R reference card
# cran.r-project.org/doc/contrib/Short-refcard.pdf

# Quick R
# www.statmethods.net/

# Cookbook for R
# www.cookbook-r.com/

# Personality project (psychology specific)
# personality-project.org/r/

#### Some websites ###

http://r-statistics.co/
http://r-statistics.co/R-Tutorial.html
http://r-statistics.co/Statistical-Tests-in-R.html


################ Things to remember ###########

# Stay updated
# R and R Studio will update regularly
# Update R first, and R Studio should automatically recongise the version you have
# Some packages may not run on old versions of R

# Packages and reliability:
# The current R repository (CRAN) has ~5604 available packages
# http://cran.r-project.org/web/packages/
# (1) Packages get updated, CHANGED and removed - stay up to date
# (2) Packages may do things you don't understand - stay educated 
# (3) Packages may have bugs - stay up to date & educated & ask for help
# (4) CITE THE PACKAGES AND VERSIONS YOU USE 

# Googling a problem in R will give you discussion boards, 
# web pages and other people's solutions. 
# Remember: there are usually multiple solutions. Find one that works for you.
# The online resources and support community are HUGE
# Luckily for us, it is populated by statisticians and programmers
# and they are happy to help
# Post to a discussion board yourself if you get stuck



#######################################################################
#######################################################################



