# R teaching Workshops.
# Norway, University of Agder
# Lotte Meteyard, 2017
# School of Psychology and Clinical Language Sciences,University of Reading


######################################################################
# Once data is in R, most of your time will be spent getting it tidy
# and doing sanity checks

# you will get used to never looking at raw data spreadsheets

# NB: there are usually several ways to do the same thing in R

#We will look at helpful packages for tidying up data
# This set of packages forms part of the 'Tidyverse'
https://www.tidyverse.org/

# Today we will look at
dplyr
reshape2
ggplot2

################## the tidyverse ##########################

# install plyr
library(dplyr)

help(package="dplyr")
#click on dplyr::dplyr
#read the section 'Single table verbs'

#we will work with filter, arrange, select, mutate, summarise

# Remind ourselves what our data looks like
str(Mydata)

####### Filter #######

# filter selects parts of the data according to some criteria
# select data for participant 2, when they made an error
filter(Mydata, SubNo == 2, ACC == 0)


####### Arrange #######

# arrange reorders rows 
# reorder entire data set by Trial
arrange(Mydata, Trial)
#look at this more easily
head(arrange(Mydata, Trial))
tail(arrange(Mydata, Trial))

####### Select #######

# select helps take out data / zoom in on data you want
# note this works best for numeric data
# select RT and Accuracy and Twitches data for all participants
select(Mydata, SubNo, ACC, RT, Twitches)

# we can store this for future use
temp <- select(Mydata, SubNo, ACC, RT, Twitches)
head(temp)
summary(temp)
dim(temp)


####### Mutate #######

# mutate allows you to add new data by transforming / using data 
# from existing columns
# create RT data in milliseconds (not seconds)
mutate(Mydata, RTms = RT*1000)
head(mutate(Mydata, RTms = RT*1000))

# we want to add this onto the existing data set, so we need to store it
Mydata <- mutate(Mydata, RTms = RT*1000)
head(Mydata)


################## generate summary data ##########################

# summarise when used with group_by() can flexibly generate summary data
# this is very useful when we want to complete further analysis

#For example, we want to see the effect of the Congruence manipulation
# first group data by participant and congruence
Participant.Mydata <- group_by(Mydata,SubNo,Congruence)
# summary RT
summarise(Participant.Mydata,
          MeanRT = mean(RT),
          SDRT = sd(RT),
          number = length(RT))
#so now we have mean RT for each Participant for Congruence conditions

#Another way to generate summary data is to use the Psych package
require(psych)
describeBy(Participant.Mydata$RTms,list(Participant.Mydata$Congruence,
                                   Participant.Mydata$Axes))
#now we have descriptive statistcs for RTms - 
#broken down by Congruence & Axes groupings




#### INDEPENDENT PRACTICE ####
# Complete questions 1 - 3


#some further methods here for exploring and summarising data
#using dplyr
#See http://genomicsclass.github.io/book/pages/dplyr_tutorial.html




################## reshape2 ##########################

#install rehsape2
library(reshape2)

#the main functions in reshape2 are melt() and cast()

#melt() and cast() allow you to move bweteen
#long and wide format data

#wide format data has a column for each variable and data in the cells
#long format data has the data in one column and other columns are used to code that data

# for a full tutorial see http://seananderson.ca/2013/10/19/reshape.html
# we'll follow that tutorial here
# use some data built into R

#look at the data first
head(airquality)

#let's melt it first
melt(airquality)  #scroll up and down to see what has happened to the data

#because we didn't specify id variables, R tells us what it is using by default
#'variable' identifies the row for 'value' (the numeric data)

#let's melt it but this time but be more specific 
# now we are keeping the columns month and day
melt(airquality, id.vars = c("Month", "Day"))

#scroll up to the column names
# you can see Month and Day have been kept
# and variable and value have the other columns
# Solar.R, Ozone and Wind are coded in variable
# the numeric data associated with these columns are under value

# we can repeat this, but give specific names
melt(airquality, id.vars = c("Month", "Day"),
            variable.name = "climate_variable", 
            value.name = "climate_value")
#scroll up to see these as column names



# going from long to wide format is a bit more tricky
# you will most often use data frames, so you need dcast

#melt first to wide format
temp <- melt(airquality, id.vars = c("Month", "Day"))
head(temp)

#go back to long format
dcast(temp, Month + Day ~ variable)
#note how Month and Day have been kept as column variables, and the rest
#of the labels in variable have become column names, with the data
#in each of them

#let's repeat this with some data we've been using
head(Mydata)

# select some data to work with
# use the select function from dplyr package
somedata <- select(Mydata, SubNo, RT, Twitches)
head(somedata)
dim(somedata) #the data has 9600 rows and 3 columns: subject, RT and Twitches

#first have to melt so that reshape2 can work with the data
melt(somedata, id.vars = c("SubNo", "Twitches"))

head(melt(somedata, id.vars = c("SubNo", "Twitches")))
tail(melt(somedata, id.vars = c("SubNo", "Twitches")))
#so the data is in the same format, but RT has been put into variable
#and the RT data itself is in value

#repeat this, but add column names and store in a data object called 'temp'
temp <- melt(somedata, id.vars = c("SubNo", "Twitches"),
             variable.name = "RT_variable", 
             value.name = "RT_value")
head(temp)
tail(temp)

#let's have one column for each subject, with average RT for each level of Twitches

#first make sure data is coded in the correct way
str(temp)
#make subject number into text, so it can become column names
temp$SubNo <- as.character(temp$SubNo)
#make Twitches as a factor/category, to make it easier to summarise across
temp$Twitches <- as.factor(temp$Twitches)
str(temp)

#now we will turn this data into wide format
#summarising the data - each participant in one column
#with average/mean RT at each level of Twitches 
dcast(temp, RT_variable + Twitches ~ SubNo, 
      fun.aggregate = mean, na.rm = TRUE)
#note that we have asked it to remove any NA data




#### INDEPENDENT PRACTICE ####
# Complete questions 4 - 6






################## tidying up names/cells/spaces/mislabelling ##########################


####### relabelling / recoding #######

#If you want to rename conditions, levels etc. on your data
#there is a nice function called 'revalue' in plyr (the precusor to dplyr)

library(plyr)  #note the warning because we already have dplyr installed

unique(Mydata$Hemisphere)
#this tells us how many 'unique' values there in this variable

# Lets relabel Hemisphere with left and right
# 1 = left
# 2 = right

#Change coding to a factor or text (otherwise revalue gets upset)
Mydata$Hemisphere<-as.factor(Mydata$Hemisphere)

#we are going to store the relabelled data in a new column called 'Hemlabel'
names(Mydata)

Mydata$Hemlabel<-revalue(Mydata$Hemisphere, c("1" = "Left", "2" = "Right"))

names(Mydata)
unique(Mydata$Hemlabel)




##### Get rid of spaces in cells with content/labels (common with excel imports)  #######

# Install the gdata package, we'll use the 'trim' function
library(gdata)
help(trim)

# Lets make some data with annoying spaces

x<-c(" hair", "hair", " nohair", " hair", "hair ", " nohair", "nohair", "hair ", " hair", " nohair", "nohair ")
unique(x)  #we can see the consequences of these spaces

x<-trim(x)  #overwrite the original data with trimmed data
unique(x)   #Now we only have two conditions



################## The importance of NAs ##########################

# NAs are REALLY important in R. Most or all functions have easy options
# to manage NAs in your data, but it must be coded with NAs, not blank cells etc.

# Let's create some messy data

#our data with spaces
x<-c(" hair", "hair", " nohair", " ", "hair ", " ", "nohair", "hair ", "XXX", " nohair", "CCC")
#a random sample of 11 numbers from 1-20, 
y<-sample(1:20, 11)
#put these together into a data fram
x<-data.frame(cbind(x,y))
#give the columns names
names(x)<-c("HairCut","Biscuits")
#now look at it
x


# check which cells are empty (convert to text before doing this if necessary)
which(x$HairCut==" ")

# Replace missing data with NA
x$HairCut[which(x$HairCut==" ")]<-NA 
# Replace cells with XXX with NA
x$HairCut[which(x$HairCut=="XXX")]<-NA 
# Replace cells with CCC with NA
x$HairCut[which(x$HairCut=="CCC")]<-NA 
x

which(x==" ")  # check again




# Recreate data 
x<-c(" hair", "hair", " nohair", " ", "hair ", " ", "nohair", "hair ", "XXX", " nohair", "CCC")
y<-sample(1:20, 11)
x<-data.frame(cbind(x,y))
names(x)<-c("HairCut","Biscuits")
x

# Check manually for NA (empty) cells
is.na(x)    # check whether all cells are NA or not (true/false)

# Replace whole row with NA if one value missing
x[which(x$HairCut==" "),]<-NA  
is.na(x)
x

# Check a specific column
is.na(x$HairCut) 



#### INDEPENDENT PRACTICE ####
# Complete questions 7 - 8




################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")



