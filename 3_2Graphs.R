# R teaching Workshops.
# Lotte Meteyard, 2016, University of Reading
# School of Psychology and Clinical Language Sciences

######################################################################
#Plots in R take a bit of getting used to
#but they are wonderful

#We've already seen some basic plotting functions
plot()
hist()
qqnorm()
qqline()
interaction.plot()
#we also saw how to use the corrplot package
#and using the lattice and effects packages for linear model plots
#plots are essential to doing good statistics
#with R, you will get used to visualising your data a lot

# This script will focus on ggplot because it is FABULOUS
library(ggplot2)
library(dplyr)



###### Summarising data for ggplot #####
#for ggplot2 the key thing is to prepare your data before hand
#and then you build layers in the plot


# Extract subject data (dplyr package)
Participant.Mydata <- group_by(Mydata.corr.a,SubNo,Task,Hemlabel,Axes)
Participant.Mydata <- summarise(Participant.Mydata,
                                SubjRT = mean(RT),
                                SubjSDRT = sd(RT))
#check number of subjects for Standard error calculation (plots usually use SE or CI)
length(levels(as.factor(Mydata.corr.a$SubNo)))
#or
length(unique(Mydata.corr.a$SubNo))




#then average across participant averages - first we will look at Task x Hemlabel
Participant.Mydata <- group_by(Participant.Mydata,Task,Hemlabel)
data.summary <- summarise(Participant.Mydata, 
              Mean = mean(SubjRT),
              StErr = mean(SubjSDRT)/sqrt(20))
data.summary
#now we've got data averaged for our conditions of interest


###### Bar Plot #####

# set up ggplot
#this puts Task on X Axis, mean on Y axis, and colour difference for Hemlabel
plot <- ggplot(data.summary,aes(x=as.factor(Task),y=Mean, fill=Hemlabel))
#need to fiddle with position to get separate bars
dodge <- position_dodge(width=0.9)
#then set up as bar plot
plot <- plot + geom_bar(position=dodge,stat="identity", aes(fill=Hemlabel))
plot

# add error bars with standard error/CI
plot <- plot + geom_errorbar(position=dodge,aes(ymin=Mean-(StErr*1.96),ymax=Mean+(StErr*1.96),width=.2))

# add axis labels
plot<-plot + xlab("Task")
plot<-plot + ylab("Reaction Time (ms)")
plot

# add title
plot<-plot + ggtitle("RT by Task and Hemisphere")
plot

# use theme and colours to make grey scale
plot<-plot + theme_bw()
plot + scale_fill_grey()

#change axis (focus view)
plot <-  plot + coord_cartesian(ylim=c(0.2, 0.7))


###### Line Plot #####
#see: http://docs.ggplot2.org/0.9.3.1/geom_smooth.html

Participant.Mydata <- group_by(Mydata.corr.a,SubNo,Task,Twitches)
Participant.Mydata <- summarise(Participant.Mydata,
                                SubjRT = mean(RT),
                                SubjSDRT = sd(RT))

#ggplot will apply its own modelling to graph the data, so we leave by Subject data

# set up data
plot <- ggplot(Participant.Mydata,aes(x=Twitches,y=SubjRT))
# plot separate panels for Task
plot <- plot + facet_grid(~Task)
#method=loess :local polynomials, will show non-linearities if there
#otherwise, can use method=lm
plot <- plot + stat_smooth(method=lm, fill="light grey") #+ geom_point()



#### INDEPENDENT PRACTICE ####
# Complete questions 1-2


#Visit the following website. Find and read through the information for Scatterplots
http://www.cookbook-r.com/Graphs/
# Look at the code for 'Basic scatterplots with regression lines'

#### INDEPENDENT PRACTICE ####
# Here is some data for Twitches (by Subject)
Participant.Mydata <- group_by(Mydata.corr.a,SubNo,Twitches)
Participant.Mydata <- summarise(Participant.Mydata,
                                SubjRT = mean(RT),
                                SubjSDRT = sd(RT))

# Complete question 3




