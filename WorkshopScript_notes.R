#My coding script, this is the top line

####### Answers to day 1 ##########
####### 1_1 Getting your Data Question 1#########

par(mfrow=c(2,2))
hist(Mydata$Twitches)
plot(density(Mydata$Twitches,na.rm=TRUE)) 
qqnorm(Mydata$Twitches)
qqline(Mydata$Twitches)



## This command shows me... 
unique(Mydata$Trial)
unique(Mydata$SubNo)



####### Answers to day 2 ##########
## 1_2 Manipulating data 

## Question 7 (correcting error from 6 where saved over CongTemp)
library(dplyr)
CongTemp <- select(Mydata, SubNo, RT, Congruence)

temp<-melt(CongTemp, id.vars = c("SubNo", "Congruence"),
             variable.name = "DataType", 
             value.name = "RT_value")
CongTemp<-temp
head(CongTemp)

detach("package:dplyr", unload=TRUE)
library(plyr)

CongTemp$Congruence<-revalue(CongTemp$Congruence, 
                             c("Incong" = "Incongruent", "Cong" = "Congruent"))
head(CongTemp)


temp<-dcast(CongTemp, DataType + SubNo ~ Congruence, 
      fun.aggregate = mean, na.rm = TRUE)

CongTemp<-temp

## 2_1 Normality_Correlation_TwoSampleTests
##  Independent practice Question 2 (correlation)

temp<-CongTemp
CongTemp<-temp<-dcast(CongTemp, DataType + SubNo ~ Congruence, 
      fun.aggregate = mean, na.rm = TRUE)

par(mfrow=c(2,2))
hist(CongTemp$Congruent)
qqnorm(CongTemp$Congruent)
qqline(CongTemp$Congruent)
hist(CongTemp$Incongruent)
qqnorm(CongTemp$Incongruent)
qqline(CongTemp$Incongruent)
#Not normally distributed - $Congruent RT especially

x<-CongTemp
names(x)
par(mfrow=c(1,1))
#scatterplot
plot(x$Congruent,x$Incongruent)
#using cor function
cor(x$Congruent,x$Incongruent,method="spearman")


#using rcorr from Hmisc
#change to data matrix first (have to extract columns with numbers- columns 3 and 4)
x<-data.matrix(x[,3:4])
head(x) #only columns 1 and 2 need to be used
rcorr(x[,1:2],type="spearman")







