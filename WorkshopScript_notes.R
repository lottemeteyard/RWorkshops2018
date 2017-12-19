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




