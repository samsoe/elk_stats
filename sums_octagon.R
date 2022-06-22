setwd("./")
library(lattice)
library("PerformanceAnalytics")
library("Hmisc")
library("reshape2")
library("ggplot2")
library("dplyr")
data <- read.csv("sums_octagon.csv",header=T, sep="," )
str(data)

plot(data$date, data$False_Engage)
plot(data$date, data$Shadow)
plot(data$date, data$Chase)
plot(data$date, data$Yawn)
plot(data$date, data$Hissing_LipCurl)
plot(data$date, data$Fleeing)
plot(data$date, data$Headup_Ears.Back)
plot(data$date, data$Head_Forward_rush)
plot(data$date, data$Front_Kick)
plot(data$date, data$Mount )
plot(data$date, data$Front_Kick)
#plot(data$date, data$Grind_teeth)
plot(data$date, data$Urinate)
plot(data$date, data$Bed)
plot(data$date, data$Feeding)
plot(data$date, data$Standing)
plot(data$date, data$Scent_Check)
plot(data$date, data$grooming)
plot(data$date, data$Scent_Check)
plot(data$date, data$contact_cam)

#total time spent doing each behavior, ranked
sum(na.omit(data$Standing)) #4620
sum(na.omit(data$Walking)) #3571
sum(na.omit(data$Chase)) #3185
sum(na.omit(data$Scent_Check)) #2360
sum(na.omit(data$contact_cam)) #2188
sum(na.omit(data$False_Engage)) #1060
sum(na.omit(data$Feeding)) #1984
sum(na.omit(data$Bed)) #1874
sum(na.omit(data$Headup_Ears.Back)) #904
sum(na.omit(data$Shadow)) #845
sum(na.omit(data$grooming )) #747
sum(na.omit(data$Fleeing)) #706
sum(na.omit(data$Head_Forward_rush)) #594
sum(na.omit(data$Hissing_LipCurl)) #117
sum(na.omit(data$Yawn)) #107
sum(na.omit(data$Front_Kick)) #57
sum(na.omit(data$Urinate)) #48
sum(na.omit(data$Grind_teeth)) #0


plot(data$date, data$num_bulls)







#
data <- read.csv("sums_octagon_transpose.csv",header=T, sep="," )
str(data)
aggregate(data$time~ data$behavior, FUN=sum) #
xyplot(data$time ~ data$date|data$behavior)
#chart.Correlation(data, histogram=TRUE, pch=19)
#rcorr(as.matrix(data))
    
spar <- subset(data, data$type=="sparring")
dom <-  subset(data, data$type=="dominance")
other <-  subset(data, data$type=="other")

spar1 <- aggregate(spar$time~ spar$type + spar$date, FUN=sum)
names(spar1) <- c("type", "date", "time")
dom1 <- aggregate(dom$time~ dom$type + dom$date, FUN=sum)
names(dom1) <- c("type", "date", "time")
other1 <- aggregate(other$time~ other$type + other$date, FUN=sum)
names(other1) <- c("type", "date", "time")


plot(spar1$date, spar1$time, type="p", col= "blue", lwd=2, ylim=c(0,2500))
lines(dom1$date, dom1$time, type="p", col="red", lwd=2, ylim=c(0,2500))
lines(other1$date, other1$time, type="p", col="black", lwd=2, ylim=c(0,2500))


plot(spar1$date, spar1$time, type="o", col= "blue", lwd=2, ylim=c(0,2500))
lines(dom1$date, dom1$time, type="o", col="red", lwd=2, ylim=c(0,2500))
lines(other1$date, other1$time, type="o", col="black", lwd=2, ylim=c(0,2500))
library(lubridate)
data <- read.csv("octagon_clean.csv",header=T, sep="," )
str(data)
plot(data$Date, data$TOD)
date.r <- as.Date(data$Date, format= "%m/%d/%y")
data$day <- yday(date.r)


data1 <- melt(data, id.vars = c("d", "day","total.min", "TOD","num_bulls","num_cows","num_calves","unclassified"), 
     measure.vars = c("False.Engage", "Shadow", "Chase", "Hissing.LipCurl","Headup.Ears.Back", "Head.Forward.rush", 
                    "Front.Kick", "Grind.teeth", "Mount", "Yawn", "Urinate", "Walking", "Bed", "Feeding",
                    "Standing", "Scent.Check", "grooming", "contact.cam", "Fleeing"))
str(data1)
levels(data1$variable)
sel.spar <- c("False.Engage", "Shadow", "Chase")
spar.d <- data1[data1$variable %in% sel.spar,]
type <- rep("sparring", length(spar.d$d))
spar.d$type <- type

sel.dom <- c("Hissing.LipCurl","Headup.Ears.Back", "Head.Forward.rush", 
             "Front.Kick", "Grind.teeth", "Mount", "Yawn")
dom.d <- data1[data1$variable %in% sel.dom,]
type <- rep("dominance", length(dom.d$d))
dom.d$type <- type


sel.other <- c("Urinate", "Walking", "Bed", "Feeding",
             "Standing", "Scent.Check", "grooming", "contact.cam", "Fleeing" )
other.d <- data1[data1$variable %in% sel.other,]
type <- rep("other", length(other.d$d))
other.d$type <- type

data2 <- rbind(spar.d,dom.d,other.d)

data2.c <- data.frame(data2$d,data2$day,data2$total.min, data2$TOD,data2$value, data2$type)
data2.c <- subset(data2.c, data2.c$data2.value>0)

#data2.c[data2.c==""]<-NA
#data2.c <- na.omit(data2.c)
#data2.c %>% mutate_if(is.character, list(~na_if(.,""))) 
colnames(data2.c) <- c("d","day", "total.min", "TOD", "value", "type")
#data2[is.na(data2)] <- " "
#ggplot(data2.c, aes(x=day, y=TOD, colour=type)) + geom_point()
#as.Date(data2.c$d, format= "%m%d%y")

spar.c <- subset(data2.c, data2.c$type=="sparring")
dom.c <- subset(data2.c, data2.c$type=="dominance")
other.c <- subset(data2.c, data2.c$type=="other")


#
sun<- read.csv("sunrise.csv",header=T, sep="," )
date.sun <- as.Date(sun$date, format= "%m/%d/%y")
sun$day <- yday(date.sun)

#
plot(1, type="n", xlab="", ylab="", xlim=c(140, 255), ylim=c(0, 1440), main="Sparring")
x <- sun$day
y1 <- sun$sunrise.minute
y2 <- rep(0,length(sun$sunrise.minute))
polygon(c(x,rev(x)),c(y2,rev(y1)),col="grey")

a <- sun$day
b1 <- sun$sunset.minute
b2 <- rep(1440,length(sun$sunset.minute))
polygon(c(a,rev(a)),c(b2,rev(b1)),col="grey")
points(spar.c$day, spar.c$total.min, pch=16, col="blue")


#
plot(1, type="n", xlab="", ylab="", xlim=c(140, 255), ylim=c(0, 1440), main="Dominance")
x <- sun$day
y1 <- sun$sunrise.minute
y2 <- rep(0,length(sun$sunrise.minute))
polygon(c(x,rev(x)),c(y2,rev(y1)),col="grey")

a <- sun$day
b1 <- sun$sunset.minute
b2 <- rep(1440,length(sun$sunset.minute))
polygon(c(a,rev(a)),c(b2,rev(b1)),col="grey")
points(dom.c$day, dom.c$total.min, pch=16, col="blue")

#
plot(1, type="n", xlab="", ylab="", xlim=c(140, 255), ylim=c(0, 1440), main="Other Behavior")
x <- sun$day
y1 <- sun$sunrise.minute
y2 <- rep(0,length(sun$sunrise.minute))
polygon(c(x,rev(x)),c(y2,rev(y1)),col="grey")

a <- sun$day
b1 <- sun$sunset.minute
b2 <- rep(1440,length(sun$sunset.minute))
polygon(c(a,rev(a)),c(b2,rev(b1)),col="grey")
points(other.c$day, other.c$total.min, pch=16, col="blue")


###
points(dom.c$d, dom.c$total.min, pch=16)

plot(spar.c$day, spar.c$total.min, pch=16)
plot(dom.c$day, dom.c$total.min, pch=16)

plot(other.c$day, other.c$total.min, pch=16)

################################################
#################### 2020 ######################
################################################
#see the xlxs file (Octagon_clean_2020.xlsx) to see how to build the appropriate spreadsheet.
#the date needs to be without slashes, and the total.min column represents the elapsed number
#of minutes until the observation. The figures have minutes on their y-axis that I swap in powerpoint.
#The times and dates on the figures (in ppt) aren't exact enough for a publication, but they're good enough
#for presentations.
setwd("/Volumes/mpgcloud/Private/mmctee/MPG/Manuscripts/Elk Octagon/2021")
data <- read.csv("Octagon_clean_2020.csv",header=T, sep="," )
str(data)
plot(data$Date, data$TOD)
date.r <- as.Date(data$Date, format= "%m/%d/%y")
data$day <- yday(date.r)

#this 2020 dataset doesn't have all of the same factors as the 2018
data1 <- melt(data, id.vars = c("d", "day", "TOD", "total.min"), 
              measure.vars = c("False.Engage", "Shadow", "Chase", "Hissing.LipCurl", "Head.Forward.rush", 
                               "Front.Kick", "Grind.teeth", "Yawn", "Urinate", "Walking", "Bed", "Feeding",
                               "Standing", "Scent.Check", "grooming", "contact.cam", "Fleeing"))
str(data1)
levels(data1$variable)
sel.spar <- c("False.Engage", "Shadow", "Chase")
spar.d <- data1[data1$variable %in% sel.spar,]
type <- rep("sparring", length(spar.d$d))
spar.d$type <- type

sel.dom <- c("Hissing.LipCurl","Headup.Ears.Back", "Head.Forward.rush", 
             "Front.Kick", "Grind.teeth", "Mount", "Yawn")
dom.d <- data1[data1$variable %in% sel.dom,]
type <- rep("dominance", length(dom.d$d))
dom.d$type <- type


sel.other <- c("Urinate", "Walking", "Bed", "Feeding",
               "Standing", "Scent.Check", "grooming", "contact.cam", "Fleeing" )
other.d <- data1[data1$variable %in% sel.other,]
type <- rep("other", length(other.d$d))
other.d$type <- type

data2 <- rbind(spar.d,dom.d,other.d)

data2.c <- data.frame(data2$d,data2$day,data2$total.min, data2$TOD,data2$value, data2$type)
data2.c <- subset(data2.c, data2.c$data2.value>0)

#data2.c[data2.c==""]<-NA
#data2.c <- na.omit(data2.c)
#data2.c %>% mutate_if(is.character, list(~na_if(.,""))) 
colnames(data2.c) <- c("d","day", "total.min", "TOD", "value", "type")
#data2[is.na(data2)] <- " "
#ggplot(data2.c, aes(x=day, y=TOD, colour=type)) + geom_point()
#as.Date(data2.c$d, format= "%m%d%y")

spar.c <- subset(data2.c, data2.c$type=="sparring")
dom.c <- subset(data2.c, data2.c$type=="dominance")
other.c <- subset(data2.c, data2.c$type=="other")


#
sun<- read.csv("sunrise.csv",header=T, sep="," )
date.sun <- as.Date(sun$date, format= "%m/%d/%y")
sun$day <- yday(date.sun)

#y axis is minutes of day
plot(1, type="n", xlab="", ylab="", xlim=c(140, 255), ylim=c(0, 1440), main="Sparring")
x <- sun$day
y1 <- sun$sunrise.minute
y2 <- rep(0,length(sun$sunrise.minute))
polygon(c(x,rev(x)),c(y2,rev(y1)),col="grey")

a <- sun$day
b1 <- sun$sunset.minute
b2 <- rep(1440,length(sun$sunset.minute))
polygon(c(a,rev(a)),c(b2,rev(b1)),col="grey")
points(spar.c$day, spar.c$total.min, pch=16, col="blue")


#
plot(1, type="n", xlab="", ylab="", xlim=c(140, 255), ylim=c(0, 1440), main="Dominance")
x <- sun$day
y1 <- sun$sunrise.minute
y2 <- rep(0,length(sun$sunrise.minute))
polygon(c(x,rev(x)),c(y2,rev(y1)),col="grey")

a <- sun$day
b1 <- sun$sunset.minute
b2 <- rep(1440,length(sun$sunset.minute))
polygon(c(a,rev(a)),c(b2,rev(b1)),col="grey")
points(dom.c$day, dom.c$total.min, pch=16, col="blue")

#
plot(1, type="n", xlab="", ylab="", xlim=c(140, 255), ylim=c(0, 1440), main="Other Behavior")
x <- sun$day
y1 <- sun$sunrise.minute
y2 <- rep(0,length(sun$sunrise.minute))
polygon(c(x,rev(x)),c(y2,rev(y1)),col="grey")

a <- sun$day
b1 <- sun$sunset.minute
b2 <- rep(1440,length(sun$sunset.minute))
polygon(c(a,rev(a)),c(b2,rev(b1)),col="grey")
points(other.c$day, other.c$total.min, pch=16, col="blue")


###
points(dom.c$d, dom.c$total.min, pch=16)

plot(spar.c$day, spar.c$total.min, pch=16)
plot(dom.c$day, dom.c$total.min, pch=16)

plot(other.c$day, other.c$total.min, pch=16)
