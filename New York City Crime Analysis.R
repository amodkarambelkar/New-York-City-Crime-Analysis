#Importing dataset into R studio using Fread.

library(data.table)

context <- fread('Dataset.csv', header = TRUE)
#Summarizing the dataset to see what it looks like
summary(context)

#------------------------QUES 1 ---------------------------------
#Plotting to see the type of crime based on different areas
library(ggplot2)
library(stringr)
missing <- context
ggplot(missing, aes(x=BORO_NM, fill=factor(LAW_CAT_CD))) + stat_count(width=0.5) +xlab('Areas') +
  ylab('Count of Crimes') + ggtitle('Plot showing different crimes in areas')
table(missing$BORO_NM)

#------------------------Ques 2 --------------------------------
#Plot to see crime in different dates
context$Date2 <- mdy(context$CMPLNT_FR_DT)
context$date_only<-format(as.Date(context$Date2,format="%Y-%m-%d"), "%d")
context$time <- chron(times=context$CMPLNT_FR_TM)
context$dateandtime = c(context$date_only,context1$time,sep="_")

ggplot(context, aes(x=context1$date_only , fill=factor(LAW_CAT_CD))) + stat_count(width=0.5) +
  xlab('Areas') +
  ylab('Count of Crimes') + 
  ggtitle('Plot showing date days ')

#--------------------------Ques 3 -----------------------------------
#Relation between time, are and type of crime
str(missing)
missing$CMPLNT_FR_DT <- as.Date(as.character(missing$CMPLNT_FR_DT),"%m/%d/%y")
year(missing$CMPLNT_FR_DT)

missing$hour <- format(as.POSIXct(missing$CMPLNT_FR_TM,format="%H:%M:%S"),"%H")

ggplot(missing, aes(x=hour, fill=factor(LAW_CAT_CD))) + stat_count(width=0.5)

ggplot(missing, aes(x=hour, fill=BORO_NM)) + stat_count(width=0.5) + facet_wrap(~LAW_CAT_CD) +
  xlab('Hours') +
  ylab('Count of Crimes') + ggtitle('Relation between Area, Hours and Type of Crime')

#--------------------------Ques 4-------------------------------------
#Crime related to factors in that area
library(ggmap)
library(hexbin)

map <- get_map(location = 'New York', zoom = 12)
mapPoints <- ggmap(map) + geom_point(aes(x = Longitude, y = Latitude, col="red"), data = context, pch=4, alpha = .3)

plot(mapPoints)


ggplot (context, aes (x = Latitude, y = Longitude, colour = LAW_CAT_CD)) + stat_density2d () + ggtitle('Crime Categories in Area')
ggplot (context, aes (x = Latitude, y = Longitude, colour = LAW_CAT_CD))+ stat_binhex (bins=5,aes (alpha = ..count..)) + facet_grid (. ~ LAW_CAT_CD)