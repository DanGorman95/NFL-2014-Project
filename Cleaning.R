data <- read.csv(file.choose(), header= T, stringsAsFactors = T) #Reading in the specific excel sheet

install.packages(dplyr) #Installing Packages
#Using Library(s)
library(hexbin)
library(dplyr)
library(RMySQL)
library(dbConnect)
library(tidyr) #Importing library for cleaning of the dataset
library(ggplot2)

#Datbase-----------------------------------------
#Connecting to MySQL Database
con = dbConnect(MySQL(), user='root', password='0000', dbname='new_schema2', host='localhost')

#List Database tables
dbListTables(con)

#Reading Table into a
c = dbSendQuery(con, "select * from new_schema2.data_09")
'data_09' = fetch(c, n=-1)

#Read Table into R for Viewing
dbReadTable(con, "new_schema2.data_09")

#Close database connection 
dbDisconnect(con)
#------------------------------------------------
View(data_09) #Viewing Data
#Cleaning------------------------------------
data_09[["NULL"]] = NULL
data_09[["CHAL"]] = NULL
data_09[["XTRA_NOTE"]] = NULL
#Removing any N/A Values
data_09[!is.na(data_09)]

#Changing TO Factors
data_09$ZONE = as.factor(data_09$ZONE)
data_09$TD = as.factor(data_09$TD)
data_09$RECEPT = as.factor(data_09$RECEPT)
data_09$TO = as.factor(data_09$TO)
data_09$PLTYPE = as.factor(data_09$PLTYPE)
data_09$DIRECTION = as.factor(data_09$DIRECTION)

#Post-Cleaning
#--------------------------------------
#Subsetting the Data for Graphing
OneScore3<-subset(data_09, QTR==4 & GAP < 7 & OFFENSE=="DET" & INTENDED=="81-C.Johnson")
OneScore4<-subset(data_09, QTR==4 & GAP < 7 & OFFENSE=="DEN" & INTENDED=="15-B.Marshall")
OneScore5<-subset(data_09, DEFENSE=="DEN" & TD=="TD")


summary(OneScore5) #summary of Subset (Denver)

#Analysis
quantile(data_09$YAC, 0.95, na.rm=T)
data_09[data_09$YAC < quantile(data_09$YAC, 0.95, na.rm=T), ]

#Dentisy Analysis
d <- density(OneScore3$YAC) # returns the density data 
plot(d, main="81-C.Johnson Density of Yards After Catch")
polygon(d, col="red", border="blue")

abline(lm(OneScore3$YAC, OneScore3$PYD))


#Plotting-----------------------------------
#Simple Plot using subset above to show Touchdowns Scored Against Denver Broncos
plot(OneScore5$YARDS, pch="TD" ,col.lab="red", xlab="Number Of TouchDowns", ylab="Yards From Endzone", main="Touchdowns Scored Vs Denver Broncos 2009", las="2")

#Calvin Johnson Yards Vs YAC W/abline
plot(OneScore3$YARDS, OneScore3$YAC,
pch = 16, col =2,
xlab = "Yards",
ylab = "Yards After Catch")
lm_PYD <- lm(OneScore3$YAC ~ OneScore3$YARDS)
abline(coef(lm_PYD), lwd =20)
par(mfrow=c(1,1))


#Marshall Vs Calvin In 4th Quarter Close Games
plot(OneScore3$YARDS, OneScore3$QTR)
par(mfrow = c(1,2))
main ="BOO"
boxplot(OneScore3$YARDS, xlab="Calvin")
boxplot(OneScore4$YARDS, xlab="Marshall")

#Hexbin Bining Showing Count of passing yards Vs Yards After Catch
a=hexbin(data_09$YAC, data_09$YARDS, xbins=40, xlab ="Yards After Catch", ylab="Passing Yards")
plot(a)

#Calvin Johnson Clutch Hist
hist(OneScore3$YARDS, breaks = 30, xlab="Yards Gained in games less than one score", main="Calvin Johnson 4th Quarter 'Clutch'", las="1")

#To Reset Plotting to One Graph
par(mfrow=c(1,1))

#Extra

boxplot(OneScore3$YARDS ~ OneScore3$QTR, main="Patriots Points Differential By Quarter", ylab="Points Gap", las=1) 

plot(OneScore3$PYD, OneScore3$YARDS, main="Randy Moss",  col=3, pch=14 , las=1)
qqnorm(OneScore3$PYD)

plot(data_09$DOWN, data_09$YAC, main="scatterplot")
axis(1, at=1:4, labels=(1:4))

