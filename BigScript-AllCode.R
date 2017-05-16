#Install & Enable Library
install.packages("devtools")
library(devtools)

#Allows Me To Install the nflscrapR library from Github Repo
install_github(repo = "maksimhorowitz/nflscrapR")

#Enable library
library(nflscrapR)

#Importing & Storing 2014 play-by-play data in a dataframe-----
pbp2014f <- season_play_by_play(2014)
summary(pbp2014f)
#Removing Rows containing a certain word
pbp2014<-pbp2014[!(pbp2014$down=="0"),]
pbp2014$down[is.na(pbp2014$down)] <- 0
#https://stackoverflow.com/questions/38257579/using-r-to-navigate-and-scrape-a-webpage-with-drop-down-html-forms/38276494#38276494
#Scraping NFL SnapCount from Football Outsiders
snap <- html_session("http://www.footballoutsiders.com/stats/snapcounts")
FootOutside<-html_form(snap)[[3]]
filled_form <-set_values(FootOutside,
                         "team" = "ALL",
                         "week" = "ALL",
                         "pos"  = "ALL",
                         "year" = "2014"             
)
d <- submit_form(session=snap, form=filled_form)
SnapCountQB <- d %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table(header=TRUE)

#Cleaning----
SnapCount[["Def Snaps"]] = NULL
SnapCount[["Def Snap Pct"]] = NULL
SnapCount[["ST Snaps"]] = NULL
SnapCount[["ST Snap Pct"]] = NULL

#Contract Values
#http://www.michaeljgrogan.com/rvest-web-scraping-using-r/
html <- read_html("http://www.spotrac.com/nfl/rankings/2014/contract-value/quarterback/")
player <- html_nodes(html, ".team-name , .tablesorter-headerUnSorted .tablesorter-header-inner")
cash <- html_nodes(html, ".noborderright:nth-child(4) :nth-child(1)")
player <- html_text(player)
cash<- html_text(cash)
Value = data.frame(player, cash)

#Contract Lenghts
html <- read_html("http://www.spotrac.com/nfl/rankings/2014/contract-length/quarterback/")
player <- html_nodes(html, ".team-name , .player .tablesorter-header-inner")
lenght <- html_nodes(html, ".noborderright:nth-child(4) :nth-child(1)")
player <- html_text(player)
lenght <- html_text(lenght)
years = data.frame(player, lenght)
View(ContractLenght)

#Merge Both
Contracts<- merge(Value, years, by = "player", all = T)

#Dealing with $ Factor issue
#http://stackoverflow.com/questions/31944103/converting-currency-with-commas-into-numeric
Contracts$cash <- as.numeric(gsub('[$,]', '', Contracts$cash))

#Writing Scraped Files to a CSV File for perm use
write.table(x=pbp2014,
            file='pbp2014.csv',
            sep=',', row.names=FALSE, quote=FALSE)

write.table(x=PLA14,
            file='playerstats14.csv',
            sep=',', row.names=FALSE, quote=FALSE)

write.table(x=QBComplete,
            file='players2014.csv',
            sep=',', row.names=FALSE, quote=FALSE)

write.table(x=Final,
            file='Combo.csv',
            sep=',', row.names=FALSE, quote=FALSE)

write.table(x=SnapCount,
            file='SnapCount.csv',
            sep=',', row.names=FALSE, quote=FALSE)

write.table(x=Contracts,
            file='Contracts.csv',
            sep=',', row.names=FALSE, quote=FALSE)

write.table(x=QBComplete,
            file='qbcomp.csv',
            sep=',', row.names=FALSE, quote=FALSE)


write.table(x=pbp2014f,
            file='pbp20.csv',
            sep=',', row.names=FALSE, quote=FALSE)

pbp2014 <- read.csv(file="pbp2014.csv", stringsAsFactors = F)
install.packages(c('RMySQL','dbConnect','ggplot')) #Installing Packages
#Using Library(s)
library(dplyr)
library(RMySQL)
library(ggplot2)
#Datbase-----------------------------------------
#Connecting to MySQL Database
con = dbConnect(MySQL(), user='nflproject', password='nflpro#', dbname='nflproject', host='mysql3.gear.host')

#WriteTable To Database
dbWriteTable(con, "pbp2014", pbp2014[1:45502, ], append = TRUE)
dbWriteTable(con, "combine", combine[, ], append = TRUE)
dbWriteTable(con, "QBRating", QB.Rating[1:32, ], append = TRUE)
dbWriteTable(con, "SnapCount", SnapCount[, ], append = TRUE)
dbWriteTable(con, "Contracts", Contracts[, ], append = TRUE)
dbWriteTable(con, "QbComplete", QBComplete[, ], append = TRUE)
dbWriteTable(con, "cluster5", cluster5[, ], append = TRUE)
dbWriteTable(con, "regression", actuals_preds[, ], append = TRUE)
dbWriteTable(con, "combodraft", Final[, ], append = TRUE)
dbWriteTable(con, "draft", draft[, ], append = TRUE)

QB.Rating <- ungroup(QB.Rating)
QBComplete <- ungroup(QBComplete)
#List Database tables
dbListTables(con)
#Reading Table into a
c = dbSendQuery(con, "select * from nflproject.pbp2014")
pbp2014 <-  fetch(c, n=-1)
d = dbSendQuery(con, "select * from nflproject.combine")
combine <-  fetch(d, n=-1)
e = dbSendQuery(con, "select * from nflproject.SnapCount")
SnapCount <-  fetch(e, n=-1)
f = dbSendQuery(con, "select * from nflproject.Contracts")
Contracts <-  fetch(f, n=-1)
g = dbSendQuery(con, "select * from nflproject.draft")
draft <-  fetch(g, n=-1)

#dbDisconnect(con)
#setOldClass(c("QB.Rating", "data.frame"))
#Cleaning------------------------------------
#Summary
summary(pbp2014)


#Changing "-" in date to "/" So I can convert date
pbp2014$Date <- gsub("-", "/", pbp2014$Date)
pbp2014$Date<- as.Date(pbp2014$Date, format = "%Y/%m/%d")

#Converting Variables to INT
#Changing Columns from Chara To INTs
cols <- c(1,3,5:6)
pbp2014[cols] <- lapply(pbp2014[cols], as.numeric)

#Removing Columns Not needed
pbp2014[["row_names"]] = NULL
combine[["row_names"]] = NULL
Contracts[["row_names"]] = NULL
draft[["row_names"]] = NULL
SnapCount[["row_names"]] = NULL

pbp2014[["time"]]  =NULL
pbp2014[["ExpPts"]] = NULL
pbp2014[["PlayAttempted"]]  =NULL
pbp2014[["ExpPts"]] = NULL
pbp2014[["Home.WP.pre"]] = NULL
pbp2014[["Away.WP.pre"]] = NULL
pbp2014[["Home.WP.post"]] = NULL
pbp2014[["Away.WP.post"]] = NULL
pbp2014[["Home.WPA"]] = NULL
pbp2014[["Away.WPA"]] = NULL
pbp2014[["EPA"]] = NULL
pbp2014[["WPA"]] = NULL
pbp2014[["Season"]] = NULL
pbp2014[["sp"]] = NULL
pbp2014[["ExPointResult"]] = NULL
pbp2014[["TwoPointConv"]] = NULL
pbp2014[["DefTwoPoint"]] = NULL
pbp2014[["Onsidekick"]] = NULL
pbp2014[["PuntResult"]] = NULL
pbp2014[["FieldGoalResult"]] = NULL
pbp2014[["FieldGoalDistance"]] = NULL

starters <- c("A.Rodgers", "S.Hill",
              "B.Roethlisberger","A.Luck",
              "T.Brady","M.Ryan",
              "P.Rivers","D.Brees",
              "T.Romo","R.Wilson",
              "P.Manning","C.Newton",
              "J.Flacco","M.Stafford",
              "R.Tannehill","E.Manning",
              "K.Orton","C.Palmer",
              "T.Bridgewater","C.Kaepernick",
              "A.Smith","J.Cutler",
              "A.Dalton","B.Bortles",
              "N.Foles","J.Locker",
              "J.McCown","D.Carr",
              "K.Cousins","R.Fitzpatrick",
              "B.Hoyer", "G.Smith")
#taking all guys who have thrown a ball 2014 and add their team tag to their names for identification
passers <- as.character(unique(paste(subset(pbp2014,PlayType=="Pass")$Passer, subset(pbp2014,PlayType=="Pass")$posteam,sep=" ")))

#identify all the starting QBs within the pass-throwers, Using the above list of players to identify QB's
starters.team <- passers[grep(paste(starters,collapse="|"),passers)]

#Adding team colour to dataset for graphing.
colour.scheme <- c("#97233F", "#C9233F", "#2A0365", "#0F4589", "#0088D4", "#DF6108", "#F03A16", "#322820",
                   "#0D254C", "#DF6108", "#006DB0", "#313F36", "#B20032", "#163F83", "#007198", "#B20032",
                   "#005E6A", "#3B0160", "#0D254C", "#C9B074", "#192E6C", "#313F36", "#C4C8CB", "#003B48",
                   "#F2C800", "#0B264D", "#54BA4C", "#840026", "#0D254C", "#B20032", "#4C96C4", "#7A2D39")


#Getting team tags  
tags <- as.data.frame(unique(pbp2014$posteam))
colnames(tags) <- "teams"
tags <- tags %>%
  arrange(teams)
tags <-as.character(tags[-1,])
#Binding Team Tags with Pos Team colour and creating a new column called colour.scheme
teamcolour <- as.data.frame(cbind(tags, posteamcolour = colour.scheme))
#Merging 
pbp2014 <- merge(pbp2014,teamcolour, by.x="posteam", by.y="tags")

#Creating a dataframe of seasonal stats for QB's
QB.Rating <- pbp2014 %>%
  mutate(names = paste(Passer))%>%
  filter(PlayType=="Pass") %>%
  mutate(QB.team = paste(Passer,posteam,sep=" ")) %>%
  group_by(QB.team, posteam, posteamcolour) %>%
  summarise(Games.Played = n_distinct(GameID),
            Total.Passes = n(),
            First.Downs.PerGame = sum(FirstDown)/Games.Played,
            Completed = length(PassOutcome[PassOutcome=="Complete"]),
            Incomplete = length(PassOutcome[PassOutcome=="Incomplete Pass"]),
            Completion.Rate = round(Completed/Total.Passes,3)*100,
            Total.Yards = sum(Yards.Gained),
            Yards.per.Att = sum(Yards.Gained)/Total.Passes,
            Interceptions = sum(InterceptionThrown),
            Interceptions.per.att = round(Total.Passes/Interceptions),
            TDs = sum(Touchdown),
            TDs.per.att = round(Total.Passes/TDs),
            TDs.per.INT = round(TDs/Interceptions,3),
            Fumbles = sum(Fumble))%>%
  filter(QB.team %in% starters.team) %>%
  arrange(QB.team,-Completion.Rate, -Total.Passes)
#QB.Rating<- QB.Rating$Name
SnapCount <- read.csv(file = "SnapCount.csv", stringsAsFactors = F)
Contracts <- read.csv("Contracts.csv", stringsAsFactors = F)
premerge <- merge(Contracts, SnapCount, by.x = "player", by.y = "Player", all = F)


QBSnap <- premerge %>%
  filter(Position =="QB")%>%
  mutate(QB.team = paste(player,Team,sep=" "))%>%
  filter(QB.team %in% starters.team)

#Merge QB.Ratings w/ SnapCounts
QBComplete <- merge(QB.Rating, QBSnap, by.x="QB.team", by.y="QB.team", all = T)
QBComplete$lenght <- gsub("years", "", QBComplete$lenght)
QBComplete$lenght <- as.numeric(QBComplete$lenght)
QBComplete$Yearly <- QBComplete$cash / QBComplete$lenght
QBComplete[["player"]] = NULL
QBComplete[["player.1"]] = NULL
QBComplete[["Player.1"]] = NULL
QBComplete[["Position"]] = NULL
QBComplete[["Team"]] = NULL
#Converting to %
#http://stackoverflow.com/questions/8329059/how-to-convert-character-of-percentage-into-numeric-in-r
QBComplete$Off.Snap.Pct <- as.numeric(sub("%","",QBComplete$Off.Snap.Pct))/100
#Reordering
QBComplete <- QBComplete[,c(1:3,18,23,19,4,21,22,5:17)]

#Filtering QB on Rushing Stats
Mobile <- pbp2014 %>%
  filter(PlayType=="Run") %>%
  mutate(QB.team = paste(Rusher,posteam,sep=" ")) %>%
  group_by(QB.team, posteam, posteamcolour) %>%
  summarise(Games.Played = n_distinct(GameID),
            Total.Runs = n(),
            Total.Yards = sum(Yards.Gained),
            Longest = max(Yards.Gained),
            Touchdown = sum(Touchdown),
            Fumbles = sum(Fumble),
            Run.PG = Total.Runs/Games.Played,
            Yards.PA = Total.Yards/Total.Runs)%>%
  filter(QB.team %in% starters.team) %>%
  arrange(QB.team)

install.packages('ape')
library(ape)
#Reading in data file
combine <- combine%>%
  filter(position == "QB")%>%
  filter(f.name %in% starters)
combine <- combine[-32,]
combine <- combine[-28,]

combine <- combine %>%
  filter(year < 2015)%>%
  #mutate(NameY = paste(f.name, year, sep = " - "))%>%
  mutate(heightinchestotal = (heightinchestotal*2.54)) %>%
  mutate(broad = broad/10)

#Rename column by name: change "beta" to "two"
names(combine)[names(combine)=="heightinchestotal"] <- "HeightInCM"

combine[is.na(combine)] <- 0

#Deleting Columns not needed
combine[["firstname"]] = NULL
combine[["lastname"]] = NULL
combine[["heightfeet"]] =NULL
combine[["heightinches"]] = NULL
combine[["twentyyd"]] = NULL
combine[["pick"]] = NULL
combine[["nflgrade"]] = NULL
combine[["X.1"]] = NULL
combine[["X.2"]] = NULL
combine[["X"]] = NULL
#combine[["f.name"]] = NULL
combine[["NameY.1"]] = NULL

#Changing To Factors/Numeric/INT
combine$college = as.factor(combine$college)
combine$position = as.factor(combine$position)

cluster1 <- subset(combine, select =c("f.name","HeightInCM","weight","fortyyd", "vertical"))
clu <- sort(starters)
QB.Rating$Name = clu
Mobile$Name = clu
cluster2 <- merge(cluster1, QB.Rating, by.x = "f.name", by.y = "Name", all = F)
cluster3 <- merge(cluster2, Mobile, by.x = "f.name", by.y = "Name", all = F)
#Rearrange
cluster4<- cluster3[,c(1:5,10:22,27:33)]
cluster4 <- cluster4 %>%
  filter(Total.Passes > 250)

library(plyr)
cluster4$vertical <- as.character(cluster4$vertical)
cluster4$vertical<- revalue(cluster4$vertical, c("0"="NA"))
cluster4$vertical <- as.numeric(cluster4$vertical)
cluster4$fortyyd <- as.character(cluster4$fortyyd)
cluster4$fortyyd<- revalue(cluster4$fortyyd, c("0"="NA"))
cluster4$fortyyd <- as.numeric(cluster4$fortyyd)
#
install.packages("VIM")
library(VIM)
#KNN Algorithm
cluster5<-kNN(cluster4 ,k=3,variable = c("vertical", "fortyyd"), dist_var = c("fortyyd", "vertical", "HeightInCM"))
cluster5<- cluster5[,c(1:25)]
#Change Row names from 1,2,3 To Player names
# prepare hierarchical cluster
library(tidyverse)
cluster5 <- cluster5 %>% remove_rownames %>% column_to_rownames(var="f.name")
hc = hclust(dist(cluster5))
plot(hc, hang=-1, xlab="", sub="")
hcd = as.dendrogram(hc, h=200)
plot(hcd)
labelColors = c("#CDB380", "#036564", "#EB6841", "#EDC951")
clusMember = cutree(hc,4)
#https://rstudio-pubs-static.s3.amazonaws.com/1876_df0bf890dd54461f98719b461d987c3d.html
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}

# using dendrapply
hc
clusDendro = dendrapply(hcd, colLab)
# make plot
plot(clusDendro,main = "h", type = "triangle", center = TRUE, horiz = T)
plot(as.phylo(hc), cex = 1.2, label.offset = 1,type = "cladogram", tip.color = labelColors[clusMember])
Example <- plot(as.phylo(hc), tip.color = labelColors[clusMember], type = "fan")
#Exporting Grpahs To JPEG High Res
jpeg("Dd.jpeg", width = 12, height = 9, units = 'in', res = 2200)
plot(clusDendro,main = "", type = "triangle", center = TRUE, horiz = T)
dev.off()
plot(as.phylo(hc), type = "unrooted")

#Exporting Grpahs To JPEG High Res
jpeg("fan.jpeg", width = 9, height = 9, units = 'in', res = 2200)
plot(as.phylo(hc), tip.color = labelColors[clusMember], type = "fan")
dev.off()

#Change Row names from 1,2,3 To Player names
QBComplete <- QBComplete %>% remove_rownames %>% column_to_rownames(var="QB.team")
#Taking Variables needed for model building
QBModel <- QBComplete[,c(4:21)]
QBModel <- QBModel[,c(1,10,7,17)]
pairs(QBModel)
# Create Training and Test data -
set.seed(87) # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(QBModel), 0.8*nrow(QBModel))  # row indices for training data
trainingData <- QBModel[trainingRowIndex, ]  # model training data
testData  <- QBModel[-trainingRowIndex, ]   # test data

#Build Model to predict Yearly salary based on trainingdata
model <- lm(Yearly ~ ., data = trainingData)
#summary model
summary(model)

#Predict our model using the 20% Test set
Pred <- predict(model, testData)
#Predict dataframe
actuals_preds <- data.frame(cbind(actuals=testData$Yearly, predicteds=Pred))
actuals_preds$ROI.Pct = ((actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)*100
options(digits = 3)
correlation_accuracy <- cor(actuals_preds)  #12.7%
head(actuals_preds)
plot(actuals_preds$predicteds, actuals_preds$actuals)
cor(QBModel)


#Visualize some diagnostics
plot(model)
# Check residuals
model.resd = rstandard(model)
model.resd
plot( model.resd, col="red")
abline(0,0)
plot(model)

#Coefficient values
coef(model)
plot
AIC(model)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)


Brady = -60621431+(684435)*64.2+(2293672)*11.75+(1459305)*4
Brady

##Diagnostic parameters-

install.packages("DAAG")
library(DAAG)
cvResults <- suppressWarnings(CVlm(data = QBModel, form.lm = Yearly ~ ., m=8, dots=FALSE, seed=87, legend.pos="topleft",  printit=FALSE))
attr(cvResults, 'ms')
