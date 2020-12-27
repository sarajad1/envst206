#load in california 2017 yield data
ag2017 <- read.csv("/users/sara/downloads/2017ag.csv")
#load in california 2018 yield data
ag2018 <- read.csv("/users/sara/downloads/2018ag.csv")

#look at just almonds all
dat17 <- ag2017[ag2017$Commodity.Code == c(261999),]
dat18 <- ag2018[ag2018$Commodity.Code == c(261999),]

#dataframe with just year, crop, county, yield, unit
dat2017 <- na.omit(data.frame(year=dat17$Year,
                           crop=dat17$Crop.Name,
                           crop.code=dat17$Commodity.Code,
                           county.code=dat17$County.Code,
                           county=dat17$County,
                           yield=dat17$Yield,
                           unit=dat17$Unit))
dat2018 <- na.omit(data.frame(year=dat18$Year,
                              crop=dat18$Crop.Name,
                              crop.code=dat18$Commodity.Code,
                              county.code=dat18$County.Code,
                              county=dat18$County,
                              yield=dat18$Yield,
                              unit=dat18$Unit))
#install packages
library(sp)
library(rgdal)
library(dplyr)
library(ggplot2)

#joining almond data for 2017 and 2018
agAll <- full_join(dat2017,dat2018)

#deleting counties that didn't produce both years
agAll <- agAll[-c(19, 40, 21, 29, 18, 39), ]
#making year into a character variable
agAll$year <- as.character(agAll$year)


#plot almond yield for 2017 and 2018 per county
ggplot(agAll, aes(x=county, y=yield, fill=year))+
  geom_bar(stat="identity", position=position_dodge2())+
  labs(x="County", y="Yield in tons")+
  theme_classic()


#reformatting almond data set to group yields by county
agAlmond <- full_join(dat2017,dat2018, by="county")
agAlmonds <- na.omit(data.frame(Crop=agAlmond$crop.x,
                                County=agAlmond$county,
                                yield.x=agAlmond$yield.x,
                                yield.y=agAlmond$yield.y))
colnames(agAlmonds) <- c("Crop", "County", "2017 Yield", "2018 Yield")

#adding column with % change in almond yield between 2017 and 2018
agAlmonds$diff <- ((agAlmonds$`2018 Yield`-agAlmonds$`2017 Yield`)/agAlmonds$`2018 Yield`)*100
#removing "sum of all others" row
agAlmonds <- agAlmonds[-c(18), ]


#read in drought data
drought2017 <- read.csv("/users/sara/downloads/dm_export_20170101_20171231.csv")
drought2018 <- read.csv("/users/sara/downloads/dm_export_20180101_20181231.csv")

#calculating average % of county in moderate drought (D1) for 2017
avg2017D1 <- aggregate(drought2017$D1, by=list(drought2017$County), FUN="mean", na.rm=TRUE)
colnames(avg2017D1) <- c("County","Average.D1")

#calculating average % of county in moderate drought (D1) for 2018
avg2018D1 <- aggregate(drought2018$D1, by=list(drought2018$County), FUN="mean", na.rm=TRUE)
colnames(avg2018D1) <- c("County","Average.D1")

#joining 2017 and 2018 drought data
datDR <- full_join(avg2017D1, avg2018D1, by="County")
colnames(datDR) <- c("County","D1.2017","D1.2018")

#add column for percent change in drought levels between two years
datDR$Percent.Change <- ((datDR$D1.2018-datDR$D1.2017)/datDR$D1.2018)*100

#only looking at counties that produced almonds
datD <- datDR[c(4,6,10,11,15,16,20,24,34,39,48,50,51,52,54,57,58), ]

#removing the "county" from each name
datD$County <- ifelse(datD$County == "Butte County",
                      "Butte",
                      ifelse( datD$County == "Colusa County",
                             "Colusa",
                     ifelse( datD$County == "Fresno County",
                                     "Fresno",
                      ifelse( datD$County == "Glenn County",
                                       "Glenn",
                      ifelse( datD$County == "Kern County",
                                       "Kern",
                      ifelse( datD$County == "Kings County",
                                      "Kings",
                      ifelse( datD$County == "Madera County",
                                       "Madera",
                      ifelse( datD$County == "Merced County",
                              "Merced",
                     ifelse( datD$County == "Sacramento County",
                                      "Sacramento",
                     ifelse( datD$County == "San Joaquin County",
                                     "San Joaquin",
                   ifelse( datD$County == "Solano County",
                                             "Solano",
                    ifelse( datD$County == "Stanislaus County",
                                   "Stanislaus",
                    ifelse( datD$County == "Sutter County",
                                           "Sutter",
                   ifelse( datD$County == "Tehama County",
                                    "Tehama",
                  ifelse( datD$County == "Tulare County",
                                            "Tulare",
                 ifelse( datD$County == "Yolo County",
                                  "Yolo",
                    ifelse( datD$County == "Yuba County",
                                          "Yuba",
                  as.character(datD$County))))))))))))))))))

#reformatting drought data set to be just county and %change          
droughtdiff <- data.frame(County=datD$County,
                          Percent.Change.Drought=datD$Percent.Change)
#making county a character
droughtdiff$County <- as.character(droughtdiff$County)

#reformatting yield data set to be just county and % change
almdiff <- data.frame(County=agAlmonds$County,
                    Percent.Change.Almonds=agAlmonds$diff)
almdiff$County <- as.character(almdiff$County)

#joining yield data and drought data
datP <- data.frame(County=droughtdiff$County,
                   Percent.Change.Drought=droughtdiff$Percent.Change.Drought,
                   Percent.Change.Almonds=almdiff$Percent.Change.Almonds)

#scatter plot of % change in drought from 2017-2018 and % change in yield from 2017-2018
plot(datP$Percent.Change.Drought, datP$Percent.Change.Almonds,
     pch = 19, 
     col = "royalblue4",
     ylab = "Percent Change in Yield",
     xlab =  "Percent Change in Drought")

#setting up regression
perchange <- lm(datP$Percent.Change.Almonds ~ datP$Percent.Change.Drought)
perchange.res <- rstandard(perchange)

#qq plot to check normality of residuals
qqnorm(perchange.res)
qqline(perchange.res)

#residuals plot
plot(datP$Percent.Change.Drought, perchange.res,
     xlab = "Percent Change in Drought",
     ylab = "standardized residual")
abline(h=0)

#results of linear regression
summary(perchange)

#plot with regression line
plot(datP$Percent.Change.Drought, datP$Percent.Change.Almonds,
     pch = 19, 
     col = "royalblue4",
     ylab = "Percent Change in Yield",
     xlab =  "Percent Change in Drought")
abline(perchange, lwd=2)
