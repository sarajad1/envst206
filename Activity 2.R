#activity 2

#make a vector of tree heights in meters
heights <- c(30,41,20, 22)

#convert to cm
heights_cm <- heights*100

#look at first tree height
heights[1]

#look at 2nd nd 3rd tree heights
heights[2:3]

#reading matrix info
help("matrix")

#set up first matrix with 2 columns and fill in by rows
mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
mat

#set up matrix filled by columns
mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
mat.bycol

#subset matrix to look at row 1, column 2
mat.bycol[1,2]

#look at all values in row 1
mat.bycol[1,]

#look at all values in column 2
mat.bycol[,2]

#data frame
datW <- read.csv("/users/sara/documents/ESData/noaa2011124.csv")
datW

#more info about dataframe
str(datW)

#converting names to factors
datW$NAME <- as.factor(datW$NAME)

#character vector
charvec <- c("banana","strawberry","apple","pear","blueberry")
charvec

#integer vector
intvec <- c(2,4,6,8,10)
intvec

#numeric vector
numvec <- c(1.1,1.2,1.3,1.4,1.5)
numvec

#find out all unique site names
levels(datW$NAME)

#mean max temperature for aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
#add na.rm argument to ignore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#standard deviation of max temp
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm = TRUE)

#average daily temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#aggregate function to get mean across all sites
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change column names
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data
datW$siteN <- as.numeric(datW$NAME)

hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col="grey75",
     border="white")

help(hist)

hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE,
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col="grey75",
     border="white")

hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE,
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col="grey75",
     border="white")

hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col="grey75",
     border="white")

#learning about dnorm
help(dnorm)

#using pnorm for freezing temps
pnorm(0, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE),
      sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))
#pnorm for 5 degrees
pnorm(5, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE),
      sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))
#pnorm to find probability of temps between 0-5
pnorm(5, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE),
      sd(datW$TAVE[datW$siteN==1],na.rm=TRUE)) - pnorm(0, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE),
                                                       sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))
#pnorm to get area of curve above 20
1 - pnorm(20, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE),
        sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))

#qnorm to get probability of 95th quantile
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#labeling site 1 mean
site1mean <- mean(datW$TAVE[datW$siteN==1], na.rm=TRUE)
site1mean

#Mean increased by 4 degrees
ccsite1mean <- site1mean + 4
ccsite1mean

#probability of observing temperatures greater than current threshold
1 - pnorm(18.51026, ccsite1mean, sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))


#histogram of daily precipitation for Aberdeen in cm
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily precipitation (cm)",
     ylab = "Relative frequency",
     col="grey75",
     border="white")

#total annual precipitation per site
PRCPtotal <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum",na.rm=TRUE)
PRCPtotal

colnames(PRCPtotal) <-c("NAME","YEAR","PRCP")
PRCPtotal

#histogram of annual precip for Aberdeen
hist(PRCPtotal$PRCP[PRCPtotal$NAME == "ABERDEEN, WA US"],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Annual precipitation (cm)",
     ylab = "Relative frequency",
     col="grey75",
     border="white")

#histogram of annual precip for Mandan
hist(PRCPtotal$PRCP[PRCPtotal$NAME == "MANDAN EXPERIMENT STATION, ND US"],
     freq=FALSE,
     main = paste(levels(datW$NAME)[3]),
     xlab = "Annual precipitation (cm)",
     ylab = "Relative frequency",
     col="grey75",
     border="white")

#labeling
mandandat <- PRCPtotal$PRCP[PRCPtotal$NAME == "MANDAN EXPERIMENT STATION, ND US"]
aberdeendat <- PRCPtotal$PRCP[PRCPtotal$NAME == "ABERDEEN, WA US"]

#Likelihood of 70cm or less in Aberdeen
pnorm(70, mean(aberdeendat, na.rm=TRUE), sd(aberdeendat,na.rm=TRUE))

#likelihood of 70cm or less in Mandan
pnorm(70, mean(mandandat, na.rm = TRUE), sd(mandandat, na.rm=TRUE))

