datB <- read.csv("/Users/sara/downloads/beaver_dam.csv")

plot(datB$dams.n, datB$area.ha, pch = 19, ylab = "Surface water area (ha)", 
     xlab = "Number of beaver dams")

#setting up regression
dam.mod <- lm(datB$area.ha ~ datB$dams.n)

#naming vector of residuals
dam.res <- rstandard(dam.mod)

#checking assumptions
qqnorm(dam.res)
qqline(dam.res)
shapiro.test(dam.res)

#residual plot
plot(datB$dams.n, dam.res, xlab = "beaver dams", ylab = "standardized residual")
abline(h= 0)

summary(dam.mod)

#plot of beaver dams and surface water
plot(datB$dams.n, datB$area.ha, pch = 19,
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab = "Number of beaver dams")
abline(dam.mod, lwd=2)

#read phenology data
pheno <- read.csv("/users/sara/documents/ESdata/red_maple_pheno.csv")

#panel of plots
par(mfrow=c(1,2))
plot(pheno$Tmax, pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Maximum temperature (C)")
plot(pheno$Prcp,pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Precipitation (mm)")
plot(pheno$Lat, pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Latitude")
plot(pheno$elev, pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Elevation (ft)")
plot(pheno$Tmin, pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Minimum temperature (C)")

pheno$siteDesc <- as.factor(pheno$siteDesc)

plot(pheno$siteDesc, pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Type of Land")

#plotting multicollinearity
plot ( ~ pheno$Lat + pheno$Tmax + pheno$Tmin + pheno$Prcp + pheno$elev + pheno$siteDesc)

#coding urban/rural as a variable
pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)
help(ifelse)

#setting up multiple regression
mlr <- lm(pheno$doy ~ pheno$Tmax + pheno$Prcp + pheno$elev + pheno$urID)
mlr

mlFitted <- fitted(mlr)

#getting residuals
mlr.res <- rstandard(mlr)

#qq plot
qqnorm(mlr.res)
#add qq line
qqline(mlr.res)

#residuals plot
plot(mlFitted, mlr.res,
     xlab = "fitted values",
     ylab = "standardized residual")
abline(h=0)

#interpreting regression
summary(mlr)


