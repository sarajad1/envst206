ch4 <- read.csv("/users/sara/documents/ESdata/lemming_herbivory.csv")

#change to factor
ch4$herbivory <- as.factor(ch4$herbivory)

#methane flux depends on herbivory
plot(ch4$CH4_Flux ~ ch4$herbivory, xlab="Treatment",
     ylab="CH4 fluxes (mgC m -2 day-1")

#shaprio wilk test to test normality
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

#bartlett test to see if variances are equal
#order important, dependent variable
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

#running t-test
t.test(ch4$CH4_Flux ~ ch4$herbivory)

#learning about t-test
help("t.test")

#read insect data
datI <- read.csv("/users/sara/documents/ESdata/insect_richness.csv")
datI$urbanName <- as.factor(datI$urbanName)

#shapiro wilk test to test normality
shapiro.test(datI$Richness[datI$urbanName == "Suburban"])
shapiro.test(datI$Richness[datI$urbanName == "Dense"])
shapiro.test(datI$Richness[datI$urbanName == "Natural"])
shapiro.test(datI$Richness[datI$urbanName == "Developed"])

#bartlett test for equal variance
bartlett.test(datI$Richness ~ datI$urbanName)

#specify linear model
in.mod <- lm(datI$Richness ~ datI$urbanName)
#running ANOVA
in.aov <- aov(in.mod)
#print anova table
summary(in.aov)

#run tukey HSD post-hoc
tukeyT <- TukeyHSD(in.aov)
tukeyT

#plot it
plot(tukeyT, cex.axis=0.75)

tapply(datI$Richness, datI$urbanName, "mean")

#setting up contigency table
species <- matrix(c(18,8,15,32), ncol=2, byrow=TRUE)
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")
species

#making a mosaic plot
mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")

#calculating expected values
total <- 18+8+15+32
#expected value for not protected declining
(18+8)*(18+15)/total
#expected value for not protected stable/increase
(18+15)*(15+32)/total
#expected value for protected decline
(8+18)*(8+32)/total
#expected value for protected stable/increase
(15+32)*(8+32)/total

expectedvalues <- matrix(c(11.75,21.25,14.25,25.75), ncol=2, byrow=TRUE)
colnames(expectedvalues) <- c("Not protected", "Protected")
rownames(expectedvalues) <- c("Declining", "Stable/Increase")
expectedvalues

#conducting chi squared test
chisq.test(species)

