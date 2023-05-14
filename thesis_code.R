setwd("~/Desktop/CBS fin/Semester 4 - thesis/data")

IMF <- read.csv("WEO_Data.csv")
names <- colnames(IMF)
names_new <- gsub("^X", "", names)
colnames(IMF) <- names_new
IMF$Subject.Descriptor[IMF$Subject.Descriptor=="Gross domestic product, constant prices"] <- "GDP growth %"
# this is nominal, I must calc growth - inflation to get real GDP growth
IMF$Subject.Descriptor[IMF$Subject.Descriptor=="Gross domestic product, current prices"] <- "GDP $bn"
IMF$Subject.Descriptor[IMF$Subject.Descriptor=="Gross national savings"] <- "Gross national savings % of GDP"
IMF$Subject.Descriptor[IMF$Subject.Descriptor=="Inflation, average consumer prices"] <- "Inflation %"
IMF$Subject.Descriptor[IMF$Subject.Descriptor=="Volume of exports of goods and services"] <- "Exports growth %"
IMF$Subject.Descriptor[IMF$Subject.Descriptor=="General government total expenditure"] <- "Gov expenditure % of GDP"
IMF$Subject.Descriptor[IMF$Subject.Descriptor=="General government net lending/borrowing"] <- "Gov net lending/borrowing % of GDP"
IMF$Subject.Descriptor[IMF$Subject.Descriptor=="General government gross debt"] <- "Gov gross debt % of GDP"
IMF$Subject.Descriptor[IMF$Subject.Descriptor=="Current account balance"] <- "Current account balance % of GDP"
tail(IMF)
IMF <- IMF[-c(1765, 1766),-4]
library(tidyr)
library(dplyr)
IMF_new <- IMF %>%pivot_longer(cols = c(4:44), names_to = "Year", values_to = "Value")%>%pivot_wider(names_from = "Subject.Descriptor", values_from = "Value")
summary(IMF_new)

library(WDI)
# all_indicators <- WDIsearch()
# FDI net inflows to private sector: BX.KLT.DINV.WD.GD.ZS
# Domestic credit to private sector: FS.AST.PRVT.GD.ZS
# Trade openness:                   NE.TRD.GNFS.ZS
# Net claims on central gov:        FS.AST.CGOV.GD.ZS
# bank assets to GDP (%):           GFDD.DI.02
WB <- WDI(country="all", indicator=c("BX.KLT.DINV.WD.GD.ZS","FS.AST.PRVT.GD.ZS", "NE.TRD.GNFS.ZS", "FS.AST.CGOV.GD.ZS"), start=1970, end=2020)
WB$iso2c <- NULL
names(WB) <- c("Country", "ISO", "Year", "FDI/GDP%", "Credit to private/GDP%", "Trade openness", "Claims on gov/GDP%")
WB[WB == ""] <- NA
WB <- WB[complete.cases(WB$ISO),]
WB2 <- WDI(country="all", indicator=c("GFDD.DI.02"), start=1980, end=2020)
WB2$iso2c <- NULL
names(WB2) <- c("Country", "ISO", "Year", "bank assets/GDP%")
unique(WB2[WB2$ISO=="",1])
WB2[WB2$Country=="Congo, Dem. Rep.","ISO"] <- "COD"
WB2[WB2$Country=="Serbia","ISO"] <- "SRB"
WB2[WB2$Country=="Timor-Leste","ISO"] <- "TLS"
WB2[WB2$Country=="West Bank and Gaza","ISO"] <- "WBG"
WB2[WB2$Country=="Yemen, Rep.","ISO"] <- "YEM"
WB2[WB2 == ""] <- NA
WB2 <- WB2[complete.cases(WB2$ISO),]
WB_new <- merge(WB, WB2[2:4], by.x = c("ISO", "Year"), all.x = TRUE)
d1 <- as.data.frame(setdiff(IMF_new$ISO, WB_new$ISO))
names(d1) <- "ISO"
d1 <- merge(d1, unique(IMF_new[1:2]), by = "ISO", all.x = TRUE)
WB_new$ISO <- ifelse(WB_new$ISO == "XKX", "UVK", WB_new$ISO)
WB_new$ISO <- ifelse(WB_new$ISO == "PSE", "WBG", WB_new$ISO)

bbg <- read.csv("bbggdp.csv")
names <- colnames(bbg)
names_new <- gsub("^X", "", names)
colnames(bbg) <- names_new
bbg <- bbg[,-2]
bbg <- merge(unique(WB_new[c("Country", "ISO")]),bbg, by.y = "Country", all.y = TRUE)
bbg$ISO[bbg$Country == "Bahamas"] <- "BHS"
bbg$ISO[bbg$Country == "Congo Democratic Republic"] <- "COD"
bbg$ISO[bbg$Country == "Egypt"] <- "EGY"
bbg$ISO[bbg$Country == "Iran"] <- "IRN"
bbg$ISO[bbg$Country == "Macao"] <- "MAC"
bbg$ISO[bbg$Country == "Micronesia in Current USD"] <- "FSM"
bbg$ISO[bbg$Country == "Sao Tome & Principe"] <- "STP"
bbg$ISO[bbg$Country == "St Lucia"] <- "LCA"
bbg$ISO[bbg$Country == "Swaziland"] <- "SWZ"
bbg$ISO[bbg$Country == "Trinidad & Tobago"] <- "TTO"
bbg$ISO[bbg$Country == "Venezuela"] <- "VEN"
bbg$ISO[bbg$Country == "Yemen"] <- "YEM"
bbg$ISO[bbg$Country == "Cape Verde"] <- "CPV"
bbg$ISO[bbg$Country == "Czech Republic"] <- "CZE"
bbg$ISO[bbg$Country == "Gambia"] <- "GMB"
bbg$ISO[bbg$Country == "Hong Kong"] <- "HKG"
bbg$ISO[bbg$Country == "Korea"] <- "KOR"
bbg$ISO[bbg$Country == "Macedonia"] <- "MKD"
bbg$ISO[bbg$Country == "Republic of Congo"] <- "COG"
bbg$ISO[bbg$Country == "St Kitts & Nevis"] <- "KNA"
bbg$ISO[bbg$Country == "St Vincent & the Grenadines"] <- "VCT"
bbg$ISO[bbg$Country == "Taiwan"] <- "TWN"
bbg$ISO[bbg$Country == "Turkey"] <- "TUR"
bbg$ISO[bbg$Country == "US"] <- "USA"
d1 <- as.data.frame(setdiff(IMF_new$ISO, bbg$ISO))
names(d1) <- "ISO"
d1 <- merge(d1, unique(IMF_new[1:2]), by = "ISO", all.x = TRUE)
bbg <- pivot_longer(bbg, cols = c(3:45), names_to = "Year", values_to = "GDP bn")

IMF_new <- merge(IMF_new, bbg[-1], by = c("ISO", "Year"), all.x = TRUE)
IMF_new <- IMF_new[,-5]

library(foreign)
politi <- read.spss("p5v2018.sav", to.data.frame=TRUE)
politi <- politi[politi$year >= 1980, -c(1:3)] #only available until 2018
names(politi)[1:3]<- c( "ISO", "Country", "Year")
#later choose indicators fragment, polity2, durable
d1 <- as.data.frame(setdiff(IMF_new$ISO, politi$ISO))
names(d1) <- "ISO"
d1 <- merge(d1, unique(IMF_new[1:2]), by = "ISO", all.x = TRUE)
politi <- politi[c(1,2, 3,5,9,10)]
politi$Country <- trimws(politi$Country)
politi2 <- merge(politi, unique(IMF_new[c(1,3)]), by = "Country", all.x = TRUE)
names(politi2)[7] <- "ISO"
unique(politi2[is.na(politi2$ISO), 1])
politi2$ISO[politi2$Country == "Bosnia"] <- "BIH"
politi2$ISO[politi2$Country == "Congo Kinshasa"] <- "COD"
politi2$ISO[politi2$Country == "Congo Brazzaville" ] <- "COG"
politi2$ISO[politi2$Country == "Congo-Brazzaville"] <- "COG"
politi2$ISO[politi2$Country == "Ivory Coast" ] <- "CIV"
politi2$ISO[politi2$Country == "Cote D'Ivoire"] <- "CIV"
politi2$ISO[politi2$Country == "Kyrgyzstan"] <- "KGZ"
politi2$ISO[politi2$Country == "Myanmar (Burma)"] <- "MMR"
politi2$ISO[politi2$Country == "Sudan-North"] <- "SDN"
politi2$ISO[politi2$Country == "South Sudan"] <- "SSD"
politi2$ISO[politi2$Country == "Sudan"] <- "SDN"
politi2$ISO[politi2$Country == "Timor Leste"] <- "TLS"
politi2$ISO[politi2$Country == "Cape Verde"] <- "CPV"
politi2$ISO[politi2$Country == "Gambia"] <- "GMB"
politi2$ISO[politi2$Country == "Germany West"] <- "DEU"
politi2$ISO[politi2$Country == "Korea South"] <- "KOR"
politi2$ISO[politi2$Country == "Laos"] <- "LAO"
politi2$ISO[politi2$Country == "Macedonia"] <- "MKD"
politi2$ISO[politi2$Country == "Vietnam North" ] <- "VNM"
politi2$ISO[politi2$Country == "Swaziland"] <- "SWZ"
politi2$ISO[politi2$Country == "Taiwan"] <- "TWN"
politi2$ISO[politi2$Country == "Turkey"] <- "TUR"
politi2$ISO[politi2$Country == "UAE"] <- "ARE"
politi2$ISO[politi2$Country == "Iran"] <- "IRN"
politi <- politi2[,c(7,1,3:6)]


# singular indicators
gbyields <- read.csv("10y bond yields OECD.csv")
gbyields <- gbyields[,c(1, 2, 9, 17)]
names(gbyields) <- c( "ISO", "Country", "Year", "Bond 10y yields%")
#many missing values

gbryields <- read.csv("gov yields IMF.csv")
names <- colnames(gbryields)
names_new <- gsub("^X", "", names)
colnames(gbryields) <- names_new
gbryields <- gbryields %>%pivot_longer(cols = c(2:62), names_to = "Year", values_to = "Real 10y bond yields%")
names(gbryields)[1] <- c("Country")
gbryields <- merge(gbryields, unique(IMF_new[c("Country", "ISO")]), by.x = "Country", all.x = TRUE)
gbryields <- gbryields[, c(4, 1:3)]

fxres <- read.csv("abs reserves-gold WB.csv")
names <- colnames(fxres)
names_new <- gsub("^X", "", names)
colnames(fxres) <- names_new
fxres <- fxres %>%pivot_longer(cols = c(3:63), names_to = "Year", values_to = "Reserves")
names(fxres) <- c( "Country", "ISO", "Year", "Reserves")
fxres <- fxres[complete.cases(fxres$ISO),]
fxres$ISO <- ifelse(fxres$ISO == "XKX", "UVK", fxres$ISO)
fxres$ISO <- ifelse(fxres$ISO == "PSE", "WBG", fxres$ISO)

reer <- read.csv("REER WB.csv")
years <- substr(names(reer)[5:66], 2, 5)
names(reer)[5:66] <- years
reer <- reer %>%pivot_longer(cols = c(5:66), names_to = "Year", values_to = "REER")
reer <- reer[,3:6]
names(reer) <- c( "Country", "ISO", "Year", "REER")
reer[reer == ".."] <- NA
reer[reer == ""] <- NA
reer <- reer[complete.cases(reer$ISO),]
reer3 <- reer
reer2 <- reer[,-1]%>%pivot_wider(names_from = "ISO", values_from = "REER")
reer2 <- reer2[,-c(268, 269)]
library(zoo)
rolling_deviation <- function(x) {
  percentage_deviation_matrix <- matrix(NA, nrow(x), ncol(x) - 1)
  for (j in 2:ncol(x)) {
    for (i in 5:nrow(x)) {
      deviation <- 100 * (x[i, j] - mean(x[(i-4):(i), j], na.rm = TRUE)) / mean(x[(i-4):(i), j], na.rm = TRUE)
      percentage_deviation_matrix[i, j - 1] <- deviation
    }
  }
  result <- cbind(x[, 1], percentage_deviation_matrix)
  colnames(result) <- colnames(x)
  return(result)
}
reer2 <- apply(reer2, c(1,2), as.numeric)
reer2 <- reer2[, colSums(!is.na(reer2)) > 0]
reer2 <- as.data.frame(reer2)
# Apply function to each column
reer2_dev <- rolling_deviation(reer2)
reer2_dev <- as.data.frame(reer2_dev)
reer <- reer2_dev %>%pivot_longer(cols = c(2:96), names_to = "ISO", values_to = "REER % dev")
reer <- merge(reer, unique(reer3[1:2]), by = "ISO", all.x = TRUE)
#many missing values

M2r <- read.csv("broad money to reserves WB.csv")
names <- colnames(M2r)
names_new <- gsub("^X", "", names)
colnames(M2r) <- names_new
colnames(M2r)[1:2] <- c("Country", "ISO")
M2r <- M2r%>%pivot_longer(cols = c(3:63), names_to = "Year", values_to = "M2/R")
names(M2r) <- c( "Country", "ISO", "Year", "M2/R")
M2r <- M2r[complete.cases(M2r$ISO),]
M2r$ISO <- ifelse(M2r$ISO == "XKX", "UVK", M2r$ISO)
M2r$ISO <- ifelse(M2r$ISO == "PSE", "WBG", M2r$ISO)

imfcredit <- read.csv("IMF credit abs.csv")
names <- colnames(imfcredit)
names_new <- gsub("^X", "", names)
colnames(imfcredit) <- names_new
imfcredit <- imfcredit%>%pivot_longer(cols = c(4:64), names_to = "Year", values_to = "IMF credit USD")
imfcredit <- imfcredit[,-3]
names(imfcredit) <- c( "Country", "ISO", "Year", "IMF credit USD")
imfcredit <- imfcredit[complete.cases(imfcredit$ISO),]
imfcredit$ISO <- ifelse(imfcredit$ISO == "XKX", "UVK", imfcredit$ISO)
imfcredit$ISO <- ifelse(imfcredit$ISO == "PSE", "WBG", imfcredit$ISO)

rGDPg <- read.csv("real GDP g IMF.csv")
names <- colnames(rGDPg)
names_new <- gsub("^X", "", names)
colnames(rGDPg) <- names_new
rGDPg <- rGDPg%>%pivot_longer(cols = c(2:62), names_to = "Year", values_to = "Real GDP growth%")
names(rGDPg)[1] <- "Country"
rGDPg <- merge(rGDPg, unique(IMF_new[c("Country", "ISO")]), by = "Country", all.x = TRUE)
rGDPg <- rGDPg[, c(4, 1:3)]

intofGDP <- read.csv("interest paid pc GDP IMF.csv")
names <- colnames(intofGDP)
names_new <- gsub("^X", "", names)
colnames(intofGDP) <- names_new
intofGDP <- intofGDP%>%pivot_longer(cols = c(2:62), names_to = "Year", values_to = "Interest paid on public debt / GDP%")
names(intofGDP)[1] <- "Country"
intofGDP <- merge(intofGDP, unique(IMF_new[c("Country", "ISO")]), by = "Country", all.x = TRUE)
intofGDP <- intofGDP[, c(4, 1:3)]


# library(dplyr)
# library(writexl)
# iso_guide <- IMF_new %>% distinct(ISO, Country)
# write_xlsx(iso_guide, "ISO guide.xlsx")

# crises <- read.csv("crises.csv")
# names <- colnames(crises)
# names_new <- gsub("^X", "", names)
# colnames(crises) <- names_new

crisisCA <- read.csv("default.csv")
names <- colnames(crisisCA)
names_new <- gsub("^X", "", names)
colnames(crisisCA) <- names_new
crisisCA <- merge(unique(IMF_new[c("Country", "ISO")]), crisisCA, by.y = "Country", all.y = TRUE)
unique(crisisCA[is.na(crisisCA$ISO),1])
crisisCA$ISO[crisisCA$Country == "Bahamas"] <- "BHS"
crisisCA$ISO[crisisCA$Country == "Dem. Rep. of Congo (Kinshasa)"] <- "COD"
crisisCA$ISO[crisisCA$Country == "Bosnia & Herzegovina "] <- "BIH"
crisisCA$ISO[crisisCA$Country == "Iran"] <- "IRN"
crisisCA$ISO[crisisCA$Country == "Côte d’Ivoire"] <- "CIV"
crisisCA$ISO[crisisCA$Country == "Dominica "] <- "DMA"
crisisCA$ISO[crisisCA$Country == "São Tomé and Príncipe"] <- "STP"
crisisCA$ISO[crisisCA$Country == "West Bank & Gaza"] <- "WBG"
crisisCA$ISO[crisisCA$Country == "eSwatini (Swaziland)"] <- "SWZ"
crisisCA$ISO[crisisCA$Country == "Trinidad & Tobago"] <- "TTO"
crisisCA$ISO[crisisCA$Country == "Laos"] <- "LAO"
crisisCA$ISO[crisisCA$Country == "Rep. Of Congo (Brazzaville)"] <- "COG"
crisisCA$ISO[crisisCA$Country == "St. Kitts & Nevis"] <- "KNA"
crisisCA$ISO[crisisCA$Country == "St. Vincent and the Grenadines "] <- "VCT"
crisisCA$ISO[crisisCA$Country == "USSR/Russia" ] <- "RUS"
crisisCA$ISO[crisisCA$Country == "Turkey"] <- "TUR"
crisisCA <- crisisCA%>%pivot_longer(cols = c(3:64), names_to = "Year", values_to = "default mil")
crisisCA <- merge(crisisCA, bbg[-1], by = c("ISO", "Year"), all.x = TRUE)
crisisCA[,"defaulted %GDP"] <- crisisCA[,4]/(crisisCA[,5]*10)
crisisCA[,"defaulted %GDP"] <- round(crisisCA[,"defaulted %GDP"],3)
summary(crisisCA)
# crisisCA <- merge(crisisCA, crises[c(1,3,6)], by = c("ISO", "Year"), all.x = TRUE)
# aa <- read.csv("default.csv")
# aa <- aa[rowSums(is.na(aa)) != 7, ]
# ab <- crisisCA[crisisCA$`defaulted %GDP`>=3,]
# ab <- ab[rowSums(is.na(ab)) != 7, ]
crisisCA$crisis <- ifelse(crisisCA$`defaulted %GDP`>=4, 1, 0)
crisisCA[,"Crisis in 1"] <- ifelse(crisisCA$crisis==1, as.numeric(crisisCA$Year)-1, 0)
crisisCA[,"Crisis in 2"] <- ifelse(crisisCA$crisis==1, as.numeric(crisisCA$Year)-2, 0)
in1y <- crisisCA[crisisCA$crisis==1,c(1,8,7)]
in1y <- in1y[rowSums(is.na(in1y)) != 3, ]
colnames(in1y) <- c("ISO", "Year", "in1y")
in2y <- crisisCA[crisisCA$crisis==1,c(1,9,7)]
in2y <- in2y[rowSums(is.na(in2y)) != 3, ]
colnames(in2y) <- c("ISO", "Year", "in2y")
crisisCA <- merge(crisisCA[c(1,2,6,7)], in1y, by = c("ISO", "Year"),all.x = TRUE)
crisisCA <- merge(crisisCA, in2y, by = c("ISO", "Year") ,all.x = TRUE)
rm(in2y, in1y)
crisisCA$Year <- as.character(crisisCA$Year)
crisisCA$in2y <- ifelse(crisisCA$ISO == "LKA"& crisisCA$Year=="2020", 1 ,crisisCA$in2y )
length(unique(crisisCA$ISO))
# library(dplyr)
# all <- IMF_new %>% 
#  merge(WB[2:8], by = c("ISO", "Year"), all.x = TRUE) %>% 
#  merge(gbyields[1,3:4], by = c("ISO", "Year"), all.x = TRUE) %>%
#  merge(gbryields[1,3:4], by = c("ISO", "Year"), all.x = TRUE)
#too many rows 
# gbyields[,"IY"] <- paste(gbyields[,1],gbyields[,3])
# gbyields$IY[duplicated(gbyields$IY)]
remove(all)
all <- merge(IMF_new, WB_new[-3], by = c("ISO", "Year"), all.x = TRUE)
all <- merge(all, gbryields[-2], by = c("ISO", "Year"), all.x = TRUE)
all <- merge(all, gbyields[-2], by = c("ISO", "Year"), all.x = TRUE)
all <- merge(all, fxres[-1], by = c("ISO", "Year"), all.x = TRUE)
all <- merge(all, reer[-4], by = c("ISO", "Year"), all.x = TRUE)
all <- merge(all, M2r[-1], by = c("ISO", "Year"), all.x = TRUE)
all <- merge(all, imfcredit[-1], by = c("ISO", "Year"), all.x = TRUE)
all <- merge(all, rGDPg[-2], by = c("ISO", "Year"), all.x = TRUE)
all <- merge(all, intofGDP[-2], by = c("ISO", "Year"), all.x = TRUE)
all <- merge(all, politi[-2], by = c("ISO", "Year"), all.x = TRUE)
all <- merge(all, crisisCA[-3], by = c("ISO", "Year"), all.x = TRUE)
all <- all[rowSums(is.na(all[, c(4:25)])) != 22, ]
all$Reserves <- as.numeric(all$Reserves) /(as.numeric(all$`GDP bn`)*10000000)
all$`IMF credit USD` <- as.numeric(all$`IMF credit USD`) /(as.numeric(all$`GDP bn`)*10000000)
summary(all)

d1 <- as.data.frame(setdiff(all$ISO, crisisCA$ISO))
names(d1) <- "ISO"
d1 <- merge(d1, unique(all[1:3]), by = "ISO", all.x = TRUE)
a <- unique(d1$ISO)
all <- all[!all$ISO %in% a, ]
all$crisis[is.na(all$crisis)] <- 0
all$in1y[is.na(all$in1y)] <- 0
all$in2y[is.na(all$in2y)] <- 0
all <- all[all$crisis!=1,]
length(which(all$in1y==0))/nrow(all)

regions <- read.csv("geo all.csv")
colnames(regions)[3] <- "ISO"
all <- merge(all, regions[c(3,6,7)], by = "ISO", all.x = TRUE)
a <- all[is.na(all$region), ]
all$region[all$ISO == "WBG"] <- "Asia"
all$sub.region[all$ISO == "WBG"] <- "Western Asia"
all$region[all$ISO == "UVK"] <- "Europe"
all$sub.region[all$ISO == "UVK"] <- "Southern Europe"
# 24 variable columns - 4:27
all <- all[rowSums(is.na(all[, c(4:27)])) <= 12, ]
all$Year <- as.numeric(all$Year)
all <- all %>%arrange(Year)
all <- all %>%arrange(ISO)
all$in2y[all$in1y ==1] <- 0 #get rid of "fake" in 2y, when I have already in1y data
unique(all$ISO)
#remove GDP bn, real GDP g (2x in data) and variables with more than 1.3k NAs
data <- all[,-c(3,12, 18, 19, 21, 23, 24, 26, 29, 32, 33)]
write.csv(all, "all.csv")

colnames(data)
length(unique(data$ISO))

library(tidyr)
data <- data %>% group_by(ISO) %>% fill(everything(), .direction = "updown") %>% ungroup()
mean(na.omit(data$`Interest paid on public debt / GDP%`))
data <-  data %>% mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
colnames(data) <- c("ISO","Year","GDP.growth","Gross.nat.savings","Inflation","Exports.growth",
                    "Gov.expenditure","Gov.net.lending.borrowing","Gov.gross.debt","Curr.acc.balance","FDI",
                    "Credit.to.private","Trade.openness","Claims.on.gov","bank.assets","Reserves","M2.R",
                    "Interest.paid.on.public.debt","polity2","durable","in1y","in2y")
facc <-  as.factor(data$in1y)
data$in1y <- facc
facc <-  as.factor(data$in2y)
data$in2y <- facc
summary(data)
##################################################################################################
##################################################################################################
##################################   MACHINE LEARNING HERE   #####################################
##################################################################################################
##################################################################################################
########### crisis in 2y

#split into 80/20 by years 
# trainB2 <- data[data$Year<=2014,-c(1,2,21)] #
# testB2 <- data[data$Year>2014,-c(1,2,21)]
# library(caret)
# trainB2 <- downSample(x = trainB2[,-19], y = trainB2$in2y, yname = "in2y")

# random 80/20
set.seed(1)
dataB2 <- data[sample(nrow(data)),]
0.8*3655
trainB2 <- dataB2[1:2924,-c(1,2,21)]
testB2 <- dataB2[2925:nrow(dataB2),-c(1,2,21)]
set.seed(218)
trainB2 <- downSample(x = trainB2[,-19], y = trainB2$in2y, yname = "in2y")
# summary(trainB2)
##################################################################################################
# normalise 
trainN <- trainB2
testN <- testB2
for(i in 1:(length(colnames(trainB2))-1)) {
    minimum<-min(trainB2[,i])
    maximum<-max(trainB2[,i])
    trainN[,i] <- as.vector(scale(trainB2[,i],center=minimum,scale=maximum-minimum)) 
    testN[,i] <- as.vector(scale(testB2[,i],center=minimum,scale=maximum-minimum)) 
}
trainN <- as.data.frame(trainN)
testN <- as.data.frame(testN)

# ROC curve
##################################################################################################
# LOGISTIC REGRESSION 
m1 <- glm(in2y~., family = 'binomial', data = trainN)
# Make predictions
m1prob <- predict(m1, testN, type = "response")
m1pred <- rep("0", dim(testN)[1])
m1pred[m1prob>0.5] <- "1"
m1tab <- table(m1pred, testN$in2y)
m1tab
sum(diag(m1tab))/sum(m1tab)
m1tab[2,2]/sum(m1tab[,2])

# ROC curve - fur results 
library(pROC)
test_roc <- roc(testN$in2y ~ m1prob, plot = TRUE, print.auc = TRUE)


##################################################################################################
# SVM
# try linear SVM
library('e1071')
set.seed(1)
m2a <- tune.svm(in2y~.,data = trainN, cost=2^(-10:10), kernel= "linear")
bestca <- m2a$best.parameters[[1]]
m2fina <- svm(in2y~.,data = trainN, cost= bestca, kernel="linear")
#make predictions
m2apred <- predict(m2fina, testN, type = "response")
m2atab <- table(m2apred, testN$in2y)
m2atab
sum(diag(m2atab))/sum(m2atab)
m2atab[2,2]/sum(m2atab[,2])


test_roc <-roc(as.numeric(testN$in2y) ~ as.numeric(m2apred), plot = TRUE, print.auc = TRUE)
library(rminer)
M <- fit(in2y~.,data = trainN, model="svm", kpar=list(sigma=0.10), C=2^(-5))
svm.imp <- Importance(M, data = trainN)
svm.imp$sresponses
# try RBF SVM - more crisis predicted, but overall less (fake crises)
set.seed(1)
m2b <- tune.svm(in2y~.,data = trainN,gamma=2^(-10:10),cost=2^(-10:10))
bestgamma <- m2b$best.parameters[[1]]
bestcb <- m2b$best.parameters[[2]]
m2finb <- svm(in2y~.,data = trainN, gamma=bestgamma, cost=bestcb)
#make predictions
m2bpred <- predict(m2finb,testN, type = "response")
m2btab <-table(m2bpred, testN$in2y)
m2btab
sum(diag(m2btab))/sum(m2btab)
test_roc <-roc(as.numeric(testN$in2y) ~ as.numeric(m2bpred), plot = TRUE, print.auc = TRUE)



##################################################################################################
# k-NN
library('FNN')
#tuning using leave-one-out
bestk=0
bestaccuracy=0
accuracy <- NULL
for(auxk in  1:30){
  mycv <- knn.cv(train= trainN[,-19], cl= trainN[,19], k=auxk)
  mytable <- table (mycv, trainN[,19])
  accuracy[auxk] <- sum(diag(mytable))/sum(mytable)
  if(bestaccuracy< accuracy[auxk]) bestk=auxk
  if(bestaccuracy< accuracy[auxk]) bestaccuracy = accuracy[auxk]}
bestk
plot(accuracy, xlab="K", ylab="Crossvalidated Accuracy", type = "l")

m5 <- knn(train= trainN[,-19], test= testN[,-19], cl= trainN[,19], k=3)
m5tab <- table (m5,testN[,19])
m5tab
sum(diag(m5tab))/sum(m5tab)
m5tab[2,2]/sum(m5tab[,2])
test_roc <-roc(as.numeric(testN$in2y) ~ as.numeric(m5), plot = TRUE, print.auc = TRUE)



##################################################################################################
library(randomForest) # despite tuning & regul., 1 to ZERO CRISES PREDICTED
# m4 <- randomForest(in2y~.,data = trainN,ntree=500,importance=TRUE)
importance(m4)

# print(m4)

library(RRF)
rf <- RRF(in2y~.,data = trainN,flagReg = 0) # build an ordinary RF 
impRF <- rf$importance 
impRF <- impRF[,"MeanDecreaseGini"] # get the importance score 
imp <- impRF/(max(impRF)) #normalize the importance scores into [0,1]
gamma <- 0.3   #A larger gamma often leads to fewer features. But, the quality of the features selected is quite stable for GRRF, i.e., different gammas can have similar accuracy performance (the accuracy of an ordinary RF using the feature subsets). See the paper for details. 
coefReg <- (1-gamma) + gamma*imp   # each variable has a coefficient, which depends on the importance score from the ordinary RF and the parameter: gamma
set.seed(1)
m4 <- RRF(in2y~.,data = trainN, ntree = 70, mtry = 4, importance=TRUE,flagReg=1, coefReg=coefReg) 
# look at training results 
m4pred <- predict(m4, testN, type = "class")
m4tab <- table(m4pred,testN$in2y)
m4tab
sum(diag(m4tab))/sum(m4tab)
m4tab[2,2]/sum(m4tab[,2])
#try different values for ntree and coefreg
m4b <- tuneRRF(x = trainN[,-19], y = trainN[,19], coefReg=coefReg, ntreeTry=5, stepFactor=2, improve=0.0001,
               trace=TRUE, plot=TRUE, doBest=FALSE)

test_roc <-roc(as.numeric(testN$in2y) ~ as.numeric(m4pred), plot = TRUE, print.auc = TRUE)

varImpPlot(m4)
varImp(m4)

print(m4)



##################################################################################################
# XGBOOST in 2y
##################################################################################################
library(caret) # Machine Learning Library
library(xgboost)
library(mltools)
grid_tune <- expand.grid(
  nrounds = c(5,10,20,50,100), #number of trees
  max_depth = c(4,5,6,7,8),
  eta = 0.2, #Learning rate c(0.025,0.05,0.1,0.3)
  gamma = c(0.5, 0.7, 0.9, 1.0), #pruning aka regularization, c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0)
  colsample_bytree = 1, # subsample ratio of columns for tree c(0.4, 0.6, 0.8, 1.0)
  min_child_weight = 1, #more than 1 = conservative c(1,2,3)
  subsample = 1 # used to prevent overfitting by sampling X% training c(0.5, 0.75, 1.0)
)
xgb_tune <- train(x = trainN[,-19],
                  y = trainN[,19],
                  trControl = trainControl(method = "cv",
                                           number=10,
                                           verboseIter = TRUE,
                                           allowParallel = TRUE),
                  tuneGrid = grid_tune,
                  method= "xgbTree",
                  metric = "Kappa",
                  verbose = TRUE)
xgb_tune$bestTune 
# Writing out the best model.
train_control <- trainControl(method = "none",
                              verboseIter = TRUE,
                              allowParallel = TRUE)
final_grid <- expand.grid(nrounds = xgb_tune$bestTune$nrounds,
                          eta = xgb_tune$bestTune$eta,
                          max_depth = xgb_tune$bestTune$max_depth,
                          gamma = xgb_tune$bestTune$gamma,
                          colsample_bytree = xgb_tune$bestTune$colsample_bytree,
                          min_child_weight = xgb_tune$bestTune$min_child_weight,
                          subsample = xgb_tune$bestTune$subsample)
xgb_model <- train(x = trainN[,-19],
                   y = trainN[,19],
                   trControl = train_control,
                   tuneGrid = final_grid,
                   method = "xgbTree",
                   verbose = TRUE)
# Prediction: 
xgb.pred <- predict(xgb_model, testN)
#' Confusion Matrix
confusionMatrix(as.factor(as.numeric(xgb.pred)), as.factor(as.numeric(testN$in2y)))
#variable importance

varImp(xgb_model)

test_roc <-roc(as.numeric(testN$in2y) ~ as.numeric(xgb.pred), plot = TRUE, print.auc = TRUE)



##################################################################################################
# Ada BOOST in 2y 
library(adabag)
library(caret)
library(plyr)
library(recipes)
library(dplyr)
results_ada <-  train(x = trainN[,-19],y = trainN[,19], method = "ada",metric = "Kappa")
results_ada$bestTune
final_grid <- expand.grid(iter = results_ada$bestTune$iter,
                          maxdepth = results_ada$bestTune$maxdepth,
                          nu = results_ada$bestTune$nu)
train_control <- trainControl(method = "none",
                              verboseIter = TRUE,
                              allowParallel = TRUE)
ada_model <- train(x = trainN[,-19],
                   y = trainN[,19],
                   trControl = train_control,
                   tuneGrid = final_grid,
                   method = "ada",
                   verbose = TRUE)

varImp(ada_model)

ada.pred <- predict(ada_model, testN)
confusionMatrix(as.factor(as.numeric(ada.pred)), as.factor(as.numeric(testN$in2y)))

test_roc <-roc(as.numeric(testN$in2y) ~ as.numeric(ada.pred), plot = TRUE, print.auc = TRUE)


##################################################################################################
# c5.0 (upgrade of 4.5) in 2y
library(C50)
model.c50 <- C5.0(in2y~., data=trainN)
c50.pred <- predict(model.c50, testN)
confusionMatrix(as.factor(as.numeric(c50.pred)), as.factor(as.numeric(testN$in2y)))


#tuned 
c50Grid <- expand.grid(.trials=c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                       .model = c("tree", "rules"),
                       .winnow = c(TRUE, FALSE))
set.seed(1) 
c5Fitvac <- train(in2y~., data=trainN,
                  method = "C5.0",
                  tuneGrid = c50Grid,
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,repeats = 10, 
                                           returnResamp="all"),
                  verbose=FALSE)
c5Fitvac$finalModel$tuneValue
xyplot(c5Fitvac,type = c("g", "p", "smooth"))
tunedc50 <- train(x = trainN[,-19],
                  y = trainN[,19],
                  trControl = trainControl(method = "none",
                                           verboseIter = TRUE,
                                           allowParallel = TRUE),
                  tuneGrid = expand.grid(.trials=c5Fitvac$finalModel$tuneValue$trials,
                                         .model = c5Fitvac$finalModel$tuneValue$model,
                                         .winnow = c5Fitvac$finalModel$tuneValue$winnow),
                  method = "C5.0",
                  verbose = TRUE)

xyplot(c5Fitvac,type = c("g", "p", "smooth"))
plot(c5Fitvac)

treeModel <- C5.0(in2y~., data=trainN, .trials=100,.model = "tree",.winnow = FALSE)
C5imp(treeModel)

C5imp(c5Fitvac)
c50.pred <- predict(tunedc50, testN)
table(c50.pred,testN$in2y)

confusionMatrix(as.factor(as.numeric(c50.pred)), as.factor(as.numeric(testN$in2y)))
test_roc <-roc(as.numeric(testN$in2y) ~ as.numeric(c50.pred), plot = TRUE, print.auc = TRUE)


##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
# data 1y
##################################################################################################


set.seed(1)
dataB1 <- data[sample(nrow(data)),]
0.8*3655
trainB1 <- dataB1[1:2924,-c(1,2,22)]
testB1 <- dataB1[2925:nrow(dataB1),-c(1,2,22)]
set.seed(218)
trainB1 <- downSample(x = trainB1[,-19], y = trainB1$in1y, yname = "in1y")
##################################
trainK <- trainB1
testK <- testB1
for(i in 1:(length(colnames(trainB1))-1)) {
  minimum<-min(trainB1[,i])
  maximum<-max(trainB1[,i])
  trainK[,i] <- as.vector(scale(trainB1[,i],center=minimum,scale=maximum-minimum)) 
  testK[,i] <- as.vector(scale(testB1[,i],center=minimum,scale=maximum-minimum)) 
}
trainK <- as.data.frame(trainK)
testK <- as.data.frame(testK)

##################################################################################################
# LOGISTIC REGRESSION 
m1 <- glm(in1y~., family = 'binomial', data = trainK)
summary(m1)
# Make predictions
m1prob <- predict(m1, testK, type = "response")
m1pred <- rep("0", dim(testK)[1])
m1pred[m1prob>0.5] <- "1"
m1tab <- table(m1pred, testK$in1y)
m1tab
sum(diag(m1tab))/sum(m1tab)
m1tab[2,2]/sum(m1tab[,2])

# ROC curve - fur results 
library(pROC)

roc(testK$in1y ~ m1prob, plot = TRUE, print.auc = TRUE)


##################################################################################################
# SVM
# try linear SVM
library('e1071')
set.seed(1)
m2a <- tune.svm(in1y~.,data = trainK, cost=2^(-10:10), kernel= "linear")
bestca <- m2a$best.parameters[[1]]
m2fina <- svm(in1y~.,data = trainK, cost= bestca, kernel="linear")
#make predictions
m2apred <- predict(m2fina, testK, type = "response")
m2atab <- table(m2apred, testK$in1y)
m2atab
sum(diag(m2atab))/sum(m2atab)
m2atab[2,2]/sum(m2atab[,2])


test_roc <-roc(as.numeric(testK$in1y) ~ as.numeric(m2apred), plot = TRUE, print.auc = TRUE)
2^10
# try RBF SVM - more crisis predicted, but overall less (fake crises)
set.seed(1)
m2b <- tune.svm(in1y~.,data = trainK,gamma=2^(-6:6),cost=2^(-6:6))
summary(m2b)
bestgamma <- m2b$best.parameters[[1]]
bestcb <- m2b$best.parameters[[2]]
m2finb <- svm(in1y~.,data = trainK, gamma=bestgamma, cost=bestcb)
#make predictions
m2bpred <- predict(m2finb,testK, type = "response")
m2btab <-table(m2bpred, testK$in1y)
m2btab
sum(diag(m2btab))/sum(m2btab)
test_roc <-roc(as.numeric(testK$in1y) ~ as.numeric(m2bpred), plot = TRUE, print.auc = TRUE)


##################################################################################################
# k-NN
library('FNN')
#tuning using leave-one-out
bestk=0
bestaccuracy=0
accuracy <- NULL
for(auxk in  1:30){
  mycv <- knn.cv(train= trainK[,-19], cl= trainK[,19], k=auxk)
  mytable <- table (mycv, trainK[,19])
  accuracy[auxk] <- sum(diag(mytable))/sum(mytable)
  if(bestaccuracy< accuracy[auxk]) bestk=auxk
  if(bestaccuracy< accuracy[auxk]) bestaccuracy = accuracy[auxk]}
bestk
plot(accuracy, xlab="K", ylab="Crossvalidated Accuracy", type = "l")

m5 <- knn(train= trainK[,-19], test= testK[,-19], cl= trainK[,19], k=3)
m5tab <- table (m5,testK[,19])
m5tab
sum(diag(m5tab))/sum(m5tab)
m5tab[2,2]/sum(m5tab[,2])
test_roc <-roc(as.numeric(testK$in1y) ~ as.numeric(m5), plot = TRUE, print.auc = TRUE)



##################################################################################################
library(randomForest)

# print(m4)

library(RRF)
rf <- RRF(in1y~.,data = trainK,flagReg = 0) # build an ordinary RF 
impRF <- rf$importance 
impRF <- impRF[,"MeanDecreaseGini"] # get the importance score 
imp <- impRF/(max(impRF)) #normalize the importance scores into [0,1]
gamma <- 0.4   #A larger gamma often leads to fewer features. But, the quality of the features selected is quite stable for GRRF, i.e., different gammas can have similar accuracy performance (the accuracy of an ordinary RF using the feature subsets). See the paper for details. 
coefReg <- (1-gamma) + gamma*imp   # each variable has a coefficient, which depends on the importance score from the ordinary RF and the parameter: gamma
set.seed(1)
m4 <- RRF(in1y~.,data = trainK, ntree = 100, mtry = 4, importance=TRUE,flagReg=1, coefReg=coefReg) #ntree tuning tops = 1, 11, 15, 
# look at training results 
m4pred <- predict(m4, testK, type = "class")   #ntree 7, 40, 
m4tab <- table(m4pred,testK$in1y)
m4tab
sum(diag(m4tab))/sum(m4tab)
m4tab[2,2]/sum(m4tab[,2])
#try different values for ntree and coefreg
m4b <- tuneRRF(x = trainK[,-19], y = trainK[,19], coefReg=coefReg, ntreeTry=5, stepFactor=2, improve=0.0001,
               trace=TRUE, plot=TRUE, doBest=FALSE)

test_roc <-roc(as.numeric(testK$in1y) ~ as.numeric(m4pred), plot = TRUE, print.auc = TRUE)

varImpPlot(m4)


##################################################################################################
# XGBOOST in 1y
##################################################################################################
library(caret) # Machine Learning Library
library(xgboost)
library(mltools)
grid_tune <- expand.grid(
  nrounds = c(10,20,50,70,100), #number of trees
  max_depth = c(4,5,6,7,8),
  eta = c(0.2,0.3,0.4,0.5,0.6,0.7), #Learning rate c(0.025,0.05,0.1,0.3)
  gamma = c(0.5, 0.7, 0.9, 1.0), #pruning aka regularization, c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0)
  colsample_bytree = 1, # subsample ratio of columns for tree c(0.4, 0.6, 0.8, 1.0)
  min_child_weight = c(1,2,3), #more than 1 = conservative c(1,2,3)
  subsample = 1 # used to prevent overfitting by sampling X% training c(0.5, 0.75, 1.0)
)
xgb_tune <- train(x = trainK[,-19],
                  y = trainK[,19],
                  trControl = trainControl(method = "cv",
                                           number=10,
                                           verboseIter = TRUE,
                                           allowParallel = TRUE),
                  tuneGrid = grid_tune,
                  method= "xgbTree",
                  metric = "Kappa",
                  verbose = TRUE)
xgb_tune$bestTune 
# Writing out the best model.
train_control <- trainControl(method = "none",
                              verboseIter = TRUE,
                              allowParallel = TRUE)
final_grid <- expand.grid(nrounds = xgb_tune$bestTune$nrounds,
                          eta = xgb_tune$bestTune$eta,
                          max_depth = xgb_tune$bestTune$max_depth,
                          gamma = xgb_tune$bestTune$gamma,
                          colsample_bytree = xgb_tune$bestTune$colsample_bytree,
                          min_child_weight = xgb_tune$bestTune$min_child_weight,
                          subsample = xgb_tune$bestTune$subsample)
xgb_model <- train(x = trainK[,-19],
                   y = trainK[,19],
                   trControl = train_control,
                   tuneGrid = final_grid,
                   method = "xgbTree",
                   verbose = TRUE)
# Prediction: 
xgb.pred <- predict(xgb_model, testK)
#' Confusion Matrix
confusionMatrix(as.factor(as.numeric(xgb.pred)), as.factor(as.numeric(testK$in1y)))
#variable importance

test_roc <-roc(as.numeric(testN$in2y) ~ as.numeric(xgb.pred), plot = TRUE, print.auc = TRUE)

varImp(xgb_model)


##################################################################################################
# Ada BOOST in 1y
library(adabag)
library(caret)
library(plyr)
library(recipes)
library(dplyr)
results_ada <-  train(x = trainK[,-19],y = trainK[,19], method = "ada",metric = "Kappa")
results_ada$bestTune
final_grid <- expand.grid(iter = results_ada$bestTune$iter,
                          maxdepth = results_ada$bestTune$maxdepth,
                          nu = results_ada$bestTune$nu)
train_control <- trainControl(method = "none",
                              verboseIter = TRUE,
                              allowParallel = TRUE)
ada_model <- train(x = trainK[,-19],
                   y = trainK[,19],
                   trControl = train_control,
                   tuneGrid = final_grid,
                   method = "ada",
                   verbose = TRUE)

varImp(ada_model)

ada.pred <- predict(ada_model, testK)
confusionMatrix(as.factor(as.numeric(ada.pred)), as.factor(as.numeric(testK$in1y)))



##################################################################################################
# c5.0 (upgrade of 4.5) in 1y
library(C50)
model.c50 <- C5.0(in1y~., data=trainK)
c50.pred <- predict(model.c50, testK)
confusionMatrix(as.factor(as.numeric(c50.pred)), as.factor(as.numeric(testK$in1y)))



#tuned 
c50Grid <- expand.grid(.trials=c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                       .model = c("tree", "rules"),
                       .winnow = c(TRUE, FALSE))
set.seed(1) 
c5Fitvac <- train(in1y~., data=trainK,
                  method = "C5.0",
                  tuneGrid = c50Grid,
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,repeats = 10, 
                                           returnResamp="all"),
                  verbose=FALSE)
c5Fitvac$finalModel$tuneValue
xyplot(c5Fitvac,type = c("g", "p", "smooth"))
tunedc50 <- train(x = trainK[,-19],
                  y = trainK[,19],
                  trControl = trainControl(method = "none",
                                           verboseIter = TRUE,
                                           allowParallel = TRUE),
                  tuneGrid = expand.grid(.trials=60,
                                         .model = c5Fitvac$finalModel$tuneValue$model,
                                         .winnow = c5Fitvac$finalModel$tuneValue$winnow),
                  method = "C5.0",
                  verbose = TRUE)
xyplot(c5Fitvac,type = c("g", "p", "smooth"))
plot(c5Fitvac)

treeModel <- C5.0(in1y~., data=trainK, .trials=60,.model = "rules",.winnow = FALSE)
C5imp(treeModel, )

c50.pred <- predict(tunedc50, testK)
table(c50.pred,testK$in1y)
confusionMatrix(as.factor(as.numeric(c50.pred)), as.factor(as.numeric(testK$in1y)))

test_roc <-roc(as.numeric(testK$in1y) ~ as.numeric(c50.pred), plot = TRUE, print.auc = TRUE)

