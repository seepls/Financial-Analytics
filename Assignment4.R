install.packages("rugarch")
library(fGarch)
library(tseries)
library(strucchange)
library(ggplot2)
library(xts)
library(urca)
library(lmtest)
library(rugarch)


dataset <- read.csv(file.choose(), header = T)
head(dataset)
dataset$Date=as.Date(dataset$Date, "%d-%m-%Y")
head(dataset)

##Set the dates
gfcdate <- as.Date("2008-09-01")
coviddate <- as.Date("2020-03-03")

#INDEX
####################Change the S

#INDEX_whole_data 
ggplot(data = dataset, aes(x = Date, y = INDEX)) + 
  geom_line(color = "#00AFBB", size = 2)
#INDEX_pregfc
ss <- subset(dataset, Date < gfcdate)
ggplot(data = ss, aes(x = Date, y = INDEX)) + 
  geom_line(color = "#00AFBB", size = 2)
#INDEX_postgfc
ss <- subset(dataset, Date > gfcdate)
ggplot(data = ss, aes(x = Date, y = INDEX)) + 
  geom_line(color = "#00AFBB", size = 2)
#INDEX_preCOVID-19
ss <- subset(dataset, Date < coviddate)
ggplot(data = ss, aes(x = Date, y = INDEX)) + 
  geom_line(color = "#00AFBB", size = 2)
#INDEX_postCOVID-19
ss <- subset(dataset, Date > coviddate)
ggplot(data = ss, aes(x = Date, y = INDEX )) + 
  geom_line(color = "#00AFBB", size = 2)


#FEX
#####################
#FEX_whole_data 
ggplot(data = dataset, aes(x = Date, y = FEX)) + 
  geom_line(color = "#00AFBB", size = 2)
#FEX_pregfc
ss <- subset(dataset, Date < gfcdate)
ggplot(data = ss, aes(x = Date, y = FEX)) + 
  geom_line(color = "#00AFBB", size = 2)
#FEX_postgfc
ss <- subset(dataset, Date > gfcdate)
ggplot(data = ss, aes(x = Date, y = FEX)) + 
  geom_line(color = "#00AFBB", size = 2)
#FEX_preCOVID-19
ss <- subset(dataset, Date < coviddate)
ggplot(data = ss, aes(x = Date, y = FEX)) + 
  geom_line(color = "#00AFBB", size = 2)
#FEX_postCOVID-19
ss <- subset(dataset, Date > coviddate)
ggplot(data = ss, aes(x = Date, y = FEX )) + 
  geom_line(color = "#00AFBB", size = 2)


#INFLATION
#####################
#INFLATION_whole_data 
ggplot(data = dataset, aes(x = Date, y = Inflation)) + 
  geom_line(color = "#00AFBB", size = 2)
#INFLATION_pregfc
ss <- subset(dataset, Date < gfcdate)
ggplot(data = ss, aes(x = Date, y = Inflation)) + 
  geom_line(color = "#00AFBB", size = 2)
#INFLATION_postgfc
ss <- subset(dataset, Date > gfcdate)
ggplot(data = ss, aes(x = Date, y = Inflation)) + 
  geom_line(color = "#00AFBB", size = 2)
#INFLATION_preCOVID-19
ss <- subset(dataset, Date < coviddate)
ggplot(data = ss, aes(x = Date, y = Inflation)) + 
  geom_line(color = "#00AFBB", size = 2)
#INFLATION_postCOVID-19
ss <- subset(dataset, Date > coviddate)
ggplot(data = ss, aes(x = Date, y = Inflation )) + 
  geom_line(color = "#00AFBB", size = 2)



#Descriptive_Statistics
#Subset of data 
pregfc <- subset(dataset, Date < gfcdate)
postgfc <- subset(dataset, Date > gfcdate)
preCovid <- subset(dataset, Date < coviddate)
PostCovid <-  subset(dataset, Date > coviddate)

#INDEX
summary(dataset$INDEX)
summary(pregfc$INDEX)
summary(postgfc$INDEX)
summary(preCovid$INDEX)
summary(PostCovid$INDEX)


#FEX
summary(dataset$FEX)
summary(pregfc$FEX)
summary(postgfc$FEX)
summary(preCovid$FEX)
summary(PostCovid$FEX)

#INFLATION
summary(dataset$Inflation)
summary(pregfc$Inflation)
summary(postgfc$Inflation)
summary(preCovid$Inflation)
summary(PostCovid$Inflation)


##########################Unit Root TEST
#INDEX
adf.test(dataset$INDEX)
#FEX
adf.test(dataset$FEX)
#INFLATION
adf.test(dataset$Inflation)


########################STRUCTURAL BREAK
stocks <- xts(dataset[,-1], order.by=as.Date(dataset[,1], "%Y/%m/%d"))

#Observation Number in INDEX
bp.S <- breakpoints(stocks$INDEX ~ 1)
summary(bp.S)

#Observation Number in FEX
bp.FEX <- breakpoints(stocks$FEX ~ 1)
summary(bp.FEX)

#observation Number in INFLATION
bp.INFLATION <- breakpoints(stocks$Inflation ~ 1)
summary(bp.INFLATION)


##############################################3
###################Co-Integration Test######Change the D to get the pregfc,postgfc etc
D <- PostCovid
#INDEXvsFEXvsINFLATION
jotest1=ca.jo(data.frame(D$INDEX,D$FEX,D$Inflation), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest1)



######################CAsuality Test
#For making series stationary as Granger-Casuality is fit on stationary only
#Change for whole data ,pregfc,postgfc etc
D<- dataset
#INDEX Cause FEX
S <- cbind(diff(D$FEX),diff(D$INDEX))
result<- VAR(S, TYPE = "const", lag.max = 20 , ic="AIC")
#INDEX Cause Inflation
S <- cbind(diff(D$Inflation),diff(D$INDEX))
result<- VAR(S, TYPE = "const", lag.max = 20 , ic="AIC")




####################VOLATLITY MODELLING
#Change for whole data ,pregfc,postgfc etc ARCH
D<-PostCovid
#INDEX
summary(garchFit(~garch(1,0), data = D$INDEX, trace = F))

#FEX
summary(garchFit(~garch(1,0), data = D$FEX, trace = F))

#INFLATION
summary(garchFit(~garch(1,0), data = D$Inflation, trace = F))


#Change for whole data ,pregfc,postgfc etc GARCH
D<-postCovid
#INDEX 
summary(garchFit(formula = ~garch(1,1), data = D$INDEX, trace = F))

#FEX
summary(garchFit(formula = ~garch(1,1), data = D$FEX, trace = F))

#INFLATION
summary(garchFit(formula = ~garch(1,1), data = D$Inflation, trace = F))

