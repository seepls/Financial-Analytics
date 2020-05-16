install.packages("rugarch")
library(fGarch)
library(tseries)
library(strucchange)
library(ggplot2)
library(xts)
library(lmtest)
install.packages("urca")
library(urca)
library(rugarch)
install.packages("tseries")
library(tseries)
install.packages("timeSeries")
install.packages("forecast")

library(timeSeries)
library(forecast)

install.packages("testthat")
library(testthat)





dataset <- read.csv(file.choose(), header = T)
head(dataset)
dataset$Date=as.Date(dataset$Date, "%m/%d/%Y")

########################## VISUALIZATION
#BSE
#BSE_whole_data
ggplot(data = dataset, aes(x = Date, y = BSE)) + 
  geom_line(color = "#800000", size = 1)
#BSE_pregfc
ss <- subset(dataset, Date < as.Date("2007-12-28"))
ggplot(data = ss, aes(x = Date, y = BSE)) + 
  geom_line(color = "#800000", size = 2)
#BSE_postgfc
ss <- subset(dataset, Date > as.Date("2007-12-28"))
ggplot(data = ss, aes(x = Date, y = BSE)) + 
  geom_line(color = "#800000", size = 2)
#BSE_preCOVID-19
ss <- subset(dataset, Date < as.Date("2019-12-28"))
ggplot(data = ss, aes(x = Date, y = BSE)) + 
  geom_line(color = "#800000", size = 2)
#BSE_postCOVID-19
ss <- subset(dataset, Date > as.Date("2019-12-28"))
ggplot(data = ss, aes(x = Date, y = BSE)) + 
  geom_line(color = "#800000", size = 2)


head(dataset)

#JSE
#JSE_whole_data
ggplot(data = dataset, aes(x = Date, y = JSE)) + 
  geom_line(color = "#FF5700", size = 1)
#JSE_pregfc
ss <- subset(dataset, Date < as.Date("2007-12-28"))
ggplot(data = ss, aes(x = Date, y = JSE)) + 
  geom_line(color = "#FF5700", size = 2)
#JSE_postgfc
ss <- subset(dataset, Date > as.Date("2007-12-28"))
ggplot(data = ss, aes(x = Date, y = JSE)) + 
  geom_line(color = "#FF5700", size = 2)
#JSE_preCOVID-19
ss <- subset(dataset, Date < as.Date("2019-12-28"))
ggplot(data = ss, aes(x = Date, y = JSE)) + 
  geom_line(color = "#FF5700", size = 2)
#JSE_postCOVID-19
ss <- subset(dataset, Date > as.Date("2019-12-28"))
ggplot(data = ss, aes(x = Date, y = JSE)) + 
  geom_line(color = "#FF5700", size = 2)


#IBOVESPA
#IBOVESPA_whole_data
ggplot(data = dataset, aes(x = Date, y = IBOVESPA)) + 
  geom_line(color = "#0BA912", size = 1)
#IBOVESPA_pregfc
ss <- subset(dataset, Date < as.Date("2007-12-28"))
ggplot(data = ss, aes(x = Date, y = IBOVESPA)) + 
  geom_line(color = "#0BA912", size = 2)
#IBOVESPA_postgfc
ss <- subset(dataset, Date > as.Date("2007-12-28"))
ggplot(data = ss, aes(x = Date, y = IBOVESPA)) + 
  geom_line(color = "#0BA912", size = 2)
#IBOVESPA_preCOVID-19
ss <- subset(dataset, Date < as.Date("2019-12-28"))
ggplot(data = ss, aes(x = Date, y = IBOVESPA)) + 
  geom_line(color = "#0BA912", size = 2)
#IBOVESPA_postCOVID-19
ss <- subset(dataset, Date > as.Date("2019-12-28"))
ggplot(data = ss, aes(x = Date, y = IBOVESPA)) + 
  geom_line(color = "#0BA912", size = 2)


#RTSI
#RTSI_whole_data
ggplot(data = dataset, aes(x = Date, y = RTSI)) + 
  geom_line(color = "#00AFBB", size = 1)
#RTSI_pregfc
ss <- subset(dataset, Date < as.Date("2007-12-28"))
ggplot(data = ss, aes(x = Date, y = RTSI)) + 
  geom_line(color = "#00AFBB", size = 2)
#RTSI_postgfc
ss <- subset(dataset, Date > as.Date("2007-12-28"))
ggplot(data = ss, aes(x = Date, y = RTSI)) + 
  geom_line(color = "#00AFBB", size = 2)
#RTSI_preCOVID-19
ss <- subset(dataset, Date < as.Date("2019-12-28"))
ggplot(data = ss, aes(x = Date, y = RTSI)) + 
  geom_line(color = "#00AFBB", size = 2)
#RTSI_postCOVID-19
ss <- subset(dataset, Date > as.Date("2019-12-28"))
ggplot(data = ss, aes(x = Date, y = RTSI)) + 
  geom_line(color = "#00AFBB", size = 2)



#SSC
#SSC_whole_data
ggplot(data = dataset, aes(x = Date, y = SSC)) + 
  geom_line(color = "#A90B74", size = 1)
#SSC_pregfc
ss <- subset(dataset, Date < as.Date("2007-12-28"))
ggplot(data = ss, aes(x = Date, y = SSC)) + 
  geom_line(color = "#A90B74", size = 2)
#SSC_postgfc
ss <- subset(dataset, Date > as.Date("2007-12-28"))
ggplot(data = ss, aes(x = Date, y = SSC)) + 
  geom_line(color = "#A90B74", size = 2)
#SSC_preCOVID-19
ss <- subset(dataset, Date < as.Date("2019-12-28"))
ggplot(data = ss, aes(x = Date, y = SSC)) + 
  geom_line(color = "#A90B74", size = 2)
#SSC_postCOVID-19
ss <- subset(dataset, Date > as.Date("2019-12-28"))
ggplot(data = ss, aes(x = Date, y = SSC)) + 
  geom_line(color = "#A90B74", size = 2)


#NYSE
#NYSE_whole_data
ggplot(data = dataset, aes(x = Date, y = NYSE)) + 
  geom_line(color = "#DEC109", size = 1)
#NYSE_pregfc
ss <- subset(dataset, Date < as.Date("2007-12-28"))
ggplot(data = ss, aes(x = Date, y = NYSE)) + 
  geom_line(color = "#DEC109", size = 2)
#NYSE_postgfc
ss <- subset(dataset, Date > as.Date("2007-12-28"))
ggplot(data = ss, aes(x = Date, y = NYSE)) + 
  geom_line(color = "#DEC109", size = 2)
#NYSE_preCOVID-19
ss <- subset(dataset, Date < as.Date("2019-12-28"))
ggplot(data = ss, aes(x = Date, y = NYSE)) + 
  geom_line(color = "#DEC109", size = 2)
#NYSE_postCOVID-19
ss <- subset(dataset, Date > as.Date("2019-12-28"))
ggplot(data = ss, aes(x = Date, y = NYSE)) + 
  geom_line(color = "#DEC109", size = 2)

#Descriptive_Statistics
#Subset of data 
pregfc <- subset(dataset, Date < as.Date("2007-12-28"))
postgfc <- subset(dataset, Date > as.Date("2007-12-28"))
preCovid <- subset(dataset, Date < as.Date("2019-12-28"))
PostCovid <-  subset(dataset, Date > as.Date("2019-12-28"))

#BSE
summary(dataset$BSE)
summary(pregfc$BSE)
summary(postgfc$BSE)
summary(preCovid$BSE)
summary(PostCovid$BSE)

#JSE
summary(dataset$JSE)
summary(pregfc$JSE)
summary(postgfc$JSE)
summary(preCovid$JSE)
summary(PostCovid$JSE)

#IBOVESPA
summary(dataset$IBOVESPA)
summary(pregfc$IBOVESPA)
summary(postgfc$IBOVESPA)
summary(preCovid$IBOVESPA)
summary(PostCovid$IBOVESPA)

#RTSI
summary(dataset$RTSI)
summary(pregfc$RTSI)
summary(postgfc$RTSI)
summary(preCovid$RTSI)
summary(PostCovid$RTSI)

#SSC
summary(dataset$SSC)
summary(pregfc$SSC)
summary(postgfc$SSC)
summary(preCovid$ssc)
summary(PostCovid$SSC)

#NYSE
summary(dataset$NYSE)
summary(pregfc$NYSE)
summary(postgfc$NYSE)
summary(preCovid$NYSE)
summary(PostCovid$NYSE)


##################pairwise-mean Whole data
d0<-0
D <- dataset
# Calculate the sample size 
n_total<-length(D$NYSE)
# Set standard deviation and mean of the difference
#USAvsIndia
sigma_diff1<-sd(D$NYSE-D$BSE)
mean_diff1<-mean(D$NYSE-D$BSE)
z1<-sqrt(n_total)*((mean_diff1-d0)/sigma_diff1)
p_value1=2*pnorm(-abs(z1))

#USAvsBrazil
sigma_diff2<-sd(D$NYSE-D$IBOVESPA)
mean_diff2<-mean(D$NYSE-D$IBOVESPA)
z2<-sqrt(n_total)*((mean_diff2-d0)/sigma_diff2)
p_value2=2*pnorm(-abs(z2))

#USAvsRussia
sigma_diff3<-sd(D$NYSE-D$RTSI)
mean_diff3<-mean(D$NYSE-D$RTSI)
z3<-sqrt(n_total)*((mean_diff3-d0)/sigma_diff3)
p_value3=2*pnorm(-abs(z3))

#USAvsSouthafrica
sigma_diff4<-sd(D$NYSE-D$JSE)
mean_diff4<-mean(D$NYSE-D$JSE)
z4<-sqrt(n_total)*((mean_diff4-d0)/sigma_diff4)
p_value4=2*pnorm(-abs(z4))

#USAvsChina
sigma_diff5<-sd(D$NYSE-D$SSC)
mean_diff5<-mean(D$NYSE-D$SSC)
z5<-sqrt(n_total)*((mean_diff5-d0)/sigma_diff5)
p_value5=2*pnorm(-abs(z5))



##################pairwise-mean Pre GFC
d0<-0
D <- pregfc
# Calculate the sample size 
n_total<-length(D$NYSE)
# Set standard deviation and mean of the difference
#USAvsIndia
sigma_diff1<-sd(D$NYSE-D$BSE)
mean_diff1<-mean(D$NYSE-D$BSE)
z1<-sqrt(n_total)*((mean_diff1-d0)/sigma_diff1)
p_value1=2*pnorm(-abs(z1))

#USAvsBrazil
sigma_diff2<-sd(D$NYSE-D$IBOVESPA)
mean_diff2<-mean(D$NYSE-D$IBOVESPA)
z2<-sqrt(n_total)*((mean_diff2-d0)/sigma_diff2)
p_value2=2*pnorm(-abs(z2))

#USAvsRussia
sigma_diff3<-sd(D$NYSE-D$RTSI)
mean_diff3<-mean(D$NYSE-D$RTSI)
z3<-sqrt(n_total)*((mean_diff3-d0)/sigma_diff3)
p_value3=2*pnorm(-abs(z3))

#USAvsSouthafrica
sigma_diff4<-sd(D$NYSE-D$JSE)
mean_diff4<-mean(D$NYSE-D$JSE)
z4<-sqrt(n_total)*((mean_diff4-d0)/sigma_diff4)
p_value4=2*pnorm(-abs(z4))

#USAvsChina
sigma_diff5<-sd(D$NYSE-D$SSC)
mean_diff5<-mean(D$NYSE-D$SSC)
z5<-sqrt(n_total)*((mean_diff5-d0)/sigma_diff5)
p_value5=2*pnorm(-abs(z5))


##################pairwise-mean Post GFC
d0<-0
D <- postgfc
# Calculate the sample size 
n_total<-length(D$NYSE)
# Set standard deviation and mean of the difference
#USAvsIndia
sigma_diff1<-sd(D$NYSE-D$BSE)
mean_diff1<-mean(D$NYSE-D$BSE)
z1<-sqrt(n_total)*((mean_diff1-d0)/sigma_diff1)
p_value1=2*pnorm(-abs(z1))

#USAvsBrazil
sigma_diff2<-sd(D$NYSE-D$IBOVESPA)
mean_diff2<-mean(D$NYSE-D$IBOVESPA)
z2<-sqrt(n_total)*((mean_diff2-d0)/sigma_diff2)
p_value2=2*pnorm(-abs(z2))

#USAvsRussia
sigma_diff3<-sd(D$NYSE-D$RTSI)
mean_diff3<-mean(D$NYSE-D$RTSI)
z3<-sqrt(n_total)*((mean_diff3-d0)/sigma_diff3)
p_value3=2*pnorm(-abs(z3))

#USAvsSouthafrica
sigma_diff4<-sd(D$NYSE-D$JSE)
mean_diff4<-mean(D$NYSE-D$JSE)
z4<-sqrt(n_total)*((mean_diff4-d0)/sigma_diff4)
p_value4=2*pnorm(-abs(z4))

#USAvsChina
sigma_diff5<-sd(D$NYSE-D$SSC)
mean_diff5<-mean(D$NYSE-D$SSC)
z5<-sqrt(n_total)*((mean_diff5-d0)/sigma_diff5)
p_value5=2*pnorm(-abs(z5))


##################pairwise-mean Pre-CoVID-19
d0<-0
D <- preCovid
# Calculate the sample size 
n_total<-length(D$NYSE)
# Set standard deviation and mean of the difference
#USAvsIndia
sigma_diff1<-sd(D$NYSE-D$BSE)
mean_diff1<-mean(D$NYSE-D$BSE)
z1<-sqrt(n_total)*((mean_diff1-d0)/sigma_diff1)
p_value1=2*pnorm(-abs(z1))

#USAvsBrazil
sigma_diff2<-sd(D$NYSE-D$IBOVESPA)
mean_diff2<-mean(D$NYSE-D$IBOVESPA)
z2<-sqrt(n_total)*((mean_diff2-d0)/sigma_diff2)
p_value2=2*pnorm(-abs(z2))

#USAvsRussia
sigma_diff3<-sd(D$NYSE-D$RTSI)
mean_diff3<-mean(D$NYSE-D$RTSI)
z3<-sqrt(n_total)*((mean_diff3-d0)/sigma_diff3)
p_value3=2*pnorm(-abs(z3))

#USAvsSouthafrica
sigma_diff4<-sd(D$NYSE-D$JSE)
mean_diff4<-mean(D$NYSE-D$JSE)
z4<-sqrt(n_total)*((mean_diff4-d0)/sigma_diff4)
p_value4=2*pnorm(-abs(z4))

#USAvsChina
sigma_diff5<-sd(D$NYSE-D$SSC)
mean_diff5<-mean(D$NYSE-D$SSC)
z5<-sqrt(n_total)*((mean_diff5-d0)/sigma_diff5)
p_value5=2*pnorm(-abs(z5))



##################pairwise-mean post COVID-19
d0<-0
D <- PostCovid
# Calculate the sample size 
n_total<-length(D$NYSE)
# Set standard deviation and mean of the difference
#USAvsIndia
sigma_diff1<-sd(D$NYSE-D$BSE)
mean_diff1<-mean(D$NYSE-D$BSE)
z1<-sqrt(n_total)*((mean_diff1-d0)/sigma_diff1)
p_value1=2*pnorm(-abs(z1))

#USAvsBrazil
sigma_diff2<-sd(D$NYSE-D$IBOVESPA)
mean_diff2<-mean(D$NYSE-D$IBOVESPA)
z2<-sqrt(n_total)*((mean_diff2-d0)/sigma_diff2)
p_value2=2*pnorm(-abs(z2))

#USAvsRussia
sigma_diff3<-sd(D$NYSE-D$RTSI)
mean_diff3<-mean(D$NYSE-D$RTSI)
z3<-sqrt(n_total)*((mean_diff3-d0)/sigma_diff3)
p_value3=2*pnorm(-abs(z3))

#USAvsSouthafrica
sigma_diff4<-sd(D$NYSE-D$JSE)
mean_diff4<-mean(D$NYSE-D$JSE)
z4<-sqrt(n_total)*((mean_diff4-d0)/sigma_diff4)
p_value4=2*pnorm(-abs(z4))

#USAvsChina
sigma_diff5<-sd(D$NYSE-D$SSC)
mean_diff5<-mean(D$NYSE-D$SSC)
z5<-sqrt(n_total)*((mean_diff5-d0)/sigma_diff5)
p_value5=2*pnorm(-abs(z5))



##########################Unit Root TEST
pregfc <- subset(dataset, Date < as.Date("2007-12-28"))
postgfc <- subset(dataset, Date > as.Date("2007-12-28"))
preCovid <- subset(dataset, Date < as.Date("2019-12-28"))
PostCovid <-  subset(dataset, Date > as.Date("2019-12-28"))
#USA
adf.test(dataset$NYSE)
adf.test(pregfc$NYSE)
adf.test(postgfc$NYSE)
adf.test(preCovid$NYSE)
adf.test(PostCovid$NYSE)
#INDIA
adf.test(dataset$BSE)
adf.test(pregfc$BSE)
adf.test(postgfc$BSE)
adf.test(preCovid$BSE)
adf.test(PostCovid$BSE)
#BRAZIL
adf.test(dataset$IBOVESPA)
adf.test(pregfc$IBOVESPA)
adf.test(postgfc$IBOVESPA)
adf.test(preCovid$IBOVESPA)
adf.test(PostCovid$IBOVESPA)
#SOUTH AFRICA
adf.test(dataset$JSE)
adf.test(pregfc$JSE)
adf.test(postgfc$JSE)
adf.test(preCovid$JSE)
adf.test(PostCovid$JSE)
#CHINA
adf.test(dataset$SSC)
adf.test(pregfc$SSC)
adf.test(postgfc$SSC)
adf.test(preCovid$SSC)
adf.test(PostCovid$SSC)
#RUSSIA
adf.test(dataset$RTSI)
adf.test(pregfc$RTSI)
adf.test(postgfc$RTSI)
adf.test(preCovid$RTSI)
adf.test(PostCovid$RTSI)

########################STRUCTURAL BREAK
stocks <- xts(dataset[,-1], order.by=as.Date(dataset[,1], "%Y/%m/%d"))

#Observation Number in INDIA
bp.BSE <- breakpoints(stocks$BSE ~ 1)
summary(bp.BSE)

#Observation Number in USA
bp.NYSE <- breakpoints(stocks$NYSE ~ 1)
summary(bp.NYSE)

#observation Number in BRAZIL
bp.IBOVESPA <- breakpoints(stocks$IBOVESPA ~ 1)
summary(bp.IBOVESPA)

#observation Number in South Africa
bp.JSE <- breakpoints(stocks$JSE ~ 1)
summary(bp.JSE)

#observation Number in China
bp.SSC <- breakpoints(stocks$SSC ~ 1)
summary(bp.SSC)

#observation Number in Russia
bp.RTSI <- breakpoints(stocks$RTSI ~ 1)
summary(bp.RTSI)



###################Co-Integration Test
pregfc <- subset(dataset, Date < as.Date("2007-12-28"))
postgfc <- subset(dataset, Date > as.Date("2007-12-28"))
preCovid <- subset(dataset, Date < as.Date("2019-12-28"))
PostCovid <-  subset(dataset, Date > as.Date("2019-12-28"))
# using urca 
#USAvsIndia
jotest1=ca.jo(data.frame(dataset$NYSE,dataset$BSE), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest1)
#pre-gfc
jotest1=ca.jo(data.frame(dataset$NYSE,pregfc$BSE), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest1)
#post -gfc
jotest1=ca.jo(data.frame(dataset$NYSE,postgfc$BSE), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest1)
#pre-covid
jotest1=ca.jo(data.frame(dataset$NYSE,preCovid$BSE), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest1)
#post-covid
jotest1=ca.jo(data.frame(dataset$NYSE,PostCovid$BSE), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest1)


#USAvsBrazil
jotest2=ca.jo(data.frame(dataset$NYSE,dataset$IBOVESPA), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest2)
#pre-gfc
jotest2=ca.jo(data.frame(dataset$NYSE,pregfc$IBOVESPA), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest2)
#post -gfc
jotest2=ca.jo(data.frame(dataset$NYSE,postgfc$IBOVESPA), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest2)
#pre-covid
jotest2=ca.jo(data.frame(dataset$NYSE,preCovid$IBOVESPA), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest2)
#post-covid
jotest2=ca.jo(data.frame(dataset$NYSE,PostCovid$IBOVESPA), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest2)

#USAvsChina
jotest3=ca.jo(data.frame(dataset$NYSE,dataset$SSC), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest3)
#pre-gfc
jotest3=ca.jo(data.frame(dataset$NYSE,pregfc$SSC), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest3)
#post -gfc
jotest3=ca.jo(data.frame(dataset$NYSE,postgfc$SSC), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest3)
#pre-covid
jotest3=ca.jo(data.frame(dataset$NYSE,preCovid$SSC), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest3)
#post-covid
jotest3=ca.jo(data.frame(dataset$NYSE,PostCovid$SSC), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest3)

#USAvsRussia
jotest4=ca.jo(data.frame(dataset$NYSE,dataset$RTSI), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest4)
#pre-gfc
jotest4=ca.jo(data.frame(dataset$NYSE,pregfc$RTSI), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest4)
#post -gfc
jotest4=ca.jo(data.frame(dataset$NYSE,postgfc$RTSI), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest4)
#pre-covid
jotest4=ca.jo(data.frame(dataset$NYSE,preCovid$RTSI), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest4)
#post-covid
jotest4=ca.jo(data.frame(dataset$NYSE,PostCovid$RTSI), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest4)

#Indiavssouthafrica
jotest5=ca.jo(data.frame(dataset$NYSE,dataset$JSE), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest5)
#pre-gfc
jotest5=ca.jo(data.frame(dataset$NYSE,pregfc$JSE), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest5)
#post -gfc
jotest5=ca.jo(data.frame(dataset$NYSE,postgfc$JSE), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest5)
#pre-covid
jotest5=ca.jo(data.frame(dataset$NYSE,preCovid$JSE), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest5)
#post-covid
jotest5=ca.jo(data.frame(dataset$NYSE,PostCovid$JSE), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest5)

######################CAsuality Test
#For making series stationary as Granger-Casuality is fit on stationary only

library(grangers)
library(lmtest)
D<-dataset
pregfc <- subset(dataset, Date < as.Date("2007-12-28"))
postgfc <- subset(dataset, Date > as.Date("2007-12-28"))
preCovid <- subset(dataset, Date < as.Date("2019-12-28"))
PostCovid <-  subset(dataset, Date > as.Date("2019-12-28"))

#USA Cause INDIA
grangertest(diff(D$BSE)~diff(D$NYSE), order=3)
grangertest(diff(pregfc$BSE)~diff(pregfc$NYSE), order=3)
grangertest(diff(postgfc$BSE)~diff(postgfc$NYSE), order=3)
grangertest(diff(preCovid$BSE)~diff(preCovid$NYSE), order=3)
grangertest(diff(PostCovid$BSE)~diff(PostCovid$NYSE), order=3)
#USA Cause Russia
grangertest(diff(D$RTSI)~diff(D$NYSE), order=3)
grangertest(diff(pregfc$RTSI)~diff(pregfc$NYSE), order=3)
grangertest(diff(postgfc$RTSI)~diff(postgfc$NYSE), order=3)
grangertest(diff(preCovid$RTSI)~diff(preCovid$NYSE), order=3)
grangertest(diff(PostCovid$RTSI)~diff(PostCovid$NYSE), order=3)

#USA Cause Southafrica
grangertest(diff(D$JSE)~diff(D$NYSE), order=3)
grangertest(diff(pregfc$JSE)~diff(pregfc$NYSE), order=3)
grangertest(diff(postgfc$JSE)~diff(postgfc$NYSE), order=3)
grangertest(diff(preCovid$JSE)~diff(preCovid$NYSE), order=3)
grangertest(diff(PostCovid$JSE)~diff(PostCovid$NYSE), order=3)

#USA Cause Brazil
grangertest(diff(D$IBOVESPA)~diff(D$NYSE), order=3)
grangertest(diff(pregfc$IBOVESPA)~diff(pregfc$NYSE), order=3)
grangertest(diff(postgfc$IBOVESPA)~diff(postgfc$NYSE), order=3)
grangertest(diff(preCovid$IBOVESPA)~diff(preCovid$NYSE), order=3)
grangertest(diff(PostCovid$IBOVESPA)~diff(PostCovid$NYSE), order=3)

#USA Cause China
grangertest(diff(D$SSC)~diff(D$NYSE), order=1)
grangertest(diff(pregfc$SSC)~diff(pregfc$NYSE), order=3)
grangertest(diff(postgfc$SSC)~diff(postgfc$NYSE), order=3)
grangertest(diff(preCovid$SSC)~diff(preCovid$NYSE), order=3)
grangertest(diff(PostCovid$SSC)~diff(PostCovid$NYSE), order=3)


####################VOLATLITY MODELLING

#Change for whole data,pregfc,postgfc etc ARCH
install.packages("fGarch")
install.packages("fBasics")
library(fBasics)
library(fGarch)

D<-dataset
pregfc <- subset(dataset, Date < as.Date("2007-12-28"))
postgfc <- subset(dataset, Date > as.Date("2007-12-28"))
preCovid <- subset(dataset, Date < as.Date("2019-12-28"))
PostCovid <-  subset(dataset, Date > as.Date("2019-12-28"))

#USA 
summary(garchFit(~garch(1,0), data = D$NYSE, trace = F))
summary(garchFit(~garch(1,0), data = pregfc$NYSE, trace = F))
summary(garchFit(~garch(1,0), data = postgfc$NYSE, trace = F))
summary(garchFit(~garch(1,0), data = preCovid$NYSE, trace = F))
summary(garchFit(~garch(1,0), data = PostCovid$NYSE, trace = F))

#Russia
summary(garchFit(~garch(1,0), data = D$RTSI, trace = F))
summary(garchFit(~garch(1,0), data = pregfc$RTSI, trace = F))
summary(garchFit(~garch(1,0), data = postgfc$RTSI, trace = F))
summary(garchFit(~garch(1,0), data = preCovid$RTSI, trace = F))
summary(garchFit(~garch(1,0), data = PostCovid$RTSI, trace = F))

#Southafrica
summary(garchFit(~garch(1,0), data = D$JSE, trace = F))
summary(garchFit(~garch(1,0), data = pregfc$JSE, trace = F))
summary(garchFit(~garch(1,0), data = postgfc$JSE, trace = F))
summary(garchFit(~garch(1,0), data = preCovid$JSE, trace = F))
summary(garchFit(~garch(1,0), data = PostCovid$JSE, trace = F))
#Brazil
summary(garchFit(~garch(1,0), data = D$IBOVESPA, trace = F))
summary(garchFit(~garch(1,0), data = pregfc$IBOVESPA, trace = F))
summary(garchFit(~garch(1,0), data = postgfc$IBOVESPA, trace = F))
summary(garchFit(~garch(1,0), data = preCovid$IBOVESPA, trace = F))
summary(garchFit(~garch(1,0), data = PostCovid$IBOVESPA, trace = F))

#China
summary(garchFit(~garch(1,0), data = D$SSC, trace = F))
summary(garchFit(~garch(1,0), data = pregfc$SSC, trace = F))
summary(garchFit(~garch(1,0), data = postgfc$SSC, trace = F))
summary(garchFit(~garch(1,0), data = preCovid$SSC, trace = F))
summary(garchFit(~garch(1,0), data = PostCovid$SSC, trace = F))
#India
summary(garchFit(~garch(1,0), data = D$BSE, trace = F))
summary(garchFit(~garch(1,0), data = pregfc$BSE, trace = F))
summary(garchFit(~garch(1,0), data = postgfc$BSE, trace = F))
summary(garchFit(~garch(1,0), data = preCovid$BSE, trace = F))
summary(garchFit(~garch(1,0), data = PostCovid$BSE, trace = F))


#Change for whole data ,pregfc,postgfc etc GARCH
D<-dataset
#USA 
summary(garchFit(formula = ~garch(1,1), data = D$NYSE, trace = F))
summary(garchFit(~garch(1,1), data = pregfc$NYSE, trace = F))
summary(garchFit(~garch(1,1), data = postgfc$NYSE, trace = F))
summary(garchFit(~garch(1,1), data = preCovid$NYSE, trace = F))
summary(garchFit(~garch(1,1), data = PostCovid$NYSE, trace = F))

#Russia
summary(garchFit(formula = ~garch(1,1), data = D$RTSI, trace = F))
summary(garchFit(~garch(1,1), data = pregfc$RTSI, trace = F))
summary(garchFit(~garch(1,1), data = postgfc$RTSI, trace = F))
summary(garchFit(~garch(1,1), data = preCovid$RTSI, trace = F))
summary(garchFit(~garch(1,1), data = PostCovid$RTSI, trace = F))

#Southafrica
summary(garchFit(formula = ~garch(1,1), data = D$JSE, trace = F))
summary(garchFit(~garch(1,1), data = pregfc$JSE, trace = F))
summary(garchFit(~garch(1,1), data = postgfc$JSE, trace = F))
summary(garchFit(~garch(1,1), data = preCovid$JSE, trace = F))
summary(garchFit(~garch(1,1), data = PostCovid$JSE, trace = F))

#Brazil
summary(garchFit(formula = ~garch(1,1), data = D$IBOVESPA, trace = F))
summary(garchFit(~garch(1,1), data = pregfc$IBOVESPA, trace = F))
summary(garchFit(~garch(1,1), data = postgfc$IBOVESPA, trace = F))
summary(garchFit(~garch(1,1), data = preCovid$IBOVESPA, trace = F))
summary(garchFit(~garch(1,1), data = PostCovid$IBOVESPA, trace = F))

#China
summary(garchFit(formula = ~garch(1,1), data = D$SSC, trace = F))
summary(garchFit(~garch(1,1), data = pregfc$SSC, trace = F))
summary(garchFit(~garch(1,1), data = postgfc$SSC, trace = F))
summary(garchFit(~garch(1,1), data = preCovid$SSC, trace = F))
summary(garchFit(~garch(1,1), data = PostCovid$SSC, trace = F))

#India
summary(garchFit(formula = ~garch(1,1), data = D$BSE, trace = F))
summary(garchFit(~garch(1,1), data = D$BSE, trace = F))
summary(garchFit(~garch(1,1), data = pregfc$BSE, trace = F))
summary(garchFit(~garch(1,1), data = postgfc$BSE, trace = F))
summary(garchFit(~garch(1,1), data = preCovid$BSE, trace = F))
summary(garchFit(~garch(1,1), data = PostCovid$BSE, trace = F))

