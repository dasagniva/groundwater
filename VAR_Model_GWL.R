################ Data Preprocessing #####################

df<-read.csv("D:\\AD Data\\Desktop Data\\shrey_data_21062022.csv", header = T)
df$Date<-as.Date(df$Date, format = "%d-%b-%y")
str(df)
library(xts)
df.ts<-xts(df[,2:4], order.by = df$Date)
df.ts$WL
plot(df.ts$WL)


############## Model Development ################
library(forecast)
summary(lm(WL ~ M_precip + M_Temp, data = df.ts))

par(mfrow = c(2,3))
acf(df.ts$M_precip, main = "Quarterly Precipitation")
pacf(df.ts$M_precip, main = "Quarterly Precipitation")
acf(df.ts$M_Temp, main = "Quarterly Temperature")
pacf(df.ts$M_Temp, main = "Quarterly Temperature")
acf(df.ts$WL, main = "Quarterly Groundwater Level")
pacf(df.ts$WL, main = "Quarterly Groundwater Level")
par(mfrow = c(1,1))

library(vars)
lagselect<-VARselect(df.ts, lag.max = 10, type = "const")
model1<-VAR(df.ts, p = 4, type = "const", season = NULL, exogen = NULL)
summary(model1)



############## Model Diagnostics #####################

#Test for Serial Autocorrelation (Portmanteau Test)
ser1<-serial.test(model1, lags.pt = 12, type = "PT.asymptotic")

#Test for heteroscedasticity (ARCH test)
heter.test<-arch.test(model1, lags.multi = 12, multivariate.only = T)

#Test for Normality of Residuals
norm <- normality.test(model1, multivariate.only = T)


#Test for structural breaks in residuals
mod.stab<-stability(model1, type = "OLS-CUSUM")
plot(mod.stab)


#Granger Causality
caus.WL<-causality(model1, cause = "WL")
caus.prep<-causality(model1, cause = "M_precip")
caus.temp<-causality(model1, cause = "M_Temp")

# Impulse Response Function
irf.WL<-irf(model1, impulse = c("M_precip", "M_Temp"),
            response = "WL", n.ahead = 20, boot = TRUE)
irf.prec<-irf(model1, impulse = "WL",
            response = "M_precip", n.ahead = 20, boot = TRUE)
irf.temp<-irf(model1, impulse = "WL",
            response = "M_Temp", n.ahead = 20, boot = TRUE)


plot(irf.temp)

#Variance Decomposition
FEVD1<-fevd(model1, n.ahead = 10)
plot(FEVD1)



######################## Forecasting #########################


#Forecasting
forecasts<-predict(model1, n.ahead = 10, ci = 0.95)
plot(forecasts, names = "WL")
fc<-forecasts$fcst
plot(fc$WL[,1])

train.prop<-0.7

k<-round(train.prop*nrow(df.ts), 0)
train<-df.ts[1:k,]
test<-df.ts[(k+1):nrow(df.ts),]
model.train<-VAR(train, p = 4, type = "const", season = NULL, exogen = NULL)
fc1<-predict(model.train, n.ahead = nrow(test), ci = 0.95)
fc<-fc1$fcst
fit<-as.xts(fc$WL[,1],order.by = index(test))
wl<-test$WL
wl[wl==0]<-NA
ape<-(100*abs(wl-fit)/wl)
plot(seq(1, 26) ~ as.vector(ape), pch = 19, col = "darkgreen",
     main = "Out of bag Absolute Percent Error (APE) v/s Number of days",
     xlab = "APE", ylab = "No. of days")
abline(lm(seq(1, 26) ~ as.vector(ape)), col = "darkred",
       lwd = 2)
sl<-lm(seq(1, 26) ~ as.vector(ape))
shelf_life<-round((sl$coefficients)%*%c(1,5),0)
