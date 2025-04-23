############################## Data Preprocessing ###############################
df<-read.csv("D:\\AD Data\\Desktop Data\\shrey_data_21062022.csv", header = T)
df$Date<-as.Date(df$Date, format = "%d-%b-%y")
str(df)
library(xts)
df.ts<-xts(df[,2:4], order.by = df$Date)
df.ts$WL
plot(df.ts$WL, main = "Groundwater Level at Patyapura Station", 
     xlab = "Time (in Quarter Years)", ylab = "Groundwater Level (in metres)",
     lwd = 1.7, col = "darkred")
plot(df.ts$M_Temp, main = "Temperature Level at Patyapura Station", 
     xlab = "Time (in Quarter Years)", ylab = "Temperature Level (in degrees Celsius)",
     lwd = 1.7, col = "darkred")
plot(df.ts$M_precip, main = "Precipitation Level at Patyapura Station", 
     xlab = "Time (in Quarter Years)", ylab = "Precipitation Level (in millimetres)",
     lwd = 1.7, col = "darkred")

############################ Copula based Model #############################################
Y <- df.ts$WL
plot(Y, type='l', lwd = 2, lty = "solid", col = "darkred",
     main="Run Chart for Groundwater Level at Patyapura Station",
     xlab="Year", ylab="PM2.5", cex.lab=1.3, cex.main=1.5)
grid()
# Histogram
hist(Y, prob=TRUE,
     main="Histogram for Groundwater Level at Patyapura Station",
     cex.lab=1.5, cex.main=1.4)
lines(density(Y), col = "navy") # add a density estimate with defaults
lines(density(Y, adjust=2), lty="dotted", lwd = 2, col = "darkred") # "smoother" density

#Neural Network Auto Regression
fit_cy <- nnetar(Y)
fcast_cy <- forecast(fit_cy)
plot(fit_cy$residuals, type = "l", xlab="Time", ylab='Residual',
     cex.lab = '1.5', main = paste('Residuals from', fcast_cy$method), lwd = 2, col = "darkred")
plot(as.vector(Y), type='l', lty=2, col='red', ylab='PM2.5',
     main = paste('Forecasts from', fcast_cy$method),
     cex.main = 1.5, cex.lab = 1.5)
lines(fit_cy$fitted, lwd = 1.5)
legend('topright', legend = c('Observed', 'Fitted'),
       lty=c(2,1), col=c('red','black'), lwd=c(1,1.5), cex=1.5)
## Durbin-Watson test for autocorrelation
print(dwtest(y~., data = data.frame(y = fit_cy$residuals[-(1:fit_cy$p)])))
## Compute residuals
ResCities1 <- fit_cy$residuals

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
#Granger Causaliy
caus.WL<-causality(model1, cause = "WL")
caus.prep<-causality(model1, cause = "M_precip")
caus.temp<-causality(model1, cause = "M_Temp")
# Impulse Response Function
irf.WL<-irf(model1, impulse = c("M_precip", "M_Temp"),
            response = "WL", n.ahead = 20, boot = TRUE, main = "ABCD")
irf.prec<-irf(model1, impulse = "WL",
              response = "M_precip", n.ahead = 20, boot = TRUE)
irf.temp<-irf(model1, impulse = "WL",
              response = "M_Temp", n.ahead = 20, boot = TRUE)
plot(irf.WL, main = "Orthogonal Impulse Response from Temperature Level",
     ylab = "Water Level (in metres)")
#Variance Decomposition
FEVD1<-fevd(model1, n.ahead = 10)
plot(FEVD1, col = c("darkgoldenrod1", "darkred", "darkgreen"))

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
     main = "Out of bag Absolute Percent Error (APE) v/s Number of quarters",
     xlab = "APE (VAR)", ylab = "No. of quarters")
abline(lm(seq(1, 26) ~ as.vector(ape)), col = "darkred",
       lwd = 2)
sl<-lm(seq(1, 26) ~ as.vector(ape))
shelf_life<-round((sl$coefficients)%*%c(1,5),0)

###############################################################################################
source("GW_Data_Pipeline_Code.txt")
str(data.final)
data.final$Station <- as.factor(data.final$Station)
data.final$Pre_Post_Monsoon <- as.factor(data.final$Pre_Post_Monsoon)
data.final$Level <- as.numeric(data.final$Level)
df_pre <- data.final[data.final$Pre_Post_Monsoon == "Premonsoon",]
df_post <- data.final[data.final$Pre_Post_Monsoon == "Postmonsoon",]
u1 <- df_pre$Level[df_pre$Station == levels(data.final$Station)[1]]
v1 <- df_pre$Level[df_pre$Station == levels(data.final$Station)[2]]
length(u1)
#Neural Network Auto Regression
fit_cy <- nnetar(u1)
fcast_cy <- forecast(fit_cy)
u <- fcast_cy$residuals
fit_cy1 <- nnetar(v1)
fcast_cy1 <- forecast(fit_cy1)
v <- fcast_cy1$residuals
data <- cbind.data.frame(u = u[-1], v)

Empiric.df<-function(data,x) {
  data<-sort(data)
  if(min(data)>0) a<-0 else a<-floor(min(data)/100)*100
  if(max(data)<0) b<-0 else b<-ceiling(max(data)/100)*100
  for(j in 1:length(x)) {
    if(x[j]<a) x[j]<-a
    if(x[j]>b) x[j]<-b
  }
  data<-c(a,data,b)
  n<-length(data)
  p<-c(rep(0,(n-1)))
  q<-c(rep(0,(n-1)))
  for(i in 2:(n-2)) {
    p[i]<-(data[i]+data[i+1])/2
    q[i]<-(i-1)/(n-2)
  }
  p[1]<-a
  p[n-1]<-b
  q[1]<-0
  q[n-1]<-1
  approx(p,q,xout=c(x))$y
}
## Transform original data to U(0,1)
Emp.index <- data.frame(u = Empiric.df(data[,1], data[,1]),
                        v = Empiric.df(data[,2],data[,2]))
## Compute CDD
r12 <- gcmr(u~v, data = Emp.index, marginal = beta.marg(link = "logit"),
            cormat = arma.cormat(0, 0) )
Er12 <- exp(r12$estimate[1] + Emp.index$v * r12$estimate[2])/
  (1 + exp(r12$estimate[1] + Emp.index$v * r12$estimate[2]))
vtou_rho2 <- var(Er12)/var(Emp.index$u)
r21 <- gcmr(v~u, data = Emp.index, marginal = beta.marg(link = "logit"),
            cormat = arma.cormat(0, 0) )
Er21 <- exp(r21$estimate[1] + Emp.index$u * r21$estimate[2])/
  (1 + exp(r21$estimate[1] + Emp.index$u * r21$estimate[2]))
utov_rho2 <- var(Er21)/var(Emp.index$v)
rslt <- data.frame(vtou_rho2,utov_rho2)