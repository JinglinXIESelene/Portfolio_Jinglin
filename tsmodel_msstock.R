

setwd("~/Desktop/portfolio_projects/R_working directory")
library(xts)
library(zoo)
library(tseries)
library(forecast)
library(lubridate)
data = read.csv("Microsoft_Stock.csv",header = T)
data$Date = as.character(data$Date)
data$Date = as.Date(data$Date,"%m/%d/%Y")
attach(data)

n = dim(data)[1]
logreturn = log(Close[2:n]/Close[1:n-1])
ts_stock_return = xts(cbind(data$Close[2:n],logreturn), data$Date[2:n])
colnames(ts_stock_return) = c("Close Price","Log Return")
#png("pic01.png",width = 4000, height = 480)
par(mfrow = c(1,2))
plot(ts_stock_return$`Close Price`, type = "l",main = "Close Price of Microsoft Over the Years")
plot(ts_stock_return$`Log Return`, main = "Log Return of Microsoft Over the Years")
graphics.off()

#
history_return = ts_stock_return$`Log Return`[1:1410]

#try ARMA model first
adf.test(history_return)
par(mfrow = c(1,2))
acf(history_return)
pacf(history_return) #ARMA
graphics.off()

model1 = auto.arima(history_return, max.p = 3, max.q = 3, ic = c("aic"))
summary(model1)
acf(model1$residuals)
Box.test(model1$residuals,lag = 5, type = c("Ljung"))#p=0.32 indicating there is no autocorrelation
acf(model1$residuals^2) #the heteroscedasticity has been detected!
qqnorm(model1$residuals)
qqline(model1$residuals) 

#add ARCH effect
library(fGarch)
garch_model1 = garchFit(formula = ~arma(1,0) + garch(1,1), history_return)
summary(garch_model1)
garch_model1@fit$matcoef
res = residuals(garch_model1)
res_std1 = res/garch_model1@sigma.t
#png("pic02.png",width = 2000, height = 480)
par(mfrow = c(1,2))
qqnorm(res_std1)
qqline(res_std1)
qqplot(qt(ppoints(length(res_std1)), df = 4), res_std1, xlab = "Q-Q plot for t dsn",ylab="t-quantiles", main = "T-distribution Q-Q Plot")
qqline(res_std1)
library(MASS)
e = residuals(garch_model1)/garch_model1@sigma.t #Standardized
fitdistr(e,"t")
n = length(e)
grid = (1:n)/(n+1)
qqplot(sort(as.numeric(e)), qt(grid,df=4),
       main="t-plot, df=4",xlab= "Standardized residual quantiles",
       ylab="t-quantiles")
abline(   lm(   qt(c(.25,.75),df=4)~quantile(e,c(.25,.75))   )   )

#detect that the t-distribution is more suitable for white noise
#refit using t-distribution
garch_model2 = garchFit(formula = ~arma(1,0) + garch(1,1), history_return, cond.dist = "std")
summary(garch_model2)
garch_model2@fit$matcoef
res2 = residuals(garch_model2)
res_std2 = res2/garch_model2@sigma.t
par(mfrow = c(1,3))
plot(res_std2, type = "l")
acf(res_std2)
acf(res_std2^2)
graphics.off()
Box.test(res_std2,lag = 10, type = c("Ljung"))

#forecast
pred = predict(garch_model2,n.ahead=100) #prediction value
plot(ts_stock_return$`Log Return`[1450:1510], type = "l")
dates = seq(as.Date("2021-03-12"), as.Date("2021-03-31"), by = 1)
length(dates)
lines(dates,pred$meanForecast,col=5)

#confidence interval
a = append(logreturn[1:1410], pred$meanForecast+1.96*pred$standardDeviation)
b = append(logreturn[1:1410], pred$meanForecast-1.96*pred$standardDeviation)
ts_compare = xts(cbind(logreturn,a,b),data$Date[2:n])
plot(ts_compare[1200:1510],type = "l")
ts_compare[1200:1510]

#simulation
mu = garch_model2@fit$par[1]
phi = garch_model2@fit$par[2]
omega = garch_model2@fit$par[3]
alpha1 = garch_model2@fit$par[4]
beta1 = garch_model2@fit$par[5]
u0 = garch_model2@residuals[1410]
sigma_t0 = garch_model2@sigma.t[1410]
sigma_hat = pred$standardDeviation

set.seed(5721)
num = 100
v = rnorm(num)

garch_simulation = function(num, sigma_hat, v, omega, alpha, beta, u0, sigma_t0){    
  list_u = c()
  for (i in 1:num){
    if (i == 1) { 
      u_i = u(omega, alpha, beta, u0, sigma_t0, v[i])
      list_u = append(list_u, u_i) #list_u[1] = u_1
      i = i+1
    }
    else{ #i=2,3,4,,
      u_i = u(omega, alpha, beta, list_u[i-1], sigma_hat[i-1], v[i])
      i = i+1
      list_u = append(list_u, u_i) #把u[i] 加到末尾
      }
  }
  return(list_u)
}

#calculating u[i]
u = function(omega, alpha, beta, u_i, sigma_hat_i, v_i_plus1 ){     
  u_i_plus1 = sqrt(omega + alpha*u_i^2 + beta* sigma_hat_i^2)*v_i_plus1
  return (u_i_plus1)
}

Y = function(num, phi, mu, u, y0){
  list_y = c()
  for (i in 1:num){
    if (i == 1){
      y_i = (1-phi)*mu + phi*y0+ u[i]
      list_y = append(list_y, y_i)
    }
    else{
      y_i = (1-phi)*mu + phi*list_y[i-1]+ u[i]
      list_y = append(list_y, y_i)
    }
  }
  return (list_y)
}

GarchProcess_u = garch_simulation(num, sigma_hat, v, omega, alpha1, beta1, u0, sigma_t0 )
Grach_Process_pred = Y(num, phi, mu, GarchProcess_u, ts_stock_return$`Log Return`[1410] )

c = append(logreturn[1:1410], Grach_Process_pred)
ts_compare = xts(cbind(logreturn,a,b,c),data$Date[2:n])
plot(ts_compare[1200:1510],type = "l")
sum(abs(ts_compare$logreturn - ts_compare$c)[1410:1510])




