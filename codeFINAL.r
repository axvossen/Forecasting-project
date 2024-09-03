library("tseries")
library("readxl")
library("stats")
library("forecast")
library("vars")
library("car")
library("xgboost")
library("caTools")
library("dplyr")
library("caret")
library("MASS")
library("tidyverse")
library("broom")
library("glmnet")
library("data.table")
library("mlr")
library("zoo")

#GDP
GDP2000<-read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/RGDPPB2000.xls", range= "A11:B106")
gdp2k_ts<-ts(GDP2000$numeric, start=2000, frequency = 4)
gdp2k_ld <- diff(log(gdp2k_ts))
gdp2k_clean <- tsclean(gdp2k_ld)


#Time series analysis
plot(GDP2000, main = "Real GDP of the Netherlands", ylab = "Real GDP", xlab = "Year", type = "l", col = "chocolate1")
plot(gdp2k_ld, col='blue')
lines(gdp2k_clean, col='red')
#Test stationarity of the RGDP####
adf.test(gdp2k_ts) #non stationarity
adf.test(gdp2k_ld) #if p-value<0.05 --> model is stationary --> ARMA(p,q)
gdp2k_ld %>% ur.kpss() %>% summary() #check with KPSS model H0: ts is stationary
jarque.bera.test(gdp2k_ld) #not normally distributed
plot(gdp2k_ld, main = "Real GDP of the Netherlands", ylab = "Real GDP", xlab = "Year", type = "l", col = "chocolate1")

#identify presence of outliers adn manage them
boxplot(gdp2k_ld, main ="boxplot of the log-differenced RGDP values", col = "green") #there are outliers
outliersgdp <- boxplot(gdp2k_ld)$out #identify value of outliers ~boxplot.stats
outliersgdp
gdp2k_clean <- tsclean(gdp2k_ld) #remove outliers
jarque.bera.test(gdp2k_clean) #follows a Normal distribution without outliers
boxplot(gdp2k_clean, main = "Boxplot of the clean RGDP", col = "chocolate1")

old.par <- par(mfrow=c(1,2))
boxplot(gdp2k_ld, main ="Boxplot of the log-differenced RGDP values", col = "chocolate1") #there are outliers
boxplot(gdp2k_clean, main = "Boxplot of the clean RGDP", col = "chocolate1") #there stay just 3 outliers instead of 7
par(old.par) 

#Comparison with/without outliers 
mean_with_outliers <- mean(gdp2k_ld)
median_with_outliers <- median(gdp2k_ld)
gdp_without_outliers <- gdp2k_ld[!gdp2k_ld %in% outliersgdp]
mean_without_outliers <- mean(gdp_without_outliers)
median_without_outliers <- median(gdp_without_outliers)
cat("With outlier: Mean =", mean_with_outliers, " Median =", median_with_outliers, "\n")
cat("Without outliers: Mean =", mean_without_outliers, " Median =", median_without_outliers, "\n")

#plot ACF and PACF####
old.par <- par(mfrow=c(2,1))
acf(gdp2k_clean, col='chocolate1') #observe a sinusoidal decaying funtion: AR(p) assumption
pacf(gdp2k_clean, col='chocolate1') #last significant spike at lag 2: p=2
par(old.par)

#ARIMA model####
ARIMA_gdpcl <- auto.arima(gdp2k_clean)#verify previous hypothesis of an AR(2) model
summary(ARIMA_gdpcl)
#test the residuals
checkresiduals(ARIMA_gdpcl, col='chocolate1')
bgtest(ARIMA_gdpcl$residuals~1)#no autocorrelation
Box.test(ARIMA_gdpcl$residuals, lag=log(length(ARIMA_gdpcl$residuals)), type="Ljung-Box")
gqtest(ARIMA_gdpcl$residuals~1)#homoscedatsicity
bptest(ARIMA_gdpcl$residuals~fitted(ARIMA_gdpcl))
jarque.bera.test(ARIMA_gdpcl$residuals) #H0

fitted.values(ARIMA_gdpcl)
#forecasts
bj_gdp_fcst <- forecast(ARIMA_gdpcl,h=12)
bj_gdp_fcst
accuracy(bj_gdp_fcst)
plot(bj_gdp_fcst, col="chocolate1", fcol="chocolate1", pi.col="blue",
     shadecols = c("moccasin", "navajowhite2"),
     main="Forecast",
     xlab="Time",
     ylab="Log-Differenced RGDP")
arimaprediction <- bj_gdp_fcst$mean
### Import data ####
#add new variables
SER2K <- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/SER2000.xls", range="A11:B106")
Prod2K <- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/prod2000.xls", range="A11:B106")
EPU2K <- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/EPU2000.xls", range="A11:B106")
REER2K <-read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/REER2000.xls", range="A11:B106")
exp2K <- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/exports2000.xls", range="A11:B106")
imp2k <- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/imports2000.xls", range="A11:B106")
EP2K <- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/EP2000.xls", range="A11:B106")
RM2K <- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/rawmaterial2000.xls", range="A11:B106")
edh2K <- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/edhealth2000.xls", range="A11:B106")
oil2k<- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/Oil2000.xls", range="A11:B106")
PPI2K <- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/PPI2000.xls", range="A11:B106")
unemp2K <- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/unemployment2000.xls", range="A11:B106")
govexp2K <- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/govexp2000.xls", range="A11:B106")
ret2k <- read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/retail2000.xls", range="A11:B106")
PCE2K <-read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/PCE2000.xls", range="A11:B106")
#ts
SER2K_ts <- ts(SER2K$numeric, start=2000, frequency = 4)
Prod2K_ts <- ts(Prod2K$numeric, start=2000, frequency = 4)
EPU2K_ts <- ts(EPU2K$numeric, start=2000, frequency = 4)
REER2K_ts <- ts(REER2K$numeric, start=2000, frequency = 4)
exp2K_ts <- ts(exp2K$numeric, start=2000, frequency = 4)
imp2k_ts <- ts(imp2k$numeric, start=2000, frequency = 4)
EP2K_ts <- ts(EP2K$numeric, start=2000, frequency = 4)
RM2K_ts <- ts(RM2K$numeric, start=2000, frequency = 4)
edh2K_ts <- ts(edh2K$numeric, start=2000, frequency = 4)
oil2k_ts <- ts(oil2k$numeric, start=2000, frequency = 4)
PPI2K_ts <- ts(PPI2K$numeric, start=2000, frequency = 4)
unemp2K_ts <- ts(unemp2K$numeric, start=2000, frequency = 4)
govexp2K_ts <- ts(govexp2K$numeric, start=2000, frequency = 4)
ret2k_ts <- ts(ret2k$numeric, start=2000, frequency = 4)
PCE2K_ts <- ts(PCE2K$numeric, start=2000, frequency = 4)


#difflog
SER2K_ld <- diff(log(SER2K_ts))
Prod2K_ld <- Prod2K_ts[2:95]
EPU2K_ld <- diff(log(EPU2K_ts))
REER2K_ld <- diff(log(REER2K_ts))
exp2K_ld<- diff(log(exp2K_ts))
imp2k_ld<- diff(log(imp2k_ts))
EP2K_ld<- diff(log(EP2K_ts))
RM2K_ld<- diff(log(RM2K_ts))
edh2K_ld<- diff(log(edh2K_ts))
oil2k_ld<- diff(log(oil2k_ts))
PPI2K_ld<- diff(log(PPI2K_ts))
unemp2K_ld<- diff(log(unemp2K_ts))
govexp2K_ld<- diff(log(govexp2K_ts))
ret2k_ld<- diff(log(ret2k_ts))
PCE2K_ld <- diff(log(PCE2K_ts))

# ARIMAX ####
ARIMAX_1 <- arima(gdp2k_clean, order = c(2,0,0), xreg = Prod2K_ld)
summary(ARIMAX_1)
ARIMAX_2 <- arima(gdp2k_clean, order = c(2,0,0), xreg = unemp2K_ld)
summary(ARIMAX_2)
ARIMAX_3 <- arima(gdp2k_clean, order = c(2,0,0), xreg = edh2K_ld)
summary(ARIMAX_3)
ARIMAX_4 <- arima(gdp2k_clean, order = c(2,0,0), xreg = exp2K_ld)
summary(ARIMAX_4)
ARIMAX_5 <- arima(gdp2k_clean, order = c(2,0,0), xreg = imp2k_ld)
summary(ARIMAX_5)

#Part II: Multivariate Forecast####

lmall2 <- lm(gdp2k_ld~unemp2K_ld + exp2K_ld + imp2k_ld + govexp2K_ld + SER2K_ld + Prod2K_ld +EPU2K_ld
            +REER2K_ld+ EP2K_ld + RM2K_ld + edh2K_ld + oil2k_ld + PPI2K_ld + ret2k_ld)
summary(lmall2)

lmsign22<- lm(gdp2k_ld~PPI2K_ld +exp2K_ld  + ret2k_ld+ unemp2K_ld +Prod2K_ld+RM2K_ld+REER2K_ld+SER2K_ld+PCE2K_ld)
summary(lmsign22)

#test variables
#PPI
summary(ur.df(PPI2K_ld, type = "none"))#stationary
adf.test(diff(PPI2K_ld))
jarque.bera.test(PPI2K_ld)#NO
PPI_clean <- tsclean(PPI2K_ld)
jarque.bera.test(PPI_clean) #still not normal 
adf.test(PPI_clean)

#export
expor2Kts_clean <- tsclean(exp2K_ld)
adf.test(exp2K_ld)#yes
summary(ur.df(expor2Kts_clean,type="none"))#yes
jarque.bera.test(exp2K_ld)#no
jarque.bera.test(expor2Kts_clean)#no

#EPU
adf.test(EPU2K_ld)#yes
summary(ur.df(EPU2K_ld, type ="none"))#yes
jarque.bera.test(EPU2K_ld)#yes

#prod
adf.test(Prod2K_ld)#yes
summary(ur.df(Prod2K_ld, type = "none"))#yes
jarque.bera.test(Prod2K_ld)#yes

#retail
retail_clean <- tsclean(ret2k_ld)
adf.test(retail_clean)
summary(ur.df(ret2k_ld, type = "none"))#yes
jarque.bera.test(ret2k_ld)#no
jarque.bera.test(retail_clean) #nop

#Energy prices
EP_clean <- tsclean(EP2K_ld)
adf.test(EP_clean)#yes
summary(ur.df(EP_clean, type = "none"))#yes
jarque.bera.test(EP2K_ld)#no
jarque.bera.test(EP_clean)#no

#unemployment
adf.test(unemp2K_ld)#yess
jarque.bera.test(unemp2K_ld)
UE_clean <- tsclean(unemp2K_ld)
jarque.bera.test(UE_clean)#yeess


#REER
adf.test(REER2K_ld)#ok
jarque.bera.test(REER2K_ld)#good


#RM
adf.test(RM2K_ld)#yes
jarque.bera.test(RM2K_ld)
RM_clean <- tsclean(RM2K_ld)
jarque.bera.test(RM_clean)#yes
adf.test(RM_clean)#also yes

#SER
adf.test(SER2K_ld)#ok
jarque.bera.test(SER2K_ld)#ok

#PCE
adf.test(PCE2K_ld)#yes
jarque.bera.test(PCE2K_ld)
PCE2K_clean <- tsclean(PCE2K_ld)
jarque.bera.test(PCE2K_clean)#yes

###VAR####
varendo22 <- data.frame(gdp2k_clean,Prod2K_ld,UE_clean,PCE2K_clean)
varexo22 <- data.frame(SER2K_ld, EPU2K_ld,RM_clean,REER2K_ld)
VARselect(varendo22, lag.max = 10, type="both", exogen =varexo22)
Varm <- VAR(varendo22,p=1)
Varm
fittedVAR <- fitted.values(Varm)

#GRANGER TEST ####
#gdp
CTgdp <- causality(Varm,cause = "gdp2k_clean")$Granger
CTgdp
CGexp <- causality(Varm, cause = "UE_clean")$Granger
CGexp
CGprod <- causality(Varm, cause = "Prod2K_ld")$Granger
CGprod
CGPCE <- causality(Varm, cause="PCE2K_clean")$Granger
CGPCE

grangertest(UE_clean~gdp2k_clean, order=1)
grangertest(UE_clean~Prod2K_ld, order=1)
grangertest(UE_clean~PCE2K_clean, order=1)

#Test of the VAR
#autocorrelation
serial <- serial.test(Varm)
serial #no autocorrelation
#Homoscedasticity
arch <- arch.test(Varm)
arch#no heteroscedasticity
#normality
N<-normality.test(Varm)
N #normally distributed

#forecast
forecastVAR <- predict(Varm,n.ahead = 12)
plot(forecastVAR, xlag="Quarters")

######## VECM  ###########
vecm.test <- ca.jo(varendo22,type="eigen",ecdet ="none",K = 2 , spec ="transitory",season = 4)
summary(vecm.test)
vec2var_vecm.test <-vec2var(vecm.test, r =3)
vec2var_vecm.test
fitted(vec2var_vecm.test)

# Forecast of 12Q
forecast_vecm<- predict(vec2var_vecm.test,n.ahead =12)
forecast_vecm
plot(forecast_vecm, xlab = "N of quarters", ylab ="Values of the series")

#Test of the VECM
#autocorrelation
serial <- serial.test(vec2var_vecm.test)
serial #no autocorrelation
#Homoscedasticity
arch <- arch.test(vec2var_vecm.test)
arch#no heteroscedasticity
#normality
N<-normality.test(vec2var_vecm.test)
N #normally distributed

#accuracy
accdata <- varendo22[1:84,]
accVECM <- ca.jo(accdata, ecdet="none",K=2, season = 4)
summary(accVECM)
v2v_acc <- vec2var(accVECM, r=3)
fcst_accVECM <- predict(v2v_acc, n.ahead = 10)
plot(fcst_accVECM)

ModelVECM<- vec2var_vecm.test
res <- residuals(ModelVECM)
fits <- fitted(ModelVECM)
for(i in 1:1){
  fc <- structure(list(mean=forecast_vecm$fcst[[i]][,"fcst"], x=varendo22[,i],
                       fitted=c(NA,NA,fits[,i])),class="forecast")
  print(forecast::accuracy(fc))
}


old.par <- par(mfrow=c(4,1))

plot(fcst_accVECM$fcst$gdp2k_clean[,1], type ='l',ylim=c( -0.03 ,0.03),col='green', main="Accuracy of the forecast from a VECM model for the RGDP",xlab= "12 last quarters", ylab="Values")
lines(varendo22$gdp2k_clean[84:96],type="l",col='blue')
legend("bottomleft", lty=1 ,legend=c("observed data","forecast"),col=c("blue","green"), title="Group")

plot(fcst_accVECM$fcst$Prod2K_ld[,1], type ='l', ylim=c( -5 ,5),col='green',main="Accuracy of the forecast from a VECM model for the Production growth",xlab= "12 last quarters", ylab="Values")
lines(varendo22$Prod2K_ld[84:96],type= "l", col='blue')
legend("bottomleft", lty=1 ,legend=c("observed data","forecast"),col=c("blue","green"), title="Group")

plot(fcst_accVECM$fcst$UE_clean[,1], type='l',ylim=c(-0.2,0.2),col='green',main="Accuracy of the forecast from a VECM model for the Unemployment rate", xlab= "12 last quarters", ylab="Values")
lines(varendo22$UE_clean[84:96], type ='l',col="blue")
legend("bottomleft", lty=1 ,legend=c("observed data","forecast"),col=c("blue","green"), title="Group")

plot(fcst_accVECM$fcst$PCE2K_clean[,1], type='l',ylim=c(-0.2,0.2),col='green',main="Accuracy of the forecast from a VECM model for the PCE", xlab= "12 last quarters", ylab="Values")
lines(varendo22$PCE2K_clean[84:96], type ='l',col="blue")
legend("bottomleft", lty=1 ,legend=c("observed data","forecast"),col=c("blue","green"), title="Group")
par(old.par)

##########################################################################"
####Part III: INNOVATION ####

#1.Kalman Filter####
#We compute KF to obtain a smoother ts to train the ANN code at step 2. Thus we will have a better prediction.
library(KFAS)
library(readxl)
library(dlm)
#import data again to be sure
GDP2000<-read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/RGDPPB2000.xls", range= "A11:B106")
gdp2k_ts<-ts(GDP2000$numeric, start=2000, frequency = 4)
gdp2k_ld <- diff(log(gdp2k_ts))
gdp2k_clean <- tsclean(gdp2k_ld)

if (!require(dlm)) {
  install.packages("dlm")
  library(dlm)
}

# modèle de filtre de Kalman
kalman_model <- dlm(
  FF = matrix(1),          # Matrice d'observation
  V = matrix(0.01),       # Variance de l'observation
  GG = matrix(1),          # Matrice de transition d'état
  W = matrix(0.01),       # Variance de la transition d'état
  m0 = matrix(0),          # État initial
  C0 = matrix(1)           # Variance de l'état initial
)

# filtre de Kalman 
kalman_filter <- dlmFilter(gdp2k_clean, mod = kalman_model)

# les états filtrés
filtered_states <- kalman_filter$m

#ariances estimées des états
state_variances <- var(kalman_filter$m)

print(filtered_states)
print(state_variances)

plot(filtered_states, type = 'l', col = 'blue')


# déf les données 
observations <- rnorm(94) 
filtered_states <- rnorm(94) 
time <- 1:94 

# intervalles de confiance
ci_upper <- filtered_states + 1.96 * sqrt(state_variances)
ci_lower <- filtered_states - 1.96 * sqrt(state_variances)

par(mar=c(5,5,2,2))

# observations
plot(time, observations, type = 'p', pch = 20, col = rgb(1,0,0,0.5), xlab = 'Time', ylab = 'Values', main = 'Comparison of observations and estimated states')

#stats filtrés
lines(time, filtered_states, col = 'cyan4', lwd = 2)


axis(2, at=seq(min(time), max(time), by=10), labels=seq(min(time), max(time), by=10))
grid(nx = NULL, ny = NULL, col = "gray", lty = 2)
legend("bottomleft", inset=.05, c("Observations","Estimated statements"), 
       col=c("chocolate1", "cyan4"), lwd=c(1,2), lty=c(NA,1), pch=c(20,NA), bty="n")


# graphique de densité des résidus

residuals <- gdp2k_clean - filtered_states
par(mfrow = c(1, 1))
hist(residuals, col = "azure1", main = "Residual Distribution", xlab = "Residuals")
lines(density(residuals), col = "chocolate1")

# nuage de points
par(mfrow = c(1, 1))
plot(filtered_states, gdp2k_clean, pch = 20, col = "cyan4", xlab = "Estimated unrealised gains and losses", ylab = "Data observed", main = "Point cloud")
abline(0, 1, col = "red", lty = 2)



#2. ANN ####
library(neuralnet)
GDP2000<-read_excel("C:/Users/axelv/OneDrive - UCL/Bureau/Master LSM/Période 2/Forecasting/Projet/Data/2000/RGDPPB2000.xls", range= "A11:B106")
gdp2k_ts<-ts(GDP2000$numeric, start=2000, frequency = 4)
gdp2k_ld <- diff(log(gdp2k_ts))
gdp2k_clean <- tsclean(gdp2k_ld)

dlm_filtered <- kalman_filter
# Extract the smoothed states
smoothed_gdp2k <- dropFirst(dlmSmooth(dlm_filtered)$s)
smoothed_gdp2k
# Create a lagged dataset for ANN training
max_lag <- 4  # Number of past quarters to use
data_length <- length(smoothed_gdp2k)

# Initialize a matrix for the lagged data
lagged_data <- matrix(NA, nrow = data_length - max_lag, ncol = max_lag + 1)

for (i in 1:(max_lag + 1)) {
  lagged_data[, i] <- smoothed_gdp2k[(1 + max_lag - i + 1):(data_length - i + 1)]
}

# Convert to a data frame
col_names <- c(paste0("Lag_", max_lag:1), "Target")
lagged_data <- data.frame(lagged_data, row.names = NULL)
names(lagged_data) <- col_names

# Split into training and test sets (if required)
set.seed(123)  # For reproducibility
training_indices <- sample(1:nrow(lagged_data), size = 0.8 * nrow(lagged_data))
training_data <- lagged_data[training_indices, ]
test_data <- lagged_data[-training_indices, ]


# Train the network
set.seed(123)  # For reproducibility
nn <- neuralnet(Target ~ Lag_4 + Lag_3 + Lag_2 + Lag_1, data = training_data,
                hidden = c(5), linear.output = TRUE)
# Initialize a vector to hold forecasts
forecasts <- numeric(12)

# Assume the last observations are available from the actual data
last_obs <- tail(lagged_data, 1)

for (i in 1:12) {
  # Predict the next step
  prediction <- compute(nn, last_obs[, 1:max_lag])
  
  # Store the forecast
  forecasts[i] <- prediction$net.result
  
  # Update the last observations to include the new prediction
  last_obs <- cbind(prediction$net.result, last_obs[, 1:(max_lag - 1)])
}

# Calculate the predicted values for the test set
predicted_values <- compute(nn, test_data[, 1:max_lag])
predicted_values <- predicted_values$net.result

# Actual target values from the test set
actual_values <- test_data$Target

# Calculate the error metric, e.g., Mean Squared Error
MSE <- mean((predicted_values - actual_values)^2)
MSE

#Code for IC

set.seed(123)  # For reproducibility

# Number of bootstrap replications
n_boot <- 1000

# Initialize matrix to store the bootstrapped forecasts
bootstrap_forecasts <- matrix(NA, nrow = n_boot, ncol = 12)

# Perform the bootstrapping
for (b in 1:n_boot) {
  boot_indices <- sample(nrow(training_data), replace = TRUE)
  boot_data <- training_data[boot_indices, ]
  nn_boot <- neuralnet(Target ~ Lag_4 + Lag_3 + Lag_2 + Lag_1, data = boot_data, hidden = c(5), linear.output = TRUE)
  last_obs_boot <- tail(lagged_data, 1)
  
  for (i in 1:12) {
    prediction_boot <- compute(nn_boot, last_obs_boot[, 1:max_lag])
    bootstrap_forecasts[b, i] <- prediction_boot$net.result
    last_obs_boot <- cbind(prediction_boot$net.result, last_obs_boot[, 1:(max_lag - 1)])
  }
}

# Calculate the mean and standard deviation of the forecasts
forecast_means <- apply(bootstrap_forecasts, 2, mean)
forecast_sds <- apply(bootstrap_forecasts, 2, sd)

# Assuming a 95% confidence interval
alpha <- 0.05
lower_bounds <- forecast_means - qnorm(1 - alpha/2) * forecast_sds
upper_bounds <- forecast_means + qnorm(1 - alpha/2) * forecast_sds
lower_bounds#clearly heteroskedastic
upper_bounds

# Assuming 'last_date' is an object of class Date
last_date <- as.Date("2000-01-01")
# create a sequence of dates for plotting
date_seq <- seq(last_date, by = "quarter", length.out = length(gdp2k_clean))

# Create a sequence of dates for the forecast
forecast_date_seq <- seq(tail(date_seq, 1) + 1, by = "quarter", length.out = length(forecasts))

# Define the range for the x-axis to cover both historical and forecast dates
range_dates <- c(date_seq, forecast_date_seq)

# Plotting the forecast with confidence intervals
plot(date_seq,smoothed_gdp2k, type = "l", col = "blue", xlab = "Date", ylab = "Log Difference of GDP", main = "GDP Forecast", xlim = range(range_dates))
lines(forecast_date_seq, forecasts, col = "red", lty = 2)
lines(forecast_date_seq,arimaprediction, col='orange')
lines(forecast_date_seq, forecast_vecm$fcst$gdp2k_clean[1:12,1], col='cyan3')
lines(forecast_date_seq,forecastVAR$fcst$gdp2k_clean[1:12,1],col='darkorchid')
lines(forecast_date_seq, lower_bounds, col='green')
lines(forecast_date_seq, upper_bounds, col='green')
legend("topleft",legend = "ANN forecast", col='red',lty = c(2))
legend("topleft", legend = c("Historical Data", "ANN Forecast", "95% CI ANN", "forecast ARIMA", "forecast VECM", "forecast VAR"), col = c("blue", "red", "green", "orange", "cyan4", "darkorchid"), lty = c(1, 2, 2,2,2,2))

#Only forecasted period
plot(forecast_date_seq, type= "l",forecasts, col='red', ylim=c(-0.005,0.015))
lines(forecast_date_seq,arimaprediction, col='orange')
lines(forecast_date_seq, forecast_vecm$fcst$gdp2k_clean[1:12,1], col='cyan3')
lines(forecast_date_seq,forecastVAR$fcst$gdp2k_clean[1:12,1],col='darkorchid')
legend("topleft", legend = c("ANN Forecast", "forecast ARIMA", "forecast VECM", "forecast VAR"), col = c("red", "orange", "cyan4", "darkorchid"), lty = c(2,2,2,2))

plot(nn, rep="best", show.weights=FALSE , col.entry='seagreen4', col.hidden='royalblue',col.out='gold')

#### Comparison of the fitted value before and after covid ####
# We assume the covid period with its worse effect is the whole year of 2020 
#2020 = data 80 to 84.
fittedARIMA <- fitted.values(ARIMA_gdpcl)
fittedARIMA
fitARMA_bef <- fittedARIMA[1:79]
fitARMA_af <- fittedARIMA[85:94]

fittedVARGDP <- fittedVAR[,1]
fittedVARGDP
fitVAR_bef <- fittedVARGDP[1:79]
fitVAR_af <-fittedVARGDP[85:93] 

fitvecm <- fitted(vec2var_vecm.test)[,1]
fitvecm
fitvecm_bef <- fitvecm[1:79]
fitvecm_af <- fitvecm[85:92]

fittedkf <- kalman_filter$m
fittedkf
fitKF_bef <-fittedkf[2:80] # data one step further than what is supposed to be
fitKF_af <- fittedkf[86:95]

gdp_bef <-gdp2k_clean[1:79]
gdp_af <- gdp2k_clean[85:94]

##Mean 
#before covid [1:79]
mean(fittedARIMA[1:79])
mean(fittedVARGDP[1:79])
mean(fitvecm[1:79])
mean(fittedkf[1:79])
mean(gdp2k_clean[1:79])

#After covid [85:]
mean(fittedARIMA[85:94])
mean(fittedVARGDP[85:93])
mean(fitvecm[85:92])
mean(fittedkf[85:95])
mean(gdp2k_clean[85:94])

##

###accuracy tests
##ARIMA
#before
mse_arimaB <- mean((fitARMA_bef - gdp_bef)^2)
mse_arimaB
rmse_arimaB <- sqrt(mse_arimaB)
rmse_arimaB
#after
mse_arimaA <- mean((fitARMA_af - gdp_af)^2)
mse_arimaA
rmse_arimaA <- sqrt(mse_arimaA)
rmse_arimaA
cat("rmse_arimaB:", rmse_arimaB, "\n", "rmse_arimaA:", rmse_arimaA, "\n")


##VAR
#Before
mse_varB <- mean((fitVAR_bef - gdp_bef)^2)
mse_varB
rmse_varB <- sqrt(mse_varB)
rmse_varB
#after
mse_varA <- mean((fitVAR_af - gdp_af[1:9])^2)
mse_varA
rmse_varA <- sqrt(mse_varA)
rmse_varA
cat("rmse_varB:", rmse_varB, "\n", "rmse_varA:", rmse_varA, "\n")

##VECM
#before
mse_vecmB <-mean((fitvecm_bef - gdp_bef)^2)
mse_vecmB
rmse_vecmB <- sqrt(mse_vecmB)
rmse_vecmB
#after
mse_vecmA <-mean((fitvecm_af - gdp_af[1:8])^2)
mse_vecmA
rmse_vecmA <- sqrt(mse_vecmA)
rmse_vecmA
cat("rmse_vecmB:", rmse_vecmB, "\n" ,"rmse_vecmA:", rmse_vecmA, "\n")

#Kalman Filter
#before
mse_KFB <-mean((fitKF_bef - gdp_bef)^2)
mse_KFB
rmse_KFB <- sqrt(mse_vecmB)
rmse_KFB
#after
mse_KFA <-mean((fitKF_af - gdp_af)^2)
mse_KFA
rmse_KFA <- sqrt(mse_KFA)
rmse_KFA
cat("rmse_KFB:", rmse_KFB, "\n","rmse_KFA:", rmse_KFA, "\n")

# Comparaison des métriques
#Before covid
cat("rmse_arimaB:", rmse_arimaB, "\n","rmse_varB:", rmse_varB, "\n","rmse_vecmB:",rmse_vecmB, "\n","rmse_KFB:", rmse_KFB, "\n")
#after covid
cat("rmse_arimaA:", rmse_arimaA, "\n","rmse_varA:", rmse_varA, "\n","rmse_vecmA:",rmse_vecmA, "\n","rmse_KFA:", rmse_KFA, "\n") 

plot(date_seq,gdp2k_clean, type = "l", col = "blue", xlab = "Date", ylab = "Log Difference of GDP", main = "fitted and actual value of the Dutch RGDP")
lines(date_seq, fittedkf[2:95], col = "red", lty = 2)
lines(date_seq,fittedARIMA, col='orange')
lines(date_seq[2:93],fitvecm, col='cyan3')
lines(date_seq[1:93],fittedVARGDP,col='darkorchid')
legend("topleft", legend = c("Actual Data", "Kalman Filter","ARIMA", "VECM", "VAR"), col = c("blue", "red", "orange", "cyan4", "darkorchid"), lty = c(1, 2,2,2,2))

