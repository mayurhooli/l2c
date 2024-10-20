###############################################
start_time <- Sys.time()

require(forecast)
require(data.table)
require(ggplot2)
require(dbscan)


data(mtcars)
dt_mtcars <- as.data.table(mtcars)

# based on rawr's approach
plot(PM1.0..ug.m3..0.65024.~X, data=df_1_2, col = "Red", xlab = "Time", ylab = "Value")
lo <- loess.smooth(df_1_2$X, df_1_2$PM1.0..ug.m3..0.65024., span = 2)
lines(lo$x,lo$y, lwd=0.3, col = "Blue")
lines(lo$x,lo$y * 1.2, lwd=0.3 , col="Orange")
lines(lo$x,lo$y / 1.2, lwd=0.3 , col="Green")

end_time <- Sys.time()

print(end_time - start_time)


###############################################
start_time <- Sys.time()


library("tsoutliers")
library("TSA")

for (i in 1:2){
  dat.ts <- ts(df_1_2$PM1.0..ug.m3..0.65024.[(((i-1)*900)+1):(i*900)], frequency=1)
  data.ts.outliers <- tso(dat.ts)
}
dat.ts <- ts(df_1_2$PM1.0..ug.m3..0.65024.[1:900], frequency=1)
plot(tso(dat.ts, discard.method="bottom-up"))

tso(df_1_2$PM1.0..ug.m3..0.65024.[1:900], types = c("IO", "AO", "LS", "SLS"))


locate.outliers(residuals(dat.ts), coefs2poly(dat.ts), types = c("AO", "LS", "TC"))
locate.outliers(resid, pars, cval = 3.5, types = c("AO", "LS", "TC"), delta = 0.7)



detectAO(dat.ts, alpha = 0.05, robust = TRUE)

fit <- auto.arima(dat.ts)
data.ts.outliers <- tso(dat.ts)
data.ts.outliers
plot(dat.ts, col = 'blue')


end_time <- Sys.time()

print(end_time - start_time)




###############################################
start_time <- Sys.time()


library("tsoutliers")
library("TSA")
library("forecast")

ts4 <- c()

for (i in 1:12){

  dat.ts <- ts(df_1_2$PM1.0..ug.m3..0.65024.[(((i-1)*1000)+1):(i*1000)], frequency=1)

  ts1 <- auto.arima(dat.ts)
  ts2 <- detectAO(ts1)
  ts3 <- ts2$ind + ((i-1) * 100)
  
  ts4 <- c(ts4, ts3)
}


dat2 <- antilog(df_1_2[-ts4,])
dat3 <- df_1_2$PM1.0..ug.m3..0.65024.

end_time <- Sys.time()

print(end_time - start_time)
