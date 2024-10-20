####################Phase 1 Filter####################
library(e1071)
library(ggplot2)
library(pryr)

antilog<-function(lx,base) {
  lbx<-lx/log(exp(1),base=base)
  result<-exp(lbx)
  result
}

df <- read.csv("G://Courses//CSCE633//Project//Data//air_quality_02.csv", header = TRUE)
df <- df[1:12312,]
pred <- c()
sd_u <- c()
sd_l <- c()

n <- 2000
nr <- nrow(df)
l1<-split(df, rep(1:ceiling(nr/n), each=n, length.out=nr))

for(i in 1:length(l1)){
  df1 <- l1[[i]]
  df1 <- df1[c(1,3)]
  df1[2] <- log(df1[2] + 1)
  # ee <- (max(df1[2] - min(df1[2])))
  # df1[2] <- df1[2] / (max(df1[2]-min(df1[2])))
  model1 <- svm(df1$PM1.0..ug.m3..0.65024.~., data = df1, scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = 16,coef0 = 0, cost = 30, nu = 0.5,class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.03,shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
  tp <- fitted(model1)
  tsdu <- tp + 1 * sd(tp)
  tsdl <- tp - 1 * sd(tp)
  pred <- c(pred, tp)
  sd_u <- c(sd_u, tsdu)
  sd_l <- c(sd_l, tsdl)
  # plot(df1$PM1.0..ug.m3..0.65024., col = "red",xlab="Index",ylab="Data",main="Took 1.61s")
  # points(pred, col = "blue")
  # points(sd_u,pch=".", col = "orange")
  # points(sd_l,pch=".", col = "orange")
}

rm(df1, l1, model1, i, n, nr, tp, tsdl, tsdu)

# plot(df1, col = "red",xlab="Index",ylab="Data",main="Took 1.61s")
# points(pred, col = "blue")
# points(sd_u,pch=".", col = "orange")
# points(sd_l,pch=".", col = "orange")

pred <- antilog(pred) - 1
sd_u <- antilog(sd_u) - 1
sd_l <- antilog(sd_l) - 1



####################Loess Comparison####################
df1 <- df[c(1,3)]

loessMod001 <- loess(df1$PM1.0..ug.m3..0.65024. ~ df1$X, span = 0.01, na.action = na.omit)
loessMod0015 <- loess(df1$PM1.0..ug.m3..0.65024. ~ df1$X, span = 0.015, na.action = na.omit)
loessMod003 <- loess(df1$PM1.0..ug.m3..0.65024. ~ df1$X, span = 0.03, na.action = na.omit)
loessMod005 <- loess(df1$PM1.0..ug.m3..0.65024. ~ df1$X, span = 0.05, na.action = na.omit)
loessMod010 <- loess(df1$PM1.0..ug.m3..0.65024. ~ df1$X, span = 0.1, na.action = na.omit)
loessMod015 <- loess(df1$PM1.0..ug.m3..0.65024. ~ df1$X, span = 0.15, na.action = na.omit)

smoothed001 <- predict(loessMod001)
smoothed0015 <- predict(loessMod0015)
smoothed003 <- predict(loessMod003)
smoothed005 <- predict(loessMod005)
smoothed010 <- predict(loessMod010)
smoothed015 <- predict(loessMod015)

rm(loessMod001, loessMod0015, loessMod003, loessMod005, loessMod010, loessMod015)

count <- 0
for(i in 1:12312){
  if(smoothed001[i] > sd_u[i] || smoothed001[i] < sd_l[i])
    count <- count + 1
}

count <- 0
for(i in 1:12312){
  if(smoothed015[i] > sd_u[i] || smoothed001[i] < sd_l[i])
    count <- count + 1
}