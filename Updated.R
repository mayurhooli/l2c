####################Initial Data####################
library(pryr)
library(e1071)

gc(reset = TRUE)
antilog<-function(lx,base) { 
  lbx<-lx/log(exp(1),base=base) 
  result<-exp(lbx) 
  result 
} 

df <- read.csv("G://Courses//CSCE633//Project//Data//air_quality_02.csv")
df <- df[1:12312,]

start_time <- Sys.time()


####################Type-2 Analysis####################
df_1 <- df[c(1,3)]
plot(df_1$PM1.0..ug.m3..0.65024., col = "red")
df_1_2 <- df_1
df_1_2[2] <- log(df_1[2] + 1)
plot(df_1_2$PM1.0..ug.m3..0.65024., col = "red", xlab = "Index", ylab = "Values")
plot(df_1_2$PM1.0..ug.m3..0.65024.[500:1000], col = "red", xlab = "Index", ylab = "Values")
plot(df_1_2$PM1.0..ug.m3..0.65024.[3100:3300], col = "red", xlab = "Index", ylab = "Values")

q1 <- as.numeric(quantile(df_1_2$PM1.0..ug.m3..0.65024.)[2])
q3 <- as.numeric(quantile(df_1_2$PM1.0..ug.m3..0.65024.)[4])
iqr <- q3 - q1
upper <- q3 + (1.5 * iqr)
lower <- q1 - (1.5 * iqr)

plot(df_1_2$PM1.0..ug.m3..0.65024., col = "red", xlab = "Index", ylab = "Values")
abline(h = upper, col = "orange")
abline(h = lower, col = "green")

plot(df_1_2$PM1.0..ug.m3..0.65024.[500:1000], col = "red", xlab = "Index", ylab = "Values")
abline(h = upper, col = "orange")
abline(h = lower, col = "green")

plot(df_1_2$PM1.0..ug.m3..0.65024.[3100:3300], col = "red", xlab = "Index", ylab = "Values")
abline(h = upper, col = "orange")
abline(h = lower, col = "green")



####################Type-2 Analysis####################
df_1 <- df[c(1,3)]
plot(df_1$PM1.0..ug.m3..0.65024., col = "red")
df_1_2 <- df_1
df_1_2[2] <- log(df_1[2] + 1)
plot(df_1_2$PM1.0..ug.m3..0.65024., col = "red", xlab = "Index", ylab = "Values")


model <-svm(df_1_2$PM1.0..ug.m3..0.65024.~., data = df_1_2, scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = 16,coef0 = 0, cost = 1, nu = 0.5,class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.003,shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
ore <- df_1_2[1]
pred_1 <- predict(model, newdata = ore)

pred_1_m1 <- pred_1 + 2 * sd(pred_1)
pred_1_m2 <- pred_1 - 2 * sd(pred_1)


plot(df_1_2$PM1.0..ug.m3..0.65024., col = "red", ylim = c(2,6), xlab = "Index", ylab = "Values") 
     # legend("topright", title="Points", c("Actual","Prediction", "Upper", "Lower"), 
     #        horiz=TRUE, lty=c(1,1), lwd=c(2,2), col=c("red","blue", "orange", "green"), bg="grey96"))
points(pred_1, col = "blue")
points(pred_1_m1, col = "orange")
points(pred_1_m2, col = "green")

plot(df_1_2$PM1.0..ug.m3..0.65024.[500:1000], col = "red", ylim = c(2,6))
points(pred_1[500:1000], col = "blue")
points(pred_1_m1[500:1000], col = "orange")
points(pred_1_m2[500:1000], col = "green")

plot(df_1_2$PM1.0..ug.m3..0.65024.[3100:3300], col = "red", ylim = c(2,6))
points(pred_1[3100:3300], col = "blue")
points(pred_1_m1[3100:3300], col = "orange")
points(pred_1_m2[3100:3300], col = "green")

for(i in 1:nrow(ore)){
  if(df_1_2$PM1.0..ug.m3..0.65024.[i] > pred_1_m1[i] || df_1_2$PM1.0..ug.m3..0.65024.[i] < pred_1_m2){
    df_1_2$PM1.0..ug.m3..0.65024.[i] <- NA
  }
}

# df_1_2$PM1.0..ug.m3..0.65024. <- antilog(df_1_2$PM1.0..ug.m3..0.65024.) - 1
rm(df_1, pred_1, pred_1_m1, pred_1_m2, model, ore)

df_mod <- data.frame(df$X, df$Date..In.UTC., df_1_2$PM1.0..ug.m3..0.65024., df_2_2$PM1.0_AE..ug.m3..0.65024., df_3_2$PM10..ug.m3..0.65024., df_4_2$PM10_AE..ug.m3..0.65024., df_5_2$PM2.5..ug.m3..0.65024., df_6_2$PM2.5_AE..ug.m3..0.65280.)
end_time <- Sys.time()

print(end_time - start_time)
gc()




####################Type-3 Analysis####################
df_1 <- df[c(1,3)]
df_1_2 <- df_1
df_1_2[2] <- log(df_1[2] + 1)

n <- 2500
nr <- nrow(df_1_2)
l1 <- split(df_1_2, rep(1:ceiling(nr/n), each=n, length.out=nr))

df_1_4 <- data.frame()
pred_1_4 <- c()
pred_1_4_m1 <- c()
pred_1_4_m2 <- c()

for (i in 1:length(l1)){
  df_1_3 <- l1[[i]]
  model <-svm(df_1_3$PM1.0..ug.m3..0.65024.~., data = df_1_3, scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = 16,coef0 = 0, cost = 1, nu = 0.5,class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.003,shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
  ore <- df_1_3[1]
  pred_1_2 <- predict(model, newdata = ore)  
  
  pred_1_2_m1 <- pred_1_2 + 2 * sd(pred_1_2)
  pred_1_2_m2 <- pred_1_2 - 2 * sd(pred_1_2)

  for(i in 1:nrow(ore)){
    if(df_1_3$PM1.0..ug.m3..0.65024.[i] > pred_1_2_m1[i] || df_1_3$PM1.0..ug.m3..0.65024.[i] < pred_1_2_m2){
      df_1_3$PM1.0..ug.m3..0.65024.[i] <- NA
    }
  }
  

  df_1_4 <- merge(df_1_3, df_1_4, all=TRUE)
  pred_1_4 <- c(pred_1_4, pred_1_2)
  pred_1_4_m1 <- c(pred_1_4_m1, pred_1_2_m1)
  pred_1_4_m2 <- c(pred_1_4_m2, pred_1_2_m2)

}


plot(df_1_4$PM1.0..ug.m3..0.65024., col = "red")
points(pred_1_4, col = "blue")
points(pred_1_4_m1, col = "orange")
points(pred_1_4_m2, col = "green")

plot(df_1_4$PM1.0..ug.m3..0.65024.[500:1000], col = "red")
points(pred_1_4[500:1000], col = "blue")
points(pred_1_4_m1[500:1000], col = "orange")
points(pred_1_4_m2[500:1000], col = "green")

plot(df_1_4$PM1.0..ug.m3..0.65024.[3100:3300], col = "red", ylim = c(2,6))
points(pred_1_4[3100:3300], col = "blue")
points(pred_1_4_m1[3100:3300], col = "orange")
points(pred_1_4_m2[3100:3300], col = "green")


df_1_2$PM1.0..ug.m3..0.65024. <- antilog(df_1_2$PM1.0..ug.m3..0.65024.) - 1
rm(df_1, df_1_2, pred_1, pred_1_m1, pred_1_m2, model, ore)

df_mod <- data.frame(df$X, df$Date..In.UTC., df_1_2$PM1.0..ug.m3..0.65024., df_2_2$PM1.0_AE..ug.m3..0.65024., df_3_2$PM10..ug.m3..0.65024., df_4_2$PM10_AE..ug.m3..0.65024., df_5_2$PM2.5..ug.m3..0.65024., df_6_2$PM2.5_AE..ug.m3..0.65280.)
end_time <- Sys.time()

print(end_time - start_time)
gc()




####################Phase 2 Filter####################

val1 <- df_1_2$PM1.0..ug.m3..0.65024.
plot(val1, col = "red")

df_sample <- read.csv("./test.csv")
pval1 <- df_sample$pval1
rm(df_sample)

length(pval1)
c1 <- list()
k <- 1
temp <- c(val1[1],val1[2])
for(i in 2:12311){
  if(pval1[i]*pval1[i-1] >= 0){
    temp <- c(temp, val1[i+1])
  }else{
    c1[[k]] <- temp
    k <- k+1
    temp <- c(val1[i+1])
  }
}
c1[[k]] <- temp

c2 <- list()
l <- 1
c2[[l]] <- c1[[1]]
i <- 2
while(i < length(c1)){
  sample <- c1[[i]]
  if(length(sample) > 15){
    l <- l + 1
    c2[[l]] <- c1[[i]]
    i <- i + 1
  }else{
    temp1 <- c2[[l]]
    temp2 <- c1[[i+1]]
    sample <- c(temp1, sample, temp2)
    c2[[l]] <- sample
    i <- i + 2
  }
}

rm(c1)
samp <- c()
pred <- c()
sd_u <- c()
sd_l <- c()

for(i in 1:length(c2)){
  df2 <- data.frame(c2[[i]])
  df2$ID <- seq.int(nrow(df2))
  # model1 <- smooth.spline(df2$ID, df2$c2..i.., df = 10)
  # model1 <- svm(df2$c2..i..~., data = df2, scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = 16,coef0 = 0, cost = 30, nu = 0.5,class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.03,shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
  model1 <- lm(c2..i..~ poly(ID, 3), data = df2)
  tp <- fitted(model1)
  tsdu <- tp + 2 * sd(tp)
  tsdl <- tp - 2 * sd(tp)
  pred <- c(pred, tp)
  sd_u <- c(sd_u, tsdu)
  sd_l <- c(sd_l, tsdl)
  samp <- c(samp, df2$c2..i..)
}

plot(samp, col = "red", ylim = c(2,6))
points(pred, col = "blue")
points(sd_u, col = "orange")
points(sd_l, col = "green")
