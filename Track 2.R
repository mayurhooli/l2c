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


####################Phase-1 Analysis####################
df_1 <- df[c(1,3)]
plot(df_1$PM1.0..ug.m3..0.65024., col = "red")
df_1_2 <- df_1
df_1_2[2] <- log(df_1[2] + 1)
plot(df_1_2$PM1.0..ug.m3..0.65024., col = "red", xlab = "Index", ylab = "Values")
plot(df_1_2$PM1.0..ug.m3..0.65024.[500:1000], col = "red", xlab = "Index", ylab = "Values")
plot(df_1_2$PM1.0..ug.m3..0.65024.[3100:3300], col = "red", xlab = "Index", ylab = "Values")

model <-svm(df_1_2$PM1.0..ug.m3..0.65024.~., data = df_1_2, scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = 16,coef0 = 0, cost = 1, nu = 0.5,class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.003,shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
ore <- df_1_2[1]
pred_1 <- predict(model, newdata = ore)

pred_1_m1 <- pred_1 + 2 * sd(pred_1)
pred_1_m2 <- pred_1 - 2 * sd(pred_1)



plot(df_1_2$PM1.0..ug.m3..0.65024., col = "red", xlab = "Index", ylab = "Values") 
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


for(i in 1:nrow(df_1_2)){
  if(df_1_2$PM1.0..ug.m3..0.65024.[i] > upper || df_1_2$PM1.0..ug.m3..0.65024.[i] < lower){
    df_1_2$PM1.0..ug.m3..0.65024.[i] <- NA
  }
}