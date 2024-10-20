####################Phase 1 Filter####################
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

######Round 1######
df_1 <- df[c(1,3)]
df_1_2 <- df_1
df_1_2[2] <- log(df_1[2] + 1)

model <-svm(df_1_2$PM1.0..ug.m3..0.65024.~., data = df_1_2, scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = 16,coef0 = 0, cost = 1, nu = 0.5,class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.003,shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
ore <- df_1_2[1]
pred_1 <- predict(model, newdata = ore)

pred_1_m1 <- pred_1 + 2 * sd(pred_1)
pred_1_m2 <- pred_1 - 2 * sd(pred_1)

for(i in 1:nrow(ore)){
  if(df_1_2$PM1.0..ug.m3..0.65024.[i] > pred_1_m1[i] || df_1_2$PM1.0..ug.m3..0.65024.[i] < pred_1_m2){
    df_1_2$PM1.0..ug.m3..0.65024.[i] <- NA
  }
}

df_1_2$PM1.0..ug.m3..0.65024. <- antilog(df_1_2$PM1.0..ug.m3..0.65024.) - 1
rm(df_1, pred_1, pred_1_m1, pred_1_m2, model, ore)


######Round 2######
df_2 <- df[c(1,4)]
df_2_2 <- df_2
df_2_2$PM1.0_AE..ug.m3..0.65024. <- log(df_2$PM1.0_AE..ug.m3..0.65024. + 1)

model <-svm(df_2_2$PM1.0_AE..ug.m3..0.65024.~., data = df_2_2, scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = 16,coef0 = 0, cost = 1, nu = 0.5,class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.003,shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
ore <- df_2_2[1]
pred_2 <- predict(model, newdata = ore)

pred_2_m1 <- pred_2 + 2 * sd(pred_2)
pred_2_m2 <- pred_2 - 2 * sd(pred_2)

for(i in 1:nrow(ore)){
  if(df_2_2$PM1.0_AE..ug.m3..0.65024.[i] > pred_2_m1[i] || df_2_2$PM1.0_AE..ug.m3..0.65024.[i] < pred_2_m2){
    df_2_2$PM1.0_AE..ug.m3..0.65024.[i] <- NA
  }
}

df_2_2$PM1.0_AE..ug.m3..0.65024. <- antilog(df_2_2$PM1.0_AE..ug.m3..0.65024.) - 1
rm(df_2, pred_2, pred_2_m1, pred_2_m2, model, ore)


######Round 3######
df_3 <- df[c(1,5)]
df_3_2 <- df_3
df_3_2$PM10..ug.m3..0.65024. <- log(df_3$PM10..ug.m3..0.65024. + 1)

model <-svm(df_3_2$PM10..ug.m3..0.65024.~., data = df_3_2, scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = 16,coef0 = 0, cost = 1, nu = 0.5,class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.003,shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
ore <- df_3_2[1]
pred_3 <- predict(model, newdata = ore)

pred_3_m1 <- pred_3 + 2 * sd(pred_3)
pred_3_m2 <- pred_3 - 2 * sd(pred_3)

for(i in 1:nrow(ore)){
  if(df_3_2$PM10..ug.m3..0.65024.[i] > pred_3_m1[i] || df_3_2$PM10..ug.m3..0.65024.[i] < pred_3_m2){
    df_3_2$PM10..ug.m3..0.65024.[i] <- NA
  }
}

df_3_2$PM10..ug.m3..0.65024. <- antilog(df_3_2$PM10..ug.m3..0.65024.) - 1
rm(df_3, pred_3, pred_3_m1, pred_3_m2, model, ore)


######Round 4######
df_4 <- df[c(1,6)]
df_4_2 <- df_4
df_4_2$PM10_AE..ug.m3..0.65024. <- log(df_4$PM10_AE..ug.m3..0.65024. + 1)

model <-svm(df_4_2$PM10_AE..ug.m3..0.65024.~., data = df_4_2, scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = 16,coef0 = 0, cost = 1, nu = 0.5,class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.003,shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
ore <- df_4_2[1]
pred_4 <- predict(model, newdata = ore)

pred_4_m1 <- pred_4 + 2 * sd(pred_4)
pred_4_m2 <- pred_4 - 2 * sd(pred_4)

for(i in 1:nrow(ore)){
  if(df_4_2$PM10_AE..ug.m3..0.65024.[i] > pred_4_m1[i] || df_4_2$PM10_AE..ug.m3..0.65024.[i] < pred_4_m2){
    df_4_2$PM10_AE..ug.m3..0.65024.[i] <- NA
  }
}

df_4_2$PM10_AE..ug.m3..0.65024. <- antilog(df_4_2$PM10_AE..ug.m3..0.65024.) - 1
rm(df_4, pred_4, pred_4_m1, pred_4_m2, model, ore)


######Round 5######
df_5 <- df[c(1,7)]
df_5_2 <- df_5
df_5_2$PM2.5..ug.m3..0.65024. <- log(df_5$PM2.5..ug.m3..0.65024. + 1)

model <-svm(df_5_2$PM2.5..ug.m3..0.65024.~., data = df_5_2, scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = 16,coef0 = 0, cost = 1, nu = 0.5,class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.003,shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
ore <- df_5_2[1]
pred_5 <- predict(model, newdata = ore)

pred_5_m1 <- pred_5 + 2 * sd(pred_5)
pred_5_m2 <- pred_5 - 2 * sd(pred_5)

for(i in 1:nrow(ore)){
  if(df_5_2$PM2.5..ug.m3..0.65024.[i] > pred_5_m1[i] || df_5_2$PM2.5..ug.m3..0.65024.[i] < pred_5_m2){
    df_5_2$PM2.5..ug.m3..0.65024.[i] <- NA
  }
}

df_5_2$PM2.5..ug.m3..0.65024. <- antilog(df_5_2$PM2.5..ug.m3..0.65024.) - 1
rm(df_5, pred_5, pred_5_m1, pred_5_m2, model, ore)


######Round 6######
df_6 <- df[c(1,8)]
df_6_2 <- df_6
df_6_2$PM2.5_AE..ug.m3..0.65280. <- log(df_6$PM2.5_AE..ug.m3..0.65280. + 1)

model <-svm(df_6_2$PM2.5_AE..ug.m3..0.65280.~., data = df_6_2, scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = 16,coef0 = 0, cost = 1, nu = 0.5,class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.003,shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
ore <- df_6_2[1]
pred_6 <- predict(model, newdata = ore)

pred_6_m1 <- pred_6 + 2 * sd(pred_6)
pred_6_m2 <- pred_6 - 2 * sd(pred_6)

for(i in 1:nrow(ore)){
  if(df_6_2$PM2.5_AE..ug.m3..0.65280.[i] > pred_6_m1[i] || df_6_2$PM2.5_AE..ug.m3..0.65280.[i] < pred_6_m2){
    df_6_2$PM2.5_AE..ug.m3..0.65280.[i] <- NA
  }
}

df_6_2$PM2.5_AE..ug.m3..0.65280. <- antilog(df_6_2$PM2.5_AE..ug.m3..0.65280.) - 1
rm(df_6, pred_6, pred_6_m1, pred_6_m2, model, ore)


df_mod <- data.frame(df$X, df$Date..In.UTC., df_1_2$PM1.0..ug.m3..0.65024., df_2_2$PM1.0_AE..ug.m3..0.65024., df_3_2$PM10..ug.m3..0.65024., df_4_2$PM10_AE..ug.m3..0.65024., df_5_2$PM2.5..ug.m3..0.65024., df_6_2$PM2.5_AE..ug.m3..0.65280.)
rm(df, df_1_2, df_2_2, df_3_2, df_4_2, df_5_2, df_6_2)
end_time <- Sys.time()

print(end_time - start_time)
gc()


####################Phase 2 Filter####################

df_cor <- df_mod[complete.cases(df_mod),]
cor1 <- cor(df_cor[3:8])

val1 <- df_mod$df_1_2.PM1.0..ug.m3..0.65024.
plot(val1, col = "red")

for(i in 2:length(val1)){
  if(is.na(val1[i]))
    val1[i] <- val1[i-1]
}
dval1 <- diff(val1, lag = 1)
dval1[is.na(dval1)] <- 0

# df_sample <- data.frame(dval1)
# write.csv(df_sample, "test.csv")
df_sample <- read.csv("test.csv")
pval1 <- df_sample$pval1
rm(df_sample)

# for(i in 2:length(pval1)){
#   if(pval1[i] == 0)
#     pval1[i] <- pval1[i-1]
# }

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

plot(samp, col = "red")
points(pred, col = "blue")
points(sd_u, col = "orange")
points(sd_l, col = "green")
