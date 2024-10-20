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


plot(df_1_2$PM1.0..ug.m3..0.65024., col = "red", xlab = "Index", ylab = "Values") 
points(pred_1_4, col = "blue")
points(pred_1_4_m1, col = "orange")
points(pred_1_4_m2, col = "green")

plot(df_1_2$PM1.0..ug.m3..0.65024.[500:1000], col = "red", ylim = c(2,6))
points(pred_1_4[500:1000], col = "blue")
points(pred_1_4_m1[500:1000], col = "orange")
points(pred_1_4_m2[500:1000], col = "green")

plot(df_1_2$PM1.0..ug.m3..0.65024.[3100:3300], col = "red", ylim = c(2,6))
points(pred_1_4[3100:3300], col = "blue")
points(pred_1_4_m1[3100:3300], col = "orange")
points(pred_1_4_m2[3100:3300], col = "green")


end_time <- Sys.time()
print(end_time - start_time)


####################Phase-2 Filter####################
# df_1_2$PM1.0..ug.m3..0.65024. <- antilog(df_1_2$PM1.0..ug.m3..0.65024.) - 1
val1 <- df_1_2$PM1.0..ug.m3..0.65024.
plot(val1, col = "red", ylab = "Values")

for(i in 1:nrow(df_1_2)){
  if(is.na(df_1_2$PM1.0..ug.m3..0.65024.[i])){
    df_1_2$PM1.0..ug.m3..0.65024.[i] <- df_1_2$PM1.0..ug.m3..0.65024.[i-1]
  }
}

val2 <- diff(val1, differences = 1)
plot(val2, col = "red", ylab = "Values")
abline(h = 0)

# write.csv(df_1_2, "./test_track_3.csv")

df_sample <- read.csv("./test_track_3.csv")
dval <- df_sample$diff1
pval1 <- df_sample$mdiff1
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
  if(length(sample) > 5){
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

#########################Phase-4 Analysis: Loess Outlier Detection#########################
df5 <- c()

for(i in 1:length(c2)){
  df2 <- data.frame(c2[[i]])
  df2$ID <- seq.int(nrow(df2))
  df3 <- data.frame(df2$ID, NA)
  plot(df2$c2..i..~df2$ID, data=df2, col = "Red", xlab = "Time", ylab = "Value", ylim = c(0,200))
  lo <- loess.smooth(df2$ID, df2$c2..i..)
  lines(lo$x,lo$y, lwd=0.5, col = "Blue")
  lines(lo$x,lo$y * 1.25, lwd=0.5 , col="Orange")
  lines(lo$x,lo$y / 1.25, lwd=0.5 , col="Green")
  
  ################################################################
  # model <- svm(df2$c2..i..~df2$ID, data = df2, scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = 16,coef0 = 0, cost = 1, nu = 0.5,class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.003,shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
  # ore <- df2[2]
  # pred_1 <- predict(model, newdata = ore)
  # 
  # pred_1_m1 <- pred_1 + 2 * sd(pred_1)
  # pred_1_m2 <- pred_1 - 2 * sd(pred_1)
  # 
  # points(pred_1, col = "blue", pch = 20)
  # points(pred_1_m1, col = "orange", pch = 4)
  # points(pred_1_m2, col = "green", pch = 4)
  
  ################################################################
  
  # model <- lm(df2$c2..i..~df2$ID, data = df2)
  # ore <- df2[2]
  # pred_1 <- predict(model, newdata = ore)
  # 
  # pred_1_m1 <- pred_1 + 2 * sd(pred_1)
  # pred_1_m2 <- pred_1 - 2 * sd(pred_1)
  # 
  # points(pred_1, col = "blue", pch = 20)
  # points(pred_1_m1, col = "orange", pch = 4)
  # points(pred_1_m2, col = "green", pch = 4)
  
  ################################################################
  
  
  if(i == 1){
    
    f1 <- approxfun(lo$x, lo$y * 1.25)
    wh1 <- which(df2$c2..i.. > f1(df2$ID))
    
    f2 <- approxfun(lo$x, lo$y / 1.25)
    wh2 <- which(df2$c2..i.. < f2(df2$ID))
    
    temp_df <- df2
    df2 <- df2[-c(wh1, wh2), ]
    
    if(nrow(df2) == 0)
      df2 <- temp_df
    
    k <- 1
    for(j in 1:nrow(df3)){
      if(df3$df2.ID[j] == df2$ID[k]){
        df3$NA.[j] <- df2$c2..i..[k]
        k <- k + 1
      }
    }
    
    plot(df3$NA.~df3$df2.ID, data=df3, col = "Red", xlab = "Time", ylab = "Value")
    
    if(sum(is.na(df3)) != 0 ){
      
      plot(df3$NA.~df3$df2.ID, data=df3, col = "Red", xlab = "Time", ylab = "Value")
      
      model <- svm(df3$NA.~df3$df2.ID, data = df3, na.action = na.omit)
      ore <- df3[1]
      pred_1 <- predict(model, newdata = ore)
      
      for(j in 1:nrow(df3)){
        if(is.na(df3$NA.[j])){
          df3$NA.[j] <- pred_1[j]
        }
      }
    }
    
    df5 <- c(df5, df3$NA.)
    plot(df3$NA.~df3$df2.ID, data=df3, col = "Red", xlab = "Time", ylab = "Value")
    
  }else{#Use i=98 when diff = 5
    
    df3[nrow(df3)+1,] <- NA
    df3$df2.ID[nrow(df3)] <- nrow(df3)
    df3[nrow(df3)+1,] <- NA
    df3$df2.ID[nrow(df3)] <- nrow(df3)
    
    df4 <- data.frame(c2[[i-1]])
    df3$NA.[1] <- df4[(nrow(df4)-1),]
    df3$NA.[2] <- df4[nrow(df4),]
    
    f1 <- approxfun(lo$x, lo$y * 1.25)
    wh1 <- which(df2$c2..i.. > f1(df2$ID))
    
    f2 <- approxfun(lo$x, lo$y / 1.25)
    wh2 <- which(df2$c2..i.. < f2(df2$ID))
    
    temp_df <- df2
    df2 <- df2[-c(wh1, wh2), ]
    
    if(nrow(df2) == 0)
      df2 <- temp_df
    
    k <- 1
    for(j in 3:nrow(df3)){
      l <- j-2
      if(df3$df2.ID[l] == df2$ID[k]){
        df3$NA.[j] <- df2$c2..i..[k]
        k <- k + 1
      }  
      if(k > nrow(df2)){
        break
      }
    }
    
    if(sum(is.na(df3)) != 0 ){
      
      plot(df3$NA.~df3$df2.ID, data=df3, col = "Red", xlab = "Time", ylab = "Value")
      
      model <- svm(df3$NA.~df3$df2.ID, data = df3, na.action = na.omit)
      ore <- df3[1]
      pred_1 <- predict(model, newdata = ore)
      
      for(j in 1:nrow(df3)){
        if(is.na(df3$NA.[j])){
          df3$NA.[j] <- pred_1[j]
        }
      }
    }
    
    plot(df3$NA.~df3$df2.ID, data=df3, col = "Red", xlab = "Time", ylab = "Value")
    df5 <- c(df5, df3$NA.[(3:nrow(df3))])
    
  }
  
}

plot(val1, pch = 20, col = "red")
points(df5, col = "orange", pch = 4)
