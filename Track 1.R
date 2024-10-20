#########################Initial Data#########################
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



#########################Phase-1 Analysis: IQR#########################
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

plot(df_1_2$PM1.0..ug.m3..0.65024.[3100:3300], col = "red", xlab = "Index", ylab = "Values", ylim = c(0,6))
abline(h = upper, col = "orange")
abline(h = lower, col = "green")


for(i in 1:nrow(df_1_2)){
  if(df_1_2$PM1.0..ug.m3..0.65024.[i] > upper || df_1_2$PM1.0..ug.m3..0.65024.[i] < lower){
    df_1_2$PM1.0..ug.m3..0.65024.[i] <- NA
  }
}

end_time <- Sys.time()
print(end_time - start_time)



#########################Phase-2 Analysis: Smoothing#########################
start_time <- Sys.time()


val1 <- antilog(df_1_2$PM1.0..ug.m3..0.65024.)-1
plot(val1, col = "red", ylab = "Values")

for(i in 1:nrow(df_1_2)){
  if(is.na(df_1_2$PM1.0..ug.m3..0.65024.[i])){
    df_1_2$PM1.0..ug.m3..0.65024.[i] <- df_1_2$PM1.0..ug.m3..0.65024.[i-1]
  }
}

val2 <- diff(val1, differences = 1)
plot(val2, col = "red", ylab = "Values")
abline(h = 0)

# write.csv(df_1_2, "./test_track_1.csv")

df_sample <- read.csv("./test_track_1.csv")
dval <- df_sample$diff1
pval1 <- df_sample$mdiff1
pval2 <- df_sample$mdiff2
plot(dval, col = "red", ylab = "Values")
points(pval1, col = "green")
points(pval2, col = "orange")
rm(df_sample)


end_time <- Sys.time()
print(end_time - start_time)

#########################Phase-3 Analysis: Trend Separation#########################
start_time <- Sys.time()

length(pval1)
c1 <- list()
k <- 1
temp <- c(val1[1],val1[2])

for(i in 2:(length(pval1)-1)){
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


end_time <- Sys.time()
print(end_time - start_time)

#########################Phase-4 Analysis: Loess Outlier Detection#########################
start_time <- Sys.time()

df5 <- c()

#1-41
#43-110
#112-160
#162-181
#183-236
#238-291

for(i in 300:384){
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


end_time <- Sys.time()
print(end_time - start_time)


plot(val1, pch = 20, col = "red")
points(df5, col = "orange", pch = 4)


