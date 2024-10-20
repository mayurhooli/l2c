####################Loess Comparison####################
antilog<-function(lx,base) { 
  lbx<-lx/log(exp(1),base=base) 
  result<-exp(lbx) 
  result 
} 

df <- read.csv("G://Courses//CSCE633//Project//Data//air_quality_02.csv")
df <- df[1:12312,]

df_1 <- df[c(1,3)]
df_1_2 <- df_1
df_1_2[2] <- log(df_1[2] + 1)

v1 <- mean(df_1_2$PM1.0..ug.m3..0.65024., na.rm = TRUE) + 2 * sd(df_1_2$PM1.0..ug.m3..0.65024., na.rm = TRUE)
v2 <- mean(df_1_2$PM1.0..ug.m3..0.65024., na.rm = TRUE) - 2 * sd(df_1_2$PM1.0..ug.m3..0.65024., na.rm = TRUE)

for(i in 1:nrow(df_1_2)){
  if(df_1_2$PM1.0..ug.m3..0.65024.[i] > v1 || df_1_2$PM1.0..ug.m3..0.65024.[i] < v2){
    df_1_2$PM1.0..ug.m3..0.65024.[i] <- NA
  }
}

df_1_2$PM1.0..ug.m3..0.65024. <- antilog(df_1_2$PM1.0..ug.m3..0.65024.) - 1

sample1 <- loess(df_1_2$PM1.0..ug.m3..0.65024. ~ df_1_2$X, span = 0.0015, na.action = na.omit)
plot(sample1$fitted, col = "red")
