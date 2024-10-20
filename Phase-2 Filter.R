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
