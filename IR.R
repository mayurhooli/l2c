library(openxlsx)
library(reshape)

df <- read.xlsx("D://Courses//CSCE670//Travel_Data.xlsx", sheet = 2)
df2 <- cast(df, User ~ Location, value = "Feature.of.Attraction")
 