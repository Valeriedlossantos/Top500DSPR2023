Top_500_Songs_2
View(Top_500_Songs_2)
Top <- Top_500_Songs_2
summary(Top_500_Songs_2)
summary(Top)
str(Top)
names(Top)
head(Top)

streak <- Top[,8] # only column 8/ streaks
Top[1:100,8] # 1-100 songs on column 8/ streaks
Top[1:100,] # 1-100 songs on all rows

Top[,1:3] # first three columns only 


first5 <- Top$streak[1:5]
mean(Top$streak)
sapply(streak, mean, na.rm = TRUE)

na.rm = TRUE
na.rm
Top$streak <- na.rm

median(Top$streak)

max(Top$streak) - min(Top$streak)

var(Top$streak)

sd(Top$streak)


IQR(Top$streak)
quantile(Top$streak, 0.25)
quantile(Top$streak, 0.75)

outliers
lower <- quantile(Top$streak, 0.25) - 1.5*IQR(Top$streak)
upper <- quantile(Top$streak, 0.75) + 1.5*IQR(Top$streak)

values <- c(Top$streak,3,9)
values[values > lower & values < upper]
values_no_outliers <- values[values > lower & values < upper]
mean(Top$streak)
median(Top$streak)


     