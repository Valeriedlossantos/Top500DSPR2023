library(tidyr)
library(janitor)
library(dplyr)

library(readr)
Top_500_Songs_2 <- read_csv("~/Documents/DSRP_2023/Data/Top 500 Songs 2.csv")
View(Top_500_Songs_2)

Top_500_Songs_2

## Part 1 #### t.test

Top_500_Songs_2 <- separate(Top_500_Songs_2,
                            released,
                            into = c("month", "year"))

Top_500_Songs_2 <- separate(Top_500_Songs_2,
                            streak,
                            into = "streak",
                            sep = "week")

Top500Song_Clean <- na.omit(Top_500_Songs_2)


Topbefore2000s <- filter(Top500Song_Clean, year < 2000)
Topafter2000s <- filter(Top500Song_Clean, year > 2000)

Topbefore2000s <- arrange(Topbefore2000s, desc(streak))
Topafter2000s <- arrange(Topafter2000s, desc(streak))



Topbefore2000s$streak <- as.numeric(Topbefore2000s$streak)
Topafter2000s$streak <- as.numeric(Topafter2000s$streak)



t.test(Topafter2000s$streak, Topbefore2000s$streak, paired = F,
       alternative = "less")

## Welch Two Sample t-test
# data:  Topafter2000s$streak and Topbefore2000s$streak
# t = 3.4675, df = 14.374, p-value = 0.9982
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#  -Inf 13.68466
# sample estimates:
# mean of x mean of y 
# 24.33333  15.25272 

t.test(Topafter2000s$streak, Topbefore2000s$streak, paired = F,
       alternative = "greater")

#Welch Two Sample t-test

# data:  Topafter2000s$streak and Topbefore2000s$streak
# t = 3.4675, df = 14.374, p-value = 0.001821
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#  4.476573      Inf
# sample estimates:
# mean of x mean of y 
# 24.33333  15.25272


## Part 2 #### anova_results

Top500 <- Top500Song_Clean


Top500$streak <- as.numeric(Top500$streak)


anova_results <- aov(streak ~ title, Top500)

summary(anova_results)
TukeyHSD(anova_results)

top3songs <- Top500 |>
  summarize(.by = title,
            count = sum(! is.na(title))) |>
  slice_max(count, n = 3)

topartists <- select(Top500, artist, position)

top3artists <- topartists |>
  summarize(.by = artist,
            count = sum(! is.na(artist))) |>
  slice_max(count, n = 3)


# a <- aov(depVar ~ indVar, data = df)

sub("No. ","", Top500$position) 

Top500$position <- as.numeric(sub("No. ", "", Top500$position))

Top500 <- filter(Top500, !is.na(position))

anova_results <- aov(position ~ title, Top500)
summary(anova_results)
TukeyHSD(anova_results)

Top500_top3artists <- Top500 |>
  filter(artist %in% top3artists$artist)

Top500_top3artists

a <- aov(position ~ artist, Top500_top3artists)
summary(a)
TukeyHSD(a)

anova_results <- aov(position ~ artist, Top500_top3artists)
anova_results

p_value <- summary(anova_results)
p_value

str(Top500)

## Part 3 #### 

t <- table(Top500$artist, Top500$title)
t

## chi squared test 
chi <- chisq.test(t)
chi$p.value
chi$residuals

corrplot(chi$residuals, is.cor = F)

chi






