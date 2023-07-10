View(Top_500_Songs_2)
Top <- Top_500_Songs_2


#box plot numeric

ggplot(data = Top[1:10,8], aes(x = streak)) +
  geom_boxplot()

#bar plot numeric vs categorical

ggplot(data = Top[1:10,], aes(x = released, y= streak)) +
  geom_bar(stat = "summary",
           fun = "mean") +

#scatter plot numeric vs numeric

ggplot(data = Top[1:10,], aes(x = streak, y = position)) +
  geom_point()
  



















