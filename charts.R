# This is just a scratch file to play around with charts before
# adding them to the app

library(dplyr)
library(ggplot2)

df <- read.csv("rockets.csv")

countries <- unique(df$Country)
statuses <- unique(df$Status.Mission)
companies <- unique(df$Company.Name)

ggplot(df %>% filter(Year >= 2009 & Year <= 2015), aes(x=Year, y=Country)) +
  geom_dotplot()


ggplot(df, aes(x=Year, fill=Status.Mission)) +
  geom_dotplot(binwidth = .25) +
  geom_rug() +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title.y = element_blank())
