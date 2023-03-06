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


df %>% ggvis(~Year, ~Company.Name) %>%
  layer_points(size := 50, size.hover := 200,
               fillOpacity := 0.2, fillOpacity.hover := 0.5,
               stroke = ~Status.Mission) %>%
  add_axis("y", title = "Company") %>%
  add_axis("x", title = "Year") %>%
  scale_nominal("stroke", domain = c(statuses),
              range = c("blue", "red", "orange", "pink"))


df %>% ggvis(~Year, ~Country) %>%
  layer_points(size := 50, size.hover := 200,
               fillOpacity := 0.2, fillOpacity.hover := 0.5,
               stroke = ~Status.Mission) %>%
  add_axis("y", title = "Country") %>%
  add_axis("x", title = "Year") %>%
  scale_nominal("stroke", domain = c(statuses),
                range = c("blue", "red", "orange", "pink"))
