library(tidyverse)
library(ggtext)
library(patchwork)
library(glue)
library(ggtext)
library(ggrepel)
setwd("/Users/takayukitamura/Documents/R_Computing")

house_px <- read.csv("/Users/takayukitamura/Documents/R_Computing/house_prices/house_px_2023_10.csv", sep = ",", 
                     header = TRUE, stringsAsFactors = FALSE )

sapply(house_px, class)
house_px$DATE <- as.Date(house_px$DATE)
# house_px$US <- as.numeric(house_px$US)
# house_px$Chicago <- as.numeric(house_px$Chicago)
# house_px$Pittsburgh <- as.numeric(house_px$Pittsburgh)
# house_px$Boston <- as.numeric(house_px$Boston)
# house_px$San.Jose <- as.numeric(house_px$San.Jose)
# house_px$SanFransisco <- as.numeric(house_px$SanFransisco)
# house_px$NY.NJ  <- as.numeric(house_px$NY.NJ)
# house_px$Austin <- as.numeric(house_px$Austin)
# house_px$Philadelphia  <- as.numeric(house_px$Philadelphia)
# house_px$Columbus.OH <- as.numeric(house_px$Columbus.OH)

# house_px_20231001 <- tribble(
#   ~DATE, ~US, ~Chicago, ~Pittsburgh, ~Boston, ~San.Jose, ~SanFransisco, 
#   ~NY.NJ, ~Austin, ~Philadelphia, ~Columbus.OH,
#   "2023-10-01", 657.67, 533.73, 503.55, 1317.28, 1270.20, 1156.56, 
#   1062.34, 891.86, 847.11, 594.53)
# 
# house_px_20231001$DATE <- as.Date(house_px_20231001$DATE, format = "%Y-%m-%d")
# 
# house_px <- rbind(house_px, house_px_20231001)
# 
# write.csv(house_px, file = "house_px_2023_10.csv")
sapply(house_px, class)

house_px <- house_px %>% 
  rename("US_Avg" = "US")


house_px %>% tail

house_px <- house_px %>%
  select(-X)

longer_house_px <- house_px %>% 
  pivot_longer(cols = c(-DATE),
               names_to = "city",
               values_to = "price")
  # filter(city %in% c(SanFransisco, Boston))

ggplot(longer_house_px, aes(x = DATE, y = price, color = city))+
  geom_line() +
  geom_label_repel(
    data = longer_house_px %>% 
      filter(DATE == '2023-10-01'),
    aes(label = city),
    min.segment.length = 0.25,
    direction = "y",
    hjust = "outward",
    vjust = 0,
    nudge_x = -2,
    nudge_y = 0,
    size = 2.75
  ) +
  theme(text = element_markdown()) +
  labs(
    x = 'Year',
    y = 'Housing Index',
    caption = "source:St.Louis Fed",
    title = 'House Price Index for major metropolitan',
    subtitle = "Pittsburgh is the most affordable among the equivalent cities") +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(t = 10, l= 15, r =50),
    panel.margin = margin(),
    axis.line = element_line(color = "gray")
  ) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none")

ggsave("house_price_major_cities.png", width = 5, height = 4)

ggplot(longer_house_px, aes(x = DATE, y = price, color = city))+
  geom_line() +
  geom_text(
    data = longer_house_px %>%
      filter(DATE == '2023-10-01'),
    aes(label = city),
    min.segment.length = 0.25,
    direction = "y",
    hjust = "outward",
    vjust = 0,
    nudge_x = 0,
    nudge_y = 0,
    size = 3.0
  ) +
  theme(text = element_markdown()) +
  labs(
    x = 'Year',
    y = 'Housing Index',
    caption = "source:St.Louis Fed",
    title = 'House Price Index for major metropolitan',
    subtitle = "Pittsburgh is the most affordable among the equivalent cities") +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(t = 10, l= 15, r =50),
    panel.margin = margin(),
    axis.line = element_line(color = "gray")
  ) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none")

