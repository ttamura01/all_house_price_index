library(tidyverse)
library(ggtext)
setwd("/Users/takayukitamura/Documents/R_Computing/house_prices")

### Correlation between home price and GDP, home price and mortgate

# read house prices
us_avg <- read.csv("house_px_2023_10.csv") %>% 
  select(DATE, US) %>% 
  rename_all(tolower)

# average house prices
us_avg_year <- us_avg %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>% 
  group_by(year) %>% 
  summarise(average_home_price = mean(us))

# read GDP and annualize
us_gdp <- read.csv("/Users/takayukitamura/Desktop/GDP.csv") %>% 
  rename_all(tolower) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>% 
  group_by(year) 


sapply(us_gdp, class)

us_gdp_annual <- us_gdp %>% 
  summarise(annual_gdp = mean(gdp))

tail(us_gdp)

# Read 30 year mortgage rates
us_30y <- read.csv("/Users/takayukitamura/Documents/US Economics/us_10y&30y.csv") %>% 
  select(date, mortgage30) 

us_30y %>% 
  ggplot(aes(x = date, y = mortgage30)) +
  geom_point()

# annualize 30 year mortgage rates 
us_30y_year <- us_30y %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>% 
  group_by(year) %>% 
  summarise(average_mortgage = mean(mortgage30))

# us_30y_year$year <- as.Date(paste0(us_30y_year$year, "-01-01"))    

## merge GDP and Home Price data
gdp_home_px <- merge(us_avg_year, us_gdp_annual, by = "year") 

sapply(gdp_home_px, class)

gdp_home_px$year <- as.Date(gdp_home_px$year, format = "%Y")

## Regression between GDP and Home Price
gdp_home_px %>% 
  ggplot(aes(x=annual_gdp, y = average_home_price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Correlation between GDP and Home price is high<br> R-squared: 0.96, since 1975",
       caption = "Source: FRED(Rederal Reserve Economic Data)") +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(),
  )

ggsave("home price vs gdp.tiff", width = 6, height = 4)

lm(average_home_price ~ annual_gdp, gdp_home_px)

summary(lm(average_home_price ~ annual_gdp, gdp_home_px))



## Regression between Mortgage Rate and Home Price
mortgage_home <- merge(us_avg_year, us_30y_year , by = "year")

mortgage_home %>% 
  ggplot(aes(x= average_mortgage, y = average_home_price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Correlation between GDP and Home price is not high<br> R-squared: 0.55, since 1975",
       caption = "Source: FRED(Rederal Reserve Economic Data)") +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(),
  )

ggsave("home price vs 30y mortgage.tiff", width = 6, height = 4)

write.csv(mortgage_home, "mortgage_home.csv")

summary(lm(average_home_price ~ average_mortgage, mortgage_home))
