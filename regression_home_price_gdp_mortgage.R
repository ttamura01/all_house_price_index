library(tidyverse)
library(ggtext)
setwd("/Users/takayukitamura/Documents/R_Computing/all_house_price_index")

### Correlation between home price and GDP, home price and mortgate

# read house prices
us_avg <- read_csv("house_px_2023_10.csv") %>% 
  select(DATE, US) %>% 
  rename_all(tolower)

updates <- tribble(~date, ~us,
                   "2024-01-01", 664.85,
                   "2024-04-01", 682.18) 

us_avg <- rbind(us_avg, updates)

# average house prices
us_avg_year <- us_avg %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>% 
  group_by(year) %>% 
  summarise(average_home_price = mean(us))

# read GDP and annualize
us_gdp <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP&scale=left&cosd=1947-01-01&coed=2024-04-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-07-25&revision_date=2024-07-25&nd=1947-01-01") %>% 
  rename_all(tolower) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>% 
  group_by(year) 


sapply(us_gdp, class)

us_gdp_annual <- us_gdp %>% 
  summarise(annual_gdp = mean(gdp))

tail(us_gdp)

# Read 30 year mortgage rates
us_30y <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=MORTGAGE30US&scale=left&cosd=1971-04-02&coed=2024-08-29&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Weekly%2C%20Ending%20Thursday&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-09-03&revision_date=2024-09-03&nd=1971-04-02") %>% 
  rename_all(tolower)

us_30y %>% 
  ggplot(aes(x = date, y = mortgage30us)) +
  geom_point()

# annualize 30 year mortgage rates 
us_30y_year <- us_30y %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>% 
  group_by(year) %>% 
  summarise(average_mortgage = mean(mortgage30us))

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

# write.csv(mortgage_home, "mortgage_home.csv")

summary(lm(average_home_price ~ average_mortgage, mortgage_home))
