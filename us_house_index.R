library(tidyverse)
library(ggtext)
library(patchwork)
library(glue)
library(ggtext)
setwd("/Users/takayukitamura/Documents/R_Computing/house_prices")

# upload house_px file
house_index <- read.csv("/Users/takayukitamura/Documents/R_Computing/house_prices/house_px_2023_10.csv", sep = ",", 
                     header = TRUE, stringsAsFactors = FALSE ) %>% 
  rename(date = DATE,
         "US_Avg" = US,
         "SanJose" = San.Jose,
         "NY.NJ" = NY.NJ,
         "Columbus" = Columbus.OH) %>% 
  select(-X)

tail(house_index)

# house_index_01012024 <- data.frame(date = "2024-01-01",
#                                "US_Avg" = 664.58,
#                                "Chicago" = 544.44,
#                                "Pittsburgh" = 505.68,
#                                "Boston" = 1315.89,
#                                "SanJose" = 1289.43,
#                                "SanFransisco" = 1181.01,
#                                "NY.NJ" = 1069.37,
#                                "Austin" = 895.90,
#                                "Philadelphia" = 837.88, 
#                                "Columbus" = 598.67)

updates <- tribble(~date, ~US_Avg, ~Chicago, ~Pittsburgh, ~Boston, ~SanJose, ~SanFransisco, 
                      ~NY.NJ, ~Austin, ~Philadelphia, ~Columbus,
                      "2024-01-01", 664.58, 544.44, 505.68, 1315.89, 1289.43, 1181.01, 
                      1069.37, 895.90, 837.88, 598.67)


house_index <- rbind(house_index, updates)


# Convert 'date' column to Date format if it's not already
house_index$date <- as.Date(house_index$date)

sapply(house_index, class)
head(house_index)
tail(house_index)

house_index[ 196, ]

# Reshape the data frame from wide to long format
house_index_long <- pivot_longer(house_index, cols = -date, names_to = "city", values_to = "price")

# Get the latest price for each city
latest_prices <- aggregate(price ~ city, data = house_index_long[house_index_long$date == as.Date("2023-10-01"), ], max)

# Reorder the levels of 'city' based on the latest price
house_index_long$city <- factor(house_index_long$city, levels = latest_prices[order(latest_prices$price, decreasing = TRUE), "city"])

# Plot the data with ggplot
# ggplot(data = house_index_long, aes(x = date, y = price, color = city)) +
#   geom_line() +
#   labs(title = "Historical House Prices Index in US major cities",
#        subtitle = "(house price of 1980-01-01 = 100)",
#        caption = "Source = FRED(Federal Reserve Bank of St.Louis)", 
#        x = NULL,
#        y = "House Price Index") +
#   theme(
#     legend.title = element_blank(),
#     plot.caption = element_markdown(color = "grey", size = 7)
#   )

ggplot(data = house_index_long, aes(x = date, y = price, color = city)) +
  geom_line() +
  labs(title = "Historical House Prices Index in US major cities",
       subtitle = "(house price of 1980-01-01 = 100)",
       caption = "Source = FRED(Federal Reserve Bank of St.Louis)", 
       x = NULL,
       y = "House Price Index") +
  theme(
    legend.title = element_blank(),
    legend.key = element_blank(),
    plot.caption = element_markdown(color = "grey", size = 7)
  ) +
  theme_minimal()

ggsave("/Users/takayukitamura/Documents/R_Computing/figures/histrical_house_prices_major_cities.png", width = 6, height = 5)

# plot just US average house price index

us_house_price <- house_index %>% 
  select(date, US_Avg) %>% 
ggplot(aes(x = date, y = US_Avg)) +
  geom_line() +
  labs(title = "Historical US Average House Prices Index",
       subtitle = "(house price of 1980-01-01 = 100)",
       caption = "Source = FRED(Federal Reserve Bank of St.Louis)", 
       x = NULL,
       y = "House Price Index") +
  theme(
    legend.title = element_blank(),
    plot.caption = element_markdown(color = "grey", size = 7)
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/figures/histrical_house_prices.tiff", width = 6, height = 4)

us_cpi_raw <- read.csv("/Users/takayukitamura/Documents/R_Computing/house_prices/cpi_aucsl.csv") %>% 
  rename_all(tolower) %>% 
  mutate("cpi_index" = (cpiaucsl/78)*100) 
  
us_cpi_raw$date <- as.Date(us_cpi_raw$date, 
                           format = "%Y-%m-%d")  


us_cpi_raw %>% 
  ggplot(aes(x=date, y = cpi_index)) + 
  geom_line()

cpi_house_index <- merge(us_cpi_raw, house_index, by = "date") %>% 
  select(date, cpi_index, US_Avg) %>% 
  rename("home_price_average" = US_Avg) %>% 
  pivot_longer(cols = -date, names_to = "inflation", 
               values_to = "index")


cpi_house_index %>% 
  ggplot(aes(x = date, y = index, color = inflation)) +
  geom_line()+
  labs(title = "Historical Inflation and US Average House Price Index",
       subtitle = "(1980-01-01 = 100)",
       caption = "Source = FRED(Federal Reserve Bank of St.Louis)", 
       x = NULL,
       y = "inflation & average house price index") +
  theme(
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.caption = element_markdown(color = "grey", size = 7)
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/figures/inflation vs home price.tiff", width = 6, height = 4)
str(us_cpi_raw)
us_cpi_raw[397,]
cpi_house_index[1,]
cpi_house_index[2,]

cpi_house_index[391,]
cpi_house_index[392,]

(average_inflation <- 394/67.1)
(average_home_price_appreciation <-658/59.9 )
(10.98497-1)

67.1*(1.033)^50

str(cpi_house_index)
                          
??"Employee"
