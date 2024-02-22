# load packages
library(tidyverse)

# import data and read as a tibble
vienna_data <- read_csv('data/raw/hotelbookingdata-vienna.csv') 
head(vienna_data)
# there are 430 observations from 24 variables

# check for duplicates, there are two duplicates
sum(duplicated(vienna_data))
vienna_data[duplicated(vienna_data),]

# remove duplicates from vienna_data
vienna_data <- vienna_data[!duplicated(vienna_data),]

# find average price
mean(vienna_data$price) # average price is 131.37

# create new variable 'type' that categorizes the accommodation type into hotel, hostel, and other
vienna_data <- vienna_data %>%
  mutate(type = case_when(
    grepl('_ACCOM_TYPE@Hotel', accommodationtype) ~ 'hotel',
    grepl('_ACCOM_TYPE@Hostel', accommodationtype) ~ 'hostel',
    TRUE ~ 'other'
  ))

# get the count for each type
vienna_data %>%
  group_by(type) %>%
  summarize(count = n())
# there are 264 hotels, 6 hostels, and 158 other types of accommodations

# remove the ' /5' in every value in the guestreviewsrating column (we know that is rated on a scale of 1 to 5)
vienna_data <- vienna_data %>%
  mutate(guestreviewsrating = as.numeric(gsub(' /5', '', guestreviewsrating)))

# find proportion of missing average customer rating
sum(is.na(vienna_data$guestreviewsrating)) / nrow(vienna_data) 
# the proportion of missing average customer rating relative to the data set is 0.0818

# find proportion of missing average customer rating by type of accommodation
vienna_data %>%
  group_by(type) %>%
  summarize(missing = sum(is.na(guestreviewsrating)) / n())
# the average missing customer rating for hotels 0.0379, 0 for hostels, and 0.215 for other accommodations

# create a table with the average price for each accommodation type, including standard deviation, median, and number of observations, and save this as a csv
vienna_data %>%
  group_by(type) %>%
  summarize(mean_price = mean(price), sd_price = sd(price), median_price = median(price), count = n()) %>%
  write_csv('tables/average_price_by_type.csv')

# create a bar plot with the average price by type of accommodation, color for each type
vienna_data %>%
  group_by(type) %>%
  summarize(mean_price = mean(price)) %>%
  ggplot(aes(x = type, y = mean_price, fill=type)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Average Price by Type of Accommodation', x = 'Type of Accommodation', y = 'Average Price') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  ggsave('plots/average_price_by_type.png')

# create a scatter plot with the average price and average customer rating, color for each type, include a linear fit
vienna_data %>%
  ggplot(aes(x = price, y = guestreviewsrating, color = type)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Average Price and Average Customer Rating', x = 'Average Price', y = 'Average Customer Rating') +
  theme_bw() +
  ggsave('plots/average_price_and_rating.png')

