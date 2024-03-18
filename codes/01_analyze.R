# Author: Jerry Hong
# Disclaimer: The code below are through the assistance of GitHub Copilot


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
  scale_x_continuous(limits = c(0, 300)) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Average Price and Customer Rating by Accommodation Type', x = 'Average Price', y = 'Average Customer Rating') +
  theme_bw() +
  ggsave('plots/average_price_and_rating.png')

# some observations include a downward trend in the for hostels, implying a higher price does not necessarily mean a higher rating
# for hotels and other accommodations, there is a positive trend, implying a higher price is associated with a higher rating

# create a histogram distributing the average customer rating
vienna_data %>%
  ggplot(aes(x = guestreviewsrating)) +
  geom_histogram(binwidth = 0.5, fill = 'blue', color = 'black') +
  labs(title = 'Distribution of Average Customer Rating', x = 'Average Customer Rating', y = 'Frequency') +
  theme_bw() +
  ggsave('plots/distribution_of_rating.png')

# create a distribution for the number of reviews
vienna_data %>%
  ggplot(aes(x = rating_reviewcount)) +
  geom_histogram(bins = 50, fill = 'orange', color = 'black') +
  labs(title = 'Distribution of Number of Reviews', x = 'Number of Reviews', y = 'Frequency') +
  theme_bw() +
  ggsave('plots/distribution_of_reviews.png')

# remove ' miles' from the 'center1distance' column and convert to numeric
vienna_data <- vienna_data %>%
  mutate(distancecitycenter = as.numeric(gsub(' miles', '', center1distance)))

# create a histogram for the average distance from the city center
vienna_data %>%
  ggplot(aes(x = distancecitycenter)) +
  geom_histogram(binwidth = 1, fill = 'green', color = 'black') +
  labs(title = 'Distribution of Distance from City Center', x = 'Distance from City Center (miles)', y = 'Frequency') +
  theme_bw() +
  ggsave('plots/distribution_of_distance.png')

# based on the figure, there seems to be a couple outliers that are far from the city center

# create a density plot for the average price
vienna_data %>%
  ggplot(aes(x = price)) +
  geom_density(fill = 'red', color = 'black') +
  labs(title = 'Density Plot of Average Price', x = 'Average Price', y = 'Density') +
  theme_bw() +
  ggsave('plots/density_of_price.png')

# create a histogram for the average price, among hotels only
vienna_data %>%
  filter(type == 'hotel') %>%
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 20, fill = 'purple', color = 'black') +
  labs(title = 'Distribution of Average Price for Hotels', x = 'Average Price', y = 'Frequency') +
  theme_bw() +
  ggsave('plots/distribution_of_price_hotels.png')

# add a density to the histogram plot of average price for hotels
vienna_data %>%
  filter(type == 'hotel') %>%
  ggplot(aes(x = price)) +
  geom_histogram(aes(y=..density..), binwidth = 20, fill = 'white', color = 'black', alpha = 0.5) +
  geom_density(lwd = 1, alpha = 0.25, fill = 'blue', color = 'black') +
  labs(title = 'Distribution of Average Price for Hotels', x = 'Average Price', y = 'Density') +
  theme_bw() +
  ggsave('plots/distribution_of_price_hotels_density.png')

# create a histogram for the average price, among hotels only but restricted to distance from city at 8
vienna_data %>%
  filter(type == 'hotel', distancecitycenter < 8) %>%
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 20, fill = 'purple', color = 'black') +
  labs(title = 'Distribution of Average Price for Hotels within 8 miles of City Center', x = 'Average Price', y = 'Frequency') +
  theme_bw() +
  ggsave('plots/distribution_of_price_hotels_8miles.png')

# create scatter plot for the average price, among hotels only and add a horizontal line where the distance is at 8
vienna_data %>%
  filter(type == 'hotel') %>%
  ggplot(aes(x = price, y = distancecitycenter)) +
  geom_point() +
  geom_hline(yintercept = 8, color = 'red', lwd = 1) +
  labs(title = 'Average Price by Distance from City Center for Hotels', x = 'Average Price', y = 'Distance from City Center (miles)') +
  theme_bw() +
  ggsave('plots/average_price_by_distance_hotels.png')

# create a box plot for the average price by type of accommodation
vienna_data %>%
  ggplot(aes(x = type, y = price, fill = type)) +
  geom_boxplot() +
  labs(title = 'Average Price by Type of Accommodation', x = 'Type of Accommodation', y = 'Average Price') +
  theme_bw() +
  ggsave('plots/boxplot_price_by_type.png')

# create a violin plot for the average price by type of accommodation
vienna_data %>%
  ggplot(aes(x = type, y = price, fill = type)) +
  geom_violin() +
  labs(title = 'Average Price by Type of Accommodation', x = 'Type of Accommodation', y = 'Average Price') +
  theme_bw() +
  ggsave('plots/violin_price_by_type.png')

# save the modified data set as a csv
write_csv(vienna_data, 'data/processed/vienna_data.csv')

