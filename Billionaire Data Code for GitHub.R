#Importing billionaire data
billionaires_data <- read.csv("Billionaires Statistics Dataset.csv")

#Displaying starting few rows of our dataset
head(billionaires_data)

#Displaying features of our dataset
colnames(billionaires_data)

#Removing '$' and ',' characters from the gdp_country column to clean the data
billionaires_data$gdp_country <- gsub("[$,]", "", billionaires_data$gdp_country)

#Converting the cleaned gdp_country column from character to numeric data type
billionaires_data$gdp_country <- as.numeric(billionaires_data$gdp_country)

#Counting the number of missing values (NA) in the gdp_country column
sum(is.na(billionaires_data$gdp_country))

library(dplyr)

#Removing rows where 'gdp_country' is NA
cleaned_data <- billionaires_data %>%
  filter(!is.na(gdp_country))
nrow(cleaned_data)

#Checking data type of gdp_country feature
class(billionaires_data$gdp_country)

#Checking data type of finalworth feature
class(billionaires_data$finalWorth)

# Filter the dataset for each country and compute the average net worth of billionaires from that country
avg_networth <- billionaires_data %>%
  group_by(country) %>%
  summarise(
    avg_networth = mean(finalWorth, na.rm = TRUE), 
    GDP = first(gdp_country) 
  )

library(ggplot2)
library(scales) # For label_dollar()

#plotting the correlation between the dependent and independent variable
ggplot(avg_networth, aes(x = GDP, y = avg_networth)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "cyan") +
  labs(title = "Correlation between Average Net Worth and GDP",
       x = "GDP",
       y = "Average Net Worth") +
  theme_minimal()

#Checking for small Values in the dataset
billionaires_data %>%
  filter(gdp_country < 1) %>%
  select(country, gdp_country)

#statistics of gdp(s) of countries 
summary(billionaires_data$gdp_country)


#adding relevant units for x axis scale value, and making the values understandable 
ggplot(avg_networth, aes(x = GDP, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "Green") +
  labs(title = "Correlation between average networth of billionaires from a country and the country's GDP",
       x = "GDP ($)",
       y = "Average networth of billionaires from a country ($)") +
  scale_x_continuous(labels = label_dollar()) + # Adding dollar symbol to x-axis units
  scale_y_continuous(labels = label_dollar()) + # Adding dollar symbol to y-axis units
  theme_minimal()

#Checking for non-finite Values in GDP and avg_networth since we don't want such values to be considered
avg_networth %>%
  filter(!is.finite(GDP) | !is.finite(avg_networth))

#Removing NA values in avg_networth
avg_networth <- avg_networth %>%
  filter(!is.na(GDP) & !is.na(avg_networth))

#Keeping only finite values
avg_networth <- avg_networth %>%
  filter(is.finite(GDP), is.finite(avg_networth)) 

#replotting the correlation between the dependent and independent variable removing 'NA' values for avg_networth
ggplot(avg_networth, aes(x = GDP, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation between average networth of billionaires from a country and the country's GDP",
       x = "GDP ($)",
       y = "Average networth of billionaires from a country ($)") +
  scale_x_continuous(labels = label_dollar()) + # Adding dollar symbol to x-axis
  scale_y_continuous(labels = label_dollar()) + # Adding dollar symbol to y-axis
  theme_minimal()

#filtering out any missing values in the dataset
avg_networth_clean <- avg_networth %>%
  filter(!is.na(GDP) & !is.na(avg_networth) & is.finite(GDP) & is.finite(avg_networth))

#Scaling GDP to billions for better readability
avg_networth_clean$GDP_scaled <- avg_networth_clean$GDP / 1e9  # Scaling GDP to billions

#Creating the plot with scaled Gdp values for better readability
ggplot(avg_networth_clean, aes(x = GDP_scaled, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Correlation between average networth of billionaires from a country and the country's GDP",
       x = "GDP ($)",
       y = "Average networth of billionaires from a country ($)") +
  scale_x_continuous(labels = label_dollar(scale = 1, suffix = "Bill")) +  # Formating x-axis labels in billions
  scale_y_continuous(labels = label_dollar()) + # Adding dollar symbol to y-axis
  theme_minimal()

#Applying a logarithmic transformation since there's a lot of variability in the data
#Applying this transformation would make make the smaller values spread out more and
#the larger values more compact. It will also give added clarity to the figure visually
ggplot(avg_networth_clean, aes(x = GDP, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_log10(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +  # Formats x-axis with dollar signs
  scale_y_continuous(labels = scales::label_dollar(scale = 1)) +  # Formats y-axis with dollar signs
  labs(title = "Correlation between Average Net Worth of Billionaires from a country and the country's GDP",
       x = "GDP (Log scale, $ in billions/trillions)",
       y = "Average networth of billionaires from a country ($)") +
  theme_minimal()

#statistics of avg networth of billionaires from a country to find min and max value of the feature
summary(avg_networth$avg_networth)

#Plotting histogram of dependent variable with density curve
ggplot(avg_networth, aes(x = avg_networth)) +
  geom_histogram(binwidth = 1500, fill = "gold", color = "black") +
  stat_function(fun = function(x) {
    dnorm(x, mean = mean(avg_networth$avg_networth, na.rm = TRUE), 
          sd = sd(avg_networth$avg_networth, na.rm = TRUE)) * 
      nrow(avg_networth) * 500  # Scaling to counts
  }, 
  color = "pink") +
  labs(title = "Distribution of average networth of billionaires from a country", 
       x = "Average networth of billionaires from a country ($)", 
       y = "Frequency") +
  theme_minimal()


#Conducting Spearman's correlation test since dependent variable doesn't follow a normal distribution
spearman_test <- cor.test(
  avg_networth_clean$GDP, 
  avg_networth_clean$avg_networth, 
  method = "spearman"
)

#Displaying results
spearman_test

#Plotting scatterplot before log transformation
normal_plot <- ggplot(avg_networth_clean, aes(x = GDP, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot Before Log Transformation",
       x = "GDP ($)",
       y = "Average Net Worth ($)") +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  theme_minimal()

install.packages("patchwork")

library(patchwork)

#Plotting scatterplot after log transformation
log_plot <- ggplot(avg_networth_clean, aes(x = GDP, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_log10(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1)) +
  labs(title = "Scatterplot After Log Transformation",
       x = "GDP (Log scale, $ in billions/trillions)",
       y = "Average Net Worth ($)") +
  theme_minimal()

#Combining the plots side by side
normal_plot + log_plot



