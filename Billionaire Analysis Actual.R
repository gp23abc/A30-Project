billionaires_data <- read.csv("Billionaires Statistics Dataset.csv")
head(billionaires_data)
head(billionaires_data,2)
colnames(billionaires_data)
billionaires_data$gdp_country <- gsub("[$,]", "", billionaires_data$gdp_country)
billionaires_data$gdp_country <- as.numeric(billionaires_data$gdp_country)
sum(is.na(billionaires_data$gdp_country))
head(billionaires_data[is.na(billionaires_data$gdp_country), ])
library(dplyr)
# Remove rows where 'gdp_country' is NA
cleaned_data <- billionaires_data %>%
  filter(!is.na(gdp_country))
nrow(cleaned_data)
class(billionaires_data$gdp_country)
class(billionaires_data$finalWorth)
# Filter the dataset for each country and compute the average net worth
avg_networth <- billionaires_data %>%
  group_by(country) %>%
  summarise(avg_networth = mean(finalWorth, na.rm = TRUE), 
            GDP = mean(gdp_country, na.rm = TRUE))
library(ggplot2)
ggplot(avg_networth, aes(x = GDP, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation between Average Net Worth and GDP",
       x = "GDP",
       y = "Average Net Worth") +
  theme_minimal()
#filtering out any missing values
avg_networth_clean <- avg_networth %>%
  filter(!is.na(GDP) & !is.na(avg_networth) & is.finite(GDP) & is.finite(avg_networth))

ggplot(avg_networth_clean, aes(x = GDP, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation between Average Net Worth and GDP",
       x = "GDP",
       y = "Average Net Worth") +
  theme_minimal()ggplot(avg_networth_clean, aes(x = GDP, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation between Average Net Worth and GDP",
       x = "GDP",
       y = "Average Net Worth") +
  theme_minimal()

nrow(cleaned_data)

ggplot(avg_networth_clean, aes(x = GDP, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation between Average Net Worth and GDP",
       x = "GDP",
       y = "Average Net Worth") +
  theme_minimal()

library(scales)

# Scale GDP to billions for better readability
avg_networth_clean$GDP_scaled <- avg_networth_clean$GDP / 1e9  # Scaling GDP to billions

# Create the plot with scaled GDP values
ggplot(avg_networth_clean, aes(x = GDP_scaled, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation between Average Net Worth of Billionaires from a country and the country's GDP",
       x = "GDP (Billions)",
       y = "Avg Net Worth of Billionaires from a country") +
  scale_x_continuous(labels = label_dollar(scale = 1, suffix = "B")) +  # Format x-axis labels in billions
  theme_minimal()

ggplot(avg_networth_clean, aes(x = GDP, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_si("B"))) +  # Log scale with SI units in billions
  labs(title = "Correlation between Average Net Worth of Billionaires from a country and the country's GDP",
       x = "GDP (Log Scale, Billions)",
       y = "Avg Net Worth of Billionaires from a country") +
  theme_minimal()

ggplot(avg_networth_clean, aes(x = GDP, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_log10(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +  # Formats x-axis with dollar signs
  scale_y_continuous(labels = scales::label_dollar(scale = 1)) +  # Formats y-axis with dollar signs
  labs(title = "Correlation between Average Net Worth of Billionaires from a country and the country's GDP",
       x = "GDP (Log Scale, $ in Billions/Trillions)",
       y = "AAvg Net Worth of Billionaires from a country ($)") +
  theme_minimal()

highest_avg_networth <- avg_networth_clean %>%
  filter(avg_networth == max(avg_networth, na.rm = TRUE))

highest_avg_networth

ggplot(avg_networth_clean, aes(x = GDP, y = avg_networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_log10(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +  # Formats x-axis with dollar signs
  scale_y_continuous(labels = scales::label_dollar(scale = 1)) +  # Formats y-axis with dollar signs
  labs(title = "Correlation between Average Net Worth of Billionaires from a country and the country's GDP",
       x = "GDP (Log Scale, $ in Billions/Trillions)",
       y = "Avg Net Worth of Billionaires from a country ($)") +
  theme_minimal()

# Histogram with normal curve
ggplot(avg_networth, aes(x = avg_networth)) +
  geom_histogram(aes(y = ..density..), binwidth = 1000000000, fill = "skyblue", color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(avg_networth$avg_networth, na.rm = TRUE), 
                            sd = sd(avg_networth$avg_networth, na.rm = TRUE)), 
                color = "red") +
  labs(title = "Distribution of Average Net Worth", 
       x = "Average Net Worth", 
       y = "Density") +
  theme_minimal()

ggplot(avg_networth, aes(x = avg_networth)) +
  geom_histogram(aes(y = ..density..), binwidth = 1000000000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Net Worth", 
       x = "Average Net Worth", 
       y = "Density") +
  theme_minimal()

head(avg_networth)

summary(avg_networth$avg_networth)

ggplot(avg_networth, aes(x = avg_networth)) +
  geom_histogram(aes(y = ..density..), binwidth = 500, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Net Worth", 
       x = "Average Net Worth", 
       y = "Density") +
  theme_minimal()

ggplot(avg_networth, aes(x = avg_networth)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Net Worth", 
       x = "Average Net Worth", 
       y = "Count") +
  theme_minimal()

# Histogram with normal curve
ggplot(avg_networth, aes(x = avg_networth)) +
  geom_histogram(aes(y = ..density..), binwidth = 500, fill = "skyblue", color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(avg_networth$avg_networth, na.rm = TRUE), 
                            sd = sd(avg_networth$avg_networth, na.rm = TRUE)), 
                color = "red") +
  labs(title = "Distribution of Average Net Worth", 
       x = "Average Net Worth", 
       y = "Density") +
  theme_minimal()

ggplot(avg_networth, aes(x = avg_networth)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  stat_function(fun = function(x) {
    dnorm(x, mean = mean(avg_networth$avg_networth, na.rm = TRUE), 
          sd = sd(avg_networth$avg_networth, na.rm = TRUE)) * 
      nrow(avg_networth) * 500  # Scale to counts
  }, 
  color = "red") +
  labs(title = "Distribution of Average Net Worth of Billionaires from a country", 
       x = "Average Net Worth of Billionaires from a country ($)", 
       y = "Frequency") +
  theme_minimal()

# GDP in original data
billionaires_data %>%
  filter(country == "Argentina") %>%
  select(gdp_country) %>%
  unique()

# GDP in the summarised data
avg_networth %>%
  filter(country == "Argentina") %>%
  select(GDP)
