# Load tidyverse
library(tidyverse)

# Read in data
click_data <- read_csv("click_data.csv")
click_data

# Find oldest and most recent date
min(click_data$visit_date)
max(click_data$visit_date)

# Read in the data
click_data <- read_csv("click_data.csv")

# Calculate the mean conversion rate by day of the week
click_data %>%
  group_by(weekdays(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Read in the data
click_data <- read_csv("click_data.csv")

# Calculate the mean conversion rate by week of the year
click_data %>%
  group_by(week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Compute conversion rate by week of the year
click_data_sum <- click_data %>%
  group_by(week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Build plot
ggplot(click_data_sum, aes(x = `week(visit_date)`,
                           y = conversion_rate)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent)

# Load powerMediation
library(powerMediation)

# Compute and look at sample size for experiment in August
total_sample_size <- SSizeLogisticBin(p1 = 0.54,
                                      p2 = 0.64,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size

# Load powerMediation
library(powerMediation)

# Compute and look at sample size for experiment in August with a 5 percentage point increase
total_sample_size <- SSizeLogisticBin(p1 = 0.54,
                                      p2 = 0.59,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size

# Group and summarize data
experiment_data_clean_sum <- experiment_data_clean %>%
  group_by(condition, visit_date) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Make plot of conversion rates over time
ggplot(experiment_data_clean_sum,
       aes(x = visit_date,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line()

# Load package for cleaning model results
library(broom)

# View summary of results
experiment_data_clean %>%
  group_by(condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Run logistic regression
experiment_results <- glm(clicked_adopt_today ~ condition ,
                          family = "binomial",
                          data = experiment_data_clean) %>%
  tidy()
experiment_results

# Load package for running power analysis
library(powerMediation)

# Run logistic regression power analysis
total_sample_size <- SSizeLogisticBin(p1 = 0.39,
                                      p2 = 0.59,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size

# Read in data for follow-up experiment
followup_experiment_data <- read_csv("followup_experiment_data.csv")

# View conversion rates by condition
followup_experiment_data %>%
  group_by(condition) %>%
  summarise(conversion_rate = mean(clicked_adopt_today))

# Run logistic regression
followup_experiment_results <- glm(clicked_adopt_today~condition,
                                   family = "binomial",
                                   data = followup_experiment_data) %>%
  tidy()
followup_experiment_results

# Compute monthly summary
eight_month_checkin_data_sum <- eight_month_checkin_data %>%
  mutate(month_text = month(visit_date, label = TRUE)) %>%
  group_by(month_text, condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))


# Plot month-over-month results
ggplot(eight_month_checkin_data_sum,
       aes(x = month_text,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line()

# Plot monthly summary
ggplot(eight_month_checkin_data_sum,
       aes(x = month_text,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent) +
  labs(x = "Month",
       y = "Conversion Rate")