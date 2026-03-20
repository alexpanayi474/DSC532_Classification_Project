install.packages("janitor")
install.packages("vip")
install.packages("skimr")
library(tidyverse)
library(janitor)
library(skimr)
library(vip)
install.packages("corrplot")
library(corrplot)
train_raw <- read_csv("train.csv")
test_raw  <- read_csv("test.csv")
train_raw %>%
  count(satisfaction) %>%
  mutate(prop = n / sum(n))
summary(train_raw)
# Count how many missing values we have in train set per variable
train_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "na_count") %>%
  arrange(desc(na_count))
# count the missing % of each variable
train_raw %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "na_percent") %>%
  arrange(desc(na_percent))
# count the missing % of each variable in test set
test_raw %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "na_percent") %>%f
arrange(desc(na_percent))
# count how many missing values we have in the test_raw data
test_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "na_count") %>%
  arrange(desc(na_count))
library(tidyverse)
library(janitor)

train <- train_raw %>%
  clean_names() %>%
  select(-x1, -id) %>%
  drop_na(arrival_delay_in_minutes)

test <- test_raw %>%
  clean_names() %>%
  select(-x1, -id) %>%
  drop_na(arrival_delay_in_minutes)
train <- train %>%
  mutate(
    satisfaction = factor(satisfaction),
    gender = factor(gender),
    customer_type = factor(customer_type),
    type_of_travel = factor(type_of_travel),
    class = factor(class)
  )

test <- test %>%
  mutate(
    satisfaction = factor(satisfaction),
    gender = factor(gender),
    customer_type = factor(customer_type),
    type_of_travel = factor(type_of_travel),
    class = factor(class)
  )
# correlation matrix of numeric data in train_set
train_raw %>% select(-...1,-id) %>%
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)
ggplot(train, aes(departure_delay_in_minutes, arrival_delay_in_minutes)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "red")
delay_model <- lm(arrival_delay_in_minutes ~ departure_delay_in_minutes,
                  data = train,
                  na.action = na.exclude)

train$arrival_delay_in_minutes[is.na(train$arrival_delay_in_minutes)] <-
  predict(delay_model, newdata = train[is.na(train$arrival_delay_in_minutes), ])
sum(is.na(train$arrival_delay_in_minutes))
#Missing values were present only in the variable Arrival Delay in Minutes (310 observations, approximately 0.3% of the dataset). Correlation analysis revealed a very strong relationship between Arrival Delay and Departure Delay (correlation coefficient = 0.97). 
#A scatterplot confirmed this near-linear relationship. Therefore, a linear regression model was used to estimate the missing arrival delay values using departure delay as the predictor.
#not needed just to see it in heatmap tha the 2 are correlated
# correlation heatmap of numerical variables
cor_mat_num <- train_raw %>% select(-...1,-id) %>%
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs")

corrplot(cor_mat_num,
         method = "color",
         order='hclust',
         addCoef.col = NULL,
         number.cex = 0.7,
         tl.cex = 0.8)
# Encode categorical variables as factors
train_encoded <- train %>%
  mutate(
    gender = factor(gender),
    customer_type = factor(customer_type),
    type_of_travel = factor(type_of_travel),
    class = factor(class),
    satisfaction = factor(satisfaction,
                          levels = c("neutral or dissatisfied", "satisfied"))
  )

# check structure
str(train_encoded[, c("gender","customer_type","type_of_travel","class","satisfaction")])
train %>%
  filter(is.na(arrival_delay_in_minutes)) %>%
  select(departure_delay_in_minutes,
         arrival_delay_in_minutes) %>%
  head()
# Remove identifier columns (no predictive meaning)
# Convert satisfaction to factor for classification
train_encoded <- train_encoded %>%
  select(-x1, -id) %>%
  mutate(
    satisfaction = factor(satisfaction)
  )
test_encoded <- test_encoded %>% select(-x1,-id) %>% mutate(satisfaction = factor(satisfaction))
glimpse(train_encoded)
train_encoded %>%
  group_by(departure_delay_in_minutes) %>%
  summarise(
    avg_arrival_delay = mean(arrival_delay_in_minutes, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = departure_delay_in_minutes,
             y = avg_arrival_delay)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Average Arrival Delay vs Departure Delay") +
  theme_minimal()
# impute the missing values
train_encoded <- train_encoded %>%
  mutate(
    departure_delay_group = case_when(
      departure_delay_in_minutes == 0 ~ "On time",
      departure_delay_in_minutes<= 15 ~ "Minor delay",
      departure_delay_in_minutes <= 60 ~ "Moderate delay",
      departure_delay_in_minutes <= 180 ~ "Major delay",
      TRUE ~ "Severe delay"
    )
  )
bin_avg <- train_encoded %>%
  filter(!is.na(arrival_delay_in_minutes)) %>%
  group_by(departure_delay_group) %>%
  summarise(
    bin_mean = mean(arrival_delay_in_minutes)
  )
train_encoded_imputed <- train_encoded %>%
  left_join(bin_avg, by = "departure_delay_group") %>%
  mutate(
    arrival_delay_in_minutes =
      ifelse(is.na(arrival_delay_in_minutes),
             bin_mean,
             arrival_delay_in_minutes)
  ) %>%
  select(-bin_mean)

train_encoded_imputed %>%
  count(departure_delay_group) %>%
  mutate(prop = n / sum(n))

# check if missing values remain in train set
sum(is.na(train_encoded_imputed$arrival_delay_in_minutes))

# Check if the imputation affects the distribution
#library(ggplot2)
ggplot() +
  geom_density(data = train_encoded,
               aes(x = arrival_delay_in_minutes),
               color = "red",
               na.rm = TRUE) +
  geom_density(data = train_encoded_imputed,
               aes(x = arrival_delay_in_minutes),
               color = "blue") +
  labs(title = "Arrival Delay Distribution: Before vs After Imputation",
       x = "Arrival Delay (minutes)",
       y = "Density") +
  theme_minimal()

summary(train_encoded$arrival_delay_in_minutes)
summary(train_encoded_imputed$arrival_delay_in_minutes)

# apply the train averages of each bin to test missing arrival delay values to prevent from data leakage (questionable)
test_encoded <- test_encoded %>%
  mutate(
    departure_delay_group = case_when(
      departure_delay_in_minutes == 0 ~ "On time",
      departure_delay_in_minutes <= 15 ~ "Minor delay",
      departure_delay_in_minutes <= 60 ~ "Moderate delay",
      departure_delay_in_minutes <= 180 ~ "Major delay",
      TRUE ~ "Severe delay"
    )
  )

test_encoded_imputed <- test_encoded %>%
  left_join(bin_avg, by = "departure_delay_group") %>%
  mutate(
    arrival_delay_in_minutes =
      ifelse(is.na(arrival_delay_in_minutes),
             bin_mean,
             arrival_delay_in_minutes)
  ) %>%
  select(-bin_mean)

sum(is.na(test_encoded_imputed$arrival_delay_in_minutes))

# keep versions were missing values are just dropped for model comparions
train_encoded_dropped <- train_encoded %>% drop_na(arrival_delay_in_minutes)
test_encoded_dropped <- test_encoded %>% drop_na(arrival_delay_in_minutes)

train_encoded_imputed %>%
  count(departure_delay_group, satisfaction) %>%
  group_by(departure_delay_group) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = departure_delay_group,
             y = prop,
             fill = satisfaction)) +
  geom_col(position = "dodge") +
  labs(title = "Satisfaction by Departure Delay Category",
       y = "Proportion") +
  theme_minimal()

train %>%
  ggplot(aes(x = satisfaction, fill = satisfaction)) +
  geom_bar() +
  labs(title = "Distribution of Passenger Satisfaction") +
  theme_minimal()

train %>%
  count(class, satisfaction) %>%
  group_by(class) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = class, y = prop, fill = satisfaction)) +
  geom_col(position = "dodge") +
  labs(title = "Satisfaction by Travel Class", y = "Proportion") +
  theme_minimal()

train %>%
  ggplot(aes(x = arrival_delay_in_minutes, fill = satisfaction)) +
  geom_histogram(bins = 50, alpha = 0.5, position = "identity") +
  coord_cartesian(xlim = c(0, 200)) +
  theme_minimal()

train %>%
  group_by(satisfaction) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

train %>%
  group_by(satisfaction) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  pivot_longer(-satisfaction) %>%
  pivot_wider(names_from = satisfaction, values_from = value) %>%
  mutate(diff = satisfied - `neutral or dissatisfied`) %>%
  arrange(desc(abs(diff)))

cat_vars

colnames(train)

glimpse(train)

# Distributions of Continuous Variables
train %>%
  select(age,
         flight_distance,
         departure_delay_in_minutes,
         arrival_delay_in_minutes) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ name, scales = "free") +
  theme_minimal()

# Distribution of Serving ratings (Discrete numeric 1-5 scale)

train %>%
  select(inflight_wifi_service:cleanliness) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = factor(value))) +
  geom_bar(fill = "darkorange") +
  facet_wrap(~ name, scales = "free") +
  theme_minimal()

# categorical
train %>%
  select(gender, customer_type, type_of_travel, class, satisfaction) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_bar(fill = "purple") +
  facet_wrap(~ name, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pairs(train)

table(train_encoded_dropped$satisfaction)

colSums(is.na(train_encoded_dropped))

#train_encoded_dropped <- train_encoded_dropped %>% select(-departure_delay_group)
fit <- glm(satisfaction ~ .,
           data = train_encoded_dropped,
           family = binomial)

summary(fit)