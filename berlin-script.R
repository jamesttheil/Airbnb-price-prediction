# Load required packages
library(pacman)

p_load(tidyverse, magrittr, psych, mice,
       nlme, stats, tidymodels, ranger, naniar)

# Reading data and inpspecting DV
berlin_listings <- read_csv("http://data.insideairbnb.com/germany/be/berlin/2021-03-12/data/listings.csv.gz")

# Inspect price as it is my DV
head(berlin_listings$price, 20)

# remove $ signs from prices
berlin_listings$price = as.numeric(gsub("\\$", "", berlin_listings$price))

# inspect distribution
berlin_listings %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 5, colour="black", fill="grey") +
  theme_bw()

# there are some extreme outliers- I will remove everything above 300
berlin_listings %>% 
  filter(price < 300) %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 5, colour="black", fill="grey") +
  theme_bw()

berlin_listings %<>% 
  filter(price < 300)


# Will need to transform the data to remedy the skew
berlin_listings %>% 
  ggplot(aes(x = log(price)))+
  geom_histogram(binwidth = .1, colour = "black", fill = "grey") +
  theme_bw() +
  labs(x = "Listing price",
       Y = "Count",
       title = "Distribution of listing prices (log transformed)")

berlin_listings %<>% 
  mutate(price_log = log(price))

# make a new df with select variables for analysis
berlin_condensed <- berlin_listings %>%
  select(price, host_response_time, bedrooms, review_scores_rating:review_scores_value,
         property_type:beds, minimum_nights, price_log, number_of_reviews, neighbourhood_group_cleansed) %>% 
  filter(bathrooms_text != "Half-bath",
         bathrooms_text != "Shared half-bath",
         bathrooms_text != "Private half-bath") %>% 
  mutate(bathrooms = parse_number(bathrooms_text),
         property_type = as.factor(property_type),
         room_type = as.factor(room_type),
         neighbourhood_group_cleansed = as.factor(neighbourhood_group_cleansed))


#-----------------------------------------------------------------------
# Deal with missing data
#-----------------------------------------------------------------------

# As there is a lot of missing data, I will impute missing values using the mice package

pct_miss_case(berlin_condensed)

gg_miss_var(berlin_condensed, show_pct = TRUE)

# create succinct df

berlin_temp <- berlin_condensed %>% 
  select(price, bedrooms, property_type:bathrooms,
         beds:price_log, neighbourhood_group_cleansed )

# All missing values imputed using predictive mean matching

berlin_impute <- mice(berlin_temp, m=5, meth = "pmm")

berlin_condensed2 <- complete(berlin_impute, 1) # check that all is okay- yes

berlin_condensed <- berlin_condensed2

berlin_condensed %>% 
  mutate(price_log = round(price_log, digits = 2))


# there are a few listings with the price at zero I will remove these as the log transform is creating Inf's

berlin_condensed %<>% 
  filter(price > 0)

# redo the transformation

berlin_condensed %<>% 
  mutate(price_log = log(price))

#----------------------------------------------------------------------------
# Further cleaning of extreme outliers

berlin_condensed %>%
  ggplot(aes(x = factor(bathrooms), y = price_log, fill  = factor(bathrooms))) +
  geom_boxplot(outlier.alpha = .5) +
  theme_bw() +
  labs(x = "Bathrooms",
       y = "Listing price",
       title = "The effect of the number of bathrooms on price")

# remove bathroom outliers
berlin_condensed %<>% 
  filter(bathrooms < 4.5)

# there are many low-frequency obscure property types- these need to be removed

berlin_condensed %<>% 
  group_by(property_type) %>% 
  mutate(freq = n()) %>% 
  ungroup() %>% 
  filter(freq > 100) %>%
  select(-freq)

#---------------------------------------------------------------------------
# Exploratory analysis

# boxplot of the effects of property type on price

berlin_condensed %>% 
  ggplot(aes(x = property_type, y = price_log, fill = property_type)) +
  geom_boxplot(outlier.alpha = .5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  labs(x = "Property type",
       y = "Listing price",
       title = "The effect of property type on price",
       fill = "Property type")

# boxplot of the effects of number of occupants on price
# as expected - the number of individuals accomodated has a positive effect on price

berlin_condensed %>% 
  ggplot(aes(x = factor(accommodates), y = price_log, fill = factor(accommodates))) +
  geom_boxplot(outlier.alpha = .5) +
  theme_bw() +
  labs(x = "Accommodates",
       y = "Listing price",
       title = "The effect of the number of occupants on price",
       fill = "Accommodates")

# boxplot of the effects of number of bathrooms on price

berlin_condensed %>% 
  ggplot(aes(x = factor(bathrooms), y = price_log, fill  = factor(bathrooms))) +
  geom_boxplot(outlier.alpha = .5) +
  theme_bw() +
  labs(x = "Bathrooms",
       y = "Listing price",
       title = "The effect of the number of bathrooms on price",
       fill = "Number of bathrooms")

# boxplot of the effects of the room type on price

berlin_condensed %>% 
  ggplot(aes(x = room_type, y = price_log, fill = room_type)) +
  geom_boxplot(outlier.alpha = .5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  labs(x = "Room type",
       y = "Listing price",
       title = "The effect of the room type on price",
       fill = "Room type")

# boxplot of the effects of the number of beds on price

berlin_condensed %>% 
  ggplot(aes(x = factor(beds), y = price_log, fill = factor(beds))) +
  geom_boxplot(outlier.alpha = .5) +
  theme_bw() +
  theme(legend.key.size = unit(.5, 'cm')) +
  labs(x = "Number of beds",
       y = "Listing price",
       title = "The effect of the number of beds on price",
       fill = "Number of beds")

# boxplot of the effects of the neighbourhood on price

berlin_condensed %>% 
  ggplot(aes(x = neighbourhood_group_cleansed, y = price_log, fill = neighbourhood_group_cleansed)) +
  geom_boxplot(outlier.alpha = .5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  labs(x = "Neighbourhood",
       y = "Listing price",
       title = "The effect of the neighbourhood on price",
       fill = "Grouped neighbourhoods")

# This correlation matrix is useful in identifying multicollinearity
# There are some correlated predictors but they are not excessively correlated

berlin_condensed %>%
  select(-c(price)) %>%
  pairs.panels(stars = T)

#------------------------------------------------------------------------------
# split data into training and testing set

berlin_split <- berlin_condensed %>% 
  initial_split(prop = 3/4)

berlin_train <- training(berlin_split)
berlin_test <- testing(berlin_split)

# Standard iterative procedure of adding variables and observing model fit to select the best model

train_model1 <- lm(price_log ~ 1, data = berlin_train)

train_model2 <- lm(price_log ~ neighbourhood_group_cleansed + room_type + 
                     minimum_nights + beds + accommodates,
                   data = berlin_train)


train_model3 <- lm(price_log ~ neighbourhood_group_cleansed + room_type +
                     minimum_nights + beds + accommodates + bathrooms,
                   data = berlin_train)

train_model4 <- lm(price_log ~ neighbourhood_group_cleansed + room_type +
                     minimum_nights + accommodates + bathrooms,
                   data = berlin_train)

train_model5 <- lm(price_log ~ neighbourhood_group_cleansed + room_type +
                     minimum_nights + accommodates + bathrooms + property_type,
                   data = berlin_train)

anova(train_model1, train_model2, train_model3,
      train_model4, train_model5)

BIC(train_model1)
BIC(train_model2)
BIC(train_model3)
BIC(train_model4)
BIC(train_model5)

summary(train_model3)

#-------------------------------------------------------------------------------
# Using tidymodels to run model 3

lm_specification <- linear_reg() %>% 
  set_engine(engine = "lm")

lm_fit <- lm_specification %>% 
  fit(price_log ~ neighbourhood_group_cleansed + room_type +
        minimum_nights + beds + accommodates + bathrooms  , data = berlin_train)

# Model evaluation 

traning_results <- lm_fit %>% 
  predict(new_data = berlin_train) %>% 
  mutate(true_values = berlin_train$price_log)

test_results <- lm_fit %>% 
  predict(new_data = berlin_test) %>% 
  mutate(true_values = berlin_test$price_log)

# Calculate root mean square error between testing and training data

traning_results %>% 
  rmse(truth = true_values, estimate = .pred)

test_results %>% 
  rmse(truth = true_values, estimate = .pred)

# Calculate Mean Absolute Percentage Error for the linear regression model

traning_results %>% 
  mape(truth = true_values, estimate = .pred)

test_results %>% 
  mape(truth = true_values, estimate = .pred)
