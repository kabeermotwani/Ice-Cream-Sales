#laod libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(epitools)

setseed(7)
setwd("/Users/kabeermotwani/Library/Mobile Documents/com~apple~CloudDocs/intro project")

#load data
sales <- read.csv("sales_data.csv")
sales <- sales %>%
  mutate(
    temperature = as.numeric(temperature),
    humidity = as.numeric(humidity),
    windspeed = as.numeric(windspeed),
    month_name = as.factor(month_name),
    weekend = as.factor(weekend),
    bank_holiday = as.factor(bank_holiday),
    school_holidays = as.factor(school_holidays),
    icecream_sales = as.integer(icecream_sales),
    hotdrink_sales = as.integer(hotdrink_sales)
  )

head(sales,5)

#counts days where less than 200 ice cream sales
icecream200count <- sum(sales$icecream_sales<200)
icecream200count

#counts total days of ice cream sales
total_days_icecream_sales <- sales %>%
  summarise(total_days = n())
total_days_icecream_sales

#calculates average days of sales under 200
icecream200 <- mean(sales$icecream_sales <200)
icecream200

################ PART 2A
#calculating the expected days of fewer than 200 ice cream sales with a 95% confidence interval
binom_test_result <- binom.test(sum(sales$icecream_sales < 200), n = nrow(sales), conf.level = 0.95)
prop_icecream_less_200 <- binom_test_result$estimate
ci_icecream_less_200 <- binom_test_result$conf.int

#printing the results
binom_test_result
prop_icecream_less_200
ci_icecream_less_200

##########PART 2B
# calculate the total sales
sales$total_sales <- sales$icecream_sales + sales$hotdrink_sales

#calculating the expected days of fewer than 200 TOTAL sales with a 95% confidence interval
binom_test_result_total <- binom.test(sum(sales$total_sales < 200), n = nrow(sales), conf.level = 0.95)
prop_total_less_200 <- binom_test_result_total$estimate
ci_total_less_200 <- binom_test_result_total$conf.int

#printing the results
binom_test_result_total
prop_total_less_200
ci_total_less_200

###############PART 2C
# the odds ratio for a purchase being an ice cream rather than a hot drink in January and in August and a 95% confidence interval for each.
jan <- sales %>% filter(month_name == "Jan")
aug <- sales %>% filter(month_name == "Aug")

# total sales for each category for each month
jan_sales_ice <- sum(jan$icecream_sales)
jan_sales_hot <- sum(jan$hotdrink_sales)
aug_sales_ice <- sum(aug$icecream_sales)
aug_sales_hot <- sum(aug$hotdrink_sales)

# calculating odds for jan and aug
janodds <- jan_sales_ice / jan_sales_hot
augodds <- aug_sales_ice / aug_sales_hot

# calculating odds ratios
janratio <- janodds
augratio <- augodds

# standard error
se_log_jan <- sqrt(1/jan_sales_ice + 1/jan_sales_hot)
se_log_aug <- sqrt(1/aug_sales_ice + 1/aug_sales_hot)

#95% confidence intervals for log odds ratios
ci_lower_log_jan <- log(janratio) - 1.96 * se_log_jan
ci_upper_log_jan <- log(janratio) + 1.96 * se_log_jan
ci_lower_log_aug <- log(augratio) - 1.96 * se_log_aug
ci_upper_log_aug <- log(augratio) + 1.96 * se_log_aug

# converting back to scale
ci_lower_exp_jan <- exp(ci_lower_log_jan)
ci_upper_exp_jan <- exp(ci_upper_log_jan)
ci_lower_exp_aug <- exp(ci_lower_log_aug)
ci_upper_exp_aug <- exp(ci_upper_log_aug)

#results
janratio
augratio
ci_lower_exp_jan
ci_upper_exp_jan
ci_lower_exp_aug
ci_upper_exp_aug




############# PART 3A
weekdays_sales <- sales[sales$weekend == 0, "total_sales"]
weekends_sales <- sales[sales$weekend == 1, "total_sales"]

t_test_result <- t.test(weekdays_sales, weekends_sales)
t_test_result

######PART 3B
# calculate standard deviation for weekdays and weekends sales
sd_weekdays <- sd(weekdays_sales)
sd_weekends <- sd(weekends_sales)
# calculate the sample size for weekdays and weekends
n_weekdays <- sum(sales$weekend == 0)
n_weekends <- sum(sales$weekend == 1)

# calculate the pooled standard deviation
sp <- sqrt(((n_weekdays - 1) * sd_weekdays^2 + (n_weekends - 1) * sd_weekends^2) / (n_weekdays + n_weekends - 2))
sp
# Calculate the effect size
d <- (324 - 600)
d

#power level of our data
power.t.test(n = 51.5,
             delta = d, # difference in means
             sd = sp, # pooled standard deviation
             sig.level = 0.05, # significance level
             power = NULL,
             type = "two.sample", # type of t-test
             alternative = "two.sided") # type of alternative hypothesis

#########PART 3C
#difference in expected values for 90% power level
power.t.test(n = 51.5,
             delta = -134, # GUESS AND CHECK
             sd = sp, # pooled standard deviation
             sig.level = 0.05, # significance level
             power = NULL,
             type = "two.sample", # type of t-test
             alternative = "two.sided") # type of alternative hypothesis

############PART 3D
#sample size for a 90% power value
power.t.test(n = 13, #GUESS AND CHECK
             delta = d, # difference in means/sp
             sd = sp, # pooled standard deviation
             sig.level = 0.05, # significance level
             power = NULL,
             type = "two.sample", # type of t-test
             alternative = "two.sided") # type of alternative hypothesis

##################PART 4A
model <- lm(icecream_sales ~ temperature + humidity + windspeed + weekend + bank_holiday + school_holidays + month_name, data = sales)
summary(model)

# creating data frame for prediction a
prediction_data_a <- data.frame(
  temperature = 18,
  humidity = 6,
  windspeed = 10,
  weekend = as.factor("0"),
  bank_holiday = as.factor("0"),
  school_holidays = as.factor("0"),
  month_name = factor("May", levels = levels(sales$month_name))
)

# make predictions
predictions_a <- predict(lm_model, newdata = prediction_data_a, interval = "predict")

# show predictions
predictions_a


# creating data frame for prediction b
prediction_data_b <- data.frame(
  temperature = 28,
  humidity = 35,
  windspeed = 5,
  weekend = as.factor("1"),
  bank_holiday = as.factor("0"),
  school_holidays = as.factor("1"),
  month_name = factor("Apr", levels = levels(sales$month_name))
)

# make predictions
predictions_b <- predict(lm_model, newdata = prediction_data_b, interval = "predict")

# show predictions
predictions_b


# creating data frame for prediction c
prediction_data_c <- data.frame(
  temperature = 12,
  humidity = 90,
  windspeed = 35,
  weekend = as.factor("0"),
  bank_holiday = as.factor("0"),
  school_holidays = as.factor("0"),
  month_name = factor("Sep", levels = levels(sales$month_name))
)

# make predictions
predictions_c <- predict(lm_model, newdata = prediction_data_c, interval = "predict")

# show predictions
predictions_c


# creating data frame for prediction d
prediction_data_d <- data.frame(
  temperature = -2,
  humidity = 75,
  windspeed = 15,
  weekend = as.factor("1"),
  bank_holiday = as.factor("0"),
  school_holidays = as.factor("0"),
  month_name = factor("Jan", levels = levels(sales$month_name))
)

# make predictions
predictions_d <- predict(lm_model, newdata = prediction_data_d, interval = "predict")

# show predictions
head(predictions_d,5)
