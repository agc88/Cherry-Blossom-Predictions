library(tidyverse)
library(caret)
library(Metric)
library(rnoaa)

#loads historical cherry blossom data from DC, Liestal, and Kyoto
#binds the data from all three cities into one dataset
cherry <- read.csv('washingtondc.csv') %>% 
  bind_rows(read.csv('liestal.csv')) %>% 
  bind_rows(read.csv('kyoto.csv'))

#subsets the data into only observations from 1950, for ease of matching with a
#later dataset
cherry <- subset(cherry, year >= 1950)
# Fits a simple least-squares regression model to all three sites.
ls_fit <- lm(bloom_doy ~ location * year, data = cherry, subset = year >= 1950)
  #Suggests increasingly earlier peak bloom dates at all three sites

# Uses the obtained models to predict the bloom dates at all three sites, including
# future bloom dates
predictions <- expand_grid(location = unique(cherry$location),
                           year = 1950:2032) %>% 
bind_cols(predicted_doy = predict(ls_fit, newdata = .))

subpred <- subset(predictions, year <= 2022)
mae(cherry$bloom_doy, subpred$predicted_doy) 
#The MAE is 5.346

#plot displaying the predictions
cherry %>% 
  right_join(predictions, by = c('year', 'location')) %>%
  filter(year >= 1950) %>% 
  ggplot(aes(x = year, y = predicted_doy)) +
  geom_line(aes(color = year > 2022), size = 1) +
  geom_point(aes(y = bloom_doy)) +
  scale_color_manual(values = c('FALSE' = 'gray50', 'TRUE' = 'blue'),
                     guide = 'none') +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")

# Fits a simple least-square regression model averaging all three prior sites, in 
# order to extrapolate for Vancouver.
ls_fit_for_van <- lm(bloom_doy ~ year, data = cherry, subset = year >= 1950)

predictions_vancouver <- tibble(location = 'vancouver',
                                year = 2023:2032) %>% 
  bind_cols(predicted_doy = round(predict(ls_fit_for_van, newdata = .)))

#appends the predictions dataset to contain Vancouver
predictions <- bind_rows(predictions, predictions_vancouver)

#peak bloom date predictions for the next 10 years

ten_yr_predictions <- predictions %>% 
  filter(year > 2022) %>% 
  mutate(predicted_doy = round(predicted_doy)) %>% 
  pivot_wider(names_from = 'location', values_from = 'predicted_doy') %>% 
  select(year, kyoto, liestal, washingtondc, vancouver)

ten_yr_predictions
#DC is predicted for March 29, which still falls within +- 5.346 days of the
#date range of the DC Cherry Blossom Festival, but let's see if we can decrease
#the MAE further.

#We will use historic weather data to improve our prediction accuracy, and also
#predict weather for the next ten years for the same purpose
stations <- ghcnd_stations()
get_temperature <- function (stationid) {
  ghcnd_search(stationid = stationid, var = c("tmax"), 
               date_min = "1950-01-01", date_max = "2022-01-31")[[1]] %>%
    mutate(year = as.integer(format(date, "%Y")),
           month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
           season = cut(month, breaks = c(0, 2, 5, 8, 11),
                        include.lowest = TRUE,
                        labels = c("Winter", "Spring", "Summer", "Fall")),
           year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, season) %>%
    summarize(tmax_avg = mean(tmax, na.rm = TRUE))
}

#consolidates hostoric temperatures in a data frame
historic_temperatures <-
  tibble(location = "washingtondc", get_temperature("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_temperature("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_temperature("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_temperature("CA001108395")))

#graphical representation
historic_temperatures %>%
  ggplot() + 
  aes(year, tmax_avg) + 
  geom_line() +
  xlim(1950, 2032) +
  labs(x = "Year", y = "Average maximum temperature (1/10 °C)") +
  facet_grid(factor(season) ~ str_to_title(location))

#extrapolates the average seasonal temperature for each year
ls_fit_temperature <- lm(tmax_avg ~ year * season + location, 
                         data = historic_temperatures)

#uses this model to predict the respectective temperatures for each location
#for the next 10 years

temperature_predictions <-
  expand_grid(location = c("washingtondc", "liestal", "kyoto", "vancouver" ),
              season = c("Winter", "Spring", "Summer", "Fall"),
              year = 1950:2032) %>%
  bind_cols(predicted_temperature = 
              predict(ls_fit_temperature, newdata = .)) %>%
  filter(season %in% c("Winter", "Spring")) %>%
  pivot_wider(names_from = season, values_from = predicted_temperature)

#predicts future bloom dates from the extrapolated temperatures
predictions_temperature <-
  temperature_predictions %>%
  left_join(cherry,
            by = c("location", "year")) %>%
  lm(bloom_doy ~ Spring * Winter, data = .) %>%
  predict(newdata = temperature_predictions) %>%
  round() %>%
  bind_cols(predicted_doy_temperature = ., temperature_predictions)

#creates a subset of predictions_temperature removing all entires from
#Vancouver and after 2022, so as to be able to determine the MAE relative
#to the original historical dataset
pred_no_van_2022 <- subset(predictions_temperature, location!='vancouver' &
                             year < 2023)
mae(pred_no_van$predicted_doy_temperature, cherry$bloom_doy)
#The MAE actually increased to 6.241

submit_predictions2 <- predictions_temperature %>% 
  filter(year > 2022) %>% 
  mutate(predicted_doy_temperature = round(predicted_doy_temperature)) %>% 
  pivot_wider(names_from = 'location', values_from = 'predicted_doy_temperature') %>% 
  select(year, kyoto, liestal, washingtondc, vancouver)

print(submit_predictions2, n=40) # For an unknwown reason, it is not displaying properly
#without me having to print 40 rows

#We will repeat this process using repeated K-fold cross-validation in hopes of 
#obtaining a lesser MAE.

set.seed(137)
train_control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 3)
model <- train(bloom_doy ~ year, data = cherry,
                method = "lm", trControl = train_control)
print(model)
#the MAE is 5.863. Now let's try with the previous model adjusted for temperature 
#taken into account. We will use cross-validation to refine the model.

set.seed(137)
model2 <- train(predicted_doy_temperature ~ year + location, 
                data = predictions_temperature, 
                method = "lm", trControl = train_control)
print(model2)
#the MAE has now drastically decreased to 1.889. Now we will use this superior
#model to predict the future bloom dates for the next 10 years.

summary(model2)
#The intercept is 483.397. The parameter for the year is -0.194. For the locations,
#the parameters are +0.482 for Liestal, +5.265 for Vancouver, and -3.145 for DC.
#Kyoto is not displaying for an unknown reason, but on the previous measurement
#with temperature accounted for it came out to roughly +2 more than Vancouver, 
#+4 more than DC, and +6 more than #Liestal. These respective parameters would be
#+ 7.625, +0.146, and +6.482. Since the DC value is different than it was previously 
#relative to the others,  this is not useful for extrapolation, and we will average 
#the other two to obtain a parameter of +7.054 for Kyoko.

#From that model, the following entries are obtained for 2023-2032

prediction_submissions <- "
year kyoto liestal washingtondc vancouver
2023 99 92 88 97
2024 98 92 88 97
2025 98 92 88 96
2026 98 91 88 96
2027 98 91 88 96
2028 98 91 87 96
2029 97 91 87 96
2030 97 90 87 95
2031 97 90 87 95
2032 97 90 87 95 "

write.csv(submission_predictions, file = "cherry_predictions.csv",
          row.names = FALSE)