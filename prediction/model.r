library(data.table)
library(prophet)
source('etl.r')

# Load data for model
data_model <- load_data_for_model()
store_nbr =1
# Function to fit the model for transactions for a single store
fit_and_predict_transactions <- function(store_nbr, data_model) {
  store_data <- data_model$get_store(store_nbr)
  
  store_transactions <- store_data$store_transactions[, .(ds = date, y = transactions)]
  store_holidays <- store_data$store_holidays
  store_holidays <- store_holidays[, .(ds, holiday, lower_window, upper_window)]
  store_holidays <- na.omit(store_holidays)

  
  # Fit Prophet model for transactions
  m <- prophet(
    yearly.seasonality = TRUE,
    weekly.seasonality = TRUE,
    holidays = store_holidays
  )

  
  # Fit the model
  m <- fit.prophet(m, store_transactions)
  
  # Create future dataframe and predict
  future <- make_future_dataframe(m, periods = 365)

  forecast <- predict(m, future)
  
  return(list(model = m, forecast = forecast, store_nbr = store_nbr))
}

# Function to fit the model for sales categories for a single store
fit_and_predict_sales <- function(store_nbr,  data_model) {
  predicted_transactions <- fit_and_predict_transactions(store_nbr ,data_model)
  store_data <- data_model$get_store(store_nbr)
  
  # Extract category sales data
  store_sales <- data_model$data_train[store_nbr == store_nbr]
  
  # Merge predicted transactions with sales data
  store_sales <- merge(store_sales, predicted_transactions, by = 'date', all.x = TRUE)
  
  # Fit Prophet model for each category
  categories <- unique(store_sales$family)
  category_models <- lapply(categories, function(category) {
    category_data <- store_sales[family == category, .(ds = date, y = sales, transactions_predicted = yhat)]
    
    # Fit Prophet model
    m <- prophet(
      yearly.seasonality = TRUE,
      weekly.seasonality = TRUE,
      holidays = store_data$store_holidays
    )
    
    m <- add_regressor(m,'oil_price')
    m <- add_regressor(m,'type')
    m <- add_regressor(m, 'transactions_predicted')
    
    # Fit the model
    m <- fit.prophet(m, category_data)
    
    # Create future dataframe and predict
    future <- make_future_dataframe(m, periods = 365)
    future <- merge(future, predicted_transactions[, .(ds, yhat)], by = 'ds', all.x = TRUE)
    
    forecast <- predict(m, future)
    
    return(list(category = category, model = m, forecast = forecast))
  })
  
  return(category_models)
}

# Iterate over all stores and fit the models
stores <- unique(data_model$data_meta$store_nbr)


sales_forecasts <- lapply(unique(data_model$data_meta$store_nbr), function(store_nbr) {
  fit_and_predict_sales(store_nbr, data_model)
})


# Combine all sales forecasts
combined_sales_forecasts <- rbindlist(lapply(sales_forecasts, function(store_forecast) {
  rbindlist(lapply(store_forecast, function(forecast) {
    forecast$forecast[, .(ds, yhat, category = forecast$category, store_nbr = forecast$model$store_nbr)]
  }), fill = TRUE)
}), fill = TRUE)

# Plot the forecast for a specific store and category (example: store 1, category 'FOOD')
store_nbr_example <- 1
category_example <- 'FOOD'
example_forecast <- sales_forecasts[[which(stores == store_nbr_example)]]
category_forecast <- example_forecast[[which(sapply(example_forecast, function(x) x$category) == category_example)]]
plot(category_forecast$model, category_forecast$forecast) +
  add_changepoints_to_plot(category_forecast$model) +
  labs(title = paste("Forecasted Sales Volume for Store", store_nbr_example, "and Category", category_example),
       x = "Date",
       y = "Predicted Sales Volume")

# Load test data and make predictions for each store and category
test_data <- fread('../store-sales-time-series-forecasting/test.csv')

# Merge test data with additional regressors
test_data_full <- merge(test_data, data_model$data_oil, by = 'date', all.x = TRUE)
test_data_full <- merge(test_data_full, data_model$data_meta, by = 'store_nbr', all.x = TRUE)

# Predict test data for each store and category
test_forecasts <- rbindlist(lapply(stores, function(store_nbr) {
  store_test_data <- test_data_full[store_nbr == store_nbr]
  store_forecasts <- sales_forecasts[[which(stores == store_nbr)]]
  
  rbindlist(lapply(store_forecasts, function(category_forecast) {
    category_test_data <- store_test_data[, .(ds = date)]
    category_test_data <- merge(category_test_data, predicted_transactions, by = 'ds', all.x = TRUE)
    predict(category_forecast$model, category_test_data)[, .(id, sales = yhat, category = category_forecast$category, store_nbr = store_nbr)]
  }), fill = TRUE)
}), fill = TRUE)

# Prepare output
output <- test_forecasts[, .(id, sales)]
fwrite(output, '../submission.csv')
