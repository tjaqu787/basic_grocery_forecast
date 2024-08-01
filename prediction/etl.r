library(data.table)
library(zoo)

national_holidays <- function() {
  holidays <- fread('../store-sales-time-series-forecasting/holidays_events.csv')
  national_df <- holidays[locale == 'National']
  
  # Filter football '%futbol%'
  football_df <- national_df[grep("futbol", description, ignore.case = TRUE)]
  football_df[, `:=`(holiday = "Football", lower_window = -2, upper_window = 1)]
  
  # Filter carnival 'Carnaval'
  carnival_df <- national_df[grep("Carnaval", description, ignore.case = TRUE)]
  carnival_df[, `:=`(holiday = "Carnaval", lower_window = -2, upper_window = 1)]
  
  # Filter Christmas 'Navidad'
  christmas_df <- national_df[description == 'Navidad']
  christmas_df[, `:=`(holiday = "Navidad", lower_window = -3, upper_window = 1)]
  
  # Filter Black Friday and Cyber Monday into shopping 'Black Friday' and 'Cyber Monday'
  shopping_df <- national_df[grep("Black Friday|Cyber Monday", description, ignore.case = TRUE)]
  shopping_df[, `:=`(holiday = "Shopping", lower_window = 0, upper_window = 0)]
  
  # Filter for the earthquake and give it a 2 week tail 'Terremoto Manabi'
  earthquake_df <- national_df[description == 'Terremoto Manabi']
  earthquake_df[, `:=`(holiday = "Earthquake", lower_window = 0, upper_window = 14)]
  
  # Filter everything else
  filtered_descriptions <- c(football_df$description, 
                             carnival_df$description, 
                             christmas_df$description, 
                             shopping_df$description, 
                             earthquake_df$description)
  
  other_events_df <- national_df[!description %in% filtered_descriptions]
  other_events_df[, `:=`(holiday = "Other", lower_window = -1, upper_window = 0)]
  
  # Handle 'Recupero' days
  recupero_df <- national_df[grep("Recupero", description, ignore.case = TRUE)]
  recupero_df[, `:=`(holiday = "Recupero", lower_window = 0, upper_window = 0)]
  
  # Combine all filtered dataframes and remove unneeded
  national_output <- rbindlist(list(football_df, carnival_df, christmas_df, shopping_df, earthquake_df, other_events_df, recupero_df), fill = TRUE)
  national_output <- national_output[transferred == FALSE]
  national_output <- national_output[, .(holiday, ds= date, lower_window, upper_window)]
  
  return(national_output)
}


store_specific_holidays <- function() {
  regional_holidays <- function(regional_df) {
    # Set description as "Other_Local"
    regional_df[, `:=`(holiday = "Other_Local", lower_window = -1, upper_window = 0)]
    return(regional_df[, .(date, holiday, locale_name, lower_window, upper_window)])
  }
  
  local_holidays <- function(local_df) {
    # Remove duplicates
    local_df <- local_df[transferred == FALSE]
    
    # Set description as "Other_Local"
    local_df[, `:=`(holiday = "Other_Local", lower_window = -1, upper_window = 1)]
    
    return(local_df[, .(date, holiday, locale_name, lower_window, upper_window)])
  }
  
  # Load holidays data
  holidays <- fread('../store-sales-time-series-forecasting/holidays_events.csv')
  holidays <- holidays[locale != 'National']
  regional_holidays_df <- regional_holidays(holidays[locale == 'Regional'])
  local_holidays_df <- local_holidays(holidays[locale == 'Local'])
  
  # Load stores data
  stores <- fread('../store-sales-time-series-forecasting/stores.csv')
  
  # Merge regional holidays with stores on state
  store_holidays_regional <- merge(stores, regional_holidays_df, by.x = "state", by.y = "locale_name", all.x = TRUE, all.y = TRUE, allow.cartesian = TRUE)
  store_holidays_regional <- store_holidays_regional[, .(date, holiday, store_nbr, lower_window, upper_window)]
  
  # Merge local holidays with stores on city
  store_holidays_local <- merge(stores, local_holidays_df, by.x = "city", by.y = "locale_name", all.x = TRUE, all.y = TRUE, allow.cartesian = TRUE)
  store_holidays_local <- store_holidays_local[, .(date, holiday, store_nbr, lower_window, upper_window)]
  
  # Combine results
  specific_store_holidays <- rbindlist(list(store_holidays_local, store_holidays_regional), fill = TRUE)
  
  return(specific_store_holidays)
}

# Function to load oil data
load_oil <- function() {
  oil_price <- fread('../store-sales-time-series-forecasting/oil.csv')
  # Rename the column dcoilwtico to price
  setnames(oil_price, 'dcoilwtico', 'oil_price')
  # Forward fill NA values
  oil_price[, oil_price := zoo::na.locf(oil_price, na.rm = FALSE)]
  oil_price[, oil_price := zoo::na.locf(oil_price, na.rm = FALSE, fromLast = TRUE)]
  return(oil_price)
}

# Function to load training data
load_training_data <- function() {
  train_data <- fread('../store-sales-time-series-forecasting/train.csv')
  train_data[, id := NULL] # Ignore id column
  train_data[, date := as.POSIXct(date)] # Convert date to datetime
  train_data[, `:=`(store_nbr = as.integer(store_nbr), family = as.factor(family), sales = as.numeric(sales), onpromotion = as.integer(onpromotion))]
  return(train_data)
}

# Function to load store metadata
load_store_meta_data <- function() {
  store_meta <- fread('../store-sales-time-series-forecasting/stores.csv')
  store_meta[, store_nbr := as.integer(store_nbr)]
  return(store_meta)
}

# Function to load transactions data
load_transactions <- function() {
  transactions <- fread('../store-sales-time-series-forecasting/transactions.csv')
  transactions[, store_nbr := as.integer(store_nbr)]
  return(transactions)
}

# Class to load data for the model
load_data_for_model <- function() {
  data_oil <- load_oil()
  data_train <- load_training_data()
  data_meta <- load_store_meta_data()
  data_transactions <- load_transactions()
  data_national_holidays <- national_holidays()
  data_local_holidays <- store_specific_holidays()
  
  get_store <- function(store_number) {
    
    store_number <- as.integer(store_number)
    store_train <- data_train[store_nbr == store_number]
    store_meta <- data_meta[store_nbr == store_number]
    store_transactions <- data_transactions[store_nbr == store_number]
    data_holidays <- data_local_holidays[store_nbr == store_number]
    data_holidays <- data_holidays[, .(date, holiday, lower_window, upper_window)]
    data_holidays <- rbindlist(list(data_national_holidays, data_holidays), fill = TRUE)
    data_holidays <- data_holidays[, .(date, holiday, lower_window, upper_window)]
    data_holidays <- na.omit(data_holidays)
    
    return(list(store_train = store_train, store_meta = store_meta, store_transactions = store_transactions, store_holidays = data_holidays))
  }
  
  return(list(data_oil = data_oil, data_train = data_train, data_meta = data_meta, data_transactions = data_transactions, data_holidays = data_local_holidays, get_store = get_store))
}
