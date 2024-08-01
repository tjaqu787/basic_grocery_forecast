Store Sales Prediction Project
Overview

This project is designed to predict store sales using time series forecasting with Prophet. The model leverages historical sales data, store transaction data, oil prices, and holiday events to generate accurate sales forecasts for each store. The project aims to help store managers and stakeholders make informed decisions based on projected sales trends.

This is based of the kaggle dataset https://www.kaggle.com/competitions/store-sales-time-series-forecasting

Methodology

The prediction process involves several key steps, each focusing on different aspects of the store's data:

    Data Preparation:
        Holiday events are categorized and prepared to account for potential seasonal effects on sales. The other data was clean enough that filling was all that was neccessary.

    Transactions Forecasting:
        A Prophet model is fitted for each store to predict future transactions (the number of visitors).
        The model includes yearly and weekly seasonality, incorporating holiday effects.

    Sales Forecasting:
        Using the predicted transactions from the previous step, a separate Prophet model is fitted for each sales category within the store.
        The sales model uses the predicted transaction volume as an additional regressor to capture the relationship between store visits and sales.
        Each category's future sales are forecasted, accounting for potential variations due to predicted changes in visitor numbers and other external factors.

    Prediction and Output:
        The models generate forecasts for a specified future period (e.g., one year).
        Predictions are compiled into a comprehensive dataset, providing store-level and category-level sales forecasts.
        The results are output to a CSV file for easy analysis and further use in decision-making processes.

Requirements

    R: Ensure R is installed along with the necessary packages, including data.table and prophet.
    Data: Input datasets are expected to be in CSV format and located in the ../store-sales-time-series-forecasting/ directory:
        holidays_events.csv
        oil.csv
        stores.csv
        train.csv
        transactions.csv

Usage

    Prepare the Environment:
        Install required R packages using install.packages(c("data.table", "prophet", "zoo")).
        Place the required data files in the specified directory.

    Run the Script:
        Execute the R script to load data, train the models, and generate predictions.
        The script outputs a CSV file containing the sales forecasts.

    Analyze Results:
        Use the generated forecasts to make data-driven decisions, such as inventory management, staffing, and marketing strategies.
