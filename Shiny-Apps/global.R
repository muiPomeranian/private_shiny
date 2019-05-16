# Loading universal stuff (packages, connecting to the other main project, DB, etc...)!..!

library(quantmod)
library(TTR)
library(sqldf)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(plotly)
library(PerformanceAnalytics)
library(timeSeries)
library(fBasics)
library(xts)
library(profvis)
library(factoextra)
library(tidyquant)
library(tibble)
library(dplyr)
library(rvest)
library(rlist)
library(stringi)
library(htmltab)


source('functions.R')
source('functions_variables.R')
name_project = "DASHBOARD STATISTICS"

# filename = 'prices_full.csv'
# prices_to_rdata(filename)

## loading prices from existing database
load('data/prices.Rdata')

companies = colnames(prices)[-(1:2)]

# This will use log return based result of stock prices to keep the statioinarity of data(differencing with lag 1)
returns = Return.calculate(prices, method = 'log')

monthly_returns = do.call(cbind, lapply(prices, monthlyReturn))

# it could be the all possilbe choices from the input data .R file
colnames(monthly_returns) = colnames(prices)

load('data/st_dev_df.Rdata')

## loading data for a web based database

# ticker name will be readed 
tickers =read.csv('data/tickers.csv', header = F)
tickers = as.character(tickers[,1])


# # ticker name will be readed 
# nasdaq =read.csv('data/nasdaq.csv', header = T, stringsAsFactors  = F)
# ##https://www.slickcharts.com/nasdaq100

nasdaq = parse_nasdaq('https://www.slickcharts.com/nasdaq100')






