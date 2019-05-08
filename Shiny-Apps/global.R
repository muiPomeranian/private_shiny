# Loading universal stuff (packages, connecting to the other main project, DB, etc...)!,

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
library(TTR)
library(fBasics)
library(xts)
library(profvis)
library(quantmod)
library(factoextra)
library(tidyquant)

source('functions.R')

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
