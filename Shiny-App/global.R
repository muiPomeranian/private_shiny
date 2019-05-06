
# Loading universal stuff (packages, connecting to the other main project, DB, etc.)

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
library("factoextra")

source('functions.R')

name_project = "DASHBOARD STATISTICS"

# filename = 'prices_full.csv'
# prices_to_rdata(filename)

## loading prices from existing database
load('data/prices.Rdata')

companies = colnames(prices)[-(1:2)]

returns = Return.calculate(prices, method = 'log')
monthly_returns = do.call(cbind, lapply(prices, monthlyReturn))
colnames(monthly_returns) = colnames(prices)

load('data/st_dev_df.Rdata')


## loading data for a web based database

tickers =read.csv('data/tickers.csv', header = F)
tickers = as.character(tickers[,1])

