## this function takes a list of dates and selects in that list only dates which are end of month..!
find_eom = function(dayz)
{
  
  days_end = c()
  for(x in 1:(length(dayz)-1))
  {
    ## check if you add one more day to current day, month changes
    if(month(dayz[x+1])!=month(dayz[x]))
    {
      days_end = c(days_end, as.character(dayz[x]))
    }
    
  }
  days_end = as.Date(days_end,origin = "1900-01-01")
  return(days_end)
}


## function to select quarterly data
select_freq = function(days_end,freq)
{
  freq = as.numeric(freq)
  ## select dates for which the month is a multiple of 3
  return(days_end[  which((month(days_end)%%freq)==0)])
  
}


## functions related to momentum calculation
## for each quarter we compute a momentum value
momentum_3m = function(dataframe, dates_review,freq)
{
  freq = as.numeric(freq)
  output_m = c()
  mins = 1
  for(x in length(dates_review):2)
  {
    print(dates_review[x])
    prices_x = dataframe[dates_review[x],]
    prices_x_3 = dataframe[dates_review[x-mins],]
    res = as.data.frame(t(as.numeric(prices_x)/as.numeric(prices_x_3)))
    res = xts(res,order.by = time(prices_x))
    colnames(res) = colnames(prices_x)
    output_m = rbind(output_m,res)
  }
  return(output_m)
}

## here we input in the function a datatable with the momentum
## for each date or quarter, we sort the data and select top 
determine_portfolio = function(output_variable, num_companies)
{
  
  portfolio = c()
  for(j in 1:nrow(output_variable))
  {
    print(j)
    seli = sort(as.data.frame(output_variable[j,]),decreasing = T)[1:num_companies]
    portfolio = rbind(portfolio,names(seli))
  }
  portfolio = xts(portfolio, order.by =time(output_variable) )
  return(portfolio)
}


## code to calculate returuns bases on a portfolio determined via the function determine_portfolio
calculate_historical_track = function( dataframe,portfolio, weighting)
{
  ## weithging = equal means that you invest the same proportion in each company on each quarter
  if(weighting =='equal')
  {
    print('equal')
    ## we create a matrix of weight where each company receives 1/total number of stocks
    ## if 20 stocks, each gets 1/20 or 5%
    ## if 10 stocks, each gets 10%
    weights = matrix(1/ncol(portfolio), nrow = nrow(portfolio), ncol = ncol(portfolio))
    
    tt  = time(portfolio)
    prices_kk = c()
    dates_kk = c()
    all_kk = c()
    
    ##here for each quarter, we compute the return of that quarter
    for( k in 1:nrow(portfolio))
    {
      print(k)
      if(k<nrow(portfolio))
      {
        ## we find out 
        ## suppose k = 1 and date is 2016-06-30
        ## so prices_K window would be the price window from that day up to the next quarter
        prices_k = window(dataframe, start = tt[k], end = tt[k+1])
        prices_k = prices_k[,portfolio[k,]]
        ## we have now a matrix with the returns of each company during that quarter
        returns_k = CalculateReturns(prices_k)
        
        ## we compute the return of the total portfolio by multiplying the weights by returns
        for(w in 2:nrow(returns_k))
        {
          ## here we multiply returns by weights to obtain the aggregated return of the portfolio on day w
          prices_kk = c(prices_kk,sum(returns_k[w,]*weights[k,]))
          dates_kk = c(dates_kk,as.character(time(returns_k[w,])))
          all_kk = cbind(dates_kk,prices_kk)
        }
      }else
      {
        prices_k = window(dataframe, start = tt[k], end = time(dataframe)[length(time(dataframe))])
        prices_k = prices_k[,portfolio[k,]]
        returns_k = CalculateReturns(prices_k)
        
        for(w in 2:nrow(returns_k))
        {
          
          prices_kk = c(prices_kk,sum(returns_k[w,]*weights[k,]))
          dates_kk = c(dates_kk,as.character(time(returns_k[w,])))
          all_kk = cbind(dates_kk,prices_kk)
        }
      }
      
    }
    resuls = c(100)
    for( u in 2:nrow(all_kk))
    {
      resuls = c(resuls, resuls[u-1]*(1+as.numeric(all_kk[u,2])))
    }
    
    all_kk = cbind(all_kk,resuls)
  }
  
  ## weithging = size means that you invest more in companies which have higher weight
  ## weight is usually determined by the market capitalization
  if(weighting =='size')
  {
    weights = matrix(0, nrow = nrow(portfolio), ncol = ncol(portfolio))
    print('size')
    for(g in 1:nrow(weights))
    {
      gg= portfolio[g,]
      gg = gsub('.Close','',gg)
      wg = nasdaq[match(gg,nasdaq$Symbol),3]
      wg = wg/sum(wg)
      
      weights[g,] = wg
    }
    
    
    tt  = time(portfolio)
    prices_kk = c()
    dates_kk = c()
    all_kk = c()
    for( k in 1:nrow(portfolio))
    {
      if(k<nrow(portfolio))
      {
        prices_k = window(dataframe, start = tt[k], end = tt[k+1])
        prices_k = prices_k[,portfolio[k,]]
        returns_k = CalculateReturns(prices_k)
        
        for(w in 2:nrow(returns_k))
        {
          
          prices_kk = c(prices_kk,sum(returns_k[w,]*weights[k,]))
          dates_kk = c(dates_kk,as.character(time(returns_k[w,])))
          all_kk = cbind(dates_kk,prices_kk)
        }
      }else
      {
        prices_k = window(dataframe, start = tt[k], end = time(dataframe)[length(time(dataframe))])
        prices_k = prices_k[,portfolio[k,]]
        returns_k = CalculateReturns(prices_k)
        
        for(w in 2:nrow(returns_k))
        {
          
          prices_kk = c(prices_kk,sum(returns_k[w,]*weights[k,]))
          dates_kk = c(dates_kk,as.character(time(returns_k[w,])))
          all_kk = cbind(dates_kk,prices_kk)
        }
      }
      
    }
    resuls = c(100)
    for( u in 2:nrow(all_kk))
    {
      resuls = c(resuls, resuls[u-1]*(1+as.numeric(all_kk[u,2])))
    }
    
    all_kk = cbind(all_kk,resuls)
  }
  
  return(all_kk)
  
}


# similar to momentum 3m, except we divide by avg price of 3 months ago
# it returns the momentum value based on those. Called Faber.
faber_3m = function(dataframe, dates_review,freq)
{
  
  freq = as.numeric(freq)
  output_m = c()
  mins = 1
  
  for(x in length(dates_review):2)
  {
    print(dates_review[x])
    prices_x = dataframe[dates_review[x],]
    
    prices_x_3 = window(dataframe, start =dates_review[x-mins] , end = dates_review[x])
    
    ## using tibble a function to compute the mean of each column using summarise_all
    prices_x_3 = as.data.frame(prices_x_3) %>% 
      summarise_all(funs(mean(., na.rm = TRUE)))
    
    res = as.data.frame(t(as.numeric(prices_x)/as.numeric(prices_x_3)))
    res = xts(res,order.by = time(prices_x))
    colnames(res) = colnames(prices_x)
    output_m = rbind(output_m,res)
  }
  return(output_m)
}
