# this will calculated the standard deviation based on sliding window.
# Also, it will return the annulized standard deviation based on market live day out of 365 days in a year
stdevwind = function(returns)
{
  st_dev_df = c()
  for (i in 253:dim(returns)[1])
  {
    wind = window(returns, start = time(returns)[i-251], end = time(returns)[i])
    stdv = StdDev.annualized(wind, scale  = 252)
    st_dev_df = rbind(st_dev_df,stdv)
    
  }
  st_dev_df = xts(st_dev_df, order.by = time(returns)[253:length(time(returns))])
  
  return(st_dev_df)
}

# this function will load a large csv file and will cast it as XTS data class after save this as R data format
# this will allow faster loading and calculation
# XTS  is special time series package which handles and allow users to compute based on time frame windows.
# If user want to compute the return between date A and B, use do window(prices, start = A, end = B)
# xts class data is working for this method.
# dataframe, is not working since data frame sees a date as a character
prices_to_rdata  = function(filename)
{
  prices = read.csv(filename)
  dates = prices[,1]
  dates = as.Date(dates, format = '%m/%d/%Y')
  prices = xts(prices[,-1], order.by = dates)
  save(prices, file = 'prices.Rdata')
}

# when comparing two price series of stocks for a given window, we need to rebase them otherwise, imagine stock 1 has a price of 100
# and stock 2 a price of 1000, you cannot put on same chart, and imagine stock price grows to 150 (+50%) while stock 2 grows to 1050 (+5%)
# we can visualise than only if you rebase to same scale
rebase = function(vector, scale)
{
  f1 = vector[1]
  print(length(vector))
  print(f1)
  for (y in 1:length(vector))
  {
    vector[y] = scale* vector[y]/as.numeric(f1)	
  }
  return(vector)
}

# this create the new environment to store the result
sp500 <- new.env()

# this function get the <<single>> stock information from source, default true for getting close price
get_single_stock = function(start,end,ticker_name, source_name)
{
  getSymbols(ticker_name, env = sp500, src = source_name, from = start, to = end)
  return(sp500[[ticker_name]][,4])
}

# this function will get the each single stock corresponding to the ticker name
getyahooprice = function(tickername, start, end)
{
  start <- start
  end <- end
  ticker_name = tickername
  source_name = 'yahoo'
  a = get_single_stock(start,end,ticker_name, source_name) # if we run this function, ticker_name will be saved
  return(a)
}

# this function will stacked each stock(what User decided to wrangle together) as a columnwise
get_multiple_stock = function(list_stocks,start, end)
{
  df_output = c()
  
  for(i in 1:length(list_stocks))
  {
    df_output = cbind(df_output, getyahooprice(list_stocks[i],start,end))
  }
  return(df_output)
}


execute_roll_linear = function(a, window_size){
  # this is the function for rollapplyr's FUN
  pred_fun = function(x) {
    r <- lm( x ~ index(x) )
    tail(predict(r, interval="prediction"),1)
  } 
  # Rolling regression (unweighted), with prediction intervals
  rolled_input <- rollapplyr( 
    as.zoo(Cl(a)), 
    width=window_size, by.column = FALSE, 
    FUN = pred_fun
  )
  return(rolled_input)
}

# below line will compute the all necessary tool to draw the rolling sliding window based linear regression
# it will draw the line for each corresponding purpose, as commented 
draw_comparison_result_plot = function(input,rolled_input){
  # rolled_input = execute_roll_linear(input,window_size)
  plot(index(input), input, type="l", lwd=2, col = 'black', las=1 ) # real plot
  lines(index(rolled_input), rolled_input$fit, col="orange", lwd=5 ) # final prediction line
  lines(index(rolled_input), rolled_input$lwr, col="purple", lwd=3, lty=3 )
  lines(index(rolled_input), rolled_input$upr, col="purple", lwd=3, lty=3 )
  abline(lm(Cl(input) ~ index(input)), col="red", lwd=3 )  # this is the line result from linear regression
  legend("topleft", legend = c('Return: Black', 'Prediction: Orange','Lower bound: Purple', 'Upper bound: Purple', 'Reg line: Red') )
}

# this will give rmse calculated score
# this automatically handles the different window size of input data
get_rmse = function(pred, original){
  original = window(original, start = start(pred))
  return(sqrt(mean( (pred - original)^2 ) ) )
}

# this will give MAE calculated score
# this automatically handles the different window size of input data
get_mae = function( pred, original){
  original = window(original, start = start(pred))
  return(sqrt(mean( abs(pred - original)) ) )
}

# this gives the summary analyze of given bunch of stocks
give_summary = function(stock_bunch)
{
  temp_pca = prcomp(stock_bunch, center = TRUE ,scale. = TRUE)
  summary(temp_pca)  
}

# this gives the variance analysis 
draw_var_explanation = function(stock_bunch){
  temp_pca = prcomp(stock_bunch, center = TRUE ,scale. = TRUE)
  res = fviz_eig(temp_pca)
  return(res)
}

# below will draw the variance comparison plot to give an idea of which stock is more important to explain market of SnP
draw_pca_var_plot = function(stock_bunch)
{
  temp_pca = prcomp(stock_bunch, center = TRUE ,scale. = TRUE)
  
  fviz_pca_ind(temp_pca,
               col.ind = "cos2", # Color by the quality of representation
               gradient.cols = c("#00AFBC", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )
  
}

## this function is to transform a PCA type output into a regular data frame to it can be used in Shiny as a DT output
pca_importance <- function(x) {
  vars <- x$sdev^2
  vars <- vars/sum(vars)
  rbind(`Standard deviation` = x$sdev, `Proportion of Variance` = vars, 
        `Cumulative Proportion` = cumsum(vars))
  
}

draw_cluster_analysis = function(df_stocks, num_center, num_start){
  df_scaled_stocks = scale(df_stocks)
  km_model = kmeans(df_scaled_stocks,centers = num_center, nstart = num_start)
  centers = data.frame(cluster = factor(1:num_center), km_model$centers)
  centers = as.data.frame(t(centers))
  names(centers) = paste("Cluster",1:num_center)
  centers$Symbol = row.names(centers)
  centers = gather(centers, "Cluster", "Mean", -Symbol)
  centers$Coloar = centers$Mean > 0
  ggplot(centers, aes(x=Symbol, y=Mean, fill = 'red')) + geom_bar(stat = 'identity', position = 'identity', width = 0.80)+
    facet_grid(Cluster ~., scales = 'free_y')
}

# by comparing the plot result from this function, 
# User can analysis the stock markets regime. For instance, for each plot shows how stock movesment behaves differently.
# Could see the momentum of effectiveness in the market
# means of the variables in each cluster. 

# below function is another way to show the similiarities between each designated stocks User choose
# risk parity model can be combined with this. 
# this could be another indicator of similarities of stock movement in the market at given time period
give_cluster_dendogram = function(df_stocks){
  df = t(df_stocks)
  d = dist(df)
  hcl = hclust(d)
  plot(hcl)
}
