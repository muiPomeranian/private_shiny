## SERVER SIDE

# That's the brain of the UI. 
# Deals with the actual calculations and data manipulation. 


shinyServer(function(input,output, session){
  options(warn=0)
  
  globaldata <- reactiveValues(
    # input tables
    alpha1 = 0,  beta1 = 0, calculating_status = FALSE
  )
  
 output$plot1 <- renderPlotly({
    prices = window(prices, start = input$dateRange[1], end = input$dateRange[2])
    vect1 = rebase(as.numeric(prices[,input$ticker]),100)
    vect2 = rebase(as.numeric(prices$Bench),100)
    # f1 = vect1[1]
    # f2 = vect2[2]
    # for (y in 1:(length(time(prices))))
    # {
    #   vect1[y] = 100* vect1[y]/as.numeric(f1)	
    #   vect2[y] = 100* vect2[y]/as.numeric(f2)	
    # 
    # }
        plot_ly( x = time(prices), y = vect1, type = 'scatter', mode = 'lines', name = 'Company') %>% 
          add_trace(y  = vect2, name = 'Benchmark',mode = 'lines')
 })
 
output$Chart2 = renderPlotly({
  st_dev_df = window(st_dev_df, start = input$dateRange[1], end = input$dateRange[2])
  plot_ly( x = time(st_dev_df), y = as.numeric(st_dev_df[,input$ticker]), type = 'scatter', mode = 'lines', name = 'plot')
})

output$plot3 = renderPlotly({
  # 
  vector1 = window(monthly_returns[,input$ticker], start = input$dateRange[1], end = input$dateRange[2])
  vector2 = window(monthly_returns$Bench, start = input$dateRange[1], end = input$dateRange[2])
  df3 = cbind(vector1,vector2)
  save(df3, file = 'df3.Rdata')
  colnames(df3)[1] = 'Portfolio'
  df3 = as.data.frame(df3)
  
  regr = lm(Portfolio ~ Bench, data = df3 )
  regr = summary(regr)
  globaldata$alpha1 = regr[["coefficients"]][1]
  globaldata$beta1 = regr[["coefficients"]][2]
  
  plot_ly(df3,  x = ~Portfolio, y = ~Bench)
 
})

## By creating a reactive value a, we compute this value only once and use for different outputs
## a will depend in input$ticker2, input$ticker3 and daterange
## Whenever one of these changes. a is recomputed automatically
# a = observe({
#   
#   if(input$ticker3=="")
#   {
#     ticker_select = input$ticker2
#   }else
#   {
#     ticker_select = input$ticker3
#   }
#   print(getyahooprice(ticker_select,input$dateRange2[1],input$dateRange2[2]))
#   as.data.frame(getyahooprice(ticker_select,input$dateRange2[1],input$dateRange2[2]))
#   
# })


output$plot5 = renderPlotly({
  if(input$ticker3=="")
  {
    ticker_select = input$ticker2
  }else
  {
    ticker_select = input$ticker3
  }
  data5 = getyahooprice(ticker_select,input$dateRange2[1],input$dateRange2[2])
 
  data5 = as.data.frame(data5)
   # print(data5)
  # plot(data5, main = input$ticker2)
    plot_ly(x = as.Date(row.names(data5)), y = data5[,1],  type = 'scatter', mode = 'lines', name = paste('Prices of:',ticker_select) )
})




output$plot6 = renderPlotly({
  if(input$ticker3=="")
  {
    ticker_select = input$ticker2
  }else
  {
    ticker_select = input$ticker3
  }

  data5 = getyahooprice(ticker_select,input$dateRange2[1],input$dateRange2[2])
  print(colnames(data5))
  
  # print(data5)
  returns5 = Return.calculate(data5, method = 'log')
  stdev5 = stdevwind(returns5)
  # 
   stdev5 = as.data.frame(stdev5)
  print(stdev5)
  # # plot(data5, main = input$ticker2)
  plot_ly(x = as.Date(row.names(stdev5)), y = stdev5[,1],  type = 'scatter', mode = 'lines', name = paste('Rolling volatility of:',ticker_select) )
})

output$chart10 = renderPlot({

  if(input$ticker3=="")
  {
    ticker_select = input$ticker2
  }else
  {
    ticker_select = input$ticker3
  }
  a = getyahooprice(ticker_select,input$dateRange2[1],input$dateRange2[2])
  
  rolled_input = execute_roll_linear(a,input$windrange)
  
  draw_comparison_result_plot(a, rolled_input)

  output$Table2 = DT::renderDataTable({
    df2 = matrix(data = NA,nrow=2,ncol = 2)
    colnames(df2) = c('Score Metric',"Value")
    df2[1,1] = 'RMSE'
    df2[2,1] = 'MAE'
    df2[1,2] = round(get_rmse(rolled_input$fit, a),3)
    df2[2,2] = round(get_mae(rolled_input$fit, a),3)
    df2 = as.data.frame(df2)
    
    DT::datatable(df2,
                  options = list(autoWidth = TRUE, searching = FALSE, dom = 't'
                                 
                  )
                  
    )
    
  })
  
  
})


output$plotPCA = renderPlot({

  if(input$areainput!="")
  {
    list_stocks = input$areainput
    list_stocks = unlist(strsplit(list_stocks, ","))
    print(list_stocks)
    dataPCA = get_multiple_stock(list_stocks, input$dateRange2[1],input$dateRange2[2])
    draw_pca_var_plot(dataPCA)
    

    

    
  }

})

output$plotVAR = renderPlot({
  
  if(input$areainput!="")
  {
    list_stocks = input$areainput
    list_stocks = unlist(strsplit(list_stocks, ","))
    print(list_stocks)
    dataPCA = get_multiple_stock(list_stocks, input$dateRange2[1],input$dateRange2[2])
    draw_var_explanation(dataPCA)
  }
  
})

output$dfImportance = DT::renderDataTable({
  
  list_stocks = input$areainput
  list_stocks = unlist(strsplit(list_stocks, ","))
  print(list_stocks)
  dataPCA = get_multiple_stock(list_stocks, input$dateRange2[1],input$dateRange2[2])
  give_summary = as.data.frame(pca_importance(give_summary(dataPCA)))
  
  colz = c()
  ## we loop through the number of tickers in the dataframe and create a vector with PCA & number, for the amount of tickers included
  for(i in 1:length(colnames(give_summary)))
  {
    colz = c(colz,paste("PCA",i))
  }
  colnames(give_summary) = colz
  give_summary = round(give_summary,2)
  DT::datatable(give_summary
                # options = list(autoWidth = TRUE, searching = FALSE, dom = 't'
                #                
                #                # columnDefs = list(
                #                #   list(className = 'dt-center', targets = 0:1),
                #                #   list(width = '100', targets = 0:1)
                #                #   # list(width = '80', targets = 1:5)
                #                # )
                               
                #)
                
  )
})


# output$calc_status = renderText ({
#   if(globaldata$calculating_status== FALSE)
#   {
#     paste("Calculating")
#   }else
#     'hello'
# 
# })



output$windowframe = renderText ({
  paste("Period: from ",input$dateRange[1]," to ",input$dateRange[2], sep='')
})

output$Table1 = DT::renderDataTable({
  
  returns_bench = window(returns$Bench, start = input$dateRange[1], end = input$dateRange[2])
  ret_benchmark = paste(round(Return.annualized(returns_bench, scale = 252, geometric = TRUE), 4) *100, "%", sep='')
  
  returns_ticker = window(returns[,input$ticker], start = input$dateRange[1], end = input$dateRange[2])
  ret_ticker =   paste(round(Return.annualized(returns_ticker, scale = 252, geometric = TRUE), 4) *100, "%", sep='')

  df_results = rbind(ret_benchmark,ret_ticker)
  rownames(df_results) = c('Benchmark', input$ticker)
  print(df_results)
   colnames(df_results) = c('Annualized Return')
   df_results = cbind(df_results, c("",round(globaldata$alpha1,4)),c("",round(globaldata$beta1,4)))
   colnames(df_results)[2] = 'Alpha vs Benchmark'
   colnames(df_results)[3] = 'Beta vs Benchmark'
   
      DT::datatable(df_results,
                    options = list(autoWidth = FALSE, searching = FALSE, dom = 't'

                                   # columnDefs = list(
                                   #   list(className = 'dt-center', targets = 0:1),
                                   #   list(width = '100', targets = 0:1)
                                   #   # list(width = '80', targets = 1:5)
                                   # )

                    )

      )

    })



      })

  

