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

  

 



### 1. BUTTON: PULLING DATA FROM SQL (can be directy included in STEP2)
# observeEvent(input$loaddata , {
#   
#   output$load_check = renderUI({
#     HTML(paste0("<b>", "<span style=\"color:red\">PLEASE INPUT BENCHMARK</span>","</b>" ))
#   })
#   
#   req(input$Benchmark)
#   
#   output$load_check = renderUI({
#     
#     paste("Selected Strategy is", input$Strategy,"From/To",as.character(input$dateRange[1]),"/",
#           as.character(input$dateRange[2]), "Benchmark is:", input$Benchmark, sep = " ")
#     
#   })
#   
#   #LOAD FROM SQL
#   all_data = new_data_pull (
#     strategy = input$Strategy,
#     sdate = as.Date(input$dateRange[1]),
#     edate = as.Date(input$dateRange[2]),
#     benchmark_choice = input$Benchmark
#   )
#   
#   save(all_data, file = "all_data.rds")
#   
#   ## TEMP MANUAL LOAD
#   #load("all_data.Rdata")
# })
# 
# ### 2. BUTTON: RUNNING THE BACKTEST STRATEGY
# observeEvent(input$run , {
#   
#   load("all_data.rds")
#   #print(all_data)
#   
#   print(input$datescarry)
#   
#   test_dates = FALSE
#   
#   if(input$datescarry == FALSE)
#   {
#     print("Database Carry is FALSE")
#     sdate = as.Date(input$dateRange2[1])
#     edate = as.Date(input$dateRange2[2])
#     
#     if(input$dateRange2[2]<input$dateRange2[1])
#     {
#       test_dates = TRUE
#       print(test_dates)
#       output$dates_check = renderUI({
#         HTML(paste0("<b>", "<span style=\"color:red\">DATES ARE INVERTED</span>","</b>" ))
#       })
#     }
#   }
#   
#   if(input$datescarry == TRUE)
#   {
#     print("Database Carry is TRUE")
#     sdate = as.Date(input$dateRange[1])
#     edate = as.Date(input$dateRange[2])
#     
#     if(input$dateRange[2]<input$dateRange[1])
#     {
#       test_dates = TRUE
#       print(test_dates)
#       output$dates_check = renderUI({
#         HTML(paste0("<b>", "<span style=\"color:red\">DATES ARE INVERTED</span>","</b>" ))
#       })
#       
#       
#     }
#   }
#   
#   # print(sdate)
#   # print(edate)
#   
#   if(test_dates==FALSE)
#   {
#     
#     strategy_name = input$Strategy
#     benchmark_name = input$Benchmark
#     
#     # RUN ENGINE
#     oo = new_engine(strategy =  input$Strategy, all_data = all_data,
#                     sdate = sdate,
#                     edate = edate,
#                     interval = input$Interval,
#                     back.time = c("ytd", "5 y", "10 y","15 y","19 y"),
#                     is.monthly=FALSE, delist=FALSE,mxw = 1, mnw = 0,
#                     stoploss = FALSE,debug=FALSE,monthDD=FALSE,
#                     costs = input$costs,commissions = input$comissions, performance = FALSE,
#                     tranching = input$tranching,
#                     num_periods= input$nump,
#                     per_frequency = input$per_freq,
#                     weight_mech = "reducing"
#     )
#     
#     save(oo, file = "oo.rds")
#     load("oo.rds")
#     
#     ####COMPUTING STATISTICS TABLE
#     pr = perf_table_2(data = oo$Returns$all.returns)
#     
#     #load("pr.Rdata")
#     df = c()
#     #print(head(pr))
#     
#     for (k in 1:5){df = cbind(df,  t(pr[[k]])) }
#     colnames(df) = c("Ann.Ret", "Stdev", "Cumul.Ret", "Sharpe Ratio", "MaxDD")
#     rownames(df) = c(strategy_name, benchmark_name)
#     
#     output$Table1 = DT::renderDataTable({
#       DT::datatable(df,
#                     options = list(autoWidth = TRUE, searching = FALSE, dom = 't',
#                                    
#                                    columnDefs = list(  
#                                      list(className = 'dt-center', targets = 0:5),
#                                      list(width = '200', targets = 0:0),
#                                      list(width = '80', targets = 1:5)
#                                    )
#                                    
#                     )
#                     
#       )
#       
#     })    
#     
#     # ####COMPUTING MONTHLY RETURNS
#     ri = monthlyRets(rets= oo$Returns$all.returns)
#     ### We pick only the relevant strategy and the benchmark
#     ri.str = as.matrix(ri[,ncol(ri)-1]) ### first the strategy
#     colnames(ri.str) = c("Strategy")
#     table.str = table.CalendarReturns(ri.str,digits=1,as.perc=TRUE,geometric=TRUE)
#     df2 = table.str
#     
#     output$Table2 = DT::renderDataTable({
#       print(df2)
#       DT::datatable(df2,
#                     rownames = TRUE,
#                     options = list(pageLength = 50, lengthChange = FALSE,autoWidth = TRUE,escape=FALSE, searching = FALSE, dom = 't'
#                                    ,columnDefs = list(list(className = 'dt-center', targets = 0:dim(df2)[2])
#                                                       # list(width = '150px', targets = 0:0),
#                                                       # list(width = '70px', targets = 1:dim(df2)[2])
#                                                       
#                                    )
#                     )
#       )
#     })
#     
#     ## CHARTS RETURN/VOl/DD
#     
#     gr_data = data_graphs(data = oo$Returns$all.returns)
#     #print(gr_data)
#     
#     ##########
#     nom_return = gr_data[[4]]
#     
#     
#     output$Chart0 <- renderPlotly({
#       plot_ly( x = time(nom_return), y = as.numeric(nom_return[,1]), type = 'scatter', mode = 'lines', name = strategy_name)      %>%
#         add_trace(y = as.numeric(nom_return[,2]), mode = 'lines', name = benchmark_name) %>%
#         layout(legend = list(x = 0.1, y = 0.9), yaxis = list(title = "$" )
#         )
#       
#     })
#     
#     # output$Chart0 <- renderPlotly({
#     #   
#     #  p = plot_ly( x = time(nom_return), y = as.numeric(nom_return[,1]), type = 'scatter', mode = 'lines', name = "strategy_name")  %>%
#     #    layout(legend = list(x = 0.1, y = 0.9))
#     #    
#     #   p =     add_trace(p, y = as.numeric(nom_return[,2]), mode = 'lines', name = "benchmark_name") 
#     #     p
#     #            # yaxis =  list(range=c(0,round( max(nom_return)))/100, tickformat = "%")
#     #     
#     # })
#     
#     ##########
#     data_returns  = gr_data[[2]]
#     output$Chart1 <- renderPlotly({
#       plot_ly( x = time(data_returns), y = as.numeric(data_returns[,1]), type = 'scatter', mode = 'lines', name = strategy_name)      %>%
#         add_trace(y = as.numeric(data_returns[,2]), mode = 'lines', name = benchmark_name)%>%
#         layout(legend = list(x = 0.1, y = 0.9),
#                yaxis =  list(title = "%" , tickformat = "%")
#         ) 
#     })
#     
#     ##########
#     data_vol  = gr_data[[1]]
#     output$Chart2 <- renderPlotly({
#       plot_ly( x = time(data_vol), y = as.numeric(data_vol[,1]), type = 'scatter', mode = 'lines', name = strategy_name)      %>%
#         add_trace(y = as.numeric(data_vol[,2]), mode = 'lines', name =benchmark_name)%>%
#         layout(legend = list(x = 0.1, y = 0.9),
#                yaxis =  list(title = "St.Dev%" , tickformat = "%"))
#     })
#     
#     ##########
#     data_dd = gr_data[[3]]
#     output$Chart3 <- renderPlotly({
#       plot_ly( x = as.Date(as.character(time(data_dd))), y = as.numeric(data_dd[,1]), type = 'scatter', mode = 'lines', name = strategy_name)      %>%
#         add_trace(y = as.numeric(data_dd[,2]), mode = 'lines', name = benchmark_name) %>%
#         layout(legend = list(x = 0.1, y = 0.2),
#                yaxis =  list(title = "DD, %" ,tickformat = "%"))
#     })
#     
#     ##########
#     logcum = gr_data[[5]]
#     output$Chart5 <- renderPlotly({
#       plot_ly( x = time(logcum), y = as.numeric(logcum[,1]), type = 'scatter', mode = 'lines', name = strategy_name)      %>%
#         add_trace(y = as.numeric(logcum[,2]), mode = 'lines', name = benchmark_name) %>%
#         layout(legend = list(x = 0.1, y = 0.9),yaxis =  list(title = "Log scale" ) )
#     })
#     
#     load("oo.Rds")
#     
#     ##### WEIGHTS CALC
#     weights = oo$Weights$top
#     # print("should print weights")
#     # print(head(weights))
#     weights = ab.xts.alltogether(weights)
#     names = vector()
#     length(names) = ncol(weights)
#     for(i in 1:ncol(weights))
#     {
#       names1 = oo$all_data$universe_table[,1:2]
#       names2 = subset(names1,names1[,1] == as.numeric(colnames(weights[,i])))
#       names[i] = as.character(names2[,2])
#     }
#     colnames(weights) = NULL
#     colnames(weights) = names
#     weights = apply(X=weights,MARGIN=2,FUN=na.zero)
#     
#     dist_data = weights
#     if(length(which(colSums(dist_data)==0))!=0)
#     {
#       
#       dist_data = dist_data[,-which(colSums(dist_data)==0)]
#     }
#     
#     print(colSums(dist_data))
#     save(dist_data, file = "dist_data.rds")
#     # print(head(dist_data))
#     print("head dist data")
#     print(length(colnames(dist_data)))
#     lez = length(colnames(dist_data))
#     
#     
#     #load("dist_data.rds")
#     dist_data = as.data.frame(dist_data)
#     
#     output$Chart4 <- renderPlotly({
#       
#       # rownames(dist_data) = as.Date(   rownames(dist_data))
#       
#       
#       
#       dist_data%>% 
#         dplyr::mutate(id = as.Date(rownames(.)) ) %>%
#         tidyr::gather(key = "variable",value = "value",-id) %>% 
#         plot_ly(x = ~id, y=~value, type="bar", color=~variable) %>%
#         layout( barmode = 'stack', yaxis =  list(title = "Distribution %" ), xaxis =  list(title = "" ))
#       
#       
#       #   if(length(colnames(dist_data))==10)
#       #   {
#       #     plot_ly( x = as.Date(rownames(dist_data)), y = dist_data[,1], type = 'bar', name = colnames(dist_data)[1])%>%
#       #       add_trace( y = ~as.numeric(dist_data[,2]), type = 'bar',name = colnames(dist_data)[2])  %>%
#       #       add_trace( y = ~as.numeric(dist_data[,3]), type = 'bar',name = colnames(dist_data)[3])  %>%
#       #       add_trace( y = ~as.numeric(dist_data[,4]), type = 'bar',name = colnames(dist_data)[4])  %>%
#       #       add_trace( y = ~as.numeric(dist_data[,5]), type = 'bar',name = colnames(dist_data)[5])  %>%
#       #       add_trace( y = ~as.numeric(dist_data[,6]), type = 'bar',name = colnames(dist_data)[6])  %>%
#       #       add_trace( y = ~as.numeric(dist_data[,7]), type = 'bar',name = colnames(dist_data)[7])  %>%
#       #       add_trace( y = ~as.numeric(dist_data[,8]), type = 'bar',name = colnames(dist_data)[8])  %>%
#       #       add_trace( y = ~as.numeric(dist_data[,9]), type = 'bar',name = colnames(dist_data)[9])  %>%
#       #       add_trace( y = ~as.numeric(dist_data[,10]), type = 'bar',name = colnames(dist_data)[10])  %>%
#       #       # layout(legend = list(x = 0.1, y = 0.9)) %>%
#       #       layout( barmode = 'stack', yaxis =  list(title = "Distribution %" ))
#       #   }
#       # 
#       
#     })
#     
#     
#     
#   }
#   
# })
# 
# ## RESET ALL TABLES AND CHARTS
# observeEvent(input$reset,{
#   
#   output$Table1 = DT::renderDataTable({ })
#   output$Table2 = DT::renderDataTable({ })
#   output$Chart0 <- renderPlotly({})
#   output$Chart1 <- renderPlotly({})
#   output$Chart2 <- renderPlotly({})
#   output$Chart3 <- renderPlotly({})
#   output$Chart4 <- renderPlotly({})
#   output$dates_check = renderUI({ })
#   
# })
