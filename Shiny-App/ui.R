
# Interface design details. 
################################################

library(shiny)
library(shinydashboard)

## UI DOES NOT CONTAIN DATA
# Just layouts, colors, boxes, button names etc etc

#################### HEADER ####################
header <- dashboardHeader(
  title = "MAIN DASHBOARD"
)

#################### SIDEBAR ####################
sidebar <- dashboardSidebar(
  collapsed = FALSE, 
  ## each menu item is a button on the left, on the sidebard
  ## not only it is a button but it is also a refence to a page in the body
  sidebarMenu
  (
    ## important to give this a tabName which will be used in the body
    menuItem("Portfolio Analysis", tabName = "tab1", icon = icon("chart-line" ))
  ),
  sidebarMenu
  (
    menuItem("Customized SP500 analysis", tabName = "tab2", icon = icon("chart-line" ))
  ),
  sidebarMenu
  (
    menuItem("Unsupervised/ML Analysis", tabName = "tab3", icon = icon("chart-line" ))
  )
  ,
  sidebarMenu(
    menuItem("About this app", tabName = "About")
  ),
  
  radioButtons('radio1', label = 'Input Asset Class', choices = c('Stocks'))
  
)

#################### BODY ####################
body <- dashboardBody(
  
  theme = shinytheme("lumen"),
  
  tabItems(
    ## this is the first page
    tabItem(
      # we need to link the tabItem to a page from Dashboard Sidebar
      tabName = "tab1",
      
      column(4,
             box( solidHeader = T, status = "primary",  title = "Random Buttons", width = NULL,
                  
                  selectInput(inputId = 'ticker',label = 'Select a company', choices = companies),
                  
                  dateRangeInput('dateRange',
                                 label = 'Time Frame Selection', 
                                 format = "mm/dd/yyyy",weekstart = 1,
                                 min = time(prices)[1],
                                 max =  time(prices)[length( time(prices))], 
                                 start = time(prices)[1], end = time(prices)[length( time(prices))],
                                 width = "100%"
                  )
                  
             )
             
      ),
      
      
      column(8,
             box( solidHeader = T, status = "primary",  title = "Data Specifications.", width = NULL,
                  tabsetPanel(
                    tabPanel("Returns", icon  = icon("chart-line"),
                             plotlyOutput("plot1")
                    ),
                    tabPanel("Rolling Volatility",icon  = icon("chart-line"),
                             plotlyOutput("Chart2")
                    ),
                    tabPanel("Monthly Returns: Alpha and Beta",icon  = icon("chart-line"),
                             plotlyOutput("plot3")
                    )
                    
                  )
                  
             ),
             
             box( solidHeader = T, status = "primary",  title = "Summary Statistics Table", width = NULL,
                  htmlOutput('windowframe'),
                  br(),
                  dataTableOutput('Table1')
             )
      )
    ),
    
    ## this is the second page
    tabItem(
      # we need to link the tabItem to a page from Dashboard Sidebar
      tabName = "tab2",
      column(4,
             box( solidHeader = T, status = "primary",  title = "Data Specifications.", width = NULL,
                  selectInput(inputId = 'ticker2',label = 'Select a tech company ticker', choices = tickers),
                  textInput(inputId = 'ticker3','Input any other ticker manually', value = ''),
                  dateRangeInput('dateRange2',
                                 label = 'Time Frame Selection', 
                                 format = "mm/dd/yyyy",weekstart = 1,
                                 start = '2018-01-01', end = Sys.time(),
                                 width = "100%"
                  ),
                  sliderInput(inputId = 'windrange', label = 'Window Range for Prediction',
                              min = 0 , max = 200,
                              value = 100, step = 5)
                  
             )
             
      ),
      
      column(8,
             box( solidHeader = T, status = "primary",  title = "Output", width = NULL,
                  tabsetPanel(
                    tabPanel("Returns", icon  = icon("chart-line"),
                             plotlyOutput("plot5")
                    ),
                    tabPanel("Rolling Volatility",icon  = icon("chart-line"),
                             plotlyOutput("plot6")
                    )
                    
                  )
                  
             ),
             
             column(8,
                    box(solidHeader = T, status = "primary",  title = "Prediction Demo", width = NULL,
                        plotOutput('chart10')
                    )
             ),
             column(4,
                    box(solidHeader = T, status = "primary",  title = "Prediction Demo Score", width = NULL,
                        htmlOutput('calc_status'),
                        dataTableOutput('Table2')
                    )
             )
             
      )
    ),
    tabItem(
      # we need to link the tabItem to a page from Dashboard Sidebar
      tabName = "tab3",
      column(4,
             box( solidHeader = T, status = "primary",  title = "Data Specifications.", width = NULL,
                  dateRangeInput('dateRange3',
                                 label = 'Time Frame Selection', 
                                 format = "mm/dd/yyyy",weekstart = 1,
                                 start = '2018-01-01', end = Sys.time(),
                                 width = "100%"
                  ),
                  textAreaInput(inputId = 'areainput2', label = "Input tickers separated by coma", value = "", width = NULL, height = NULL,
                                cols = NULL, rows = NULL, placeholder = NULL, resize = NULL),
                  
                  actionButton("do", "Calculate PCA", icon = icon('save'), width = 400 ,
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),  br(),  br(),
                  uiOutput('num_center'),
                  numericInput(inputId = 'numstart', label = "number of random initialization start", value = 1),
                  
                  actionButton("do2", "Calculate Cluster", icon = icon('save'), width = 400 ,
                               style="color: #fff; background-color: #39954E; border-color: #2e6da4")
             )
             
      ),
      
      column(8,
             box( solidHeader = T, status = "primary",  title = "PCA Results: explains market variance with each date", width = NULL,
                  tabsetPanel(
                    
                    tabPanel("PCA Plot", icon  = icon("chart-line"),
                             plotOutput('plotPCA')
                    ),
                    tabPanel("Variance Percentage Explanation Histogramm",icon  = icon("chart-line"),
                             plotOutput("plotVAR")
                    ),
                    tabPanel("Importance of components Summary Table",icon  = icon("chart-line"),
                             dataTableOutput("dfImportance")
                    )
                    
                  )
                  
             ),
             box( solidHeader = T, status = "primary",  title = "Cluster Results: Shows market risk,trend parity", width = NULL,
                  tabsetPanel(
                    
                    tabPanel("Cluster Analysis", icon  = icon("chart-line"),
                             plotOutput('plotCluster')
                    ),
                    tabPanel("Cluster Dendogramm",icon  = icon("chart-line"),
                             plotOutput("plotDend")
                    )
                  )
                  
             )
      )
      
    ),
    ## this is the third page
    tabItem(
      tabName = "About",
      h1('part 1: Protfolio Analysis'),
      br(),
      h4(
        'To run this part, we need stored data, multiple columns of each different stocks, and if you add 
        bench mark, you could see how it performs differently. Bench mark data could be given by portfolio 
        manager and subject to change.'
      ), 
      br(),
      h4('If user wants to customize input stock, user can go to next dash board, <Customized snp500 market analysis>'),
      h4('rolling anulized volatility will be shown for derivative trader, and also alpha and beta extracted from linear regression will be shown also'),
      br(),
      br(),
      h1('part 2: Customized snp500 market Analysis'),
      br(), br(),
      h4(
        'Anualized rolling volatility curve which equity derivative trader can use this as an another source of signal to see market'
      ),
      h4('
         In case user wants to analyze stocks included in SnP500, user can use customize input.
         '),
      br(),
      h4('
         User select their own preferred asset class from SnP500, and do following analysis.
         '),
      h4(
        'Rolling Linear regression prediction(shows upper,lower bound of prediction also), with prediction score.'
      ),
      h4(
        'This predictions hyper parameter(window size) can be adjusted according to the user'
      ),
      h4(
        'one restriction of window size: 0<= window size < length(targeted_stock)'
      ),
      br(),
      br(),
      br(),
      h1('part 3: machine learning Analysis'),
      br(),
      h4(
        'Here, User can use unsupervised machine learning algorithm to cluster and see the variance analysis in the snp stock market'
      ),
      br(),
      h4(
        'PCA analysis. Here, user will see the variance analysis. Bigger variance shows how its coresponding stock has more importance or bigger impact to explain SnP500 market at given period'
      ),
      h4(
        'To run PCA analysis, Use input section separated by coma. Put more than 2 stocks to analyze. Just type the ticker name of stocks of SnP500.'
      ),
      br(),
      h4(
        'User can use k-mean clustering algorithm. Input parameter is dynamically adjusting to the current User choice stocks '
      ),
      h4(
        'User can see the dendogram, and clustesring plot to see their similiarity, moving trend, importance, and market regimes movement which is very useful in trading'
      )
      
      )
      )
  )

# Create the UI using the header, sidebar, and body
#################### PAGE ####################

ui <- dashboardPage(
  
  title = "Portfolio Analysis Dashboard", skin = "blue" ,
  header, sidebar,body)




