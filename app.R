# Copyright (c) 2017 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

reqPackage <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
reqPackage('DT')
reqPackage('lubridate')
reqPackage('shiny')
reqPackage('xts')

##############################

# 日付文字列の書式
format.6 <- "%Y%m%d"
format.8 <- "%Y/%m/%d"
format.9 <- "%Y年%m月%d日"

# 列の編集
order.1 <- c(1,3,4,5,6,7)
order.2 <- c(1,2,3,4,6,5)
order.3 <- c(1,4,3,2,6,5)

# 列名の編集
colsnames <- c("取引日", "受入金額", "払出金額", "詳細１", "詳細２", "現在高")

##############################

# 列の編集
colsort <- function(data, order){
  data <- data[ , order]
  
  data[ , 2] <- ifelse(is.na(data[ , 2]), 0,  data[ , 2])
  data[ , 3] <- ifelse(is.na(data[ , 3]), 0,  data[ , 3])
  data[ , 5] <- ifelse(is.na(data[ , 5]), "", data[ , 5])
  data[ , 6] <- ifelse(is.na(data[ , 6]), 0,  data[ , 6])
  
  data[ , 2] <- gsub(",", "",  data[ , 2])
  data[ , 3] <- gsub(",", "",  data[ , 3])
  data[ , 6] <- gsub(",", "",  data[ , 6])
  
  data[ , 2] <- gsub("円", "", data[ , 2])
  data[ , 3] <- gsub("円", "", data[ , 3])
  data[ , 6] <- gsub("円", "", data[ , 6])
  
  data[ , 2] <- gsub(" ", "",  data[ , 2])
  data[ , 3] <- gsub(" ", "",  data[ , 3])
  data[ , 6] <- gsub(" ", "",  data[ , 6])
  
  data[ , 2] <- as.numeric(data[ , 2])
  data[ , 3] <- as.numeric(data[ , 3])
  data[ , 6] <- as.numeric(data[ , 6])
  
  colnames(data) <- colsnames
  return( data )
}

# 行の編集
rowfilter <- function(data, format, dateRange){
  Sys.setlocale("LC_TIME","C")
  data.char <- as.character(data[ , 1])
  
  fmt <- format.6
  if(grepl("/", data.char)){
    fmt <- format.8
  } else if(grepl("年", data.char)){
    fmt <- format.9
  }
  data[ , 1] <- as.Date(
    data.char,
    format=fmt
  )
  
  # cat(as.Date(data$取引日),"\t",dateRange[1],"\t",dateRange[2],"\n")
  cond <- ((as.Date(data$取引日) >= dateRange[1]) && (as.Date(data$取引日)<= dateRange[2]))
  data <- data[cond, ]
  return( data )
}

##############################

# Define UI for application that draws a histogram
ui <- fluidPage(# App title ----
                titlePanel("AccountBook"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Input: Select a file ----
                    fileInput(
                      "file1",
                      "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                    ),
                    
                    dateRangeInput('dateRange',
                                   label = 'Date range input: yyyy-mm-dd',
                                   start = Sys.Date() + years(-10) , end = Sys.Date()
                    ),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Input: Select number of rows to display ----
                    radioButtons(
                      "disp",
                      "Display",
                      choices = c(Head = "head",
                                  All = "all"),
                      selected = "head"
                    )
                    
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(# Output: Data file ----
                            #tableOutput("contents")
                            DT::dataTableOutput("table"))
                  
                  
                ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  # output$contents <- renderTable({
  output$table <- DT::renderDataTable(DT::datatable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # if(datapath.endsWith('.csv'))
    
    df <- read.csv(
      input$file1$datapath,
      header = T,
      na.strings = "",
      skip = 6,
      stringsAsFactors = F,
      # fileEncoding = "cp932"
      fileEncoding = "utf8"
    )
    df.sorted <- colsort(df, order.1)
    df.filtered <- rowfilter(df.sorted, format.1, input$dateRange)
    
    # df <- df.sorted
    df <- df.filtered
    
    # endif
    
    if (input$disp == "head") {
      data <- (head(df))
    }
    else {
      data <- (df)
    }
  },
  options = list(
    paging = FALSE
  )))
  
}

# Run the application
shinyApp(ui = ui, server = server)
