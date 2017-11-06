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
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE))
      stop("Package not found")
  }
}
reqPackage('DT')
reqPackage('lubridate')
reqPackage('plyr')
reqPackage('reshape2')
reqPackage('shiny')
reqPackage('xts')

##############################

# 列名の編集
colsnames <- c("取引日", "受入金額", "払出金額", "詳細１", "詳細２", "現在高")
colsnames.g <- c("取引年月", "入出金額", "詳細")

##############################

# 列の編集
colsort <- function(data, order) {
  data <- data[, order]

  data[, 1] <- ifelse(is.na(data[, 1]), "", data[, 1])
  data[, 2] <- ifelse(is.na(data[, 2]), 0,  data[, 2])
  data[, 3] <- ifelse(is.na(data[, 3]), 0,  data[, 3])
  data[, 4] <- ifelse(is.na(data[, 4]), "", data[, 4])
  data[, 5] <- ifelse(is.na(data[, 5]), "", data[, 5])
  data[, 6] <- ifelse(is.na(data[, 6]), 0,  data[, 6])

  data[, 2] <- gsub(",", "",  data[, 2])
  data[, 3] <- gsub(",", "",  data[, 3])
  data[, 6] <- gsub(",", "",  data[, 6])

  data[, 2] <- gsub("円", "", data[, 2])
  data[, 3] <- gsub("円", "", data[, 3])
  data[, 6] <- gsub("円", "", data[, 6])

  data[, 2] <- gsub(" ", "",  data[, 2])
  data[, 3] <- gsub(" ", "",  data[, 3])
  data[, 6] <- gsub(" ", "",  data[, 6])

  data[, 2] <- as.numeric(data[, 2])
  data[, 3] <- as.numeric(data[, 3])
  data[, 6] <- as.numeric(data[, 6])

  colnames(data) <- colsnames
  return(data)
}

# 日付文字列の書式
checkDateFormat <- function(dateStr) {
  fmt <- "%Y%m%d"
  # if(nchar(dateStr)==6){
  #   fmt <- "%Y%m%d"
  # } else
  if (grepl("/", dateStr) && nchar(dateStr) == 8) {
    fmt <- "%Y/%m/%d"
  } else if (grepl("年", dateStr) && nchar(dateStr) == 9) {
    fmt <- "%Y年%m月%d日"
  }
  return(fmt)
}

# 行の編集
rowfilter <- function(data, dateRange) {
  Sys.setlocale("LC_TIME", "C")
  data.char <- as.character(data[, 1])
  data[, 1] <- as.Date(data.char,
                       format = checkDateFormat(data.char))

  # cat(as.Date(data$取引日),"\t",dateRange[1],"\t",dateRange[2],"\n")
  cond <-
    ((as.Date(data$取引日) >= dateRange[1]) &&
       (as.Date(data$取引日) <= dateRange[2]))
  data <- data[cond,]
  return(data)
}

#月別詳細1別に集計
groupByMonthAndDetail1 <- function(df) {
  df[,1] <- substr(df[,1],1,7)
  df[,2] <- ifelse(df[,2]>0,df[,2],-1*df[,3])
  df[,4] <- df[,4]

  df <- df[,c(1,2,4)]
  colnames(df) <- colsnames.g

  df.wide <- dcast(df, 取引年月 ~ 詳細, sum, value.var = "入出金額")
  # return(df.wide)

  df.wide.mean <- apply(df.wide[, -1], 2, mean)
  df.wide.order <- 1+c(0, order(df.wide.mean, decreasing=T))
  return(df.wide[,df.wide.order])
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

                    # Input: Choose start date and end date
                    dateRangeInput(
                      'dateRange',
                      label = 'Date range input: yyyy-mm-dd',
                      start = Sys.Date() + years(-10) ,
                      end = Sys.Date()
                    ),

                    # Horizontal line ----
                    tags$hr(),

                    # Input: Select number of rows to display ----
                    radioButtons(
                      "disp",
                      "Display",
                      choices = c(All = "all",
                                  Group = "group",
                                  Head = "head",
                                  Tail = "tail"),
                      selected = "all"
                    ),

                    # Input: Choose column order
                    radioButtons(
                      "order",
                      "Order",
                      choices = c(
                        order_1_3_4_5_6_7 = "1,3,4,5,6,7",
                        order_1_2_3_4_6_5 = "1,2,3,4,6,5",
                        order_1_4_3_2_6_5 = "1,4,3,2,6,5"
                      ),
                      selected = "1,3,4,5,6,7"
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
    df.sorted <-
      colsort(df, as.integer(unlist(strsplit(
        input$order, ","
      ))))

    if ((mode(df.sorted[,2])=="numeric") &&
        (mode(df.sorted[,3])=="numeric") &&
        (mode(df.sorted[,6])=="numeric")) {
      df.filtered <- rowfilter(df.sorted, input$dateRange)

      # df <- df.sorted
      df <- df.filtered

      # endif

      if (input$disp == "head") {
        data <- (head(df))
      } else if (input$disp == "tail") {
        data <- (tail(df))
      } else if (input$disp == "group") {
        data <- groupByMonthAndDetail1(df)
      } else {
        data <- (df)
      }
    }
  },
  options = list(paging = FALSE)))

}

# Run the application
shinyApp(ui = ui, server = server)
