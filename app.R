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
reqPackage('colorspace')
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
colsort <- function(data.colsort, order) {
  data.colsort <- data.colsort[, order]

  data.colsort[, 1] <-
    ifelse(is.na(data.colsort[, 1]), "", data.colsort[, 1])
  data.colsort[, 2] <-
    ifelse(is.na(data.colsort[, 2]), 0,  data.colsort[, 2])
  data.colsort[, 3] <-
    ifelse(is.na(data.colsort[, 3]), 0,  data.colsort[, 3])
  data.colsort[, 4] <-
    ifelse(is.na(data.colsort[, 4]), "", data.colsort[, 4])
  data.colsort[, 5] <-
    ifelse(is.na(data.colsort[, 5]), "", data.colsort[, 5])
  data.colsort[, 6] <-
    ifelse(is.na(data.colsort[, 6]), 0,  data.colsort[, 6])

  data.colsort[, 2] <- gsub(",", "",  data.colsort[, 2])
  data.colsort[, 3] <- gsub(",", "",  data.colsort[, 3])
  data.colsort[, 6] <- gsub(",", "",  data.colsort[, 6])

  data.colsort[, 2] <- gsub("円", "", data.colsort[, 2])
  data.colsort[, 3] <- gsub("円", "", data.colsort[, 3])
  data.colsort[, 6] <- gsub("円", "", data.colsort[, 6])

  data.colsort[, 2] <- gsub(" ", "",  data.colsort[, 2])
  data.colsort[, 3] <- gsub(" ", "",  data.colsort[, 3])
  data.colsort[, 6] <- gsub(" ", "",  data.colsort[, 6])

  data.colsort[, 2] <- as.numeric(data.colsort[, 2])
  data.colsort[, 3] <- as.numeric(data.colsort[, 3])
  data.colsort[, 6] <- as.numeric(data.colsort[, 6])

  colnames(data.colsort) <- colsnames
  return(data.colsort)
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
rowfilter <- function(data.rowfilter, dateRange) {
  Sys.setlocale("LC_TIME", "C")
  data.rowfilter.char <- as.character(data.rowfilter[, 1])
  data.rowfilter[, 1] <-
    as.Date(data.rowfilter.char, format = checkDateFormat(data.rowfilter.char))

  cond <-
    ((as.Date(data.rowfilter$取引日) >= dateRange[1]) &&
       (as.Date(data.rowfilter$取引日) <= dateRange[2]))
  data.rowfilter <- data.rowfilter[cond,]
  return(data.rowfilter)
}

#月別詳細1別に集計
groupByMonthAndDetail1 <- function(data.g, flag) {
  data.g[, 1] <- substr(data.g[, 1], 1, 7)
  data.g[, 2] <-
    ifelse(data.g[, 2] > 0, data.g[, 2], -1 * data.g[, 3])
  data.g[, 4] <- data.g[, 4]

  data.h <- data.g[, c(1, 2, 4)]
  colnames(data.h) <- colsnames.g

  data.h.wide <- dcast(data.h,   取引年月   ~   詳細, sum, value.var = "入出金額")
  # return(data.g.wide)

  data.h.wide.mean <- apply(data.h.wide[, -1], 2, mean)

  if (flag) {
    data.h.wide.order <- 1 + c(0, order(data.h.wide.mean, decreasing = T))
    return(data.h.wide[, data.h.wide.order])
  } else {
    return(data.h.wide.mean)
  }
}

readAndSort <- function(fpath, forder) {
  if (endsWith(fpath, '.csv')) {
    df.csv <- read.csv(
      fpath,
      #
      header = T,
      na.strings = "",
      skip = 6,
      stringsAsFactors = F,
      # fileEncoding = "cp932"
      fileEncoding = "utf8"
    )
    return(colsort(df.csv, as.integer(unlist(
      strsplit(forder,
               ",")
    ))))
  }
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
                      choices = c(
                        All = "all",
                        Group = "group",
                        Head = "head",
                        Tail = "tail"
                      ),
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
                  mainPanel(
                    # Output: Data file ----
                    #tableOutput("contents")
                    DT::dataTableOutput("table_d"),

                    DT::dataTableOutput("table_g"),

                    plotOutput(outputId = "plot_g", width = "400px", height = "400px")
                  )
                ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table_d <- DT::renderDataTable(DT::datatable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)
    df.sorted <- readAndSort(input$file1$datapath, input$order)

    if ((mode(df.sorted[, 2]) == "numeric") &&
        (mode(df.sorted[, 3]) == "numeric") &&
        (mode(df.sorted[, 6]) == "numeric")) {
      df.filtered <- rowfilter(df.sorted, input$dateRange)

      if (input$disp == "head") {
        data <- (head(df.filtered))
      } else if (input$disp == "tail") {
        data <- (tail(df.filtered))
      } else if (input$disp == "group") {
        data <- groupByMonthAndDetail1(df.filtered, TRUE)
      } else {
        data <- (df.filtered)
      }
    }
  },
  options = list(paging = FALSE)))

  output$table_g <- DT::renderDataTable(DT::datatable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)
    df.sorted <- readAndSort(input$file1$datapath, input$order)

    if ((mode(df.sorted[, 2]) == "numeric") &&
        (mode(df.sorted[, 3]) == "numeric") &&
        (mode(df.sorted[, 6]) == "numeric")) {
      df.filtered <- rowfilter(df.sorted, input$dateRange)

      if (input$disp == "group") {
        data <- cbind(t(groupByMonthAndDetail1(df.filtered, FALSE)))
        data <- data.frame(t(data[, order(data, decreasing = T)]))
      } else {
        data <- NULL
      }
    }
  },
  options = list(paging = FALSE)))

  output$plot_g <- renderPlot({

    req(input$file1)
    df.sorted <- readAndSort(input$file1$datapath, input$order)

    if ((mode(df.sorted[, 2]) == "numeric") &&
        (mode(df.sorted[, 3]) == "numeric") &&
        (mode(df.sorted[, 6]) == "numeric")) {
      df.filtered <- rowfilter(df.sorted, input$dateRange)

      if (input$disp == "group") {
        data <- cbind(t(groupByMonthAndDetail1(df.filtered, FALSE)))
        data <- data.frame(t(data[, order(data, decreasing = T)]))
        windowsFonts(gothic=windowsFont("MS Gothic"))
        par(family="gothic")
        barplot(t(data)[,1], col=rainbow_hcl(ncol(data)), names.arg = names(t(data)))
      }
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
