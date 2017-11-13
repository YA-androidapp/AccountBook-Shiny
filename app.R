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
      stop('Package not found')
  }
}
reqPackage('colorspace')
reqPackage('dplyr')
reqPackage('DT')
reqPackage('ggplot2')
reqPackage('lubridate')
reqPackage('plyr')
reqPackage('reshape2')
reqPackage('shiny')
reqPackage('xts')

##############################

# 列名の編集
colsnames <- c('取引日', '受入金額', '払出金額', '詳細１', '詳細２', '現在高')
colsnames.ga <- c('取引年月', '入出金額', '詳細')
colsnames.gd <- c('日', '詳細')

##############################

# 列の編集
colsort <- function(data.colsort, order) {
  data.colsort <- data.colsort[, order]

  data.colsort[, 1] <-
    ifelse(is.na(data.colsort[, 1]), '', data.colsort[, 1])
  data.colsort[, 2] <-
    ifelse(is.na(data.colsort[, 2]), 0,  data.colsort[, 2])
  data.colsort[, 3] <-
    ifelse(is.na(data.colsort[, 3]), 0,  data.colsort[, 3])
  data.colsort[, 4] <-
    ifelse(is.na(data.colsort[, 4]), '', data.colsort[, 4])
  data.colsort[, 5] <-
    ifelse(is.na(data.colsort[, 5]), '', data.colsort[, 5])
  data.colsort[, 6] <-
    ifelse(is.na(data.colsort[, 6]), 0,  data.colsort[, 6])

  data.colsort[, 2] <- gsub(',', '',  data.colsort[, 2])
  data.colsort[, 3] <- gsub(',', '',  data.colsort[, 3])
  data.colsort[, 6] <- gsub(',', '',  data.colsort[, 6])

  data.colsort[, 2] <- gsub('円', '', data.colsort[, 2])
  data.colsort[, 3] <- gsub('円', '', data.colsort[, 3])
  data.colsort[, 6] <- gsub('円', '', data.colsort[, 6])

  data.colsort[, 2] <- gsub(' ', '',  data.colsort[, 2])
  data.colsort[, 3] <- gsub(' ', '',  data.colsort[, 3])
  data.colsort[, 6] <- gsub(' ', '',  data.colsort[, 6])

  data.colsort[, 2] <- as.numeric(data.colsort[, 2])
  data.colsort[, 3] <- as.numeric(data.colsort[, 3])
  data.colsort[, 6] <- as.numeric(data.colsort[, 6])

  colnames(data.colsort) <- colsnames
  return(data.colsort)
}

# 日付文字列の書式
checkDateFormat <- function(dateStr) {
  fmt <- '%Y%m%d'
  # if(nchar(dateStr)==6){
  #   fmt <- '%Y%m%d'
  # } else
  if (grepl('/', dateStr) && nchar(dateStr) == 8) {
    fmt <- '%Y/%m/%d'
  } else if (grepl('年', dateStr) && nchar(dateStr) == 9) {
    fmt <- '%Y年%m月%d日'
  }
  return(fmt)
}

# 行の編集
rowfilter <- function(data.rowfilter, dateRange) {
  Sys.setlocale('LC_TIME', 'C')
  data.rowfilter.char <- as.character(data.rowfilter[, 1])
  data.rowfilter[, 1] <-
    as.Date(data.rowfilter.char, format = checkDateFormat(data.rowfilter.char))

  cond <-
    ((as.Date(data.rowfilter$取引日) >= dateRange[1]) &&
       (as.Date(data.rowfilter$取引日) <= dateRange[2]))
  data.rowfilter <- data.rowfilter[cond, ]
  return(data.rowfilter)
}

#月別詳細1別に集計
groupByMonthAndDetail1_Amount <- function(data.g, flag) {
  data.h <- data.g
  data.h[, 1] <- substr(data.h[, 1], 1, 7)
  data.h[, 2] <- ifelse(data.h[, 2] > 0, data.h[, 2], -1 * data.h[, 3])
  data.h[, 3] <- data.h[, 4]
  data.h <- data.h[, c(1, 2, 3)]
  colnames(data.h) <- colsnames.ga

  data.h.wide <- dcast(data.h,  取引年月  ~  詳細, sum, value.var = '入出金額')
  data.h.wide.mean <- apply(data.h.wide[, -1], 2, mean)

  if (flag) {
    data.h.wide.order <-
      1 + c(0, order(data.h.wide.mean, decreasing = T))
    return(data.h.wide[, data.h.wide.order])
  } else {
    return(data.h.wide.mean)
  }
}

statmode <- function(x) {
  y <- names(rev(sort(table(x))))[1]
  return(y)
}

groupByMonthAndDetail1_Day <- function(data.g, flag) {
  data.h <- data.g
  data.h[, 1] <- as.integer(substr(data.g[, 1], 9, 10))
  data.h[, 2] <- data.h[, 4]
  data.h <- data.h[, c(1, 2)]
  colnames(data.h) <- colsnames.gd

  print('data.h')
  print(data.h)

  print('1')
  data.h.wide <- dcast(data.h,  詳細 ~ 日, length, value.var = '日')
  print('2')
  data.h.wide.mean <- apply(data.h.wide[, -2], 2, length)

  if (flag) {
    data.h.wide.order <-
      1 + c(0, order(data.h.wide.mean, decreasing = T))
    return(data.h.wide[, data.h.wide.order])
  } else {
    return(data.h.wide.mean)
  }
}

readAndSort <- function(fpath) {
  if (endsWith(fpath, '.csv')) {
    # ヘッダ行数検出
    f <- file(fpath, 'r', encoding = 'Shift_JIS')
    i <- 0
    j <- 0
    s <- 0
    repeat {
      str = readLines(con = f, 1)
      if (length(unlist(strsplit(str, ','))) > 2) {
        j <- j + 1
        if (j > 3) {
          # ヘッダ行1 + データ行3
          s <- i - j + 1
          break
        }
      } else {
        j <- 0
      }
      i <- i + 1
    }
    close(f)

    df.csv <- read.csv(
      fpath,
      #
      header = T,
      na.strings = '',
      skip = s,
      stringsAsFactors = F,
      fileEncoding = 'cp932'
      #fileEncoding = 'utf8'
    )

    fo <- c()
    if (colnames(df.csv)[3] == '受入金額.円.') {
      fo <- c(1, 3, 4, 5, 6, 7)
    }

    return(colsort(df.csv, fo))
  }
}

##############################

# Define UI for application that draws a histogram
ui <- fluidPage(# App title ----
                titlePanel('AccountBook'),

                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Input: Select a file ----
                    fileInput(
                      'file1',
                      'Choose CSV File',
                      multiple = TRUE,
                      accept = c('text/csv',
                                 'text/comma-separated-values,text/plain',
                                 '.csv')
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
                      'disp',
                      'Display',
                      choices = c(
                        All = 'all',
                        Group_amount = 'group_amount',
                        Group_day = 'group_day',
                        Head = 'head',
                        Tail = 'tail'
                      ),
                      selected = 'all'
                    )

                  ),

                  # Main panel for displaying outputs ----
                  mainPanel(
                    # Output: Data file ----
                    #tableOutput('contents')
                    DT::dataTableOutput('table_d'),

                    DT::dataTableOutput('table_g'),

                    plotOutput(
                      outputId = 'plot_d',
                      width = '400px',
                      height = '400px'
                    ),

                    plotOutput(
                      outputId = 'plot_g',
                      width = '400px',
                      height = '400px'
                    )
                  )
                ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table_d <- DT::renderDataTable(DT::datatable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)
    df.sorted <- readAndSort(input$file1$datapath)

    if ((mode(df.sorted[, 2]) == 'numeric') &&
        (mode(df.sorted[, 3]) == 'numeric') &&
        (mode(df.sorted[, 6]) == 'numeric')) {
      df.filtered <- rowfilter(df.sorted, input$dateRange)

      if (input$disp == 'head') {
        data <- (head(df.filtered))
      } else if (input$disp == 'tail') {
        data <- (tail(df.filtered))
      } else if (input$disp == 'group_amount') {
        data <- groupByMonthAndDetail1_Amount(df.filtered, TRUE)
      } else if (input$disp == 'group_day') {
        data <- groupByMonthAndDetail1_Day(df.filtered, TRUE)
      } else {
        data <- df.filtered
      }
    }
  },
  options = list(paging = FALSE)))

  output$table_g <- DT::renderDataTable(DT::datatable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)
    df.sorted <- readAndSort(input$file1$datapath)

    if ((mode(df.sorted[, 2]) == 'numeric') &&
        (mode(df.sorted[, 3]) == 'numeric') &&
        (mode(df.sorted[, 6]) == 'numeric')) {
      df.filtered <- rowfilter(df.sorted, input$dateRange)

      if (input$disp == 'group_day') {
        data <- cbind(t(groupByMonthAndDetail1_Day(df.filtered, FALSE)))
        data <- data.frame(t(data[, order(data, decreasing = T)]))
        print(data)
      } else if (input$disp == 'group_amount') {
        data <- cbind(t(groupByMonthAndDetail1_Amount(df.filtered, FALSE)))
        data <- data.frame(t(data[, order(data, decreasing = T)]))
        print(data)
      } else {
        data <- NULL
      }
    }
  },
  options = list(paging = FALSE)))

  output$plot_d <- renderPlot({
    req(input$file1)
    df.sorted <- readAndSort(input$file1$datapath)

    if ((mode(df.sorted[, 2]) == 'numeric') &&
        (mode(df.sorted[, 3]) == 'numeric') &&
        (mode(df.sorted[, 6]) == 'numeric')) {
      df.filtered <- rowfilter(df.sorted, input$dateRange)
      df.filtered.2 <- df.filtered[, c(1, 6)]
      df.filtered.2[, 2] <- as.numeric(df.filtered.2[, 2])

      df.filtered.2 <-
        df.filtered.2 %>% group_by(df.filtered.2$取引日) %>%
        arrange(df.filtered.2$現在高) %>%
        filter(row_number() == 1)

      print(df.filtered.2)

      df.xts <- xts(df.filtered.2, as.POSIXct(df.filtered.2$取引日))

      windowsFonts(gothic = windowsFont('MS Gothic'))
      par(family = 'gothic')

      g <- ggplot(df.xts,
                  aes(x =   取引日,
                      y =   現在高,
                      group = 1))
      g <- g + geom_line(colour = 'red',
                         linetype = 1,
                         size = 0.5)
      g <- g + geom_smooth (method = 'lm')
      g <- g + xlab('Date')
      g <- g + ylab('Amount')
      g <- g + ggtitle('AccountBook')
      plot(g)

    }


  })

  output$plot_g <- renderPlot({
    req(input$file1)
    df.sorted <- readAndSort(input$file1$datapath)

    if ((mode(df.sorted[, 2]) == 'numeric') &&
        (mode(df.sorted[, 3]) == 'numeric') &&
        (mode(df.sorted[, 6]) == 'numeric')) {
      df.filtered <- rowfilter(df.sorted, input$dateRange)

      if (input$disp == 'group_amount') {
        data <- cbind(t(groupByMonthAndDetail1_Amount(df.filtered, FALSE)))
        data <- data.frame(t(data[, order(data, decreasing = T)]))
        windowsFonts(gothic = windowsFont('MS Gothic'))
        par(family = 'gothic')
        barplot(t(data)[, 1],
                col = rainbow_hcl(ncol(data)),
                names.arg = names(t(data)))
      }

    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
