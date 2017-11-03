# Copyright (c) 2017 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(# App title ----
                titlePanel("Uploading Files"),
                
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
    
    df <- read.csv(
      input$file1$datapath,
      header = T,
      na.strings = "",
      skip = 6,
      stringsAsFactors = F,
      # fileEncoding = "cp932"
      fileEncoding = "utf8"
    )
    
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
