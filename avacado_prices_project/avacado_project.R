library(tidyverse)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(wordcloud)
library(tm)
library(wordcloud2)
library(DT)


avacado <- read_csv("C:/Users/Andrew/Desktop/CSC_324/CSC324Repo/avacado_prices_project/avocado.csv")

# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  
  dashboardHeader(title = "Avocados"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Avocado Sales", tabName = "plu"),
      menuItem("Avocado Prices Over Time", tabName = "prices"),
      menuItem("Data Table", tabName = "table"),
      menuItem("Regions Wordcloud", tabName = "cloud")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("plu",
              selectInput("sales_input", "Differentiated Attributes:", 
                          c("All PLUs", "4046", "4225", "4770", "All Bag Sizes", "Small Bags",
                            "Large Bags", "XLarge Bags")),
              box(plotOutput("sales_line"), width=16)
      ),
      tabItem("prices",
              box(plotOutput("prices_line"), width = 16)
      ),
      tabItem("table",
              fluidPage(
                h1("Avocado Prices Data Table"),
                dataTableOutput("avocado_table")
              )
      ),
      tabItem("cloud",
              box(wordcloud2Output("cloud"), width = 16)
      )
    )
  )
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  #grouping the data by date so that we can take the average statistic for each date
  #if its grouped like this we can easily visualize changes over time on average throughout
  #all of the regions covered by the data set.
  by_date <- arrange(mutate(avacado, dates = as.Date(`Date`, "%m/%d/%Y")), `dates`)
  by_date <- group_by(by_date, dates)
  by_date <- summarise_at(by_date, c('Total Volume', '4046', '4225', '4770', 'AveragePrice', 'Total Bags', 'Small Bags', 'Large Bags', 'XLarge Bags'), mean)
  

  #sales over time
      
  output$sales_line <- renderPlot({
    if (input$sales_input == "All PLUs") {
      
        ggplot(by_date, aes(x = dates, y=`Total Volume`)) +  
          geom_point(color = "blue") +
          xlab("Dates 2015-2018") +
          ylab("Quantity Sold") +
          ggtitle("Avocado Sales Vary Over Time") + 
          geom_smooth()
    
    } else if (identical(input$sales_input, "4046")) {
        ggplot(by_date, aes(x = dates, y=`4046`)) +  
          geom_point(color = "green") +
          xlab("Dates 2015-2018") +
          ylab("Quantity Sold") +
          ggtitle("Avocado Sales Vary Over Time") + 
          geom_smooth(color = "green")
      
    } else if (identical(input$sales_input, "4225")) {
        ggplot(by_date, aes(x = dates, y=`4225`)) +  
          geom_point(color = "purple") +
          xlab("Dates 2015-2018") +
          ylab("Quantity Sold") +
          ggtitle("Avocado Sales Vary Over Time") + 
          geom_smooth(color = "purple")
      
    } else if (identical(input$sales_input, "4770")) {
        ggplot(by_date, aes(x = dates, y=`4770`)) +  
          geom_point(color = "red") +
          xlab("Dates 2015-2018") +
          ylab("Quantity Sold") +
          ggtitle("Avocado Sales Vary Over Time") + 
          geom_smooth(color = "red")

    } else if (identical(input$sales_input, "All Bag Sizes")) {
      ggplot(by_date, aes(x = dates, y=`Total Bags`)) +  
        geom_point(color = "blue") +
        xlab("Dates 2015-2018") +
        ylab("Quantity Sold") +
        ggtitle("Quantity of Bagged Avocadoes Sold Increases Over Time") + 
        geom_smooth(color = "blue")
      
    } else if (identical(input$sales_input, "Small Bags")) {
      ggplot(by_date, aes(x = dates, y=`Small Bags`)) +  
        geom_point(color = "purple") +
        xlab("Dates 2015-2018") +
        ylab("Quantity Sold") +
        ggtitle("Small Bag Sales Fluctuate but Typically Increase Over Time") + 
        geom_smooth(color = "purple")
      
    } else if (identical(input$sales_input, "Large Bags")) {
      ggplot(by_date, aes(x = dates, y=`Large Bags`)) +  
        geom_point(color = "red") +
        xlab("Dates 2015-2018") +
        ylab("Quantity Sold") +
        ggtitle("Large Bag Sales Build Over Time") + 
        geom_smooth(color = "red")
      
    } else if (identical(input$sales_input, "XLarge Bags")) {
      ggplot(by_date, aes(x = dates, y=`XLarge Bags`)) +  
        geom_point(color = "red") +
        xlab("Dates 2015-2018") +
        ylab("Quantity Sold") +
        ggtitle("XL Bag Sales progress slowly over time") + 
        geom_smooth(color = "red")
      
    }
  })

  
  #prices over time
  output$prices_line <- renderPlot({
    ggplot(by_date, aes(x = dates, y=`AveragePrice`)) +
      geom_point(color = "green") +
      geom_line(color = "green") +
      xlab("Dates 2015-2018") +
      ylab("Average Price") +
      ggtitle("Avacado Prices Oscillate Over Time")
  })
  
  
  #avocado table
  output$avocado_table <- renderDataTable(avacado)
  
  #regions word cloud tab
  docs <- Corpus(VectorSource(avacado$region))
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
  output$cloud <- renderWordcloud2({
    wordcloud2(data=df, size=0.13)
  })
}

shinyApp(ui, server)


#resources:
# https://stackoverflow.com/questions/44473832/word-cloud-in-r-shiny-dashboard-showing-up-in-viewing-pane-instead-of-app-window
# https://www.youtube.com/watch?v=41jmGq7ALMY&t=772s
# https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
# https://dplyr.tidyverse.org/reference/summarise_all.html
# https://ggplot2.tidyverse.org/reference/labs.html
# https://www.statology.org/r-mean-by-group/
# https://r4ds.had.co.nz/transform.html