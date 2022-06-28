#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readr)
library(tidyverse)
library(magrittr)
library(wordcloud)
library(tidytext)
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)

install.packages("devtools")
library(choroplethrZip)
library(choroplethr)
library(devtools)
install_github('arilamstein/choroplethrZip@v1.5.0')

library(dplyr)
library(lubridate)

cleaned_data <- read_csv("final_cleaned_data.csv")
keyword_count<- read_csv("keyword_count.csv")
zipcode<-read_csv("final_cleaned_data(zipcode).csv")
min_date<-min(cleaned_data$host_since)
max_date<-max(cleaned_data$host_since)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MISM 6210 - Airbnb Rental Price Analysis in NYC"),

    fluidRow(
        column(3,
               dateRangeInput("date_r","Date Range",start = min_date,end = max_date),
               selectInput(inputId= "year",
                           label = "Select Year",
                           choice = c(2011,2012,2013,2014,2015,2016,2017,2018),
                           selected = "purple")

        ),
        column(3,
               selectInput(inputId= "cvariable",
                           label = "Select Categorical variable",
                           choice = c("property_type","room_type","bed_type","cancellation_policy"),
                           selected = "purple"),

               selectInput(inputId= "numvariable",
                           label = "Select Numerical variable",
                           choice = c("bedrooms","beds","bathrooms"),
                           selected = "purple")

        ),
        column(3,
              
               selectInput(inputId= "city",
                           label = "City",
                           choice = c("New York","Queens","Bronx"),
                           selected = "purple")

        ),
        column(3,
               sliderInput(inputId= "num_bin",
                           label = "Number of bins:",
                           min = 1,
                           max = 150,
                           value = 100),
               sliderInput(inputId= "max_x",
                           label = "Max x value:",
                           min = 1,
                           max = 1000,
                           value =800 ),
        ),
    ),

    fluidRow(
        column(12,
               tabsetPanel(
                   id = "tabset",
                   tabPanel("pan1",
                            #Output
                            fluidRow(
                                column(6,
                                       plotOutput("p1s1")
                                ),
                                column(6,
                                       plotOutput("p1s2")
                                )
                            ),

                            fluidRow(
                                column(12,
                                       plotOutput("p1s3")
                                )
                            )

                   ),
                   tabPanel("pan2",
                            #Output
                            fluidRow(
                              column(12,
                                     plotOutput("p2s1")
                              ),
                              column(12,
                                     plotOutput("p2s2")
                              )
                            )
                   ),
                   tabPanel("pan3",
                            #Output
                            plotOutput("p3s1"),
                            plotOutput("p3s2"),
                            plotOutput("p3s3"),
                            plotOutput("p3s4")
                   ),
                   tabPanel("pan4",
                            #Output
                            fluidRow(
                              column(6,
                                     plotOutput("p4s3")
                              ),
                              column(6,
                                     plotOutput("p4s2")
                              )
                            ),
                            plotOutput("p4s1")
                   ),
                   tabPanel("pan5",
                            #Output
                            fluidRow(
                              column(6,
                                     plotOutput("p5s1")
                              ),
                              column(6,
                                     plotOutput("p5s2")
                              )
                            ),
                            #Output
                            fluidRow(
                              column(6,
                                     plotOutput("p5s3")
                              ),
                              column(6,
                                     plotOutput("p5s4")
                              )
                            ),
                           # plotOutput("p5s3"),
                            plotOutput("p5s5")
                   ),
                   tabPanel("pan6",
                            #Output
                            plotOutput("p6s1")
                   )
               ))

    )


)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$p1s1 <- renderPlot({
        cleaned_data%>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        filter(city==input$city)%>%
            ggplot(aes(price))+
            geom_histogram(aes(y=..density..),
                           bins=100,
                           color="white",
                           fill="purple")+
            labs(x="Price",
                 y="Count",
                 title="Price Distribution")+
            coord_cartesian(xlim=c(0,input$max_x))+

            geom_density(alpha=.5,fill="#94c4d4")

    })

    output$p1s2 <- renderPlot({
        cleaned_data%>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        filter(city==input$city)%>%
          ggplot()+
          aes(price)+
          geom_density( alpha=.6,
                        color="white",
                        fill="pink")+
          labs(x="Price",
              y="Count",
              title="Log10 Price Distribution")+
          coord_cartesian(xlim=c(-1,input$max_x))+
          scale_x_log10()
    })

    output$p1s3 <- renderPlot({
      cleaned_data%>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        filter(city==input$city)%>%
        ggplot()+
        aes(x = host_since,y = price)+
        geom_point(color="#72a1b3",size=4,shape=3)+
        geom_smooth()+
        labs(x = "Host Since",
             y = "Price",
             title = "Host Since vs. Price")

    })

    output$p2s1 <- renderPlot({
      #wide to long
      type_data_long <-
        cleaned_data %>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        select(city,property_type,room_type,bed_type,price,cancellation_policy) %>%
        gather(key="measure",value="value",-c(city,price))

      type_data_long %>%
        filter(measure==input$cvariable)%>%
        ggplot()+
        aes(x = value,y = price,fill = value)+
        geom_boxplot()+
        facet_wrap(~city,ncol = 3)+
        theme(
          axis.text.x = element_text(angle=90)
        )+
        labs(x="Categorical Variable",y="Price",title = "Categorical Variable Analysis")

    })
    output$p2s2 <- renderPlot({
      #wide to long
      num_data_long <-
        cleaned_data %>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        filter(accommodates <= 6) %>%
        select(city,bedrooms, beds,bathrooms,price) %>%
        gather(key="measure",value="value",-c(city,price))

      num_data_long %>%
        filter(measure == input$numvariable)%>%
        ggplot(aes(x = value, y =city)) +
        geom_tile(aes(fill = price)) +
        scale_fill_gradientn(colors = c("#c1e8ff", "#002b42")) +
        labs(x = "Numerical variable", y = "City",title = "Numerical Variable Analysis")

    })
    
    output$p3s1<-renderPlot({
      # Which locations have better ratings?
      review_data_long <-
        zipcode %>%
        select(city,zipcode,host_since,review_scores_rating,number_of_reviews,reviews_per_month,price) %>%
        gather(key="measure",value="value",-c(city,host_since,zipcode,price))
      
      zipReviews <- review_data_long %>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        group_by(zipcode = zipcode) %>%
        summarise(avg_loc_review = mean(value, na.rm = TRUE))
      
      colnames(zipReviews) <- c("region","value")
      zipReviews$region <- as.character(zipReviews$region)
      nyc_fips = c(36005, 36047, 36061, 36081, 36085)
      
      g_locations <- zip_choropleth(zipReviews,
                                    county_zoom = nyc_fips,
                                    title = "Review Variable by Region",
                                    legend = "Average Review Varable") + ggtitle("Review by Area") +
        theme(plot.caption = element_text(color = "black"))+ scale_fill_brewer("Review Score Rating",palette=5)
      
      g_locations
    })
    
    output$p3s2<-renderPlot({
      cleaned_data%>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        filter(city==input$city)%>%
        ggplot()+
        aes(x = review_scores_rating,y = price)+
        geom_point(color="#72a1b3",size=4,shape=3)+
        geom_smooth()+
        labs(x = "Review Scores Rating",
             y = "Price",
             title = "Review Scores Rating vs. Price")
      
      
    })
    
    output$p3s3<-renderPlot({
      cleaned_data%>%
        filter(city==input$city)%>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        ggplot()+
        aes(x = number_of_reviews,y = price)+
        geom_point(color="#72a1b3",size=4,shape=3)+
        geom_smooth()+
        labs(x = "Number of Reviews",
             y = "Price",
             title = "Number of Reviews vs. Price")
      
      
    })
    
    output$p3s4<-renderPlot({
      cleaned_data%>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        filter(city==input$city)%>%
        ggplot()+
        aes(x = reviews_per_month,y = price)+
        geom_point(color="#72a1b3",size=4,shape=3)+
        geom_smooth()+
        labs(x = "Reviews Per Month",
             y = "Price",
             title = "Reviews Per Month vs. Price")



    })

    output$p4s1<-renderPlot({
      
      zipPrices <- zipcode %>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        group_by(zipcode = zipcode) %>% summarise(avg_price = mean(price, na.rm = TRUE))
      
      colnames(zipPrices) <- c("region","value")
      zipPrices$region <- as.character(zipPrices$region)
      nyc_fips = c(36005, 36047, 36061, 36081, 36085)
      
      g_price_location <- zip_choropleth(zipPrices,
                                         county_zoom = nyc_fips,
                                         title = "Average Price by Region",
                                         legend = "Average Score") + ggtitle("Average Price by Area") +
        theme(plot.caption = element_text(color = "black"))+scale_fill_brewer("Average Price",palette=9)
      
      g_price_location
    })



    output$p4s2<-renderPlot({
      cleaned_data%>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        ggplot()+
        aes(x = accommodates,y = price,fill = city)+
        geom_boxplot(alpha=0.8)+
        labs(x = "Accomodates", y = "Price",title = " Accomodates vs. Price")+
        facet_wrap(~city,ncol = 3)

    })
    
    output$p4s3<-renderPlot({
      gdp <- cleaned_data%>%
        select(city,gdp)%>%
        distinct()
      
      cleaned_data %>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        group_by(city) %>%
        summarise(sumprice = sum(price)) %>%
        left_join(gdp) %>%
        mutate(percent_price = sumprice/sum(sumprice),percent_gdp = gdp/sum(gdp)) %>%
        select(city,percent_price,percent_gdp) %>%
        gather(key="measure",value="value",-city) %>%
        ggplot()+
        aes(x=city,y=value,fill= measure) +
        geom_bar(stat="identity",position="dodge",width=0.7)+
        labs(x = "City",y = "Value",title = " GDP vs. Price")+
        scale_y_continuous(breaks = seq(0,0.9,length.out=10),labels = c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%"))
      
      
    })


    output$p5s1<-renderPlot({
      #use the anti-join to remove the stop words
      tokenized_data <- cleaned_data %>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        select(id,text)%>%
        unnest_tokens(word,text)%>%
        anti_join(stop_words)

      tokenized_data %>%
         count(word,sort=TRUE)%>%
         with(wordcloud(word,n,min.freq = 500,max.word=30,, colors = c("#e06f69","#357b8a", "#7db5b8", "#59c6f3")))



    })
    output$p5s2<-renderPlot({
      cleaned_data%>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        ggplot(aes(ave_sentiment))+
        geom_histogram(aes(y=..density..),
                       bins=input$num_bin,
                       color="white",
                       fill="purple")+
        labs(x="Average Sentiment",
             y="Count",
             title="Average Sentiment Distribution")+
        coord_cartesian(xlim=c(0,1))+

        geom_density(alpha=.5,fill="#94c4d4")

    })


    output$p5s3<-renderPlot({
      #scatter plot
      cleaned_data%>%
        filter(host_since>=input$date_r[1])%>%
        filter(host_since<=input$date_r[2])%>%
        ggplot()+
        aes(x = ave_sentiment,y = price)+
        geom_point(color="#72a1b3",size=4,shape=3)+
        geom_smooth()+
        labs(x = "Average sentiment",
             y = "Price",
             title = "Average Sentiment vs. Price")

    })



      output$p5s4 <- renderPlot({
        cleaned_data %<>%
          select(-c("ADP","AUX","CCONJ","DET","INTJ","NUM","PART","PRON","PROPN","PUNCT","SCONJ","SYM","X"))
        #wide to long
        pos_data_long <-
          cleaned_data %>%
          gather(key="measure",value="value",names(cleaned_data)[34:37])
        #plot all histograms
        ggplot(pos_data_long,aes(value))+
          geom_histogram(aes(y=..density..),
                         bins=input$num_bin,
                         color="white",
                         fill="purple")+
          labs(x="Value",
               y="Density")+
          coord_cartesian(ylim=c(0,.1),xlim=c(0,150))+
          facet_wrap(~measure)



    })
      
    output$p6s1 <- renderPlot({
      reviewsNum <- cleaned_data %>%
        select(host_since,number_of_reviews)%>%
        group_by(date = host_since) %>%
        summarise(number = sum(number_of_reviews))

      ggplot(reviewsNum[year(reviewsNum$date) == input$year,], aes(date, number)) +
        geom_point(na.rm=TRUE, color = "#007A87", alpha=0.5) +geom_smooth(color = "#FF5A5F")+
        ggtitle("Seasonality in Demand") +
        labs(x = "Month", y = "Reviews") +
        theme(plot.caption = element_text(color = "black"))





    })


}

# Run the application
shinyApp(ui = ui, server = server)




