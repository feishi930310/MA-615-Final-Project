library(shiny)
library(shinythemes) 
shinyUI(navbarPage( theme = shinytheme("lumen"),
                    
                    h5("Trump Tweet Analysis"),
                    
                    tabPanel(
                             sidebarLayout( 
                               
                               sidebarPanel( 
                              
                                 helpText("After change options please put button Query on the bottom"),
                                 selectInput("mini", label = h3("Please select time unit for time series plot"), 
                                             choices = list('Hour'=1,'Minute'=2), selected = 1),
                                 
                                 sliderInput("s2", h3("maximum words in word cloud"),
                                             min = 10, max = 300, value = 100),
                                 sliderInput("s3", h3("number of top words in log-ratio plot"),
                                             min = 2, max = 20, value = 15),
                                 radioButtons("ci", 
                                              label = h3("Confidence in possion test"), 
                                              choices = list("90%" = 1, 
                                                             "95%" = 2,
                                                             "99%" = 3)),
                           submitButton(h4("Query"))
                               ),                                   
                            
                               mainPanel( "Plots&Tables",
                                 tabsetPanel( 
                                   tabPanel("Time series", plotOutput("f1")),
                                   tabPanel("Word cloud before", plotOutput("f2")),
                                   tabPanel("Word cloud after", plotOutput("f3")),
                                   tabPanel("Words Different", plotOutput("f4")),
                                   tabPanel("Possion Test", dataTableOutput("tb1")),
                                   tabPanel("Scatterplot",  plotOutput("f5")),
                                   tabPanel("Error bars", plotOutput("f6"))
                                 
                                   
                               )
                             )               
                             
                             
                    
                    
                    
))
))