df<-read.csv("equity data.csv",header=TRUE)
library(shiny)
shinyUI(fluidPage(
  titlePanel("Explore Equity Data"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select Two Variables to Compare"),
      
      selectInput("var1", "Variable 1:",choices = names(df), 
                  selected="cur_smoke_q1"),
      
      selectInput("var2", "Variable 2:", choices = names(df),
                  selected="le_agg_q1_F"),
      
      selectInput("var3", "Subtract This:", choices = names(df),
                  selected="W.Low.income"),
      
      selectInput("var4", "From This:", choices = names(df),
                  selected="B.Low.income"),
      
      selectInput("peer_list","Peer City List:", choices=c("Current", "Baseline"),
                  selected= "Current")
      #use bottom two to allow automatic subtraction
      #add another ranking graph for the subtracted var
      #also add a scatterplot of var 1 and subtracted var
      #and allow the variables to be the titles of the graphs for jpeg download
    ),
    
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Variable 1 Rankings", plotOutput("plot2"),
                           p("Cities are sorted into green, yellow, and red using natural breaks to group cities together on similar levels, such that green represents a group of cities that are above average, yellow a group clustering around average, and red those substantially below average.")),
                  tabPanel("Variable 2 Rankings", plotOutput("plot3"),
                           p("Cities are sorted into green, yellow, and red using natural breaks to group cities together on similar levels, such that green represents a group of cities that are above average, yellow a group clustering around average, and red those substantially below average.")),
                  tabPanel("Subtracted rankings", plotOutput("plot4")),
                  tabPanel("Var1 and Var2 Scatterplot", plotOutput("plot1")),
                  tabPanel("Var1 and Subtracted", plotOutput("plot5"))
                        )
  )
)))