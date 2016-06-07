library(shiny)
df<-read.csv("equity data.csv",header=TRUE)
choices_text<- c("Low Income Female Life Expectancy",
                 "Low Income Male Life Expectancy",
                 "Low Income Smoking Percent",
                 "Low Income Obesity Rate",
                 "Low Income Exercise in last 30 days")
shinyUI(fluidPage(
  img(src = "GLP_logo.png", align= "right"),
  titlePanel("Poverty Data Explorer"),
  p("An online data visualization tool from the", a("Greater Louisville Project", href="http://greaterlouisvilleproject.com/", target="_blank")),

  sidebarLayout(
    sidebarPanel(
      helpText("Select two variables to compare"),
      
      selectInput("var1", "Variable 1:",choices = choices_text, 
                  selected="Male Life Expectancy, Low Income"),
      
      selectInput("var2", "Variable 2:", choices = choices_text,
                  selected="Smoking, Low Income"),
      
      helpText("The next two variables will be subtracted"),
      
      selectInput("var3", "Difference Between:", choices = choices_text,
                  selected="Male Life Expectancy, Low Income"),
      
      selectInput("var4", "And:", choices = choices_text,
                  selected="Female Life Expectancy, Low Income"),
      
      selectInput("peer_list","Peer City List:", choices=c("Current", "Baseline"),
                  selected= "Current"),
      
      p("Data is from the", a("Brookings Institution,", href="http://www.brookings.edu/research/interactives/2016/five-evils-multidimensional-poverty-race"), a("Robert Wood Johnson Foundation,", href="http://www.countyhealthrankings.org/"), "and", a("Health Inequality Project.", href="https://healthinequality.org/"), "St. Louis is a population-weighted average of St. Louis County and St. Louis City.")
      
      #use bottom two to allow automatic subtraction
      #add another ranking graph for the subtracted var
      #also add a scatterplot of var 1 and subtracted var
      #and allow the variables to be the titles of the graphs for jpeg download
    ),
    
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Variable 1 Rankings", plotOutput("rank1"),
                           p("Cities are sorted into green, yellow, and red using natural breaks to group cities together on similar levels, such that green represents a group of cities that are above average, yellow a group clustering around average, and red those substantially below average.")),
                  tabPanel("Variable 2 Rankings", plotOutput("rank2"),
                           p("Cities are sorted into green, yellow, and red using natural breaks to group cities together on similar levels, such that green represents a group of cities that are above average, yellow a group clustering around average, and red those substantially below average.")),
                  tabPanel("Difference Rankings", plotOutput("rank3")),
                  tabPanel("Variables 1 and 2 Scatterplot", plotOutput("scatter1")),
                  tabPanel("Variable 1 and Difference Scatterplot", plotOutput("scatter2"))
                        )
  )
)))