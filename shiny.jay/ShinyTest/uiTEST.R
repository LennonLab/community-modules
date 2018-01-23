library(shiny)
library(simecol)


# Define UI for application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("LV Test!"),
  
  # Sidebar with a slider input for the number of bins
  fluidRow(
    
  column(2,
              sliderInput("b",
                "resource growth rate:",
                  min = 0,max = 1,value = 0.1),
    
              sliderInput("c",
                "prey population growth rate:",
                min = 0,max = 1,value = 0.1),
         
  

          ),

  fluidRow(

  column(2
            sliderInput("d",
              "encounter rate of predator and prey:",
              min = 0,max = 1, value = 0.1),
  
            sliderInput("f",
              "death rate of predator population:",
              min = 0,max = 1,value = 0.1)
         ),


))


    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
