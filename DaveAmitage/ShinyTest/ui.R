shinyUI(fluidPage(
  titlePanel("LotkaVolterraTest"),
  
  fluidRow(
    
    column(3, 
           sliderInput("r",
                       "prey growth rate:",
                       min = 0,max = 1,value = 0.25)),
    
    column(3, 
           sliderInput("K",
                       "prey carrying capacity:",
                       min = 1,max = 50,value = 10)),

    column(3, 
           sliderInput("a",
                       "prey capture rate:",
                       min = 0,max = 1, value = 0.2))
  ),
  
fluidRow(
  
    column(3, 
            sliderInput("c",
                       "prey to predator conversion efficiency:",
                       min = 0.01,max = 1, value = .15)),
    
    column(3, 
           sliderInput("m",
                       "predator mortality rate:",
                       min = 0,max = 1, value = 0.075)),
    
    column(3, 
           sliderInput("time",
                       "years:",
                       min = 100,max = 10000, value = 100, step = 100))

  ),
  

mainPanel(
  plotOutput("distPlot")
)
))
  



