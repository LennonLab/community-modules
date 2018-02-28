shinyUI(fluidPage(
  titlePanel("Predator-Prey model example"),
  
  fluidRow(
    
    column(3, 
           sliderInput("r",
                       "Growth rate of prey:",
                       min = 0.01, max = 1,value = 0.2, step = 0.01)),
    
    column(3, 
           checkboxInput("CarCap", "Density-dependent prey growth?", value = FALSE)),
    
    column(3, 
           sliderInput("K",
                       "Carrying capacity of prey:",
                       min = 1,max = 100, value = 10)),

    column(3, 
           sliderInput("a",
                       "Capture rate of prey by predator:",
                       min = 0,max = 1, value = 0.2, step = 0.01))
    

  ),
  
fluidRow(
  
  column(3, 
         sliderInput("h",
                     "Handling time of prey by predator:",
                     min = 0,max = 10, value = 0)),
    column(3, 
            sliderInput("c",
                       "Assimilation efficiency of predator:",
                       min = 0.01, max = 1, value = .15, step = 0.05)),
    
    column(3, 
           sliderInput("m",
                       "Mortality rate of predator:",
                       min = 0,max = 1, value = 0.075, step = 0.01))


  ),
  
fluidRow(
  
  column(3, 
         sliderInput("time",
                     "model runtime:",
                     min = 100,max = 1000, value = 100, step = 100))
  

         
),

mainPanel(
  plotOutput("distPlot", height = 800, width = 800)
)
))
  



