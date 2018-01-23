# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Lotka Volterra Simulations"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    
    radioButtons("spN", label = h2("Choose your [fake] community"),
                 choices = list("Prey only" = 1, "Prey + Predator" = 2, "Prey 1 + Prey 2" = 3, 
                                "Prey 1 + Prey 2 + Predator" = 4),selected = 1),
 
           sliderInput("r1",
                       "Growth rate of prey 1:",
                       min = 0, max = 1,value = 0.2, step = 0.1),
    

           sliderInput("r2",
                       "Growth rate of prey 2:",
                       min = 0, max = 1,value = 0.2, step = 0.1),
    

           sliderInput("K1",
                       "Carrying capacity of prey 1:",
                       min = 10,max = 100, value = 20),
    
    

           sliderInput("K2",
                       "Carrying capacity of prey 2:",
                       min = 10,max = 100, value = 20),
    

         sliderInput("a21",
                     "Competitive effect of prey 2 on prey 1 
                     (values greater than 1 indicate inter- > intra-specific competition):",
                     min = 0, max = 2, value = 1, step = .1),
  

         sliderInput("a12",
                     "Competitive effect of prey 1 on prey 2
                     (values greater than 1 indicate inter- > intra-specific competition):",
                     min = 0, max = 2, value = 1, step = .1),
  

         sliderInput("e1",
                     "Encounter rate of predator on prey 1:",
                     min = 0, max = 1, value = .1, step = 0.1),

         sliderInput("e2",
                     "Encounter rate of predator on prey 2:",
                     min = 0, max = 1, value = .1, step = 0.1),


            sliderInput("c",
                       "Assimilation efficiency of predator:",
                       min = 0.05, max = 1, value = .15, step = 0.05),
    

           sliderInput("m",
                       "Mortality rate of predator:",
                       min = 0,max = 1, value = 0.1, step = 0.1),


         sliderInput("time",
                     "Model runtime (steps):",
                     min = 100,max = 1000, value = 100, step = 100)
  


  ),
   

mainPanel(
  
  plotOutput("distPlot", height = 800, width = 800)
)
))



