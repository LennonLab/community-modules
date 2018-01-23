# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Exponential and Logistic Growth"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    
    radioButtons("spN", label = h2("Choose growth type"),
                 choices = list("Exponential" = 1, "Logistic" = 2),selected = 1),
 
           sliderInput("r",
                       "Growth rate of population 1:",
                       min = 0, max = 1,value = 0.2, step = 0.1),
    

           sliderInput("K",
                       "Carrying capcity:",
                       min = 0, max = 100,value = 20, step = 2),
  ),
   

mainPanel(
  
  plotOutput("distPlot", height = 800, width = 800)
)
))



