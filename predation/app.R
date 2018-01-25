# rsconnect::setAccountInfo(name='jaytlennon', token='F8514597B861B8FD199A346586E3EF58', secret='owstvIzgllZN3hxqTIXzXKOEwB7Mr0ayiPcOus6R')
# rsconnect::deployApp('/Users/lennonj/GitHub/community-modules/predation/')

library(shiny)
library(deSolve)
library(ggplot2)
library(tidyr)

# Define functions
predation <-function(times, init, parms) {
  with(as.list(c(init, parms)),{
    dN <- (r * N * (1 - N / K)) - (a * N * P)
    dP <- e * P * (a * N^2 /(1 + a * h * N^2)) - (m * P) 
    #dP <- (e * a * N * P) - (m * P) # neutrally stble equation
    list(c(dN, dP))
  })
}

# Construct Shiny App
ui <- fluidPage(
  titlePanel("Predation Model"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Model Options"),
      
      helpText("Parameters for predation model"),
      h4("Starting Values"),
      textInput("N", label = "Initial Prey Population Size, N", value = 1),
      textInput("P", label = "Initial Predator Population Size, P", value = 1),
      textInput("ts", label = "Number of time steps to simulate", value = 100),
      
      h4("Parameters"),
      sliderInput("r", label = "Intrinsic Rate of Increase, r",
                  min = 0, max = 2, value = 0.5, step = 0.1),
      sliderInput("K", label = "Carrying Capacity, K", 
                  min = 0, max = 100, value = 100, step = 1),
      sliderInput("e", label = "Predator conversion efficiency, e", 
                  min = 0, max = 1, value = 0.2, step = 0.05),
      sliderInput("a", label = "Predator attack rate, a",
                  min = 0, max = 1, value = 0.1, step = 0.05),
      sliderInput("h", label = "Predator handling time, h",
                  min = 0, max = 1, value = 0.1, step = 0.05),
      sliderInput("m", label = "Predator death rate, m",
                  min = 0, max = 2, value = 0.5, step = 0.1)
    ),
    
    mainPanel(
      plotOutput("dynamics"),
      plotOutput("phase")
    )
    )
)

server <- function(input, output) {
  
  # Run the model if parameters change
  dataInput <- reactive({
    init <- c(N = as.numeric(input$N), P = as.numeric(input$P))
    parms <- c(r = input$r, K = input$K, a = input$a, e = input$e, h = input$h,
               m = input$m)
    times <- seq(0, as.numeric(input$ts), 0.1)
    
    lsoda(y = init, times = times, func = predation, parms = parms)
  })
  
  # Plot the output if parameters change
  
  output$dynamics <- renderPlot({
    data <- as.data.frame(dataInput())
    data %>% gather(-time, key = species, value = density) %>%
    ggplot(aes(y = density, x = time, color = species)) + 
      geom_line(size = 2) + 
      scale_color_manual(values = c("red", "blue")) +
      theme_minimal() +
      ylab("Population Size (N)") + 
      xlab("Time (t)") + 
      theme(axis.title = element_text(size = 20),
            axis.text  = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 16))
 })
  
   output$phase <- renderPlot({
     data <- as.data.frame(dataInput())
     data %>% 
       ggplot(aes(y = P, x = N)) + 
       geom_path(size = 2) + 
       theme_minimal() +
       ylab("Predator Density, P") + 
       xlab("Prey Density, N") + 
       theme(axis.title = element_text(size = 20),
             axis.text  = element_text(size = 16))
  })
}

shinyApp(ui, server)
