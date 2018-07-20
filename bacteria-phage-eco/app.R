# After modifying code and running app locally, you need to set up the account
# Visit https://www.shinyapps.io and get token then run following in console:
# rsconnect::setAccountInfo(name='lennonlab', token='F8514597B861B8FD199A346586E3EF58', secret='owstvIzgllZN3hxqTIXzXKOEwB7Mr0ayiPcOus6R')
# Then run following in console to deploy app
# rsconnect::deployApp('/Users/lennonj/GitHub/community-modules/bacteria-phage-eco/')
# You site should be ready to use: https://lennonlab.shinyapps.io/bacteria-phage-eco/

# Load packages
library(shiny)
library(deSolve)
library(ggplot2)
library(tidyr)

# Define functions
predation <-function(times, init, parms) {
  with(as.list(c(init, parms)),{
    dR <- (r * d) - (N * umax * (R/(Ks + R))) * (y * (N * umax * (R/(Ks + R)))) - (R * d)
    dN <- (N * umax * (R/(Ks + R))) - (P * N * a) - (N * d)
    dP <- (b * N * P * a) - (P * N * a) - (d * P) 
    list(c(dR, dN, dP))
  })
}

# Construct Shiny App
ui <- fluidPage(
  titlePanel("Chemostat Ecological Model"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Model Options"),
      
      helpText("Parameters for predation model"),
      h4("Starting Values"),
      textInput("R", label = "Initial Resource Concentration, R", value = 0.5),
      textInput("N", label = "Initial Host Population Size, N", value = 1000),
      textInput("P", label = "Initial Phage Population Size, P", value = 10000),
      textInput("ts", label = "Number of time steps to simulate", value = 1000),
      
      h4("Parameters"),
      sliderInput("d", label = "dilution rate, d",
                  min = 0.001, max = 0.5, value = 0.15, step = 0.01),
      sliderInput("r", label = "resource concentration (r)",
                  min = 0.01, max = 25, value = 0.5, step = 0.1),
      sliderInput("umax", label = "maximum growth rate (umax)", 
                  min = 0.05, max = 5, value = 0.75, step = 0.05),
      sliderInput("Ks", label = "half saturation constant (Ks)", 
                  min = 0.005, max = 1.5, value = 0.075, step = 0.005),
      sliderInput("y", label = "bacterial growth yield (y)",
                  min = 1e-07, max = 1e-05, value = 0.000002, step = 1e-06),
      sliderInput("b", label = "burst size (b)",
                  min = 10, max = 200, value = 80, step = 10),
      # a.ticks <- c(1e-07, 2.5e-07, 5e-07, 7.5e-07, 10e-07),
      # sliderInput(inputId = "a", label = "adsorption rate, a", min = 1, max = 5,
      #             value = 1, ticks = a.ticks, step =1)
    sliderInput("a", label = "adsorption rate (a)",
                min = 1e-07, max = 1e-05, value = 3e-06, step = 1e-06)
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
    init <- c(R = as.numeric(input$R), N = as.numeric(input$N), P = as.numeric(input$P))
    #init <- c(R = 0.5, N = 1000, P = 10000)
    #parms <- c( = input$r, K = input$K, a = input$a, e = input$e, h = input$h,
    #           m = input$m)
  parms <- c(d = input$d, umax = input$umax, Ks = input$Ks, b = input$b, 
             a = input$a, r = input$r, y = input$y)
  times <- seq(0, as.numeric(input$ts), 0.1)
    
  lsoda(y = init, times = times, func = predation, parms = parms)
  })
  
  # Plot the output if parameters change
  
  output$dynamics <- renderPlot({
    data <- as.data.frame(dataInput())
    data %>% gather(-time, key = species, value = density) %>%
    ggplot(aes(y = density, x = time, color = species)) + 
      geom_line(size = 2) + 
      scale_color_manual(values = c("red", "blue", "green")) +
      theme_minimal() +
      ylab("Population Size") + 
      xlab("Time (t)") + 
      scale_y_log10() +
      annotation_logticks(sides = "l") +
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
       ylab("Phage Density, P") + 
       xlab("Populaton Density") + 
       theme(axis.title = element_text(size = 20),
             axis.text  = element_text(size = 16))
  })
}

shinyApp(ui, server)
