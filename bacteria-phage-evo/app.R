# rsconnect::setAccountInfo(name='jaytlennon', token='F8514597B861B8FD199A346586E3EF58', secret='owstvIzgllZN3hxqTIXzXKOEwB7Mr0ayiPcOus6R')
# rsconnect::deployApp('/Users/lennonj/GitHub/community-modules/bacteria-phage-evo/')

library(shiny)
library(deSolve)
library(ggplot2)
library(tidyr)

# Define functions
predation <-function(times, init, parms) {
  with(as.list(c(init, parms)),{
    dR <- (r * d) - (Ns * umax * (R/(Ks + R))) * (y * (Ns * umax * (R/(Ks + R)))) - (Nr * umax * (1-uc) * (R/(Ks + R))) * (y * (Nr * umax * (1-uc) * (R/(Ks + R)))) - (R * d)
    dNs <- (Ns * umax * (R/(Ks + R))) - (P * Ns * a) - (Ns * d)
    dNr <- (Nr * ((1-uc) * umax) * (R/(Ks + R))) - (P * Nr * a * (1-ac)) - (Nr * d)
    dP <- (b * Ns * P * a) - (P * Ns * a) + (b * Nr * P * a * (1-ac)) - (P * Nr * a * (1-ac)) - (d * P) 
    list(c(dR, dNs, dNr, dP))
  })
}

# Construct Shiny App
ui <- fluidPage(
  titlePanel("Chemostat Evolution Model"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Model Options"),
      
      helpText("Parameters for predation model"),
      h4("Starting Values"),
      textInput("R", label = "Initial Resource Concentration, R", value = 0.5),
      textInput("Ns", label = "Initial Host Population Size - Sensitive, Ns", value = 1000),
      textInput("Nr", label = "Initial Host Population Size - Resistant, Nr", value = 1),
      textInput("P", label = "Initial Phage Population Size, P", value = 10000),
      textInput("ts", label = "Number of time steps to simulate", value = 1000),
      
      h4("Parameters"),
      sliderInput("d", label = "dilution rate (d)",
                  min = 0.001, max = 0.5, value = 0.15, step = 0.01),
      sliderInput("r", label = "resource concentration (r)",
                  min = 0.01, max = 25, value = 0.5, step = 0.1),
      sliderInput("umax", label = "maximum growth rate (umax)", 
                  min = 0.05, max = 5, value = 0.75, step = 0.05),
      sliderInput("uc", label = "cost of resistance on umax (%)", 
                  min = 0, max = 1, value = 0.1, step = 0.01),
      sliderInput("Ks", label = "half saturation constant (Ks)", 
                  min = 0.005, max = 1.5, value = 0.075, step = 0.005),
      sliderInput("y", label = "bacterial growth yield (y)",
                  min = 1e-07, max = 1e-05, value = 0.000002, step = 1e-06),
      sliderInput("b", label = "burst size (b)",
                  min = 10, max = 200, value = 80, step = 10),
      sliderInput("a", label = "adsorption rate (a)",
                  min = 1e-07, max = 1e-05, value = 3e-06, step = 1e-06),
      sliderInput("ac", label = "reduced absorption on mutant (%)", 
                  min = 0, max = 1, value = 0.8, step = 0.01)
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
  init <- c(R = as.numeric(input$R), Ns = as.numeric(input$Ns), Nr = as.numeric(input$Nr), P = as.numeric(input$P))
  parms <- c(d = input$d, umax = input$umax, Ks = input$Ks, b = input$b, 
             a = input$a, r = input$r, y = input$y, uc = input$uc, ac = input$ac)
  times <- seq(0, as.numeric(input$ts), 0.1)
    
  lsoda(y = init, times = times, func = predation, parms = parms)
  })
  
  # Plot the output if parameters change

  output$dynamics <- renderPlot({
    data <- as.data.frame(dataInput())
    data %>% gather(-time, key = species, value = density) %>%
    ggplot(aes(y = density, x = time, color = species)) +
      geom_line(size = 2) +
      scale_color_manual(values = c("red", "blue", "green", "orange")) +
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
       ggplot(aes(y = Nr, x = Ns)) +
       geom_path(size = 2) +
       theme_minimal() +
       ylab("Nr") +
       xlab("Ns") +
       theme(axis.title = element_text(size = 20),
             axis.text  = element_text(size = 16))
  })
}

shinyApp(ui, server)
