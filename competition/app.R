# rsconnect::setAccountInfo(name='jaytlennon', token='F8514597B861B8FD199A346586E3EF58', secret='owstvIzgllZN3hxqTIXzXKOEwB7Mr0ayiPcOus6R')
# rsconnect::deployApp('/Users/lennonj/GitHub/community-modules/competition/')

library(shiny)
library(deSolve)
library(ggplot2)
library(tidyr)

# Define functions
competition <-function(times, init, parms) {
  with(as.list(c(init, parms)),{
    dN1 <- r1 * N1 * ((K1-N1-a21*N2)/K1)
    dN2 <- r2 * N2 * ((K2-N2-a12*N1)/K2)
    list(c(dN1, dN2))
  })
}

# Construct Shiny App
ui <- fluidPage(
  titlePanel("Competition Model"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Model Options"),
      
      helpText("Select parameters for the competition model."),
      h4("Starting Values"),
      textInput("N1", label = "Initial Population Size, N1", value = 1),
      textInput("N2", label = "Initial Population Size, N2", value = 1),
      textInput("ts", label = "Number of time steps to simulate", value = 100),
      
      h4("Set Growth Parameters"),
      sliderInput("r1", label = "Intrinsic Rate of Increase, r1",
                  min = 0, max = 2, value = 0.5, step = 0.1),
      sliderInput("r2", label = "Intrinsic Rate of Increase, r2",
                  min = 0, max = 2, value = 0.5, step = 0.1),
      sliderInput("K1", label = "Carrying Capacity, K1", 
                  min = 0, max = 100, value = 100, step = 1),
            sliderInput("K2", label = "Carrying Capacity, K2", 
                  min = 0, max = 100, value = 100, step = 1),
      sliderInput("a21", label = "Competition coefficient, a21",
                  min = 0, max = 1, value = 0.1, step = 0.1),
      sliderInput("a12", label = "Competition coefficient, a12",
                  min = 0, max = 1, value = 0.1, step = 0.1)
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
    init <- c(N1 = as.numeric(input$N1), N2 = as.numeric(input$N2))
    parms <- c(r1 = input$r1, r2 = input$r2, K1 = input$K1, K2 = input$K2,
               a21 = input$a21, a12 = input$a12)
    times <- seq(0, as.numeric(input$ts), 1)
    
    lsoda(y = init, times = times, func = competition, parms = parms)
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
  # })
  # 
  # 
  # output$phase <- renderPlot({
  #   data <- as.data.frame(dataInput())
  #   data %>% 
  #     ggplot(aes(y = N1, x = N2)) + 
  #     geom_line(size = 2) + 
  #     theme_minimal() +
  #     ylab("N2") + 
  #     xlab("N1") + 
  #     theme(axis.title = element_text(size = 20),
  #           axis.text  = element_text(size = 16))
  })
}

shinyApp(ui, server)
