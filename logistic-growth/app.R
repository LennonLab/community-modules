# rsconnect::setAccountInfo(name='jaytlennon', token='F8514597B861B8FD199A346586E3EF58', secret='owstvIzgllZN3hxqTIXzXKOEwB7Mr0ayiPcOus6R')
# rsconnect::deployApp('/Users/lennonj/GitHub/community-modules/logistic-growth/')

library(shiny)
library(deSolve)
library(ggplot2)

# Define functions
growth <-function(time, init, parms) {
  with(as.list(c(init, parms)),{
    dN <- r * N * (1 - N / K)
    list(dN)
  })
}

# Construct Shiny App
ui <- fluidPage(
  titlePanel("Logistic Growth Model"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Model Options"),
      
      helpText("Select parameters for the logistic growth model."),
      
      h4("Starting Values"),
      textInput("N", label = "Initial Population Size, N", value = 1),
      textInput("ts", label = "Number of time steps to simulate", value = 100),
      
      h4("Set Growth Parameters"),
      sliderInput("K", label = "Carrying Capacity, K", 
                  min = 0, max = 100, value = 100, step = 1),
      sliderInput("r", label = "Intrinsic Rate of Increase, r",
                  min = 0, max = 2, value = 0.5, step = 0.1)
    ),
    
    mainPanel(
      plotOutput("dynamics")
    )
    )
)

server <- function(input, output) {
  
  # Run the model if parameters change
  dataInput <- reactive({
    init <- c(N = as.numeric(input$N))
    parms <- c(r = input$r, K = input$K)
    times <- seq(0, as.numeric(input$ts), 1)
    
    lsoda(y = init, times = times, func = growth, parms = parms)
  })
  
  # Plot the output if parameters change
  output$dynamics <- renderPlot({
    data <- as.data.frame(dataInput())
    ggplot(data, aes(y = N, x = time)) + 
      geom_line(size = 2) + 
      theme_minimal() +
      ylab("Population Size (N)") + 
      xlab("Time (t)") + 
      theme(axis.title = element_text(size = 20),
            axis.text  = element_text(size = 16))
  })
}

shinyApp(ui, server)
