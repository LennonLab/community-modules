library(shiny)
library(simecol)
library(ggplot2)

# Define server logic 
shinyServer(function(input, output) {

  
  
  lvNoK <- new("odeModel",
             main = function (time, init, parms) {
               x <- init
               p <- parms
               dx1 <-   x[1] * p["r"]  - (p["a"]/(1+p["a"]*p["h"] * x[2])) * x[1] * x[2] 
               dx2 <-   ((p["c"] * p["a"])/(1+p["a"]*p["h"] * x[2])) * x[1] * x[2] - p["m"] * x[2]
               list(c(dx1, dx2))
             },
             parms  = c(r=0.2, a = 0.1, c = 0.1, m = 0.1, h = 1),
             times  = c(from=0, to=100, by=0.5),
             init   = c(prey=1, predator=1),
             solver = "rk4"
  )  
  
  lvK <- new("odeModel",
            main = function (time, init, parms) {
              x <- init
              p <- parms
              dx1 <-   x[1] * p["r"] * (1 - (x[1]/p["K"])) - (p["a"]/(1+p["a"]*p["h"] * x[2])) * x[1] * x[2] 
              dx2 <-   ((p["c"] * p["a"])/(1+p["a"]*p["h"] * x[2])) * x[1] * x[2] - p["m"] * x[2]
              list(c(dx1, dx2))
            },
            parms  = c(r=0.2, K=10, a = 0.1, c = 0.1, m = 0.1, h = 1),
            times  = c(from=0, to=100, by=0.5),
            init   = c(prey=1, predator=1),
            solver = "rk4"
  )  
  
  times(lvK)["by"] <- 1
  times(lvK)["to"] <-10000
  
  times(lvNoK)["by"] <- 1
  times(lvNoK)["to"] <-10000
  

  
  dataInput <- reactive({
    parms(lvK) = c(r = input$r,
                   K = input$K,
                   a = input$a,
                   c = input$c,
                   m = input$m,
                   h = input$h)
    
    parms(lvNoK) = c(r = input$r,
                   a = input$a,
                   c = input$c,
                   m = input$m,
                   h = input$h)
    
    out(sim(lvK))
  })
  
  output$distPlot <- renderPlot({    
    
    par(mfrow = c(2,2))
    plot(dataInput()$prey ~ dataInput()$time, type = "l", xlim = c(0,input$time),
          main = "prey")
    
    plot(dataInput()$predator ~ dataInput()$time, type = "l", xlim = c(0,input$time),
          main = "predator")
    
    plot(dataInput()$prey,dataInput()$predator, main="Phase Diagram", type = "l",
                  xlab = "prey", ylab = "predator")
    
  })
})

