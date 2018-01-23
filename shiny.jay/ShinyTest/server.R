library(shiny)
library(simecol)

shinyServer(function(input, output) {

  lvK <- new("odeModel",
            main = function (time, init, parms) {
              x <- init
              p <- parms
              dx1 <-   x[1] * p["r"] * (1 - (x[1]/p["K"])) - p["a"] * x[1] * x[2] 
              dx2 <-   p["c"] * p["a"] * x[1] * x[2] - p["m"] * x[2]
              list(c(dx1, dx2))
            },
            parms  = c(r=0.2, K=10, a = 0.1, c = 0.1, m = 0.1),
            times  = c(from=0, to=100, by=0.5),
            init   = c(prey=1, predator=1),
            solver = "rk4"
  )  
  
  times(lvK)["by"] <- 5
  times(lvK)["to"] <-10000

  
  
  output$distPlot <- renderPlot({
    
    parms(lvK) = c(r = input$r,
                   K = input$K,
                   a = input$a,
                   c = input$c,
                   m = input$m)
    
    par(mfrow = c(2,2))
    plot(out(sim(lvK))$prey ~ out(sim(lvK))$time, type = "l", xlim = c(0,input$time),
         ylim = c(max(0, min(out(sim(lvK))[2])), min(100000,max(out(sim(lvK))[2]))), main = "prey",
    )
    
    plot(out(sim(lvK))$predator ~ out(sim(lvK))$time, type = "l", xlim = c(0,input$time),
         ylim = c(max(0, min(out(sim(lvK))[3])), min(100000,max(out(sim(lvK))[3]))), main = "predator",
    )
    
    plot(out(sim(lvK))$prey,out(sim(lvK))$predator, main="Phase Diagram", type = "l",
                  xlab = "prey", ylab = "predator")
    
  })
})
