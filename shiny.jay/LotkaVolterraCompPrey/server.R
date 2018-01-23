library(shiny)
library(simecol)
library(scatterplot3d)

# Define server logic 
shinyServer(function(input, output) {

  
  
  lvK <- new("odeModel",
            main = function (time, init, parms) {
              x <- init
              p <- parms
              dx1 <-   x[1] * p["r1"] * (1 - (x[1] / p["K1"]) - ((p["a21"] * x[2])/p["K1"])) - p["e1"] * x[1] * x[3]
              dx2 <-   x[2] * p["r2"] * (1 - (x[2] / p["K2"]) - ((p["a12"] * x[1])/p["K2"])) - p["e2"] * x[2] * x[3]
              dx3 <-   (p["c"] * ((p["e1"] * x[1]) + (p["e2"] * x[2]))) - p["m"] * x[3]
              list(c(dx1, dx2, dx3))
            },
            parms  = c(r1=0.2, r2 = 0.2, K1 = 10, K2 = 10,  a12 = 1.1, a21 = 1.1, 
                       e1 = 0.1, e2 = 0.1, c = 0.1, m = 0.1),
            times  = c(from=0, to=100, by=0.5),
            init   = c(prey1 = 1, prey2 = 1, predator = 1),
            solver = "rk4"
  )  
  
  times(lvK)["by"] <- 1
  times(lvK)["to"] <-10000

  
  
  dataInput <- reactive({
    
    init(lvK)["prey1"] <- .1
    init(lvK)["prey2"] <- ifelse(input$spN == 3||input$spN == 4,.1,0)
    init(lvK)["predator"] <- ifelse(input$spN == 2||input$spN == 4,.1,0)
    
    parms(lvK) = c(r1 = input$r1,
                   r2 = ifelse(input$spN == 3||input$spN == 4,input$r2,0),
                   K1 = input$K1,
                   K2 = ifelse(input$spN == 3||input$spN == 4,input$K2,1),
                   a12 = ifelse(input$spN == 3||input$spN == 4,input$a12,0),
                   a21 = ifelse(input$spN == 3||input$spN == 4,input$a21,0),
                   e1 = ifelse(input$spN == 2||input$spN == 4,input$e1,0),
                   e2 = ifelse(input$spN == 4,input$e2,0),
                   c = ifelse(input$spN == 2||input$spN == 4,input$c,0),
                   m =ifelse(input$spN == 2||input$spN == 4,input$m,1)
                   )

    out(sim(lvK))
  })

  
  output$distPlot <- renderPlot({    

  par(mfrow = c(2,2))
    
    col1 = ifelse(input$spN == 3||input$spN == 4, "forestgreen", "white")
    col2 = ifelse(input$spN == 2||input$spN == 4, "firebrick", "white")
    
        plot(dataInput()$prey1 ~ dataInput()$time, type = "l", lwd = 2, col = "dodgerblue", xlim = c(0,input$time),
                   main = "", xlab = "time", ylab = "population size",
                   ylim = c(0, 
                            ifelse(input$spN == 1, max(dataInput()$prey1),
                              ifelse(input$spN == 2, max(c(dataInput()$prey1,dataInput()$predator)),
                                ifelse(input$spN == 3, max(c(dataInput()$prey1,dataInput()$prey2)),
                                       max(c(dataInput()$prey1,dataInput()$prey2,dataInput()$predator))
                                )))))
                                                          
                                                          

                            
         lines(dataInput()$prey2 ~ dataInput()$time, type = "l", lwd = 2,
               col = col1, xlim = c(0,input$time))
          
         lines(dataInput()$predator ~ dataInput()$time, type = "l", lwd = 2,
                col = col2, xlim = c(0,input$time))
  
  legend("topright", c("Prey 1","Prey 2", "Predator"), lty=c(1,1),  lwd=c(2.5,2.5),
         col=c("dodgerblue", col1, col2))
          
  plot(1, type="n", axes=F, xlab="", ylab="")
  
    scatterplot3d(dataInput()$prey1[1:input$time],dataInput()$prey2[1:input$time],dataInput()$predator[1:input$time], main="Phase Diagram", type = "l",
                  xlab = "prey1", ylab = "prey2", zlab = "predator", col.axis = "grey", lwd = 2)
    
    
  })
})

