# https://www.r-bloggers.com/shiny-desolve-and-ggplot-play-nicely-together/
# https://github.com/mbjoseph/2hostSIR/blob/master/server.R
# https://github.com/mkiang/DiseaseDynamics/blob/master/server.R
# http://onlinelibrary.wiley.com/doi/10.1002/psp4.12052/full
require(shiny)
require(deSolve)
require(ggplot2)

# Define server logic 
shinyServer(function(input, output) {

# Define model  
  growth <- function(parms, times) {
    derivs <- function(times, state, parms){
    with(as.list(c(state, parms)),{
      dN <- r * N * (1 - N / K)
      return(list(dN))
    }
    )
  }
  
    state <- c(N = 0.1)
    parms <- c(r = 0.1, K = 10)
    times <- seq(0, 100, 1)
  
 return(ode(y, times, derivs, parms))
  }

  
  
  
  
  output$guessPlot <- reactivePlot(function() {
    times <- seq(0, input$tmax, by = input$tint)
    out <- growth(parms,times)
    d <- 
    
    guess_pars<-c(A = input$A, B = input$B, C = input$C)
    #     alpha<-input$alpha
    guess <- as.data.frame(solveLorenz(guess_pars, tout))
    print(ggplot(as.data.frame(guess)) + geom_path(aes(X, Y, col=Z), alpha=input$alpha, lwd=.3))
    #     plot(guess$X, guess$Y)
  })
  
})


  
#   dataInput <- reactive({
#     
#     # init(lvK)["prey1"] <- .1
#     # init(lvK)["prey2"] <- ifelse(input$spN == 3||input$spN == 4,.1,0)
#     # init(lvK)["predator"] <- ifelse(input$spN == 2||input$spN == 4,.1,0)
#     
#     parms(growth) = c(r = input$r,
#                    K = input$K,
#                    )
# 
#    # out(sim(lvK))
#   })
# 
#   
#   output$distPlot <- renderPlot({    
# 
#   par(mfrow = c(1,1))
#     
#     # col1 = ifelse(input$spN == 3||input$spN == 4, "forestgreen", "white")
#     # col2 = ifelse(input$spN == 2||input$spN == 4, "firebrick", "white")
#     
#         plot(dataInput()$prey1 ~ dataInput()$time, type = "l", lwd = 2, col = "dodgerblue", xlim = c(0,input$time),
#                    main = "", xlab = "time", ylab = "population size",
#                    ylim = c(0, 
#                             ifelse(input$spN == 1, max(dataInput()$prey1),
#                               ifelse(input$spN == 2, max(c(dataInput()$prey1,dataInput()$predator)),
#                                 ifelse(input$spN == 3, max(c(dataInput()$prey1,dataInput()$prey2)),
#                                        max(c(dataInput()$prey1,dataInput()$prey2,dataInput()$predator))
#                                 )))))
#                                                           
#                                                           
# 
#                             
#          lines(dataInput()$prey2 ~ dataInput()$time, type = "l", lwd = 2,
#                col = col1, xlim = c(0,input$time))
#           
#          lines(dataInput()$predator ~ dataInput()$time, type = "l", lwd = 2,
#                 col = col2, xlim = c(0,input$time))
#   
#   legend("topright", c("Prey 1","Prey 2", "Predator"), lty=c(1,1),  lwd=c(2.5,2.5),
#          col=c("dodgerblue", col1, col2))
#           
#   #plot(1, type="n", axes=F, xlab="", ylab="")
#   
#    # scatterplot3d(dataInput()$prey1[1:input$time],dataInput()$prey2[1:input$time],dataInput()$predator[1:input$time], main="Phase Diagram", type = "l",
#    #              xlab = "prey1", ylab = "prey2", zlab = "predator", col.axis = "grey", lwd = 2)
#     
#     
#   })
# })
# 
