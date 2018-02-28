library(shiny)
library(simecol)
library(scatterplot3d)

shinyServer(function(input, output) {
  
  
  data(lv3)
  times(lv3)["by"] <- 5
  times(lv3)["to"] <- 1000
  
  
  output$distPlot <- renderPlot({
    parms(lv3) = c(b = input$b,
                   c = input$c,
                   d = input$d,
                   
                   e = .1,
                   f = input$f,
                   g = 0)

    # draw the histogram with the specified number of bins
par(mfrow = c(2,2))
    plot(out(sim(lv3, hmax = 1))$s ~ out(sim(lv3))$time, type = "l",
         ylim = c(max(0, min(out(sim(lv3))[2])), min(100000,max(out(sim(lv3))[2]))), main = "resource",
    )
    
    plot(out(sim(lv3, hmax = 1))$p ~ out(sim(lv3))$time, type = "l",
         ylim = c(max(0, min(out(sim(lv3))[3])), min(100000,max(out(sim(lv3))[3]))), main = "prey",
         )
    
    plot(out(sim(lv3, hmax = 1))$k ~ out(sim(lv3))$time, type = "l",
         ylim = c(max(0, min(out(sim(lv3))[4])), min(100000,max(out(sim(lv3))[4]))), main = "predator",
    )

scatterplot3d(out(sim(lv3))$s,out(sim(lv3))$p,out(sim(lv3))$k, main="Phase Diagram", type = "l",
              xlab = "resource", ylab = "prey", zlab = "predator")

  })
})