binary1 <- rbinom(100,1,0.5)
binary2 <- rbinom(100,1,0.5)
cont1   <- rnorm(100)
cont2   <- rnorm(100)

dat <- as.data.frame(cbind(binary1, binary2, cont1, cont2))

dat$binary1 <- as.factor(dat$binary1)
dat$binary2 <- as.factor(dat$binary2)
dat$cont1 <- as.numeric(dat$cont1)
dat$cont2 <- as.numeric(dat$cont2)

library(shiny)
#library(rCharts)

shinyServer(function(input, output, session) {
  
  inputVar1 <- reactive({
    parse(text=sub(" ","",paste("dat$", input$variable1)))
  })
  
  inputVar2 <- reactive({
    parse(text=sub(" ","",paste("dat$", input$variable2)))
  })
  
  
  output$uni_factor = renderText({
    if ( (input$bivariate==FALSE) & (is.factor(eval(inputVar1()))==TRUE) ) { 
      table(eval(inputVar1()))
    }
  })
  output$uni_numeric = renderPrint({
    if( (input$bivariate==FALSE) & (is.numeric(eval(inputVar1()))==TRUE) ) {
      summary(eval(inputVar1()))
    }
  })
  output$bi_factor1_factor2 = renderText({
    if ( (input$bivariate==TRUE) & (is.factor(eval(inputVar1()))==TRUE) & (is.factor(eval(inputVar2()))==TRUE) ) {
      table(eval(inputVar1()), eval(inputVar2()))
    }    
  })
  output$bi_numeric1_numeric2 = renderPrint({
    if ( (input$bivariate==TRUE) & (is.numeric(eval(inputVar1()))==TRUE) & (is.numeric(eval(inputVar2()))==TRUE) ) { 
      cor(eval(inputVar1()), eval(inputVar2()))
    }
  })
  output$bi_numeric1_factor2 = renderPrint({
    if ( (input$bivariate==TRUE) & (is.numeric(eval(inputVar1()))==TRUE) & (is.factor(eval(inputVar2()))==TRUE) ) { 
      by(eval(inputVar2()), eval(inputVar1()), summary)
    }
  })
  output$bi_factor1_numeric2 = renderPrint({
    if ( (input$bivariate==TRUE) & (is.factor(eval(inputVar1()))==TRUE) & (is.numeric(eval(inputVar2()))==TRUE) ) { 
      by(eval(inputVar1()), eval(inputVar2()), summary)
    }
  })
  
  
  observe({  
    if ( (input$bivariate==FALSE) & (is.factor(eval(inputVar1()))==TRUE) ) {
      print("uni f")
      updateTabsetPanel(session, inputId="analysisTabs", selected="panel_uni_factor")
    } 
    else if( (input$bivariate==FALSE) & (is.numeric(eval(inputVar1()))==TRUE) ) {
      print("uni n")
      updateTabsetPanel(session, inputId="analysisTabs", selected="panel_uni_numeric")
    } 
    else if ( (input$bivariate==TRUE) & (is.factor(eval(inputVar1()))==TRUE) & (is.factor(eval(inputVar2()))==TRUE) ) {
      print("bi f f")
      updateTabsetPanel(session, inputId="analysisTabs", selected="panel_bi_ff")
    }
    else if ( (input$bivariate==TRUE) & (is.numeric(eval(inputVar1()))==TRUE) & (is.numeric(eval(inputVar2()))==TRUE) ) {
      print("bi n n")
      updateTabsetPanel(session, inputId="analysisTabs", selected="panel_bi_nn")
    } 
    else if ( (input$bivariate==TRUE) & (is.factor(eval(inputVar1()))==TRUE) & (is.numeric(eval(inputVar2()))==TRUE) ) {
      print("bi f n")
      updateTabsetPanel(session, inputId="analysisTabs", selected="panel_bi_fn")
    } 
    else if ( (input$bivariate==TRUE) & (is.numeric(eval(inputVar1()))==TRUE) & (is.factor(eval(inputVar2()))==TRUE) ) {
      print("bi n f")
      updateTabsetPanel(session, inputId="analysisTabs", selected="panel_bi_nf")
    }
    
  })#end observe
  
})