library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Conditional Tab Switch Example"),
  sidebarPanel(
    wellPanel(
      selectInput(inputId = "variable1",label = "Select First Variable:", 
                  choices = c("Binary Variable 1 (Factor)" = "binary1",
                              "Binary Variable 2 (Factor)" = "binary2", 
                              "Continuous Variable 1 (Numeric)" = "cont1",
                              "Continuous Variable 2 (Numeric)" = "cont2"),
                  selected = "Binary Variable 1 (Factor)"
      )
    ),
    
    wellPanel(
      checkboxInput("bivariate", "Proceed to Bivariate Analysis", FALSE),
      conditionalPanel(
        condition="input.bivariate==true",
        selectInput(inputId = "variable2", 
                    label = "Select Second Variable:",
                    choices = c("Binary Variable 1 (Factor)" = "binary1",
                                "Binary Variable 2 (Factor)" = "binary2", 
                                "Continuous Variable 1 (Numeric)" = "cont1",
                                "Continuous Variable 2 (Numeric)" = "cont2"),
                    selected = "Binary Variable 2 (Factor)"
        )
      )
    )
  ),
  
  mainPanel(
    h5("Output"),
    tabsetPanel(id ="analysisTabs",
                tabPanel(title = "Univariate Numeric", value="panel_uni_numeric",
                         h4(" Univariate Numeric"),                       
                         verbatimTextOutput("uni_numeric")),
                tabPanel(title = "Univariate Factor", value="panel_uni_factor",
                         h4(" Univariate Factor"),                       
                         verbatimTextOutput("uni_factor")),
                tabPanel(title = "Bivariate Numeric-Numeric", value="panel_bi_nn",
                         h4(" Bivariate Numeric Numeric"),                       
                         verbatimTextOutput("bi_numeric1_numeric2")),
                tabPanel(title = "Bivariate Factor-Factor", value="panel_bi_ff",
                         h4(" Bivariate Factor Factor"),                       
                         verbatimTextOutput("bi_factor1_factor2")),
                tabPanel(title = "Bivariate Numeric-Factor", value="panel_bi_nf",
                         h4(" Bivariate Numeric Factor"),                       
                         verbatimTextOutput("bi_numeric1_factor2")),
                tabPanel(title = "Bivariate Factor-Numeric", value="panel_bi_fn",
                         h4(" Bivariate Factor Numeric"),                       
                         verbatimTextOutput("bi_factor1_numeric2"))
                
    )
  )  
))