options(shiny.sanitize.errors = F, warnings = F)
library(kappaSize);library(shiny);library(shinycustomloader)

ui <- navbarPage("Sample size with Kappa",
                 tabPanel("Power based",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("ncat", "Number of outcome categories", 2:5, inline = T),
                              uiOutput("prev"),
                              radioButtons("nrater", "Number of raters", 2:6, selected = 3, inline = T),
                              numericInput("kappa0", "Kappa: null hypothesis", value = 0.0001, min = 0.0001, max = 0.9999),
                              numericInput("kappa1", "Kappa: alternative hypothesis", value = 0.389, min = 0.0001, max = 0.9999),
                              sliderInput("alpha", "Type I error", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                              sliderInput("beta", "Power (1 - Type II error)", value = 0.8, min = 0.7, max = 0.95, step = 0.05)
                            ),
                            mainPanel(
                              withLoader(textOutput("nsample"), type="html", loader="loader6"),
                              tags$style(type="text/css", "#nsample { height: 50px; width: 100%; text-align:center; font-size: 30px;}"),
                              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                              wellPanel(
                                h4(strong("Reference")),
                                h5("Michael A Rotondi (2018). kappaSize: Sample Size Estimation Functions for Studies of Interobserver Agreement. R package version 1.2."),
                                a("https://CRAN.R-project.org/package=kappaSize", href="https://CRAN.R-project.org/package=kappaSize")
                                
                              )
                            )
                          )
                          
                 ),
                 tabPanel("CI based",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("ncat_ci", "Number of outcome categories", 2:5, inline = T),
                              uiOutput("prev_ci"),
                              radioButtons("nrater_ci", "Number of raters", 2:6, selected = 3, inline = T),
                              numericInput("kappa_ci", "Kappa to test", value = 0.733, min = 0.0001, max = 0.9999),
                              numericInput("interval_ci", "Bound (±)", value = 0.1, min = 0, max = 0.5, step = 0.01),
                              sliderInput("alpha_ci", "Type I error", value = 0.05, min = 0.01, max = 0.1, step = 0.01)
                            ),
                            mainPanel(
                              withLoader(textOutput("nsample_ci"), type="html", loader="loader6"),
                              tags$style(type="text/css", "#nsample_ci { height: 50px; width: 100%; text-align:center; font-size: 30px;}"),
                              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                              wellPanel(
                                h4(strong("Reference")),
                                h5("Michael A Rotondi (2018). kappaSize: Sample Size Estimation Functions for Studies of Interobserver Agreement. R package version 1.2."),
                                a("https://CRAN.R-project.org/package=kappaSize", href="https://CRAN.R-project.org/package=kappaSize")
                                
                              )
                            )
                          )
                          
                 )
                 
)


server <- function(input, output, session) {
  
  output$prev <- renderUI({
    ncat <- as.integer(input$ncat)
    if (ncat == 2){
      numericInput("props1", "Prevalence", value = 0.2, min = 0.01, max = 0.99, step = 0.01)
    } else if (ncat == 3){
      tagList(
        strong("Prevalence of event categories: Sum must be 1"),
        splitLayout(
          cellWidths = c("33.3%", "33.3%", "33.3%"),
          numericInput(paste0("props", 1), 1, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 2), 2, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 3), 3, value = 1/ncat, min = 0.01, max = 0.99)
        ) 
      )
    } else if (ncat == 4){
      tagList(
        strong("Prevalence of event categories: Sum must be 1"),
        splitLayout(
          cellWidths = c("25%", "25%", "25%", "25%"),
          numericInput(paste0("props", 1), 1, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 2), 2, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 3), 3, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 4), 4, value = 1/ncat, min = 0.01, max = 0.99)
        )
      )
    } else{
      tagList(
        strong("Prevalence of event categories: Sum must be 1"),
        splitLayout(
          cellWidths = c("20%", "20%", "20%", "20%", "20%"),
          numericInput(paste0("props", 1), 1, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 2), 2, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 3), 3, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 4), 4, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 5), 5, value = 1/ncat, min = 0.01, max = 0.99)
        )
      )
    }
  })
  
  
  CalcFunc <- reactive({
    switch(input$ncat,
           "2" = kappaSize::PowerBinary,
           "3" = kappaSize::Power3Cats,
           "4" = kappaSize::Power4Cats,
           "5" = kappaSize::Power5Cats)
    
  })

  observeEvent(CalcFunc(),{
    output$nsample <- renderText({
      validate(
        need((input$kappa0 >= 0 & input$kappa0 <= 1 & input$kappa1 >= 0 & input$kappa1 <= 1), "Kappa must be in range 0-1")
      )
      req(input$props1)
      ncat <- as.integer(input$ncat)
      props <- input$props1
      if (ncat >2){
        props <- sapply(1:ncat, function(x){input[[paste0("props", x)]]})
      }
      
      out <- CalcFunc()(input$kappa0, input$kappa1, props = props, raters=as.integer(input$nrater), alpha=input$alpha, power=input$beta)
      
      return(paste0("A minimum of ", round(out$N)[1], " subjects are required for this study of interobserver agreement."))
    })
    
    
  })
  
  
  
  output$prev_ci <- renderUI({
    ncat <- as.integer(input$ncat_ci)
    if (ncat == 2){
      numericInput("props1_ci", "Prevalence", value = 0.2, min = 0.01, max = 0.99, step = 0.01)
    } else if (ncat == 3){
      tagList(
        strong("Prevalence of event categories: Sum must be 1"),
        splitLayout(
          cellWidths = c("33.3%", "33.3%", "33.3%"),
          numericInput(paste0("props", 1, "_ci"), 1, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 2, "_ci"), 2, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 3, "_ci"), 3, value = 1/ncat, min = 0.01, max = 0.99)
        ) 
      )
    } else if (ncat == 4){
      tagList(
        strong("Prevalence of event categories: Sum must be 1"),
        splitLayout(
          cellWidths = c("25%", "25%", "25%", "25%"),
          numericInput(paste0("props", 1, "_ci"), 1, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 2, "_ci"), 2, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 3, "_ci"), 3, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 4, "_ci"), 4, value = 1/ncat, min = 0.01, max = 0.99)
        )
      )
    } else{
      tagList(
        strong("Prevalence of event categories: Sum must be 1"),
        splitLayout(
          cellWidths = c("20%", "20%", "20%", "20%", "20%"),
          numericInput(paste0("props", 1, "_ci"), 1, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 2, "_ci"), 2, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 3, "_ci"), 3, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 4, "_ci"), 4, value = 1/ncat, min = 0.01, max = 0.99),
          numericInput(paste0("props", 5, "_ci"), 5, value = 1/ncat, min = 0.01, max = 0.99)
        )
      )
    }
  })
  
  
  CalcFunc_ci <- reactive({
    switch(input$ncat,
           "2" = kappaSize::CIBinary,
           "3" = kappaSize::CI3Cats,
           "4" = kappaSize::CI4Cats,
           "5" = kappaSize::CI5Cats)
    
  })
  
  observeEvent(CalcFunc_ci(),{
    output$nsample_ci <- renderText({
      validate(
        need((input$kappa_ci >= 0 & input$kappa_ci <= 1), "Kappa must be in range 0-1")
      )
      req(input$props1_ci)
      ncat <- as.integer(input$ncat_ci)
      props <- input$props1_ci
      if (ncat >2){
        props <- sapply(1:ncat, function(x){input[[paste0("props", x, "_ci")]]})
      }
      
      out <- CalcFunc_ci()(input$kappa_ci, input$kappa_ci - input$interval_ci, input$kappa_ci + input$interval_ci, props = props, raters=as.integer(input$nrater_ci), alpha=input$alpha_ci)
      
      return(paste0("A minimum of ", round(out$n)[1], " subjects are required for this study of interobserver agreement."))
    })
    
    
  })
  
}


shinyApp(ui, server)