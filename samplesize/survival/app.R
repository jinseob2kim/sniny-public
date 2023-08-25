options(shiny.sanitize.errors = F, warnings = F)
library(gsDesign);library(shiny);library(shinycustomloader)

ui <- navbarPage("Sample size: survival analysis",
                 tabPanel("Log-rank based",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("lambda1", "Event rate: control group (/1 person-year)", value = 0.036, min = 0, max = 1),
                              numericInput("lambda2", "Event rate: treatment group (/1 person-year)", value = 0.06, min = 0, max = 1),
                              numericInput("Ts", "Maximum study duration", value = 4, min = 0, max = NA),
                              numericInput("Tr", "Accrual (recruitment) duration (≤ maximum study duration)", value = 3, min = 0, max = NA),
                              numericInput("eta", "Equal dropout rate for both groups.", value = 0.02, min = 0, max = NA),
                              numericInput("ratio", "Ratio: treatment/control", value = 1, min = 0, max = NA),
                    
                              radioButtons("type", "Sample size based on", c("hazard ratio" = "rr", "hazard difference" = "rd"), selected = "rr", inline = T),
                              radioButtons("entry", "Patient entry type", c("uniform" = "unif"), selected = "unif", inline = T),
                              radioButtons("sided", "One or two-sided test", c("one-sided" = 1, "two-sided" = 2), selected = 2, inline = T),
                              sliderInput("alpha", "Type I error", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                              sliderInput("power", "Power (1 - Type II error)", value = 0.8, min = 0.7, max = 0.95, step = 0.01)
                            ),
                            mainPanel(
                              withLoader(textOutput("nsample"), type="html", loader="loader6"),
                              tags$style(type="text/css", "#nsample { height: 50px; width: 100%; text-align:center; font-size: 30px;}"),
                              br(),br(),
                              withLoader(verbatimTextOutput("nsample_rconsole"), type="html", loader="loader6"),
                              
                              h3("This app use", a("nsurvival()", href = "https://www.rdocumentation.org/packages/gsDesign/versions/3.0-1/topics/nSurvival"), "function in", tags$b("gsDesign"), "package"),
                              wellPanel(
                                h4(strong("Reference")),
                                h5("Lachin JM and Foulkes MA (1986), Evaluation of Sample Size and Power for Analyses of Survival with Allowance for Nonuniform Patient Entry, Losses to Follow-Up, Noncompliance, and Stratification.", em("Biometrics"), ", 42, 507-519. Schoenfeld D (1981), The Asymptotic Properties of Nonparametric Tests for Comparing Survival Distributions.", em("Biometrika"), ", 68, 316-319."),
                                a("https://CRAN.R-project.org/package=gsDesign", href="https://CRAN.R-project.org/package=gsDesign")
                                
                              )
                            )
                          )
                          
                 )
                 
)


server <- function(input, output, session) {
  
  out.nsurvival <- reactive({
    validate(
      need((input$lambda1 > 0 & input$lambda1 < 1 & input$lambda2 > 0 & input$lambda2 < 1), "Both event rates must be in range 0-1")
    )
    validate(
      need((input$Ts > 0 & input$Tr > 0), "Both duration must be > 0")
    )
    
    validate(
      need((input$eta >= 0 & input$eta < 1), "Drop rate must be in range 0-1 (include 0)")
    )
    validate(
      need((input$ratio > 0), "Treatment/control ratio must be > 0")
    )
    
    out <- gsDesign::nSurvival(lambda1= input$lambda1 , lambda2=input$lambda2 , eta = input$eta, Ts = input$Ts, Tr = input$Tr, ratio = input$ratio, sided= as.integer(input$sided), alpha= input$alpha, beta = 1 - input$power,
                               type = input$type, entry = input$entry)
    return(out)
  })
  
  output$nsample <- renderText({
    
    return(paste0("A minimum of ", round(out.nsurvival()$n), " subjects are required for this study"))
  })
  
  output$nsample_rconsole <- renderPrint({
    out.nsurvival()
  })  
  
  
}


shinyApp(ui, server)