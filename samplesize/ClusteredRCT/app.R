source("global.R")



ui <- navbarPage("Sample size",
                 tabPanel("Clustered RCT",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("type_trait", "Trait type", c("Continuous", "Binary"), inline = T),
                              wellPanel(
                                h4("Base information"),
                                uiOutput("info_trait"),
                                radioButtons("alpha", "Alpha", c(0.05, 0.1), selected = 0.05, inline = T),
                                radioButtons("tail", "Tailed-tests", c("One-sided" = T, "Two-sided" = F), selected = F, inline = T),
                                radioButtons("power", "Power (1-beta)", c(0.8, 0.9), selected = 0.8, inline = T)
                              ),
                              wellPanel(
                                h4("Cluster information"),
                                numericInput("m", "Cluster size", value = 4),
                                #sliderInput("icc", "Intra-cluster coefficient (ICC)", min = 0, max = 1, value = 0.05)
                                radioButtons("icc", "Intra-cluster coefficient (ICC)", c("Continuois: 교육대 (0.2747)" = 0.2747, "Outcome 1: ARI (0.0506)" = 0.0506, "Outcome 2: HAdV (0.0653)" = 0.0653), selected = 0.2747, inline = T)
                                
                              )
                            ),
                            mainPanel(
                              withLoader(DTOutput("table"), type="html", loader="loader6"),
                              wellPanel(
                                h5("References"),
                                h5("Rutterford, C., Copas, A., & Eldridge, S. (2015). Methods for sample size determination in cluster randomized trials. International Journal of Epidemiology, 44(3), 1051–1067. doi:10.1093/ije/dyv113"),
                                h5("Wu, S., Crespi, C. M., & Wong, W. K. (2012). Comparison of methods for estimating the intraclass correlation coefficient for binary responses in cancer prevention cluster randomized trials. Contemporary Clinical Trials, 33(5), 869–880. doi:10.1016/j.cct.2012.05.004")
                              )
                            )
                          )
                 )
                 
                 
                 
)


server <- function(input, output, session) {
  
  observeEvent(input$type_trait, {
    output$info_trait <- renderUI({
      if (input$type_trait == "Binary"){
        tagList(
          sliderInput("p1", "P1: value of control group ", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("p2", "P2: value of intervention group ", min = 0, max = 1, value = 0.2, step = 0.01)
        )
      } else{
        tagList(
          numericInput("mu1", "Mean 1: control group", value = 54.1431),
          numericInput("mu2", "Mean 2: intervention group", value = 54.1431 * 0.8),
          numericInput("sd", "Standard deviation (common)", value = 9.91519)
        )
      }
    })
  })
    
    
  n_sample <- reactive({
    z_alpha <- ifelse(input$tail, qnorm(1 - as.numeric(input$alpha)), qnorm(1 - as.numeric(input$alpha)/2))
    z_beta <- qnorm(as.numeric(input$power))
    
    if (input$type_trait == "Binary"){
      p1 <- input$p1
      p2 <- input$p2
      n_sample_old <- ((z_alpha + z_beta)^2 * (p1*(1 - p1) + p2*(1 - p2))) / (p1 - p2)^2
    } else{
      d <- (input$mu1 - input$mu2)/input$sd
      n_sample_old <- (z_alpha + z_beta)^2 * 2 / d^2 
    }
    return(n_sample_old * (1 + (input$m -1)*as.numeric(input$icc))) 
  })
  
  output$table <- renderDT({
    
    if (input$type_trait == "Binary"){
      out <- data.frame("value" = c(input$p1, input$p2, round(n_sample())))
      rownames(out) <- c("P1", "P2", "Sample size per arm")
      cap.tail <- ifelse(input$tail, "one-sided", "two-sided")
      cap <- paste("Sample size calculation for clustered RCT: binary traits -", "alpha ", input$alpha, "_power", input$power, "_", cap.tail, "_clustersize_", input$m, "_icc_", input$icc, sep="")
    } else{
      out <- data.frame("value" = c(input$mu1, input$mu2, input$sd, round(n_sample())))
      rownames(out) <- c("Mean 1", "Mean 2", "SD", "Sample size per arm")
      cap.tail <- ifelse(input$tail, "one-sided", "two-sided")
      cap <- paste("Sample size calculation for clustered RCT: continuous traits -", "alpha ", input$alpha, "_power", input$power, "_", cap.tail, "_clustersize_", input$m, "_icc_", input$icc, sep="")
    }
    
    datatable(out, rownames = T, extensions = "Buttons", caption = cap,
              options = c(jstable::opt.tb1(cap),
                          list(scrollX = TRUE)
                          )
                          
              ) %>% formatStyle(0, target = "row" ,backgroundColor = styleEqual("Sample size per arm", "yellow"))
    
    
  })
  
  
}

shinyApp(ui, server)