library(shiny)
library(ggplot2)

sd_Type <- c("Population", "Sample")

ui <- fluidPage(
  titlePanel("Hypothesis Testing for Mean in 1 Population"),
  sidebarLayout(
    sidebarPanel(
      numericInput("mu", "Population Mean:", value = 400),
      numericInput("mean", "Sample Mean:", value = 398),
      numericInput("n_sample", "Sample Size:", value = 40, min = 1),
      selectInput("sd_Type", "Standard Deviation Type:", sd_Type),
      numericInput("sd", "Standard Deviation:", value = 35, min = 0.01),
      numericInput(
        "alpha",  
        "Significance Level (alpha):", 
        value = 0.05, 
        min = 0.01, 
        max = 0.1
      ),
      actionButton("update", "Update Plot")
    ),
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("results")
    )
  )
)

server <- function(input, output) {
  df <- eventReactive(input$update, {
    input$n_sample - 1
  })
  
  statistics_test <- eventReactive(input$update, {
    (input$mean - input$mu) / (input$sd/sqrt(input$n_sample))
  })
  
  p_value <- eventReactive(input$update, {
    ts <- statistics_test()
    if(input$sd_Type == "Population") {
      2 * pnorm(abs(ts), lower.tail = FALSE)
    } else if(input$sd_Type == "Sample") {
      if (input$n_sample > 30) {
        2 * pnorm(abs(ts), lower.tail = FALSE)
      } else {
        2 * pt(abs(ts), df(), lower.tail = FALSE)
      }
    }
  })
  
  critical_values <- eventReactive(input$update, {
    alpha <- input$alpha
    if (input$sd_Type == "Population"){
      critical_val <- qnorm(1 - alpha/2)
    } else {
      if (input$n_sample > 30){
        critical_val <- qnorm(1 - alpha/2)
      } else {
        critical_val <- qt(1 - alpha/2, df())
      }
    }
    c(-critical_val, critical_val)
  })
  
  output$plot <- renderPlot({
    input$update
    
    statistics <- seq(-4, 4, length = 1000)
    
    if (input$sd_Type == "Population" || (input$sd_Type == "Sample" && input$n_sample > 30)){
      prob <- dnorm(statistics, mean = 0, sd = 1)
    } else {
      prob <- dt(statistics, df())
    }
    
    cv <- critical_values()
    lower_cv <- cv[1]
    upper_cv <- cv[2]
    
    ggplot(data = data.frame(statistics, prob), aes(x = statistics, y = prob)) +
      geom_line(color = "black") +
      geom_area(data = subset(data.frame(statistics, prob), statistics < lower_cv), 
                aes(x = statistics, y = prob), fill = "red", alpha = 0.5) +
      geom_area(data = subset(data.frame(statistics, prob), statistics > upper_cv), 
                aes(x = statistics, y = prob), fill = "red", alpha = 0.5) +
      geom_vline(xintercept = lower_cv, linetype = "dashed") +
      geom_vline(xintercept = upper_cv, linetype = "dashed") +
      geom_vline(xintercept = statistics_test(), linetype = "solid", color = "blue") +
      annotate("text", x = lower_cv, y = max(prob)*0.9, 
               label = paste("CV =", round(lower_cv, 2)), hjust=1.1, size = 4, color="red") +
      annotate("text", x = upper_cv, y = max(prob)*0.9, 
               label = paste("CV =", round(upper_cv, 2)), hjust=-0.1, size = 4, color="red") +
      annotate("text", x = statistics_test(), y = max(prob)*0.7, 
               label = paste("TS =", round(statistics_test(), 2)), hjust = -0.1, size = 4, color = "blue") +
      labs(title = "Hypothesis Testing for the Population Mean",
           x = "Test Statistic",
           y = "Probability Density") +
      theme_minimal()
  })
  
  output$results <- renderPrint({
    cat("Test Statistic (TS):", round(statistics_test(), 4), "\n")
    cat("P-Value:", round(p_value(), 4), "\n")
    cat("Critical Values:", round(critical_values(), 4), "\n")
    if(p_value() < input$alpha){
      cat("Kesimpulan: Tolak H0")
    } else {
      cat("Kesimpulan: Gagal Tolak H0")
    }
  })
}

shinyApp(ui = ui, server = server)
