library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Hypothesis Testing for Mean in 1 Population"),
  sidebarLayout(
    sidebarPanel(
      numericInput("mu", "Population Mean:", value = 400),
      numericInput("mean", "Sample Mean:", value = 398),
      numericInput("n_sample", "Sample Size:", value = 40, min = 1),
      numericInput("sd", "Sample Standard Deviation:", value = 35, min = 0.01),
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
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  df <- eventReactive(input$update, {
    input$n_sample - 1
  })
  
  t_hit <- eventReactive(input$update, {
    (input$mean - input$mu) / (input$sd/sqrt(input$n_sample))
  })
  
  critical_values <- eventReactive(input$update, {
    alpha <- input$alpha
    t_critical <- qt(1-alpha/2, df())
    c(-t_critical, t_critical)
  })
  
  output$plot <- renderPlot({
    input$update
    
    t <- seq(-3.5, 3.5, length = 1000)
    prob <- dt(t, df())
    
    cv <- critical_values()
    lower_cv <- cv[1]
    upper_cv <- cv[2]
    
    ggplot(data = data.frame(t, prob), aes(x = t, y = prob)) +
      geom_line(color = "black") +
      geom_area(data = subset(data.frame(t, prob), t < lower_cv), aes(x = t, y = prob), fill = "red", alpha = 0.5) +
      geom_area(data = subset(data.frame(t, prob), t > upper_cv), aes(x = t, y = prob), fill = "red", alpha = 0.5) +
      geom_vline(xintercept = lower_cv, linetype = "dashed") +
      geom_vline(xintercept = upper_cv, linetype = "dashed") +
      geom_vline(xintercept = t_hit(), linetype = "solid", color = "blue") +
      annotate("text", x = lower_cv, y = 0.17, label = paste("CV =", round(lower_cv, 2)),hjust=1.1, size = 4, color="red")+
      annotate("text", x = upper_cv, y = 0.17, label = paste("CV =", round(lower_cv, 2)),hjust=-0.1, size = 4, color="red")+
      annotate("text", x = t_hit(), y = 0.1, label = paste("TS =", round(t_hit(), 2)), hjust = -0.1, size = 4, color = "blue") +
      labs(title = "Hypothesis Testing for the Population Mean",
           x = "Z-score (CLT)",
           y = "Probability") +
      theme_minimal()
  })
  
}

shinyApp(ui = ui, server = server)