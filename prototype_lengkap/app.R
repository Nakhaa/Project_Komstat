library(shiny)
library(ggplot2)

sd_Type <- c("Population", "Sample")
onePopTest <- c("Mean Test", "Proportion Test", "Variance Test")
properTest <- c("One-Tailed Left", "One-Tailed Right", "Two-Tailed")

popTest_tabs <- tabsetPanel(
  id = "popTest",
  type = "hidden",
  tabPanel("Mean Test",
    numericInput("mu", "Population Mean:", value = 400),
    numericInput("mean", "Sample Mean:", value = 398),
    selectInput("sd_Type", "Standard Deviation Type:", sd_Type),
    numericInput("sd", "Standard Deviation:", value = 35, min = 0.01),
  ),
  
  tabPanel("Proportion Test",
    numericInput("p_pop", "Population Proportion", value = 0.08),
    numericInput("p_sample", "Sample Proportion", value = 0.05),
  ),
  
  tabPanel("Variance Test",
    numericInput("var_pop", "Population Variance", value = 1),
    numericInput("var_sample", "Sample Variance", value = 2),
  )
)

onePopMeanTest_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("distributionPlot"))
  )
}

onePopMeanTest_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$calculate, {
      df <- input$n_sample -1
      testStatistics <- (input$mean - input$mu) / (input$sd/sqrt(input$n_sample))
      
      p_value <- {
        if((input$sd_Type == "Population") || (input$sd_Type == "Sample" & input$n_sample >= 30)) {
          2 * pnorm(abs(testStatistics), lower.tail = FALSE)
        } else {
          2 * pt(abs(testStatistics), df, lower.tail = FALSE)
        }
      }
      
      alpha <- 1 - input$conf_int
      
      critical_values <- {
        if ((input$sd_Type == "Population") || (input$sd_Type == "Sample" && input$n_sample >= 30)) {
          critical_val <- qnorm(1 - alpha/2)
          c(-critical_val, critical_val)
          
        } else {
          critical_val <- qt(1 - alpha/2, df)
          c(-critical_val, critical_val)
        }
      }
      
      quantile <- seq(-3.5, 3.5, length = 1000)
      lower_cv <- critical_values[1]
      upper_cv <- critical_values[2]
      
      if (input$sd_Type == "Population" || (input$sd_Type == "Sample" && input$n_sample > 30)){
        prob <- dnorm(quantile, mean = 0, sd = 1)
      } else {
        prob <- dt(quantile, df)
      }
      
      output$distributionPlot <- renderPlot({
        ggplot(data = data.frame(quantile, prob), aes(x = quantile, y = prob)) +
          geom_line(color = "black") +
          geom_area(data = subset(data.frame(quantile, prob), quantile < lower_cv), 
                    aes(x = quantile, y = prob), fill = "red", alpha = 0.5) +
          geom_area(data = subset(data.frame(quantile, prob), quantile > upper_cv), 
                    aes(x = quantile, y = prob), fill = "red", alpha = 0.5) +
          geom_vline(xintercept = lower_cv, linetype = "dashed") +
          geom_vline(xintercept = upper_cv, linetype = "dashed") +
          geom_vline(xintercept = testStatistics, linetype = "solid", color = "blue") +
          annotate("text", x = lower_cv, y = max(prob)*0.9, 
                   label = paste("CV =", round(lower_cv, 2)), hjust=1.1, size = 4, color="red") +
          annotate("text", x = upper_cv, y = max(prob)*0.9, 
                   label = paste("CV =", round(upper_cv, 2)), hjust=-0.1, size = 4, color="red") +
          annotate("text", x = testStatistics, y = max(prob)*0.7, 
                   label = paste("TS =", round(testStatistics, 2)), hjust = -0.1, size = 4, color = "blue") +
          labs(title = "Hypothesis Testing for the Population Mean",
               x = "Test Statistic",
               y = "Probability Density") +
          theme_minimal()
      })
    })
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("onePopTest", "Select Proper Population Test", choices = onePopTest, selected = "Mean Test"),
      selectInput("properTest", label = "Select Proper Population Test for the Mean", choices = properTest, selected = "Two-Tailed"),
      
      popTest_tabs,
      
      numericInput("n_sample", "Sample Size:", value = 40, min = 1),
      numericInput("conf_int", "Confidence interval", value = 0.95, min = 0, max=1),
      
      actionButton("calculate", label = "Calculate!")
    ),
    mainPanel(
      onePopMeanTest_ui("meanTest")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$onePopTest, {
    updateTabsetPanel(inputId = "popTest", selected = input$onePopTest)
    updateSelectInput(inputId = "properTest", label = paste("Select Proper Population Test for the", input$onePopTest))
  })
  
  onePopMeanTest_server(id = "meanTest")
}

shinyApp(ui, server)