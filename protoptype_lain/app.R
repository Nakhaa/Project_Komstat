library(shiny)
library(ggplot2)

sd_Type <- c("Population", "Sample")
onePopTest <- c("Mean Test", "Proportion Test", "Variance Test")
properTest <- c("One-Tailed Left", "One-Tailed Right", "Two-Tailed")

# UI Components
popTest_tabs <- tabsetPanel(
  id = "popTest",
  type = "hidden",
  tabPanel("Mean Test",
           numericInput("mu", "Population Mean Hypothesis:", value = 400),
           numericInput("mean", "Sample Mean:", value = 398),
           selectInput("sd_Type", "Standard Deviation Type:", sd_Type),
           numericInput("sd", "Standard Deviation:", value = 35, min = 0.01)
  ),
  tabPanel("Proportion Test",
           numericInput("p_pop", "Population Proportion Hypothesis", value = 0.08),
           numericInput("p_sample", "Sample Proportion", value = 0.05)
  ),
  tabPanel("Variance Test",
           numericInput("var_pop", "Population Variance", value = 0.8),
           numericInput("var_sample", "Sample Variance", value = 1.2)
  )
)

# Module UI Plot Distribution
distributionPlot_ui <- function(id) {
  ns <- NS(id)  # Namespace for module
  tagList(
    plotOutput(ns("distributionPlot"))
  )
}

# Module Server One Population Mean Test
onePopMeanTest_server <- function(id, mu, mean, sd, n_sample, sd_Type, conf_int, calculate, properTest) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(calculate(), {  # Ensure button click triggers
      df <- n_sample() - 1
      testStatistics <- (mean() - mu()) / (sd() / sqrt(n_sample()))
      
      alpha <- 1 - conf_int()
      
      switch (properTest(),
        "Two-Tailed" = {
          critical_values <- if (sd_Type() == "Population" || n_sample() >= 30) {
            critical_val <- qnorm(1 - alpha / 2)
            c(-critical_val, critical_val)
          } else {
            critical_val <- qt(1 - alpha / 2, df)
            c(-critical_val, critical_val)
          }
          
          lower_cv = critical_values[1]
          upper_cv = critical_values[2]
        },
        
        "One-Tailed Left" = {
          lower_cv <- if (sd_Type() == "Population" || n_sample() >= 30) {
            critical_val <- qnorm(1 - alpha)
            c(-critical_val)
          } else {
            critical_val <- qt(1 - alpha, df)
            c(-critical_val)
          }
          
          upper_cv = NA
        },
        
        "One-Tailed Right" = {
          upper_cv <- if (sd_Type() == "Population" || n_sample() >= 30) {
            critical_val <- qnorm(1 - alpha)
            c(critical_val)
          } else {
            critical_val <- qt(1 - alpha, df)
            c(critical_val)
          }
          
          lower_cv = NA
        }
      )
      
      quantile <- seq(-3.5, 3.5, length = 1000)
      
      prob <- if (sd_Type() == "Population" || n_sample() > 30) {
        dnorm(quantile, mean = 0, sd = 1)
      } else {
        dt(quantile, df)
      }
      
      output$distributionPlot <- renderPlot({
        ggplot(data = data.frame(quantile, prob), aes(x = quantile, y = prob)) +
          geom_line(color = "black") +
          
          # Shaded critical regions - check for valid critical values
          geom_area(data = subset(data.frame(quantile, prob), !is.na(lower_cv) & quantile < lower_cv), 
                    aes(x = quantile, y = prob), fill = "red", alpha = 0.5) +
          geom_area(data = subset(data.frame(quantile, prob), !is.na(upper_cv) & quantile > upper_cv), 
                    aes(x = quantile, y = prob), fill = "red", alpha = 0.5) +
          
          # Vertical lines for critical values - check for valid critical values
          { if (!is.na(lower_cv)) geom_vline(xintercept = lower_cv, linetype = "dashed", color = "red") } +
          { if (!is.na(upper_cv)) geom_vline(xintercept = upper_cv, linetype = "dashed", color = "red") } +
          
          # Vertical line for test statistic
          geom_vline(xintercept = testStatistics, linetype = "solid", color = "blue") +
          
          # Annotations - check for valid critical values
          { if (!is.na(lower_cv)) annotate("text", x = lower_cv, y = max(prob)*0.9, 
                                           label = paste("CV =", round(lower_cv, 2)), hjust = 1.1, size = 4, color = "red") } +
          { if (!is.na(upper_cv)) annotate("text", x = upper_cv, y = max(prob)*0.9, 
                                           label = paste("CV =", round(upper_cv, 2)), hjust = -0.1, size = 4, color = "red") } +
          annotate("text", x = testStatistics, y = max(prob)*0.7, 
                   label = paste("TS =", round(testStatistics, 2)), hjust = -0.1, size = 4, color = "blue") +
          
          # Plot labels and theme
          labs(title = "Hypothesis Testing for the Population Mean",
               x = "Test Statistic",
               y = "Probability Density") +
          theme_minimal()
      })
    })
  })
}

# Module Server One Population Proportion Test
onePopPropTest_server <- function(id, p_pop, p_sample, n_sample, conf_int, calculate, properTest){
  moduleServer(id, function(input, output, session){
    observeEvent(calculate(), {
      testStatistics <- (p_sample() - p_pop())/(sqrt((p_pop()*(1-p_pop()))/n_sample()))
      
      alpha <- 1 - conf_int()
      
      switch (properTest(),
        "Two-Tailed" = {
          critical_values <- qnorm(1 - alpha / 2)
          cv <- c(-critical_values, critical_values)
          
          lower_cv = cv[1]
          upper_cv = cv[2]
        },
        
        "One-Tailed Left" = {
          lower_cv <- -(qnorm(1 - alpha))
          upper_cv = NA
        },
        
        "One-Tailed Right" = {
          upper_cv <- qnorm(1 - alpha)
          lower_cv = NA
        }
      )
      
      quantile <- seq(-3.5, 3.5, length = 1000)
      
      prob <- dnorm(quantile, mean = 0, sd = 1)
      
      output$distributionPlot <- renderPlot({
        ggplot(data = data.frame(quantile, prob), aes(x = quantile, y = prob)) +
          geom_line(color = "black") +
          
          # Shaded critical regions - check for valid critical values
          geom_area(data = subset(data.frame(quantile, prob), !is.na(lower_cv) & quantile < lower_cv), 
                    aes(x = quantile, y = prob), fill = "red", alpha = 0.5) +
          geom_area(data = subset(data.frame(quantile, prob), !is.na(upper_cv) & quantile > upper_cv), 
                    aes(x = quantile, y = prob), fill = "red", alpha = 0.5) +
          
          # Vertical lines for critical values - check for valid critical values
          { if (!is.na(lower_cv)) geom_vline(xintercept = lower_cv, linetype = "dashed", color = "red") } +
          { if (!is.na(upper_cv)) geom_vline(xintercept = upper_cv, linetype = "dashed", color = "red") } +
          
          # Vertical line for test statistic
          geom_vline(xintercept = testStatistics, linetype = "solid", color = "blue") +
          
          # Annotations - check for valid critical values
          { if (!is.na(lower_cv)) annotate("text", x = lower_cv, y = max(prob)*0.9, 
                                           label = paste("CV =", round(lower_cv, 2)), hjust = 1.1, size = 4, color = "red") } +
          { if (!is.na(upper_cv)) annotate("text", x = upper_cv, y = max(prob)*0.9, 
                                           label = paste("CV =", round(upper_cv, 2)), hjust = -0.1, size = 4, color = "red") } +
          annotate("text", x = testStatistics, y = max(prob)*0.7, 
                   label = paste("TS =", round(testStatistics, 2)), hjust = -0.1, size = 4, color = "blue") +
          
          # Plot labels and theme
          labs(title = "Hypothesis Testing for the Population Proportion",
               x = "Test Statistic",
               y = "Probability Density") +
          theme_minimal()
      })
    })
  })
}

# Module Server One Population variance Test
onePopVarTest_server <- function(id, var_pop, var_sample, n_sample, conf_int, calculate, properTest) {
  moduleServer(id, function(input, output, session){
    observeEvent(calculate(), {
      df <- n_sample() - 1
      testStatistics <- ((n_sample() - 1)* var_sample()) / (var_pop())
      
      alpha <- 1 - conf_int()
      
      switch(properTest(),
       "Two-Tailed" = {
         lower_cv <- qchisq(alpha / 2, df, lower.tail = T)
         upper_cv <- qchisq(alpha / 2, df, lower.tail = F)
       },
       "One-Tailed Left" = {
         lower_cv <- qchisq(alpha, df, lower.tail = T)
         upper_cv <- NA
       },
       "One-Tailed Right" = {
         upper_cv <- qchisq(alpha, df, lower.tail = F)
         lower_cv <- NA
       }
      )
      
      # Dynamic quantile range
      quantile <- seq(0, qchisq(0.999, df), length = 1000)
      prob <- dchisq(quantile, df)
      
      output$distributionPlot <- renderPlot({
        ggplot(data = data.frame(quantile, prob), aes(x = quantile, y = prob)) +
          geom_line(color = "black") +
          
          # Shaded critical regions
          geom_area(data = subset(data.frame(quantile, prob), !is.na(lower_cv) & quantile < lower_cv), 
                    aes(x = quantile, y = prob), fill = "red", alpha = 0.5) +
          geom_area(data = subset(data.frame(quantile, prob), !is.na(upper_cv) & quantile > upper_cv), 
                    aes(x = quantile, y = prob), fill = "red", alpha = 0.5) +
          
          # Vertical lines for critical values
          { if (!is.na(lower_cv)) geom_vline(xintercept = lower_cv, linetype = "dashed", color = "red") } +
          { if (!is.na(upper_cv)) geom_vline(xintercept = upper_cv, linetype = "dashed", color = "red") } +
          
          # Vertical line for test statistic
          geom_vline(xintercept = testStatistics, linetype = "solid", color = "blue") +
          
          # Annotations
          { if (!is.na(lower_cv)) annotate("text", x = lower_cv, y = max(prob)*0.9, 
                                           label = paste("CV =", round(lower_cv, 2)), hjust = 1.1, size = 4, color = "red") } +
          { if (!is.na(upper_cv)) annotate("text", x = upper_cv, y = max(prob)*0.9, 
                                           label = paste("CV =", round(upper_cv, 2)), hjust = -0.1, size = 4, color = "red") } +
          annotate("text", x = testStatistics, y = max(prob)*0.7, 
                   label = paste("TS =", round(testStatistics, 2)), hjust = -0.1, size = 4, color = "blue") +
          
          # Plot labels and theme
          labs(title = "Hypothesis Testing for Population Variance",
               x = "Test Statistic",
               y = "Probability Density") +
          theme_minimal()
      })
    })
  })
}


# Main UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("onePopTest", "Select Proper Population Test", choices = onePopTest, selected = "Mean Test"),
      selectInput("properTest", label = "Select Proper Population Test for the Mean", choices = properTest, selected = "Two-Tailed"),
      
      popTest_tabs,
      numericInput("n_sample", "Sample Size:", value = 40, min = 1),
      numericInput("conf_int", "Confidence Interval:", value = 0.95, min = 0, max = 1),
      actionButton("calculate", label = "Calculate!")
    ),
    mainPanel(
      uiOutput("dynamic_plot")
    )
  )
)

# Main Server
server <- function(input, output, session) {
  observeEvent(input$onePopTest, {
    updateTabsetPanel(inputId = "popTest", selected = input$onePopTest)
  })
  
  observeEvent(input$calculate, {
    switch(input$onePopTest,
      "Mean Test" = {
        onePopMeanTest_server("meanTest",
                              mu = reactive(input$mu),
                              mean = reactive(input$mean),
                              sd = reactive(input$sd),
                              n_sample = reactive(input$n_sample),
                              sd_Type = reactive(input$sd_Type),
                              conf_int = reactive(input$conf_int),
                              calculate = reactive(input$calculate),
                              properTest = reactive(input$properTest)
        )
 
        output$dynamic_plot <- renderUI(distributionPlot_ui("meanTest"))
      },
      
      "Proportion Test" = {
        onePopPropTest_server("propTest",
                              p_pop = reactive(input$p_pop),
                              p_sample = reactive(input$p_sample),
                              n_sample = reactive(input$n_sample),
                              conf_int = reactive(input$conf_int),
                              calculate = reactive(input$calculate),
                              properTest = reactive(input$properTest)
        )
        
        output$dynamic_plot <- renderUI(distributionPlot_ui("propTest"))
      },
      
      "Variance Test" = {
        onePopVarTest_server("varTest",
                              var_pop = reactive(input$var_pop),
                              var_sample = reactive(input$var_sample),
                              n_sample = reactive(input$n_sample),
                              conf_int = reactive(input$conf_int),
                              calculate = reactive(input$calculate),
                              properTest = reactive(input$properTest)
        )
        
        output$dynamic_plot <- renderUI(distributionPlot_ui("varTest"))
      },
    )
  }) 
}

shinyApp(ui, server)
