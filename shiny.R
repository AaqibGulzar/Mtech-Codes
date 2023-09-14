


# Extract slope, intercept, and R-squared value from the linear model
slope <- coef(fftlm)[2]
intercept <- coef(fftlm)[1]
r_squared <- summary(fftlm)$r.squared

# Create a scatter plot with a fitted line
ggplot(fft, aes(x = gsk, y = RP100)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter plot of gsk and rp100",
       x = "gsk",
       y = "rp100") +
  annotate("text", x = 4, y = 9, label = paste0("slope = ", round(slope, 2), "\n",
                                                "intercept = ", round(intercept, 2), "\n",
                                                "R-squared = ", round(r_squared, 2)))



library(shiny)
library(ggplot2)



library(shiny)
library(ggplot2)

library(shiny)
library(ggplot2)

library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      p("This app demonstrates the central limit theorem by generating samples from a normal distribution with a mean of 0 and a standard deviation of 1. Use the sliders to adjust the sample size and number of samples."),
      sliderInput("sample_size", "Sample Size", min = 1, max = 1000, value = 10),
      sliderInput("sample_num", "Number of Samples", min = 1, max = 1000, value = 10),
      actionButton("generate", "Generate Data"),
      plotOutput("hist", click = "hist_click"),
      verbatimTextOutput("details")
    ),
    mainPanel()
  )
)

server <- function(input, output) {
  data <- reactive({
    n <- input$sample_size
    k <- input$sample_num
    as.data.frame(t(replicate(k, rnorm(n))))
  })
  
  output$hist <- renderPlot({
    p <- ggplot(data(), aes(x = V1)) +
      geom_histogram(fill = "steelblue", color = "white", binwidth = 0.1) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
      labs(title = paste("Sample Size:", input$sample_size, "| Number of Samples:", input$sample_num),
           x = "Sample Mean", y = "Count") +
      theme_bw()
    
    if (!is.null(input$hist_click)) {
      p <- p + geom_point(data = data.frame(mean = input$hist_click), aes(x = mean, y = 0), size = 5, color = "red")
    }
    
    p
  })
  
  output$details <- renderPrint({
    x <- input$hist_click
    if (!is.null(x)) {
      sample_index <- which(sapply(data(), function(sample) any(sample >= x - 0.05 & sample <= x + 0.05)))
      if (length(sample_index) > 0) {
        sample <- data()[, sample_index]
        obs_index <- which(abs(sample - x) == min(abs(sample - x)))
        paste0("Sample ", sample_index, ", Observation ", obs_index, ": ", round(sample[obs_index], 2))
      }
    }
  })
}

shinyApp(ui, server)


library(shiny)
library(ggplot2)
library(shinyWidgets)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      p("This app demonstrates the central limit theorem by generating samples from a normal distribution with a mean of 0 and a standard deviation of 1. Use the sliders to adjust the sample size and number of samples, and the color picker to change the color of the bars in the histogram."),
      sliderInput("sample_size", "Sample Size", min = 1, max = 1000, value = 10),
      sliderInput("sample_num", "Number of Samples", min = 1, max = 1000, value = 10),
      colorPickr("bar_color", "Choose Color:", value = "#69b3a2"),
      actionButton("generate", "Generate Data"),
      plotOutput("hist", click = "hist_click"),
      verbatimTextOutput("details")
    ),
    mainPanel()
  )
)

server <- function(input, output) {
  data <- reactive({
    n <- input$sample_size
    k <- input$sample_num
    as.data.frame(t(replicate(k, rnorm(n))))
  })
  
  output$hist <- renderPlot({
    p <- ggplot(data(), aes(x = V1)) +
      geom_histogram(fill = input$bar_color, color = "white", binwidth = 0.1) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
      labs(title = paste("Sample Size:", input$sample_size, "| Number of Samples:", input$sample_num),
           x = "Sample Mean", y = "Count") +
      theme_bw()
    
    if (!is.null(input$hist_click)) {
      p <- p + geom_point(data = data.frame(mean = input$hist_click), aes(x = mean, y = 0), size = 5, color = "red")
    }
    
    p
  })
  
  output$details <- renderPrint({
    x <- input$hist_click
    if (!is.null(x)) {
      sample_index <- which(sapply(data(), function(sample) any(sample >= x - 0.05 & sample <= x + 0.05)))
      if (length(sample_index) > 0) {
        sample <- data()[, sample_index]
        obs_index <- which(abs(sample - x) == min(abs(sample - x)))
        paste0("Sample ", sample_index, ", Observation ", obs_index, ": ", round(sample[obs_index], 2))
      }
    }
  })
}

shinyApp(ui, server)


