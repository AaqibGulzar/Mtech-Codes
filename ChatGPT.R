library(tidyverse)

ggplot(mtcars) + 
  geom_point(aes(x = hp,y = mpg,color = as.factor(cyl))) +
  labs(x = "Horse Power",y = "Milleage per Gallon") + theme(legend.title="Cylinder Number")


 install.packages("pak")
pak::pak("MichelNivard/gptstudio")



install.packages("gptstudio")
library(gptstudio)
gptstudio:::addin_chatgpt()

Sys.setenv(OPENAI_API_KEY = "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")


library(ggplot2)
library(dplyr)



# load example dataset
data(mtcars)

# set theme for ggplot
theme_set(theme_bw())

# create plot
ggplot(data = mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
  geom_line(size = 1) +
  scale_x_continuous(name = "Weight", breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(name = "Miles per gallon", breaks = seq(10, 35, by = 5)) +
  scale_color_manual(name = "Cylinders", values = c("#F8766D", "#00BA38", "#619CFF")) +
  labs(title = "Mileage vs. Weight by Cylinder", subtitle = "Data from the 1974 Motor Trend US magazine",
       caption = "Source: mtcars dataset in R") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10)),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)

# Load data
data(mtcars)

# Define UI
ui <- fluidPage(
  titlePanel("Mileage vs. Weight by Cylinder"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cyl_input",
                  label = "Cylinders:",
                  choices = unique(mtcars$cyl),
                  selected = unique(mtcars$cyl)[1])
    ),
    mainPanel(
      plotOutput("lineplot", height = "500px")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Filter data based on input selection
  selected_data <- reactive({
    mtcars %>% filter(cyl == input$cyl_input)
  })
  
  # Create ggplot object
  output$lineplot <- renderPlot({
    ggplot(selected_data(), aes(x = wt, y = mpg)) +
      geom_line() +
      labs(title = "Mileage vs. Weight by Cylinder") +
      xlab("Weight") +
      ylab("Miles Per Gallon")
  })
}

# Run the app
shinyApp(ui = ui, server = server)



library(shiny)
library(plotly)
library(dplyr)

data(diamonds)

# Define the user interface
ui <- fluidPage(
  plotlyOutput("diamond_plot")
)

# Define the server logic
server <- function(input, output) {
  
  # Create the plotly plot
  output$diamond_plot <- renderPlotly({
    diamonds %>%
      plot_ly(
        x = ~carat,
        y = ~price,
        color = ~cut,
        size = ~depth,
        type = "scatter",
        mode = "markers",
        marker = list(sizemode = "diameter")
      ) %>%
      layout(title = "Diamonds Dataset")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)



library(ggplot2)

# Generate population data
set.seed(123)
population <- rnorm(10000, mean = 50, sd = 10)

ui <- fluidPage(
  titlePanel("Central Limit Theorem"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Sample Size:",
                  min = 1,
                  max = 1000,
                  value = 10),
      actionButton("update", "Go!")
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)

server <- function(input, output) {
  
  # Generate sample means from population
  sample_means <- function(n, size) {
    replicate(n, mean(sample(size, replace = TRUE, population)))
  }
  
  # Update plot with new histogram
  observeEvent(input$update, {
    output$hist <- renderPlot({
      hist(sample_means(1000, input$n), breaks = 50, col = "blue", main = "Distribution of Sample Means")
    })
  })
}

shinyApp(ui = ui, server = server)



