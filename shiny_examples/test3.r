library(shiny)
library(plotly)
library(animation)

# Define UI
ui <- fluidPage(
  titlePanel("MTCars Animation Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Controls"),
      p("This dashboard shows an animated scatter plot of the mtcars dataset."),
      p("The animation cycles through different cylinder counts (4, 6, 8)."),
      br(),
      actionButton("refresh", "Refresh Plot", class = "btn-primary"),
      br(),
      br(),
      h4("Dataset Info:"),
      p("• Weight (wt) vs Miles per Gallon (mpg)"),
      p("• Grouped by number of cylinders"),
      p("• Animation shows different cylinder groups")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Animated Plot", 
                 plotlyOutput("animatedPlot", height = "600px")),
        tabPanel("Static Plot", 
                 plotOutput("staticPlot", height = "400px")),
        tabPanel("Data", 
                 dataTableOutput("dataTable"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Animated plotly chart
  output$animatedPlot <- renderPlotly({
    input$refresh  # This creates a dependency on the refresh button
    
    p <- plot_ly(mtcars, x = ~wt, y = ~mpg, frame = ~cyl,
                 text = ~paste("Car:", rownames(mtcars), 
                              "<br>Weight:", wt,
                              "<br>MPG:", mpg,
                              "<br>Cylinders:", cyl),
                 hovertemplate = "%{text}<extra></extra>") %>%
      add_markers(size = ~hp, color = ~factor(cyl), 
                  colors = c("red", "blue", "green"),
                  alpha = 0.7) %>%
      animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
      layout(
        title = "Weight vs MPG by Cylinder Count",
        xaxis = list(title = "Weight (1000 lbs)"),
        yaxis = list(title = "Miles per Gallon"),
        showlegend = TRUE
      )
    
    p
  })
  
  # Static plot for comparison
  output$staticPlot <- renderPlot({
    plot(mtcars$wt, mtcars$mpg, 
         col = factor(mtcars$cyl),
         pch = 19,
         xlab = "Weight (1000 lbs)",
         ylab = "Miles per Gallon",
         main = "Static: Weight vs MPG by Cylinder Count")
    legend("topright", 
           legend = paste(sort(unique(mtcars$cyl)), "cylinders"),
           col = 1:length(unique(mtcars$cyl)),
           pch = 19)
  })
  
  # Data table
  output$dataTable <- renderDataTable({
    mtcars
  }, options = list(pageLength = 10, scrollX = TRUE))
}

# Run the application
shinyApp(ui = ui, server = server)