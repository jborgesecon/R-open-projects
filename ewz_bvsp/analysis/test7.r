# tangent_plot.r
#
# Description:
# This script launches a Shiny web application that interactively displays a
# parabola (y = x^2) and its tangent line. A slider allows the user to
# move the point of tangency along the curve, and the plot updates in real-time.
#
# Required Packages: shiny, ggplot2
# To install: install.packages(c("shiny", "ggplot2"))
#
# How to run:
# 1. Make sure 'shiny' and 'ggplot2' are installed.
# 2. Save this code as a file named 'tangent_plot.r'.
# 3. Open R or RStudio and run the command: shiny::runApp('path/to/tangent_plot.r')
#    (replace 'path/to/' with the actual path to the file).
#    Alternatively, in RStudio, you can just click the "Run App" button.

# Load necessary libraries
library(shiny)
library(ggplot2)

# --- 1. Define the User Interface (UI) ---
# The UI determines the layout of the web application.
ui <- fluidPage(
    
    # Application title
    titlePanel("Interactive Tangent Line on a Parabola"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar panel for user inputs
        sidebarPanel(
            # Slider input for selecting the x-coordinate of the tangent point
            sliderInput("x_point",                 # The input ID
                        "Select x-coordinate for tangent point:", # Label displayed to the user
                        min = -5,                  # Minimum value of the slider
                        max = 5,                   # Maximum value of the slider
                        value = 1,                 # Default starting value
                        step = 0.1)                # Increment of the slider
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            # Output: The plot will be rendered here
            plotOutput("distPlot")
        )
    )
)

# --- 2. Define the Server Logic ---
# The server function contains the instructions to build and update the objects
# displayed in the UI.
server <- function(input, output) {
    
    # Function for the parabola
    parabola_func <- function(x) {
        return(x^2)
    }
    
    # Derivative of the parabola function (defines the slope of the tangent)
    derivative_func <- function(x) {
        return(2 * x)
    }
    
    # renderPlot() creates the plot. It's a reactive expression, so it
    # automatically re-runs whenever the inputs it depends on (input$x_point) change.
    output$distPlot <- renderPlot({
        
        # Get the current x-coordinate from the slider
        x_val <- input$x_point
        
        # Calculate the corresponding y-coordinate on the parabola
        y_val <- parabola_func(x_val)
        
        # Calculate the slope of the tangent line at that point using the derivative
        slope <- derivative_func(x_val)
        
        # Calculate the y-intercept of the tangent line.
        # From the line equation y = mx + c, we get c = y - mx
        intercept <- y_val - slope * x_val
        
        # Create the plot using ggplot2
        ggplot(data.frame(x = seq(-6, 6, length.out = 100)), aes(x = x)) +
            
            # 1. Plot the parabola y = x^2
            stat_function(fun = parabola_func, color = "blue", size = 1.2) +
            
            # 2. Add the tangent line using its slope and intercept
            geom_abline(intercept = intercept, slope = slope, color = "red", size = 1, linetype = "dashed") +
            
            # 3. Add a point to show the point of tangency
            geom_point(aes(x = x_val, y = y_val), color = "red", size = 4) +
            
            # Set plot limits for a consistent view
            xlim(-6, 6) +
            ylim(-2, 30) +
            
            # Add labels and a title
            labs(
                title = paste("Tangent Line at x =", x_val),
                subtitle = "Parabola: y = x^2",
                x = "x-axis",
                y = "y-axis"
            ) +
            
            # Use a clean theme and center the title
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5, size=16, face="bold"),
                plot.subtitle = element_text(hjust = 0.5)
            ) + 
            coord_fixed(ratio = 0.2) # Adjust aspect ratio for better visuals
    })
}

# --- 3. Run the Application ---
# This command combines the UI and server logic into a runnable Shiny application.
shinyApp(ui = ui, server = server)
