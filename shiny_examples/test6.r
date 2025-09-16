# Interactive Parabola with Moving Tangent Line
# Install required packages if needed
if (!require(plotly)) install.packages("plotly")
if (!require(dplyr)) install.packages("dplyr")

library(plotly)
library(dplyr)

# =============================================================================
# Mathematical Functions
# =============================================================================

# Define the parabola function: f(x) = ax² + bx + c
parabola_func <- function(x, a = 1, b = 0, c = 0) {
  return(a * x^2 + b * x + c)
}

# Derivative function: f'(x) = 2ax + b
derivative_func <- function(x, a = 1, b = 0) {
  return(2 * a * x + b)
}

# Tangent line equation: y = f'(x₀)(x - x₀) + f(x₀)
tangent_line <- function(x, x0, a = 1, b = 0, c = 0) {
  slope <- derivative_func(x0, a, b)
  y0 <- parabola_func(x0, a, b, c)
  return(slope * (x - x0) + y0)
}

# =============================================================================
# Create Interactive Plot Data
# =============================================================================

create_interactive_parabola <- function(a = 0.5, b = 0, c = -2, 
                                      x_range = c(-6, 6), 
                                      n_points = 200,
                                      n_frames = 100) {
  
  cat("🎯 Creating interactive parabola data...\n")
  
  # X values for the parabola
  x_vals <- seq(x_range[1], x_range[2], length.out = n_points)
  
  # Y values for the parabola
  y_vals <- parabola_func(x_vals, a, b, c)
  
  # Create parabola data
  parabola_data <- data.frame(
    x = x_vals,
    y = y_vals,
    type = "parabola"
  )
  
  # Create data for different tangent positions
  tangent_positions <- seq(x_range[1] + 1, x_range[2] - 1, length.out = n_frames)
  
  all_data <- data.frame()
  
  for(i in 1:length(tangent_positions)) {
    x0 <- tangent_positions[i]
    
    # Parabola data for this frame
    frame_parabola <- data.frame(
      x = x_vals,
      y = y_vals,
      frame = i,
      type = "parabola",
      tangent_x = x0,
      stringsAsFactors = FALSE
    )
    
    # Tangent line data
    tangent_x_range <- seq(x0 - 2, x0 + 2, length.out = 50)
    tangent_y_vals <- tangent_line(tangent_x_range, x0, a, b, c)
    
    frame_tangent <- data.frame(
      x = tangent_x_range,
      y = tangent_y_vals,
      frame = i,
      type = "tangent",
      tangent_x = x0,
      stringsAsFactors = FALSE
    )
    
    # Point of tangency
    point_y <- parabola_func(x0, a, b, c)
    frame_point <- data.frame(
      x = x0,
      y = point_y,
      frame = i,
      type = "point",
      tangent_x = x0,
      stringsAsFactors = FALSE
    )
    
    # Combine all data for this frame
    frame_data <- rbind(frame_parabola, frame_tangent, frame_point)
    all_data <- rbind(all_data, frame_data)
  }
  
  return(all_data)
}

# =============================================================================
# Create the Interactive Plot
# =============================================================================

# Generate the data
plot_data <- create_interactive_parabola(a = 0.3, b = 0, c = -2, 
                                        x_range = c(-8, 8), 
                                        n_frames = 80)

# Create the animated plot
p_interactive <- plot_data %>%
  plot_ly(x = ~x, y = ~y, frame = ~frame, type = 'scatter', mode = 'lines',
          color = ~type, 
          colors = c("parabola" = "#2E86AB", "tangent" = "#F24236", "point" = "#F6AE2D"),
          line = list(width = 3),
          hovertemplate = ~paste(
            "<b>", type, "</b><br>",
            "x: %{x:.2f}<br>",
            "y: %{y:.2f}<br>",
            "Tangent at x =", round(tangent_x, 2),
            "<extra></extra>"
          )) %>%
  add_markers(data = subset(plot_data, type == "point"), 
              x = ~x, y = ~y, frame = ~frame,
              marker = list(size = 12, color = "#F6AE2D", 
                          line = list(color = "white", width = 2)),
              showlegend = FALSE,
              hovertemplate = ~paste(
                "<b>Point of Tangency</b><br>",
                "x: %{x:.2f}<br>",
                "y: %{y:.2f}<br>",
                "Slope:", round(2*0.3*tangent_x, 2),
                "<extra></extra>"
              )) %>%
  layout(
    title = list(
      text = "<b>Interactive Parabola with Moving Tangent Line</b><br><span style='font-size:14px;color:gray'>f(x) = 0.3x² - 2, f'(x) = 0.6x</span>",
      x = 0.5,
      font = list(size = 18)
    ),
    xaxis = list(
      title = "x",
      range = c(-8.5, 8.5),
      gridcolor = "lightgray",
      showgrid = TRUE,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1
    ),
    yaxis = list(
      title = "y", 
      range = c(-3, 15),
      gridcolor = "lightgray",
      showgrid = TRUE,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    font = list(family = "Arial", size = 12),
    legend = list(
      orientation = "h",
      x = 0.5,
      y = -0.15,
      xanchor = 'center',
      bgcolor = "rgba(255,255,255,0.8)",
      bordercolor = "gray",
      borderwidth = 1
    )
  ) %>%
  animation_opts(
    frame = 100,
    transition = 80,
    redraw = FALSE,
    easing = "linear"
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Tangent at x = ",
      font = list(color = "red", size = 16)
    ),
    len = 0.8,
    x = 0.1,
    y = 0,
    bgcolor = "lightgray",
    bordercolor = "gray",
    borderwidth = 1,
    tickcolor = "red"
  ) %>%
  animation_button(
    x = 0.9, y = 0,
    bgcolor = "lightblue",
    bordercolor = "blue",
    font = list(color = "darkblue")
  )

# =============================================================================
# Enhanced Version with Multiple Parabolas
# =============================================================================

create_multiple_parabolas <- function() {
  cat("🎨 Creating enhanced version with multiple parabolas...\n")
  
  # Different parabolas to choose from
  parabolas <- list(
    list(a = 0.3, b = 0, c = -2, name = "f(x) = 0.3x² - 2", color = "#2E86AB"),
    list(a = -0.2, b = 1, c = 3, name = "g(x) = -0.2x² + x + 3", color = "#A23B72"),
    list(a = 0.1, b = -0.5, c = 1, name = "h(x) = 0.1x² - 0.5x + 1", color = "#F18F01")
  )
  
  return("Enhanced version ready for development!")
}

# =============================================================================
# Usage Instructions & Demo
# =============================================================================

cat("\n🎯 Interactive Parabola with Tangent Line Ready!\n")
cat("📋 Features:\n")
cat("   📈 Blue parabola: f(x) = 0.3x² - 2\n")
cat("   📏 Red tangent line: moves along the parabola\n")
cat("   🎯 Yellow point: shows point of tangency\n")
cat("   🎛️ Interactive slider: control tangent position\n")
cat("   ▶️ Play button: auto-animate the tangent\n")
cat("   📊 Hover info: see coordinates and slope\n")

cat("\n🚀 To view the plot:\n")
cat("   Type: p_interactive\n")
cat("   Or save to HTML: htmlwidgets::saveWidget(p_interactive, 'tangent_demo.html')\n")

# Display the plot
cat("\n🎬 Displaying interactive plot...\n")
p_interactive
