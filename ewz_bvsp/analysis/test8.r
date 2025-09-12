# Enable Shiny auto-reload for development
options(shiny.autoreload = TRUE)

# Load necessary libraries
library(shiny)
library(magick)
library(ggplot2)

# Try to load additional libraries
tryCatch({
  library(rstudioapi)
  library(base64enc)
}, error = function(e) {
  # Install base64enc if not available
  if (!require(base64enc, quietly = TRUE)) {
    install.packages("base64enc")
    library(base64enc)
  }
})

# --- GIF Generation ---
# This part of the script will run once to generate the GIF.
# It's placed outside the server and ui logic.

# Get the current working directory and create paths relative to the script location
script_dir <- tryCatch({
  dirname(rstudioapi::getActiveDocumentContext()$path)
}, error = function(e) {
  getwd()
})

if (is.null(script_dir) || script_dir == "" || script_dir == ".") {
  script_dir <- getwd()
}

# Create a directory to store the animation frames
anim_frames_dir <- file.path(script_dir, "anim_frames")
if (!dir.exists(anim_frames_dir)) {
  dir.create(anim_frames_dir)
}

# Generate frames for the GIF
for (i in 1:60) {
  # Create a data frame for the sine wave
  # The wave will appear to move by shifting the phase 'i'
  data <- data.frame(
    x = seq(0, 2 * pi, length.out = 500),
    y = sin(seq(0, 2 * pi, length.out = 500) + i / 10)
  )

  # Create a plot for the current frame
  p <- ggplot(data, aes(x = x, y = y)) +
    geom_line(color = "#337ab7", size = 1.5) +
    labs(title = "Animated Sine Wave", x = "", y = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      panel.grid.major = element_line(colour = "#e0e0e0"),
      panel.grid.minor = element_blank()
    ) +
    ylim(-1.1, 1.1) # Keep y-axis consistent across frames

  # Save the plot as a PNG image
  ggsave(
    filename = file.path(anim_frames_dir, paste0("frame_", sprintf("%03d", i), ".png")),
    plot = p,
    width = 6,
    height = 4,
    dpi = 100
  )
}

# List all the generated frame files
frame_files <- list.files(anim_frames_dir, full.names = TRUE, pattern = ".png")

# Read the frames into a magick object
frames <- image_read(frame_files)

# Animate the frames
animation <- image_animate(frames, fps = 100) # 15 frames per second

# Create the 'www' directory if it doesn't exist
# Shiny serves files from the 'www' directory automatically
www_dir <- file.path(script_dir, "www")
if (!dir.exists(www_dir)) {
  dir.create(www_dir)
}

# Save the animation as a GIF file in the 'www' directory
gif_path <- file.path(www_dir, "sine_wave.gif")
image_write(animation, path = gif_path)

# Clean up the individual frame files and directory
unlink(anim_frames_dir, recursive = TRUE)

# Print confirmation that GIF was created
cat("GIF created at:", gif_path, "\n")
cat("File exists:", file.exists(gif_path), "\n")
cat("Working directory:", getwd(), "\n")
cat("Script directory:", script_dir, "\n")

# Check if the GIF exists before starting the app
if (!file.exists(gif_path)) {
  stop("GIF file not found at: ", gif_path)
}

# --- Shiny App UI ---
ui <- fluidPage(
  titlePanel("Shiny App with an Animated GIF"),
  mainPanel(
    tags$h3("Here is an animated sine wave:"),
    tags$p("This GIF was generated using the 'magick' and 'ggplot2' packages in R."),
    
    # Debug information
    tags$p(paste("Looking for GIF at: sine_wave.gif")),
    tags$p(paste("Working directory: ", getwd())),
    tags$p(paste("www directory: ", www_dir)),
    tags$p(paste("GIF exists: ", file.exists(gif_path))),
    
    # Try multiple approaches to display the image
    tags$div(
      tags$h4("Method 1: Standard Shiny approach"),
      tags$img(src = "sine_wave.gif",
               alt = "Animated sine wave",
               style = "max-width: 100%; border: 1px solid #ddd; border-radius: 5px; margin-top: 20px;")
    ),
    
    tags$div(
      tags$h4("Method 2: Base64 encoded (fallback)"),
      uiOutput("base64_image")
    )
  )
)

# --- Shiny App Server ---
server <- function(input, output) {
  # Fallback method: serve image as base64 encoded data
  output$base64_image <- renderUI({
    if (file.exists(gif_path)) {
      # Read the file as binary and convert to base64
      gif_data <- readBin(gif_path, "raw", file.info(gif_path)$size)
      gif_base64 <- base64enc::base64encode(gif_data)
      
      tags$img(src = paste0("data:image/gif;base64,", gif_base64),
               alt = "Animated sine wave (base64)",
               style = "max-width: 100%; border: 1px solid #red; border-radius: 5px; margin-top: 20px;")
    } else {
      tags$p("GIF file not found for base64 encoding")
    }
  })
}

# --- Run the App ---
# Set the working directory to the script directory before running
setwd(script_dir)
cat("Final working directory:", getwd(), "\n")

# Launch Shiny app with auto-reload enabled
# The app will automatically refresh when you save changes to this file
shinyApp(ui = ui, server = server, options = list(
  host = "127.0.0.1",
  port = 8080,
  launch.browser = TRUE
))
