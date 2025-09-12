library(gganimate)
library(ggplot2)
library(dplyr)

# Create sample data
data <- data.frame(
  x = rep(1:10, 10),
  y = rep(1:10, each = 10),
  time = rep(1:10, each = 10),
  value = rnorm(100)
)

# Set up plotting device with custom dimensions for interactive viewing
windows(width = 12, height = 3)  # For Windows - using your desired wide aspect ratio
# Create animated plot
p <- ggplot(data, aes(x = x, y = y, size = value, color = value)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c() +
  theme_minimal() +
  transition_time(time) +
  labs(title = "Time: {frame_time}")

# Render animation
anim <- animate(p, width = 800, height = 600, fps = 10, duration = 3)

# Save as GIF
print(anim)