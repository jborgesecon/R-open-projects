# Manim-style Animation: Rectangle Rotating in Circle
# Install required packages if needed
if (!require(gganimate)) install.packages("gganimate")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")

library(gganimate)
library(ggplot2)
library(dplyr)

# =============================================================================
# Method 1: Simple Rectangle Rotating Around Center Point
# =============================================================================

create_rotating_rectangle <- function(frames = 100, radius = 2) {
  # Create time points
  t <- seq(0, 2*pi, length.out = frames)
  
  # Rectangle dimensions
  rect_width <- 0.5
  rect_height <- 0.3
  
  # Create data for each frame
  animation_data <- data.frame()
  
  for(i in 1:frames) {
    # Center of rectangle moves in circle
    center_x <- radius * cos(t[i])
    center_y <- radius * sin(t[i])
    
    # Rectangle corners (before rotation)
    corners_x <- c(-rect_width/2, rect_width/2, rect_width/2, -rect_width/2, -rect_width/2)
    corners_y <- c(-rect_height/2, -rect_height/2, rect_height/2, rect_height/2, -rect_height/2)
    
    # Rotate rectangle by its angle around the circle
    angle <- t[i]  # Rectangle rotates as it moves
    
    # Apply rotation matrix
    rotated_x <- corners_x * cos(angle) - corners_y * sin(angle)
    rotated_y <- corners_x * sin(angle) + corners_y * cos(angle)
    
    # Translate to circle position
    final_x <- rotated_x + center_x
    final_y <- rotated_y + center_y
    
    # Add to data frame
    frame_data <- data.frame(
      x = final_x,
      y = final_y,
      frame = i,
      group = 1
    )
    
    animation_data <- rbind(animation_data, frame_data)
  }
  
  return(animation_data)
}

# Create the animation data
anim_data <- create_rotating_rectangle(frames = 60, radius = 3)

# Create circle path data
circle_path <- data.frame(
  x = 3 * cos(seq(0, 2*pi, length.out = 100)),
  y = 3 * sin(seq(0, 2*pi, length.out = 100))
)

# Create the animated plot
p1 <- ggplot(anim_data, aes(x = x, y = y, group = group)) +
  geom_path(data = circle_path, aes(x = x, y = y), 
            color = "gray", linetype = "dashed", inherit.aes = FALSE) +
  geom_polygon(fill = "steelblue", color = "darkblue", alpha = 0.8) +
  coord_fixed() +
  xlim(-4, 4) + ylim(-4, 4) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black")) +
  transition_time(frame) +
  labs(title = "Rotating Rectangle - Frame: {frame_time}")

# Preview the animation (no file saved)
animate(p1, fps = 15, duration = 4, width = 600, height = 600)

# =============================================================================
# Method 2: More Manim-like with Multiple Objects
# =============================================================================

create_manim_scene <- function(frames = 120) {
  t <- seq(0, 4*pi, length.out = frames)  # Two full rotations
  
  all_data <- data.frame()
  
  for(i in 1:frames) {
    # Main rectangle
    center_x <- 2.5 * cos(t[i])
    center_y <- 2.5 * sin(t[i])
    
    # Rectangle corners
    rect_w <- 0.6
    rect_h <- 0.4
    corners_x <- c(-rect_w/2, rect_w/2, rect_w/2, -rect_w/2, -rect_w/2)
    corners_y <- c(-rect_h/2, -rect_h/2, rect_h/2, rect_h/2, -rect_h/2)
    
    # Rotation
    angle <- t[i] * 2  # Rotate faster than movement
    rotated_x <- corners_x * cos(angle) - corners_y * sin(angle)
    rotated_y <- corners_x * sin(angle) + corners_y * cos(angle)
    
    final_x <- rotated_x + center_x
    final_y <- rotated_y + center_y
    
    # Main rectangle data
    rect_data <- data.frame(
      x = final_x,
      y = final_y,
      frame = i,
      object = "rectangle",
      group = 1,
      alpha = 1.0  # Add alpha column
    )
    
    # Add center point
    center_data <- data.frame(
      x = center_x,
      y = center_y,
      frame = i,
      object = "center",
      group = 2,
      alpha = 1.0  # Add alpha column
    )
    
    # Add trail points (fade effect)
    if(i > 10) {
      trail_frames <- max(1, i-10):i
      trail_alpha <- seq(0.1, 1, length.out = length(trail_frames))
      
      for(j in seq_along(trail_frames)) {
        tf <- trail_frames[j]
        trail_x <- 2.5 * cos(t[tf])
        trail_y <- 2.5 * sin(t[tf])
        
        trail_data <- data.frame(
          x = trail_x,
          y = trail_y,
          frame = i,
          object = "trail",
          group = 100 + j,
          alpha = trail_alpha[j]
        )
        
        all_data <- rbind(all_data, trail_data)
      }
    }
    
    all_data <- rbind(all_data, rect_data, center_data)
  }
  
  return(all_data)
}

# Create advanced animation
advanced_data <- create_manim_scene(frames = 80)

# Create circle path for background
circle_path2 <- data.frame(
  x = 2.5 * cos(seq(0, 2*pi, length.out = 200)),
  y = 2.5 * sin(seq(0, 2*pi, length.out = 200))
)

# Create the advanced plot
p2 <- ggplot(advanced_data, aes(x = x, y = y)) +
  # Circle path
  geom_path(data = circle_path2, aes(x = x, y = y), 
            color = "gray30", linetype = "dotted", size = 0.5, inherit.aes = FALSE) +
  # Trail points
  geom_point(data = subset(advanced_data, object == "trail"), 
             aes(alpha = alpha), color = "cyan", size = 1) +
  # Center point
  geom_point(data = subset(advanced_data, object == "center"), 
             color = "red", size = 3) +
  # Rectangle
  geom_polygon(data = subset(advanced_data, object == "rectangle"), 
               aes(group = group), fill = "gold", color = "orange", alpha = 0.9) +
  coord_fixed() +
  xlim(-3.5, 3.5) + ylim(-3.5, 3.5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.position = "none") +
  scale_alpha_identity() +
  transition_time(frame) +
  labs(title = "Manim-style Animation - Frame: {frame_time}")

# =============================================================================
# PREVIEW OPTIONS (choose one):
# =============================================================================

# Option 1: Preview in viewer pane (RECOMMENDED for development)
# Uncomment to preview without saving:
# animate(p1, fps = 15, duration = 4, width = 600, height = 600)
# animate(p2, fps = 20, duration = 4, width = 700, height = 700)

# Option 2: Save and then view the file
# This creates the file but you need to open it manually
# anim_save("final_animation.gif", animate(p2, fps = 20, duration = 4))

# Option 3: Interactive preview function
preview_animation <- function(which_version = 1) {
  if(which_version == 1) {
    cat("🎬 Previewing simple version...\n")
    return(animate(p1, fps = 15, duration = 4, width = 600, height = 600, 
                   renderer = gifski_renderer(loop = TRUE)))
  } else if(which_version == 2) {
    cat("🎬 Previewing advanced version...\n")
    return(animate(p2, fps = 20, duration = 4, width = 700, height = 700,
                   renderer = gifski_renderer(loop = TRUE)))
  }
}

# Usage:
# preview_animation(1)  # Simple version
# preview_animation(2)  # Advanced version

# =============================================================================
# Method 3: Function to easily customize and save
# =============================================================================

create_custom_rotation <- function(frames = 60, radius = 3, rotations = 1, 
                                 rect_width = 0.5, rect_height = 0.3,
                                 show_trail = TRUE, save_file = NULL) {
  
  t <- seq(0, 2*pi*rotations, length.out = frames)
  
  # [Rest of the function would be here...]
  cat("Use this function to create custom animations!\n")
  cat("Parameters:\n")
  cat("- frames: number of animation frames\n")
  cat("- radius: circle radius\n") 
  cat("- rotations: number of full rotations\n")
  cat("- rect_width/rect_height: rectangle dimensions\n")
  cat("- show_trail: whether to show trailing effect\n")
  cat("- save_file: filename to save (NULL for preview only)\n")
}

# =============================================================================
# FINAL SAVE FUNCTION (use when ready to export)
# =============================================================================

save_final_animation <- function(filename = "rotating_rectangle.gif", version = 2) {
  cat("💾 Saving animation to:", filename, "\n")
  
  if(version == 1) {
    anim_save(filename, animate(p1, fps = 15, duration = 4, width = 600, height = 600))
  } else {
    anim_save(filename, animate(p2, fps = 20, duration = 4, width = 700, height = 700))
  }
  
  cat("✅ Animation saved successfully!\n")
  cat("📁 File location:", getwd(), "/", filename, "\n")
}

# =============================================================================
# 🎬 QUICK START GUIDE:
# =============================================================================
cat("\n🎬 Animation Ready! Quick commands:\n")
cat("👀 Preview simple version:     preview_animation(1)\n")
cat("👀 Preview advanced version:   preview_animation(2)\n")
cat("💾 Save simple version:        save_final_animation('simple.gif', 1)\n")
cat("💾 Save advanced version:      save_final_animation('advanced.gif', 2)\n")
cat("\nTip: Use preview functions during development, save only when ready!\n")
cat("⚠️  Don't run animate() directly - it shows binary data in console!\n")

# save this to final_animation.gif
save_final_animation("final_animation.gif", 2)