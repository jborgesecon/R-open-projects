# Manim-style Morphing Animation: Rectangle → Circle
# Install required packages if needed
if (!require(gganimate)) install.packages("gganimate")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")

library(gganimate)
library(ggplot2)
library(dplyr)

# =============================================================================
# Morphing Rectangle to Circle Animation
# =============================================================================

create_morphing_animation <- function(frames = 100, center_x = 0, center_y = 0) {
  
  # Simple morphing: Rectangle → Circle (stationary in center)
  # Phase 1 (frames 1-20): Hold as rectangle
  # Phase 2 (frames 21-80): Morph from rectangle to circle  
  # Phase 3 (frames 81-100): Hold as circle
  
  all_data <- data.frame()
  
  for(i in 1:frames) {
    
    # Calculate morphing progress
    if(i <= 20) {
      # Phase 1: Pure rectangle
      morph_progress <- 0
    } else if(i <= 80) {
      # Phase 2: Morphing
      morph_progress <- (i - 20) / 60  # 0 to 1 over 60 frames
    } else {
      # Phase 3: Pure circle
      morph_progress <- 1
    }
    
    # Shape stays in center
    pos_x <- center_x
    pos_y <- center_y
    
    # Morphing logic: interpolate between rectangle and circle
    n_points <- 100  # More points for smoother morphing
    
    if(morph_progress == 0) {
      # Pure rectangle
      rect_width <- 2.0
      rect_height <- 1.2
      
      # Simple rectangle points (no rotation)
      shape_x <- c(-rect_width/2, rect_width/2, rect_width/2, -rect_width/2, -rect_width/2)
      shape_y <- c(-rect_height/2, -rect_height/2, rect_height/2, rect_height/2, -rect_height/2)
      
    } else if(morph_progress == 1) {
      # Pure circle
      circle_radius <- 1.0
      t_circle <- seq(0, 2*pi, length.out = n_points)
      shape_x <- circle_radius * cos(t_circle)
      shape_y <- circle_radius * sin(t_circle)
      
    } else {
      # Morphing between rectangle and circle
      rect_width <- 2.0
      rect_height <- 1.2
      circle_radius <- 1.0
      
      # Create rectangle points (many points for smooth morphing)
      t_rect <- seq(0, 1, length.out = n_points)
      rect_x <- rep(0, n_points)
      rect_y <- rep(0, n_points)
      
      for(j in 1:n_points) {
        t <- t_rect[j]
        if(t <= 0.25) {
          # Bottom edge
          rect_x[j] <- rect_width * (4*t - 0.5)
          rect_y[j] <- -rect_height/2
        } else if(t <= 0.5) {
          # Right edge  
          rect_x[j] <- rect_width/2
          rect_y[j] <- rect_height * (4*(t-0.25) - 0.5)
        } else if(t <= 0.75) {
          # Top edge
          rect_x[j] <- rect_width * (0.5 - 4*(t-0.5))
          rect_y[j] <- rect_height/2
        } else {
          # Left edge
          rect_x[j] <- -rect_width/2
          rect_y[j] <- rect_height * (0.5 - 4*(t-0.75))
        }
      }
      
      # Create circle points
      t_circle <- seq(0, 2*pi, length.out = n_points)
      circle_x <- circle_radius * cos(t_circle)
      circle_y <- circle_radius * sin(t_circle)
      
      # Smooth interpolation with easing
      ease_progress <- morph_progress^2 * (3 - 2*morph_progress)  # Smooth step
      shape_x <- rect_x * (1 - ease_progress) + circle_x * ease_progress
      shape_y <- rect_y * (1 - ease_progress) + circle_y * ease_progress
    }
    
    # No rotation - keep it simple and stationary
    final_x <- shape_x + pos_x
    final_y <- shape_y + pos_y
    
    # Create data frame for this frame
    frame_data <- data.frame(
      x = final_x,
      y = final_y,
      frame = i,
      group = 1,
      morph_progress = morph_progress
    )
    
    all_data <- rbind(all_data, frame_data)
  }
  
  return(all_data)
}

# =============================================================================
# Create the simple morphing animation
# =============================================================================

# Generate animation data
cat("🎬 Creating simple morphing animation data...\n")
morph_data <- create_morphing_animation(frames = 100)

# Create the plot
p_morph <- ggplot(morph_data, aes(x = x, y = y, group = group)) +
  # Main morphing shape
  geom_polygon(aes(fill = morph_progress), color = "white", alpha = 0.9, size = 1) +
  
  # Color gradient: blue (rectangle) → gold (circle)
  scale_fill_gradient(low = "steelblue", high = "gold", guide = "none") +
  
  # Styling
  coord_fixed() +
  xlim(-2, 2) + ylim(-1.5, 1.5) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white", hjust = 0.5, size = 16)
  ) +
  
  # Animation
  transition_time(frame) +
  labs(title = "Rectangle → Circle Morphing | Progress: {closest_state}%")

# =============================================================================
# Preview and Save Functions
# =============================================================================

preview_morphing <- function() {
  cat("👀 Previewing simple morphing animation...\n")
  return(animate(p_morph, fps = 25, duration = 4, width = 600, height = 400,
                 renderer = gifski_renderer(loop = TRUE)))
}

save_morphing <- function(filename = "final_animation.gif") {
  cat("💾 Saving simple morphing animation to:", filename, "\n")
  cat("⏳ This may take a moment...\n")
  
  anim_save(filename, animate(p_morph, fps = 25, duration = 4, width = 600, height = 400))
  
  cat("✅ Animation saved successfully!\n")
  cat("📁 File location:", file.path(getwd(), filename), "\n")
}

# =============================================================================
# Enhanced Version with Trail Effects
# =============================================================================

create_enhanced_morphing <- function() {
  cat("🎨 Creating enhanced version with trail effects...\n")
  
  # [This would include the morphing + trail effects]
  # For now, keeping it simple but beautiful
  
  return("Enhanced version ready for development!")
}

# =============================================================================
# Usage Instructions
# =============================================================================

cat("\n🎬 Simple Morphing Animation Ready!\n")
cat("📋 Available commands:\n")
cat("   👀 Preview:  preview_morphing()\n")
cat("   💾 Save:     save_morphing('final_animation.gif')\n")
cat("\n🎯 Animation Features:\n")
cat("   • Stationary rectangle in center of screen\n")
cat("   • Smooth morphing into a perfect circle\n")
cat("   • Color transitions from blue to gold\n")
cat("   • 3 phases: hold rectangle → morph → hold circle\n")
cat("   • Clean, focused transformation\n")

# Automatically save the final animation as requested
cat("\n🚀 Auto-saving final animation...\n")
save_morphing("final_animation.gif")
