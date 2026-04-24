s_plot <- function(tslist, y_axis = "", highlight = FALSE, save = TRUE, filename = "timeseries_plot.pdf") {
  fig_width <- 14
  fig_height <- 5
  bg_col <- "white"
  # bg_col <- rgb(0, 0, 0, 0.1)


  merged_xts <- do.call(merge, tslist)

  df <- data.frame(date = zoo::index(merged_xts), zoo::coredata(merged_xts))

  ts_names <- names(tslist)
  colnames(df) <- c("date", ts_names)

  df_long <- pivot_longer(df, cols = -date, names_to = "series", values_to = "value")

  p <- ggplot(
    df_long,
    aes(
      x = date,
      y = value,
      color = series,
      # linetype = "solid"
      linetype = series
    )
  )

  if (highlight) {
    p <- p +
      # Highlight COVID-19 pandemic (approx. March 2020 to Dec 2020)
      annotate(
        "rect",
        xmin = as.Date("2020-02-01"),
        xmax = as.Date("2020-12-31"),
        ymin = -Inf,
        ymax = Inf,
        fill = rgb(0, 0, 0, 0.1)
      ) +
      # Highlight Invasion of Ukraine (Feb 24, 2022 to end of 2022)
      annotate(
        "rect",
        xmin = as.Date("2022-02-24"),
        xmax = as.Date("2022-08-31"),
        ymin = -Inf,
        ymax = Inf,
        fill = rgb(0, 0, 0, 0.1)
      ) +
      # Highlight 2024 US Elections (Oct 2024 to Nov 2024)
      # Tariff War & "Liberation Day" (Apr 2025 to May 2025)
      annotate(
        "rect",
        xmin = as.Date("2024-11-05"),
        xmax = as.Date("2025-05-01"),
        ymin = -Inf,
        ymax = Inf,
        fill = rgb(0, 0, 0, 0.1)
      ) +
      # Local Politics instability
      # announcement of candidate for presidency (Dec 5 2025)
      annotate(
        "rect",
        xmin = as.Date("2025-12-01"),
        xmax = as.Date("2025-12-31"),
        ymin = -Inf,
        ymax = Inf,
        fill = rgb(0, 0, 0, 0.1)
      )
  }

  p <- p +
    geom_line(linewidth = 0.8) +
    geom_text(
      data = df_long[df_long$date == max(df_long$date), ],
      aes(label = series, x = date, y = value, color = series),
      hjust = 0, # Left-align the text
      nudge_x = 10, # Push it slightly to the right of the line end
      size = 4, # Adjust text size
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = setNames(
        c("#000000", "#686868"),
        ts_names
      )
    ) +
    scale_linetype_manual(
      values = setNames(
        c("solid", "solid"),
        ts_names
      )
    ) +
    labs(
      title = NULL,
      x = "Date",
      y = y_axis
    ) +
    theme(
      plot.margin = margin(t = 10, r = 20, b = 10, l = 20, unit = "pt"),
      plot.background = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = bg_col, color = NA),
      # Add a thin 0.1 border around the panel so it stops floating
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.1),
      text = element_text(color = "black"),
      legend.title = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.position = "none"
    )

  if (save) {
    ggsave(
      filename = file.path("outputs", filename),
      plot = p,
      width = fig_width,
      height = fig_height,
      units = "in",
      device = "pdf"
    )
  } else {
    p
  }
}

acf_pacf <- function(ts, v_name, save = TRUE) {
  acf_plot <- forecast::ggAcf(ts, lag.max = 20) +
    labs(
      title = paste0("ACF: ", v_name),
      subtitle = "MA(q)",
      x = "lag",
      y = "ACF"
    ) +
    theme_minimal()

  pacf_plot <- forecast::ggPacf(ts, lag.max = 20) +
    labs(
      title = paste0("PACF: ", v_name),
      subtitle = "AR(p)",
      x = "lag",
      y = "PACF"
    ) +
    theme_minimal()

  combined_plot <- acf_plot / pacf_plot

  if (save) {
    ggsave(
      filename = paste0("acf_pacf", v_name, ".pdf"),
      plot = combined_plot,
      width = 14,
      height = 7,
      units = "in",
      device = "pdf"
    )
  } else {
    combined_plot
  }
} 
