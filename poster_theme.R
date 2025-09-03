## plot theme

poster_theme <- theme(
  aspect.ratio = 1,
  axis.line = element_line(size = 1, color = "black"),
  panel.grid.major = element_blank(),   # Remove major grid lines
  panel.grid.minor = element_blank(),   # Remove minor grid lines
  axis.ticks = element_line(size = 0.5),  # Add axis ticks
  axis.text = element_text(size = 12),    # Increase axis text size
  axis.title = element_text(size = 14),   # Increase axis title size
  plot.title = element_text(size = 16)    # Increase plot title size
)