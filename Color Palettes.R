
library(ggplot2)
library(viridisLite)

# Create a sample dataset
data <- data.frame(
  category = c("A", "B", "C", "D", "E"),
  value = c(10, 20, 30, 40, 50)
)

# Define the number of colors
n <- 5

# List of palettes
palettes <- list(
  rainbow = rainbow(n),
  hcl = hcl(h = seq(15, 375, length = n), l = 65, c = 100),
  qualitative1 = RColorBrewer::brewer.pal(n, "Set1"),
  qualitative2 = RColorBrewer::brewer.pal(n, "Set2"),
  qualitative3 = RColorBrewer::brewer.pal(n, "Set3"),
  viridis = viridis(n),
  magma = magma(n),
  plasma = plasma(n),
  inferno = inferno(n),
  heat = heat.colors(n),
  gray = gray.colors(n),
  sequential1 = RColorBrewer::brewer.pal(n, "Blues"),
  sequential2 = RColorBrewer::brewer.pal(n, "Greens"),
  sequential3 = RColorBrewer::brewer.pal(n, "Purples"),
  diverging1 = RColorBrewer::brewer.pal(n, "RdYlBu"),
  diverging2 = RColorBrewer::brewer.pal(n, "RdYlGn"),
  diverging3 = RColorBrewer::brewer.pal(n, "PuOr"),
  matlab = cm.colors(n)
)

 ggplot(data, aes(x = category, y = value, fill = category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = palettes$diverging1) +
    labs(title = paste(palette_name, "Palette Bar Plot"))
  

