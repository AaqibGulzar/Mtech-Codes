t <- seq(5, -1, length.out = 1000) * pi

spiral <- data.frame(x = sin(t) * 1:1000, 
                     y = cos(t) * 1:1000,
                     text = paste("i am aaqib gulzar","this is my laptop","ok google ok")
)

ggplot(spiral, aes(x, y, label = text)) +
  geom_textpath(size = 7, vjust = 2, include_line = FALSE) +
  coord_equal(xlim = c(-1500, 1500), ylim = c(-1500, 1500))


df <- expand.grid(x = seq(nrow(volcano)), y = seq(ncol(volcano)))
df$z <- as.vector(volcano)

ggplot(df, aes(x, y, z = z)) + 
  geom_contour_filled(bins = 6, alpha = 0.6) + 
  geom_labelcontour(bins = 6, size = 2.5, padding = unit(0.05, "in")) + 
  scale_fill_manual(values = terrain.colors(11)) + 
  theme_classic() +
  theme(legend.position = "none")

ggplot(iris, aes(x = Sepal.Length, colour = Species)) +
  geom_textpath(aes(label = Species), stat = "density",
                size = 6, fontface = 2, hjust = 0.2, vjust = 0.3)


