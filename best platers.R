best_players = tibble(name = c("Messi","Cristiano","Neymar","Haaland"),
                      Speed = c(80,92,79,88),
                      Dribble = c(96,89,92,70),
                      Rightfoot = c(77,95,88,74),
                      Leftfoot = c(95,90,75,83),
                      Header = c(60,92,62,85),
                      Freekick = c(90,91,75,70),
                      Penalty = c(80,95,80,81),
                      Playmaking = c(93,82,86,78),
                      Power = c(80,95,80,92),
                      Longshot = c(80,96,80,82),
                      Physique = c(82,96,70,88))



# 1.0 DATA PREPARATION ----
data_prep_tbl <- best_players %>%
  rename(type = 1) %>%
  pivot_longer(
    cols = -type
  ) %>%
  mutate(
    type = as_factor(type),
    name = as_factor(name)
  )

data_prep_tbl

# 2.0 BASIC RADAR PLOT ----

# The trick
coord_radar <- function (theta = "x", start = 0, direction = 1, clip = "on") {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction), clip = clip,
          is_linear = function(coord) TRUE)
}


basic_radar_plot <- data_prep_tbl %>%
  ggplot(aes(name, value, group = type)) +
  geom_polygon(aes(fill = type, color = type), alpha = 0.25) +
  geom_point(aes(color = type),shape = 12) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0,100,10)) +
  coord_radar(start = 0, clip = "off") +
  facet_wrap(~ type)

basic_radar_plot


#png("radar2.png", width = 12, height = 8,res=2000,units="in")  # Adjust the width and height as needed

# 3.0 DRESS UP THE RADAR PLOT ----
basic_radar_plot  +
  theme_minimal() +
  scale_fill_manual(values = c("#588f3a", "#E7B800", "#00AFBB", "#FC4E07")) +
  scale_color_manual(values = c("#588f3a", "#E7B800", "#00AFBB", "#FC4E07")) +
  labs(
    title = "Best Players in Their",
    subtitle = "PRIME!",
    legend = NULL,
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(t = 5, b = 20),
      size = 28,
      face = "bold",
      family = "serif"
    ),
    plot.subtitle = element_text(
      color = "dodgerblue",
      hjust = 0.5,
      margin = margin(t = 0, b = 20),
      size = 20,
      face = "bold",
      
    ),
    strip.text = element_text(
      size = 18,
      face = "bold",
      margin = margin(t = 5, b = 5)
    ),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      vjust = -1,
      size = 8
    ),
    panel.spacing = unit(50, "points"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = c(rep("#ebebeb", 11), NA))
  ) 

#dev.off()
