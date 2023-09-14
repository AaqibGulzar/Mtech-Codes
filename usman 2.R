library(readxl)
X2011 <- read_excel("~/usman reg_files/2011.xlsx")



ggplot(X2011) + geom_line(aes(x = Cycle, y = Area),linewidth = 1,color = "blue") + 
  geom_point(aes(x = Cycle, y = Area), size = 4,alpha = 0.5,color = "steelblue") +
  xlab("Year") + ylab("Area") +
  theme(panel.border = element_rect(
    color = "black",
    fill = NA,
    linewidth = 1
  )) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 12, face = "bold"),  # Make the axis tick labels bold
    axis.ticks.length = unit(0.2, "cm")
  ) +
  scale_x_continuous(breaks = seq(2011, 2023, 1)) +
  scale_y_continuous(breaks = seq(min(X2011$Area), max(X2011$Area), 8000))


ggsave("usman.png",height=20,width=30,units="cm",dpi=400)
