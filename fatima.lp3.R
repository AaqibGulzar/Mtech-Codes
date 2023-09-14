library(readxl)
library(lmomco)
library(tidyverse)

Sheshnag <- read_excel("D:/Lidder Data/Sheshnag.xlsx", 
                       col_types = c("date", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric"))

Sheshnag %>% is.na() %>% sum()

Sheshnag = Sheshnag[,c(1,2)] %>% na.omit()
Sheshnag$Date = as.Date(Sheshnag$Date)
colnames(Sheshnag) = c("date","q")
Sheshnag$date = year(Sheshnag$date)
Sheshnag %>% head()


years = unique(Sheshnag$date)
maxval = function(year) {
  max = (Sheshnag %>% filter(date == year))$q %>% max()
  return(max)
}


max_values <- sapply(years, maxval)

df <- data.frame(year = years, max_values = max_values)


Q = log10(df$max_value) 

#Generate plotting positions
n = length(Q)
r = n + 1 - rank(Q)  # highest Q has rank r = 1
RP = (n + 1)/r
RP = sort(RP,decreasing=T)
EP = 1/RP
moments::skewness(Q)
skews = c(-2.544,  0.050, 0.853, 1.245, 1.643, 1.890, 2.104, 2.294)

q = mean(Q) + skews*sd(Q)
QTtick = 10^(mean(Q) + skews*sd(Q))
rp = c(1.01,2,5,10,25,50,100,200)
increment <- 1.96 * sd(Q)/(n^0.5)

lb = 10^(q - increment)
ub = 10^(q + increment)

color1 <- c("95% CI" = "red")


ggplot() +
  geom_point(aes(x = rp, y = QTtick, fill = "Flood Quantiles")) +
  geom_line(aes(x = rp, y = QTtick), linewidth = 0.4) +
  scale_fill_manual(values = color2) + 
  geom_line(aes(x = rp, y = lb, color = "95% CI"))  +
  geom_line(aes(x = rp, y = ub), color = "red") + 
  scale_color_manual(values = color1) +
  xlab("Return Period (Years)") + ylab("Discharge (cfs)")  +
  ggtitle("Flood Frequency Analysis of Sheshnag using LP3") + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  theme(plot.title = element_text(color = "dodgerblue3", size = 10,
                                  face = "bold",
                                  hjust=0.1,
                                  vjust=-12),
    legend.text = element_text(size = 11), 
        legend.title = element_text(size = 15),
        panel.background = element_rect("white"),
        panel.grid = element_line(colour = "grey", linetype = "dashed", linewidth = 0.1)) +
  guides(fill = guide_legend(title = NULL, override.aes = list(color = NULL)),
         color = guide_legend(title = NULL, override.aes = list(fill = NULL))) +
  scale_x_continuous(breaks=seq(0,200,25))


#ggsave("lp3.png",dpi=1000,width=8,height=6,units="in")
