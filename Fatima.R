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

df <- data.frame(year = years, max_value = max_values)


# Extract the discharge data
discharge <- df$max_value %>% log10()

# Fit the Pearson Type III distribution
lmr <- lmoms(discharge)
params <- parpe3(lmr)

# Calculate the quantiles for specific return periods
return_periods <- c(1.01, 2, 5, 10, 25, 50, 100, 200, 500)
quantiles <- quape3(1 - 1 / return_periods, params)
10^quantiles
# Calculate the standard errors for the quantiles
n <- length(discharge)
increment <- 1.96 * sd(discharge)/(n^0.5)

# Create a data frame for plotting
plot_data <- data.frame(Return_Period = return_periods,
                        Quantile = quantiles,
                        Lower_CI = 10^(quantiles - increment),
                        Upper_CI = 10^(quantiles + increment))


color1 <- c("95% CI" = "lightblue")
color2 <- c("Flood Quantiles" = "darkred")

ggplot(plot_data, aes(x = Return_Period, y = 10^(Quantile))) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI,fill = "95% CI"), alpha = 0.5) + 
  scale_fill_manual(values=color1) +
  geom_line(aes(color = "Flood Quantiles"),linewidth = 0.5) +
  scale_color_manual(values=color2) +
  geom_point(color ="steelblue",size=2)+
  scale_x_continuous(trans = "log10", breaks = c(1.01,2, 5, 10, 25, 50, 100, 200, 500),
                     labels = c(1.01,2, 5, 10, 25, 50, 100, 200, 500)) +
  scale_y_log10() +
  labs(x = "Return Period", y = "Discharge", title = "Flood Frequency Curve for Sheshnag") + theme_bw()+
  theme(legend.title=element_blank(),legend.key.size = unit(0.9, 'cm'),
        legend.position=c(0.9,0.5)) 
#ggsave("FFC.png",dpi=500,height=20,width=30,units="cm")





