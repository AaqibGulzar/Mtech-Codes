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
avgval = function(year) {
  avg = (Sheshnag %>% filter(date == year))$q %>% mean()
  return(avg)
}


avg_values <- sapply(years, avgval)

df2 <- data.frame(year = years, avg_values = avg_values)



years = unique(Sheshnag$date)
maxval = function(year) {
  max = (Sheshnag %>% filter(date == year))$q %>% max()
  return(max)
}


max_values <- sapply(years, maxval)

df <- data.frame(year = years, max_values = max_values)

ggplot() + 
  geom_point(data=df,aes(x = year,y = max_values),color = "tomato",size = 2.6) +
  geom_line(data = df,aes(x = year,y = max_values),color = "tomato",linewidth = 0.6) +
  theme_minimal() +  
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  xlab("Year") + ylab("Annual Maximum Peak values") + theme_bw() + 
  scale_x_continuous(breaks=seq(1975,2019,5))
  


#ggsave("Annual MAx.png",dpi=500,width=8,height=6,units="in")
  
ggplot() + 
  geom_point(data=df2,aes(x = year,y = avg_values),color = "steelblue",size = 2.6) +
  geom_line(data = df2,aes(x = year,y = avg_values),color = "steelblue",linewidth = 0.6) +
  theme_minimal() +  
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  xlab("Year") + ylab("Annual Average Peak values") + theme_bw() + 
  scale_x_continuous(breaks=seq(1975,2019,5))  

#ggsave("Annual Avg.png",dpi=500,width=8,height=6,units="in")
       
max = "red"
avg = "steelblue"

library(ggnewscale)
ggplot() + 
  geom_point(data=df2,aes(x = year,y = avg_values,color = "avg"),size = 2.6) +
  geom_line(data = df2,aes(x = year,y = avg_values,color = "avg"),linewidth = 0.6) +
    scale_color_manual(values=avg) +
  new_scale_color() +
  geom_point(data=df,aes(x = year,y = max_values,color = "max"),size = 2.6) +
  geom_line(data = df,aes(x = year,y = max_values,color = "max"),linewidth = 0.6) +
    scale_fill_manual(values=max) + 
  theme_minimal() +  
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  xlab("Year") + ylab("Comparison of  Peak values") + theme_bw() + 
  scale_x_continuous(breaks=seq(1975,2019,5))  

#ggsave("comparison.png",dpi=800,width=8,height=6,units="in")

















     