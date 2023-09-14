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



df_rank = arrange(df,-desc(max_values))
df_rank
df_rank$rank = seq(40,1,-1)
df_rank$qi = (df_rank$rank - 0.44)/(40 + 1 -2*0.44)
df_rank$nep = 1 - df_rank$qi
df_rank$Tp_est = 1/(1- df_rank$nep)


# In this step, the data is assumed to follow the 'Gumbel' or Extreme Value Type 1' distribution. 
# The CDF of the Extreme Value Type I or Gumbel distribution 

x_bar = mean(df_rank$max_values)
Var = var(df_rank$max_values)
alpha = ((6^0.5) * (Var)^0.5)/3.14
u = x_bar - 0.5772* alpha 

df_rank$p_theor = exp(1)^( -exp(1)^ (-(df_rank$max_values - u) / alpha))
df_rank$Tp_theor = 1/(1 - df_rank$p_theor)



ggplot(df_rank, aes(x = Tp_est, y = max_values)) +
  geom_point(aes(color = "Tp_est")) +
  geom_line(aes(x = Tp_theor, color = "Tp_theor")) +
  labs(x = "Return Period", y = "max_values") +
  scale_color_manual(name = "Tp Values", values = c("Tp_est" = "blue", "Tp_theor" = "red")) +
  theme_minimal()






