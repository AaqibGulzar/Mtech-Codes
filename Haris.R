library(readxl)
library(reshape2)
library(ggplot2)
Peak_discharge <- read_excel("~/Peak discharge.xlsx")
View(Peak_discharge)
peaks = melt(Peak_discharge[-64,],id.vars="Year")
peaks$value  = as.numeric(peaks$value)
fig = plot_ly(
  data = peaks ,
  x = ~ Year,
  y =  ~ value,
  color = ~ variable,
  type="scatter",
  mode = "lines+markers "
)


fig = plot_ly(data=Peak_discharge,
              x = ~Year,
              y = ~ Khanbal,
              name="Khanbal",
              type = "scatter",
              mode = "lines+markers")
fig = fig %>% add_trace(y =~Sangam,
                        name = "Sangam",
                        mode = "lines+markers")
