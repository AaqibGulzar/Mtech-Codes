library(tidyverse)
databasit <- read_csv("c:/Users/ASUS/OneDrive/Documents/Basit/total_pptn_basit.csv")
View(databasit)

# Extract the date and hour components using string manipulation
basit = databasit %>% 
  mutate(date = substr(date_time, 1, 10),
         hour = substr(date_time, 12, 13),
         hour = ifelse(hour == "", "0h", paste0(hour, "h")))


basit = basit[,c(-1,-10)] %>% na.omit()


basit

basit <- basit %>%
  separate(date, into = c("year", "month", "day"), sep = "-")

basit


basit_max <- function() {
  stations <- c("tp_srinagar","tp_qazigund","tp_pahalgam",
                 "tp_kupwara","tp_kokernag","tp_gulmarg","tp_konibal","tp_awantipora")
  years <- as.character(seq(2000, 2020))
  hours <- c("0h", "06h", "12h", "18h")
  max_vals <- matrix(0, nrow = length(stations) * length(years) * length(hours), ncol = 4)
  row_counter <- 1
  
  for (station in stations) {
    for (year in years) {
      for (hour in hours) {
        df <- basit[basit$year == year & basit$hour == hour, ]
        max_val <- max(df[[station]]) %>% round(digits=3)
        max_vals[row_counter, ] <- c(station, year, hour, max_val)
        row_counter <- row_counter + 1
      }
    }
  }
  
  colnames(max_vals) <- c("Station", "Year", "Hour", "Max Value")
  return(as.data.frame(max_vals))
}

max_data =  basit_max()
write.csv(max_data,"Maximum values Full.csv")
