
# ------------------------------ |> DATA TIDYING AND CLEANING <| --------------------------------------- 


library(tidyverse)
data("CO2")
CO2
tibble(CO2)

CO2_tb = as_tibble(CO2) #convert into a tibble

CO2_tb |> select(1,2,5) # Select only desired columns

CO2_tb |> filter(uptake > 30) # filter based on a condition

CO2_tb |> group_by(Plant)  # grouped data, this will however give same dataset but will
                            # mention the number of groups.
CO2_tb |> 
  group_by(Plant) |>               # groupn then
  summarise(meanup = mean(uptake),   # summarise per group !!!
            sdup = sd(uptake))

CO2_tb |> 
  group_by(Plant) |>              
  summarise(meanup = mean(uptake),
            sdup = sd(uptake),cv = sdup/meanup)  # add new column, "cv", even though sdup and
                                                # meanup are yet to be created!!!!!!!!!!!!!!!

CO2_tb |> 
  group_by(Plant) |>
  summarise(meanup = mean(uptake),             # same thing!!!!!!!!!!!!!!!!!!!!!!
            sdup = sd(uptake)) |> mutate(cv = sdup/meanup) |> 
  arrange(desc(cv)) # arrange as per the variable, in decreasing order



Group the mtcars tibble by the gear variable, summarize the medians of the mpg and
disp variables, and mutate a new variable that is the mpg median divided by the disp
median, all chained together with the %>% operator.







