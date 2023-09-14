#Extract a variable from a pipes stream without quitting it.
library(tidyverse)
mtcars %>% 
    mutate(cyl=cyl*2) %>%
  {
    mean_temp <<- 3*mean(.$gear)  # use `<<-` to creaye a global variable
    .                              # add `.` to resume where you left         
  } %>%
  filter(cyl>mean_temp)