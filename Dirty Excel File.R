library(tidyverse)

segments = c("consumer","corporate","home_office")
ship_modes = c("first_class","same_day",
               "second_class","standard_class")

 group_shipping_names = paste0(
   rep(segments,each = length(ship_modes)),
   "_",
   rep(ship_modes,length(segments))
 )  

group_shipping_names

clean_data = readxl::read_excel(
  "c:/Users/ASUS/Downloads/1.-Badly-Structured-Sales-Data-1.xlsx",
  sheet = 1,
  skip = 1,
  .name_repair = janitor::make_clean_names
) %>%
  select(!starts_with("x")) %>%
  rename(order_id = "ship_mode") %>%
  rename_with( ~ c("order_id" , group_shipping_names)) %>%
  slice(-1) %>%
  pivot_longer(
    cols = -1,
    names_pattern = glue::glue('({paste0(segments, collapse = "|")})_(.*)'),
    names_to = c('segment', 'shipping_mode'),
    values_drop_na = TRUE
  ) %>%
  arrange(segment, shipping_mode, order_id)

#write.csv(clean_data,"clean_data.csv")


