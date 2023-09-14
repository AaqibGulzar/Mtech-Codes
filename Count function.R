not_cancelled= flights %>% filter(!is.na(dep_delay),!is.na(arr_time))
not_cancelled2 <-flights %>% filter(!is.na(arr_delay))
................................................................................
 not_cancelled2 %>%
   group_by(year, month, day) %>%
   summarise(avg_delay1 = mean(arr_delay),
             avg_delay2 = mean(arr_delay[arr_delay > 0]))
............................................................................... 
 
                                      #[Use of count function].
                                      #Inside the count function,the first argument
                                      # will get counted as per its category.
 flights %>% count(tailnum)......    #will count flights as per their tailnum

 
  delay_per_carrier = flights %>% 
   count(carrier, wt = arr_delay).....#here we calculate,asper
                                      # the carrier,their arrival delays.
 delay_per_carrier %>% 
   arrange(desc(n))                   #in descending order
 
................................................................................
  #here < 500 will give either true or false,then sum() will give the count.
 #also mean() of this will give us a proportion of flights with dep_time <500
 
flights %>%
  group_by(year, month, day) %>%
  summarise(early = sum(dep_time < 500, na.rm = T))


flights %>%
  group_by(year, month, day) %>%
  summarise(early = mean(dep_time < 500, na.rm = T))

..............................................................................

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(carat), binwidth = 0.5)  #histogram for cont variable.
diamonds %>%
  count(cut_width(carat, 0.5))  # counting the width of interval by 
                                #combining count with cut_width.


#to plot multiple histograms at once,bars might not be useful,
#instead we use lines.
ggplot(data = small_carat) +
  geom_freqpoly(mapping = aes(
    x = carat,
    color = cut,
    binwidth = 0.1
  )) +
  theme_classic() #change to geom_histogram and fill =cut to see a colorful plot

 
diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y)) 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 