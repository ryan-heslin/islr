#My solution to an r/rstates problem
#true/false col + counter
#mutate days ifelse(days-lag(days >1, lage(days)))
##Use cut!
##

heat <- tibble::tibble(day = seq_len(100), temp = rnorm(100, mean = 30, sd = 10))
#To test 4-day streaks at start or end of series
heat[1:4, "temp"] <- 31
heat[96:100, "temp"] <- 29
heat_days <- heat$day[heat$temp >25]

wave_starts <- heat_days[(heat_days - lag(heat_days) > 1 | is.na(lag(heat_days))) & lead(heat_days, 4) - heat_days == 4] %>% .[!is.na(.)]
wave_ends <- heat_days[(lead(heat_days) - heat_days > 1 | is.na(lead(heat_days))) & heat_days -lag(heat_days, 4) == 4] %>% .[!is.na(.)]

#Create vector of breaks to divide potential waves, adding 1 to ends to prevent overlapping intervals.
days <- map2(wave_starts, wave_ends, ~seq(.x, .y)) %>% reduce(c)

#Divide column of potential heat wave days into intervals. Then group by each streak of potential wave days
#and check if at least 3 had temps above 30. Finally, join this table to original.
wave_check <- tibble(day = days, range= cut(days, breaks = c(wave_starts, max(wave_ends)), include.lowest = TRUE, right = FALSE,
                                            labels = seq(1, length(wave_starts)))) %>% group_by(range) %>% 
  mutate(wave = sum(heat$temp[day] > 30) >2) %>% 
  ungroup()
heat <- left_join(heat, wave_check %>% select(-range), by = "day")

#Get heat days