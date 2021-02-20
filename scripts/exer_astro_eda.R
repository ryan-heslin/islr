library(tidyverse)
library(tidytuesdayR)

theme_set(theme_minimal())

astronauts <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')
View(astronanuts)
glimpse(astronauts)
str(astronauts)
summary(astronauts)


splitnames <- (str_split(astronauts$name, ",", simplify = T))

#Combine character matrix entries into names
fun <- function(row){
  new <- paste(splitnames[row, 2 ], splitnames[row, 1], splitnames[row, 3])
  new  
}

new_name <- str_trim(map_chr(seq(1, nrow(splitnames)), fun))
str_match(astronauts$name, ("^(.*),\\s(.*),?"))
astronauts <-astronauts %>% rename(mission_eva = field21) %>% 
  mutate(name = if_else(str_detect(name, ","), new_name, name),
         across(where(is.character), as_factor))

astronauts <- astronauts %>%  group_by(name) %>%
  mutate(career_span = max(year_of_mission) - min(year_of_mission),
                                        select_age = year_of_selection - year_of_birth,
                                        num_occupation = length(unique(occupation)),
                                        avg_mission_length = mean(hours_mission)) %>% 
  ungroup()
by_astro <- astronauts %>% group_by(name)
#Who had the longest careers?
astronauts %>% arrange(desc(career_span))



#Did selection age vary by year?
astronauts %>% group_by(year_of_selection) %>%
  mutate(mean = mean(select_age)) %>% 
  ggplot(aes(year_of_selection, mean, col = fct_lump_n(nationality, 5))) +
  geom_point() +
  geom_line() +
  geom_smooth()
#Which stronauts had the highest avg missio nlength

astronauts %>% filter(total_number_of_missions >1) %>% 
  arrange(desc(avg_mission_length))

# How was EVA time divided on the most productive missions?
# 
temp <- astronauts %>% group_by(mission_title) %>%
  mutate(mission_eva = sum(eva_hrs_mission), name = fct_drop(name)) %>%
  
  ungroup()

temp %>% filter(mission_eva >= sort(unique(temp$mission_eva), decreasing = TRUE)[6]) %>%
  ggplot(aes(x = fct_reorder(name, eva_hrs_mission/mission_eva), y =  eva_hrs_mission/mission_eva, position = "fill")) +
  facet_wrap( . ~ mission_title, scales = "free") +
  geom_col(aes(fill = name)) +
  scale_x_discrete(drop = T) +
  theme(legend.position = "none")+
  coord_flip()
 
  


astronauts %>% count(mission_title, eva_hrs_mission)
#Did EVA duration increase by year?
astronauts %>% group_by(year_of_mission) %>%
  mutate(sum = sum(eva_hrs_mission)) %>% 
  ggplot(aes(year_of_mission, sum)) +
  geom_point() +
  geom_line()

#How many missions had international crews?
temp <- astronauts %>% group_by(mission_title) %>% 
  mutate(intl = if_else(n_distinct(nationality) > 1, T, F)) %>%
  ungroup()
  temp <- temp %>% distinct(mission_title, .keep_all = T) 
  aggregate(temp$intl, list(temp$year_of_mission), mean) %>% 
  ggplot(aes(Group.1, x)) +
  geom_point() +
  geom_line()

# Were military and civiliana stronauts meaningfully different? Nope.
astronauts %>% group_by(military_civilian) %>% 
  summarize(across(total_eva_hrs:avg_mission_length, mean)) %>%
  pivot_longer(cols = total_eva_hrs:avg_mission_length) %>% 
  ggplot(aes(military_civilian, value, fill = military_civilian)) +
  geom_col() +
  facet_wrap(. ~ name, scales = "free")

# How much time as spent in orbit each year?
astronauts %>% group_by(year_of_mission, nationality) %>% 
  summarize(orbit_time = sum(hours_mission), nationality = nationality) %>% distinct() %>% 
  ggplot(aes(year_of_mission, orbit_time, fill = fct_lump_n(nationality, 2))) +
  geom_area(position = "stack", alpha = .3, outline.type = "full")
  stat_smooth(geom = "area", method = "loess", position = "stack", span = .3, alpha = .3)
  
#Which astronauts had the longest career spans?
astronauts %>% slice_max(career_span, n = 10) %>%
  ggplot(aes(x =name, y = max(year_of_mission))) + 
  labs(title = "Long-Runners", ylab = "Career Span")+
  geom_segment(aes(y = year_of_selection, yend = year_of_mission, xend = name,
                   col = name), lineend = "butt") +
  theme(legend.position = "none") +
  geom_point(aes(name, year_of_mission, size = hours_mission, col = name), alpha = .3) +
  coord_flip()
  

 
  
  
  


