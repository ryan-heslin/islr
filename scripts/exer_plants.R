#TIdy Tuesdays 8/18/20: Endangered Plants
#Ryan Heslin
#
library(tidyverse)
library(purrr)
library(rlang)
library(lubridate)


theme_set(theme_minimal())
plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

walk(list(plants, actions, threats), View)

#View list of dataframes automatically
check_data <- function(dfs){
  library(purrr)
  
  walk(dfs, View)
  walk(dfs, glimpse)
  walk(dfs, str)
}

check_data(list(plants, actions, threats))

# PRovided cleaning scrpt
threats <- plants %>% 
  pivot_longer(cols = contains("threat"), names_to = "threat_type", 
               values_to = "threatened", names_prefix = "threat_") %>% 
  mutate(threat_type = case_when(
    threat_type == "AA" ~ "Agriculture & Aquaculture",
    threat_type == "BRU" ~ "Biological Resource Use",
    threat_type == "RCD" ~ "Commercial Development",
    threat_type == "ISGD" ~ "Invasive Species",
    threat_type == "EPM" ~ "Energy Production & Mining",
    threat_type == "CC" ~ "Climate Change",
    threat_type == "HID" ~ "Human Intrusions",
    threat_type == "P" ~ "Pollution",
    threat_type == "TS" ~ "Transportation Corridor",
    threat_type == "NSM" ~ "Natural System Modifications",
    threat_type == "GE" ~ "Geological Events",
    threat_type == "NA" ~ "Unknown",
    TRUE ~ NA_character_
  )) 

actions <- plants %>% select(-contains("threat")) %>% pivot_longer(cols = contains("action"), names_to = "action_type", 
              values_to = "action_taken", names_prefix = "action_") %>% 
  mutate(action_type = case_when(
           action_type == "LWP" ~ "Land & Water Protection",
           action_type == "SM" ~ "Species Management",
           action_type == "LP" ~ "Law & Policy",
           action_type == "RM" ~ "Research & Monitoring",
           action_type == "EA" ~ "Education & Awareness",
           action_type == "NA" ~ "Unknown",
           TRUE ~ NA_character_
         ),
         action_type = as_factor(action_type)) 
  

combined <- threats %>% inner_join(actions) %>% 
  mutate(across(binomial_name:threat_type, as_factor),
         action_type = as_factor(action_type),
         year_last_seen = fct_relevel(year_last_seen, c("Before 1900", "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020"))) %>%
  group_by(binomial_name) %>% 
  mutate(total_actions = sum(action_taken), total_threats=sum(threatened))

  #What decades saw the most extinctions?
  #
  combined %>% filter(red_list_category == "Extinct") %>% 
    group_by(year_last_seen) %>% 
    summarize(count = n_distinct(binomial_name)) %>% 
    ggplot(aes(year_last_seen, count, fill = year_last_seen)) +
    geom_col() +
    labs(title = "Time of Extinction", ylab = "Number of Species", xlab = "Time Last Seen") +
    theme(legend.position = "none")
  
  plants %>% ggplot(aes(continent, fill = continent)) +
  geom_bar()
  
  #Which actions are most commonly taken?
  threats %>% full_join(actions, by = c("binomial_name")) %>% 
    filter(action_type != "Unknown") %>% 
    group_by(binomial_name) %>%
    mutate(both = if_else(threatened ==1 & action_taken ==1, 1, 0)) %>% 
    ungroup() %>% 
    group_by(threat_type, action_type) %>% 
    summarize(interactions = sum(both)) %>% 
    ggplot(aes(action_type, interactions, fill = action_type)) +
    geom_col(position = "dodge") +
    theme(axis.text.x = element_blank()) +
    facet_wrap(. ~ threat_type, scales = "free_y") +
    ggtitle("Actions by Threat Type")
  
  #Actions and threats did not differ across preservation status
  action_summary <- actions %>% group_by(red_list_category) %>% 
    summarize(avg_actions = sum(action_taken) /n_distinct(binomial_name))
  threats %>% group_by(red_list_category) %>% 
    summarize(avg_threats =sum(threatened) /n_distinct(binomial_name)) %>% 
    left_join(action_summary, by = "red_list_category") %>% 
    pivot_longer(cols = c(avg_threats, avg_actions)) %>% 
    ggplot(aes(name, value, fill = red_list_category)) +
        geom_col(position = "dodge")
  
#How did action type differ by era?
combined %>% filter(action_type != "Unknown") %>% 
  group_by(year_last_seen, action_type) %>% 
  summarize(decade_action_count = sum(action_taken)) %>% 
  mutate(year_last_seen = fct_relabel(year_last_seen, function(x) x <- str_remove(x, "-.*$")),
  year_last_seen =as.numeric(as.character(fct_recode(year_last_seen, "1880" = "Before 1900")))) %>% 
  ggplot(aes(year_last_seen, decade_action_count, col = action_type)) +
  geom_line()

plants %>%
  mutate(year_last_seen = fct_relabel(year_last_seen, function(x) x <- str_remove(x, "-.*$")),
         year_last_seen =as.numeric(as.character(fct_recode(year_last_seen, "1880" = "Before 1900")))) %>% 
           ggplot(aes(group, year_last_seen, col = continent)) +
  geom_jitter() +
  coord_flip() +
  scale_y_continuous(breaks = seq(1880, 2020, by =20))

plants %>% 
  mutate(year_last_seen = fct_relabel(year_last_seen, function(x) x <- str_remove(x, "-.*$")),
         year_last_seen =as.numeric(as.character(fct_recode(year_last_seen, "1880" = "Before 1900")))) %>% 
  group_by(year_last_seen) %>% 
  summarize(prop_extinct = n() /nrow(plants)) %>% 
  ungroup() %>% 
  mutate(cum_extinct = cumsum(prop_extinct)) %>% 
  ggplot(aes(year_last_seen, cum_extinct)) +
  geom_area(alpha = .3, fill = "red") +
  geom_line(aes(y = prop_extinct, color = "gray", linetype = "dashed")) +
  scale_y_continuous(breaks = seq(1880, 2020, by =20)) +
  scale_fill_manual(name = "Cumulative Proportion Extinct")

#Not very intersting, but useful learning
group_mods <- actions %>% filter(action_type != "Unkown") %>% 
  group_by(binomial_name) %>% 
  mutate(total_actions = sum(action_taken)) %>% 
  ungroup() %>% 
  group_by(continent) %>% 
  nest() %>% 
  mutate(mods = map(
    data, ~lm(total_actions~ group, data = .x)),
    resids = map(mods, residuals),
    preds = map(mods, predict)) %>% 
  unnest(c(data, resids, preds)) 

#Predicted value by continent
group_mods %>% 
  distinct(continent, preds, .keep_all = TRUE) %>% 
  ggplot(aes(group, preds, fill = continent))+
  geom_col(position = "dodge")