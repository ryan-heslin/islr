library(tidyverse)
library(readxl)
library(here)
library(naniar)

raw <- read_xlsx("voice_data.xlsx")
glimpse(raw)

#line 6460
# replace NAs
clean <- replace_with_na_all(raw, ~.x == "NA")
glimpse(clean)

# remove copypaste error
clean <- clean[-6460,]
glimpse(clean)    

#Several columns give means for time intervals of a measure. Convert to long
#

#

test <- clean %>% pivot_longer(cols = contains("mean"), names_to = c("sound", "measure", "interval"),
                               names_pattern = "^([^_]+)_(mean)s?\\d{0,2}(\\d)?")
test$sound

test %>% filter(is.na(interval)) %>% 
  ggplot(aes(interval, value)) + geom_col()
#clean <- clean %>% pivot_longer(contains("mean"), names_to = "name2", values_to = "overall_mean")
  #separate(name, sep = "_", into = "interval")

#clean <- clean  %>% mutate(measure = pivot_longer(contains("mean"), names_pattern = ("(.{3})_means(\\d{3})"), names_sep = "_" names_to = c("measure, overall, interval, value), values_to =)

#str_detect(colnames(clean),".{3}_means\\d{3}")
