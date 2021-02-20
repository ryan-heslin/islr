library(tidyverse)
library(lubridate)
library(pdftools)
library(dslabs)

#From Intro to Data Science ch. 4
#Download a pdf table containing data
temp_file <- tempfile()
url <- paste0("https://www.pnas.org/content/suppl/2015/09/16/",
              "1510159112.DCSupplemental/pnas.201510159SI.pdf")
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)
data("raw_data_research_funding_rates")
#Create list of each line
tab <- str_split(raw_data_research_funding_rates, "\n") %>% unlist(.)

#Colnams are lines 3 and 5
tab[3:4]

#Clean each name vector
main_names <- tab[3] %>% str_trim() %>% 
  str_replace_all(",\\s.", "") %>% 
  str_split("\\s{2,}", simplify = T)

subnames <- tab[4] %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)nain_names

main_names <- c(main_names[1], str_c(rep(main_names, each = 3), subnames[-1], sep = "_"))

#Then read data values, trim and split on spaces, and set names
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(main_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% as_tibble()


fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf",
                 package="dslabs")
system("cmd.exe", input = paste("start", fn))

clean_rows <- function(x){
  out <- x
  if(!str_detect(substr(x, 1,1), "[:alpha:]")){
    out <- str_remove_all(x, "[:alpha:]")
  }
  out <- str
}

#Load table
text2 <- pdf_text(fn)
text2 <- text2 %>% str_split("\n") %>% unlist()

#My method: roguh but workable
#1. Clean excess text from rows
#Delete sumamry rows
#3. Split rows on spaces
#4. Remove header rows (now NA)
#
tabs <- text2 %>% reduce(., paste) %>%
  str_split("6/4/2018\\s+Departamento de Salud - Registro Demográfico - División de Calidad y Estadísticas Vitales\r", simplify = T) %>% 
  str_split("\r") %>% 
  map(., str_trim) %>% 
  map_depth(., 1, ~discard(., ~(str_length(.x) < 4 |str_detect(.x, "Total|Avg|Max|Min|Med")))) %>% 
  map_depth(., 2, ~str_extract(.x, "([\\d]+(\\s+|$)){5}")) %>% 
  map_depth(., 2,  str_trim) %>% 
  map_depth(., 2, ~str_split(.x, "\\s+", simplify = T)) %>% 
  map_depth(., 1,  ~discard(., ~(any(is.na(.x)))))
  tabs <- tabs[-1]
  
  #Give list month nae,s
  #2. Enframe
  #3. Construct month column
  #4. Bind rows
  #Rename, rearrange columns
  tabs2 <- tabs %>% rlang::set_names(nm = c("January", "Februrary", "March", "April", "May", "June", "July", "August", "September", "October",
                          "November", "December")) %>% 
    map(., ~enframe(.x, name = "Day")) %>% 
    imap(., ~cbind(.x, Month = rep(.y, times = nrow(.x)))) %>% 
    map(., ~unnest_wider(.x, col = value)) %>% 
    reduce(., rbind) %>% 
    select(-1) %>% 
    rename("Day" ="...1", "2015" = "...2",  "2016" ="...3", "2017" = "...4", "2018" = "...5") %>% 
    select(Day, Month, `2015`:`2018`) %>% 
    mutate(across(-Month, as.numeric)) %>%
    mutate(Month = factor(Month, levels = month.name)) %>% 
    pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Deaths") %>% 
    arrange(Year, Month, Day)
    

  
    
  
  

