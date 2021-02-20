library(gutenbergr)
vec <-  gutenberg_download(46) %>% filter(text !="") %>% pull(text) %>% as.vector() %>% 
reduce(., paste) %>% 
  str_split(., "(\\?|!)\\s|((?<!((Mrs|Miss|Mr)|)\\.\"?)\\s|(!)\\s", simplify = T) #periods,s others separately # %>% map_chr(., str_trim) #split agin to deal with quotes

sample1 <- data.frame(sentences = sample(vec, size = 20, replace = F)) %>%
  mutate(words= sentences %>% map_dbl(., ~str_count(.x, "\\s") +1)) 


#mple(., n = 20, replace = F)
  
  
  

