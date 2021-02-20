library(tidyverse)
library(nycflights13)

#Miscellaneous exercises
mtcars$ID <- rep(NA, nrow(mtcars))
library(stringr)
for (i in 1:nrow(mtcars)){
  mtcars$ID[i] <- paste0(unlist(strsplit(rownames(mtcars)[i]," "))[1],as.integer(rnorm(1,150,150)))
}

cars_table <- mtcars %>% 
  mutate(performance = hp/mpg, price = performance *3000, year = as.integer(rnorm(nrow(mtcars), 2000,10)))%>%
  select(ID, performance, year)

cars_table <- cars_table %>% sample_frac(size = 0.5, replace = FALSE) 

cars_inner <- inner_join(mtcars, cars_table, by = "ID")


#cars_inter <- intersect(cars_table, mtcars)


cars <- mtcars %>% sample_frac(size = 0.5, replace = FALSE)

union(cars, mtcars)

airdata <- airquality[4:6]
airdata

airdata <- airdata %>%  pivot_wider(names_from = Month, values_from = Temp) 
aridata <- airdata %>% pivot_longer(cols = `5`:`9`, names_to = "Month", )

#R for Data Science ch. 13
#
##In general, it is most importatnt to understand coomon variables linking tables of interest
##
#Keys unqiuqly ID observaions and consist of one or several variables. 
#Primary keys ID observations in their own tables. 
#Foreign keys identidy observations in other tables.
#A single variable may be both types.
#A priamry key should never have a count greater than one
planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

#Tables may lack primary keys: Here, multiple flights reuse the same number each day
flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)

#If this occurs, use row number to create a surrogate key
flights %>% 
  mutate(key = row_number())
#A primary key in one table, combined with a foreign key in aontehr, forms a relation, usually 1-to-many
#

# Mutating Joins ----------------------------------------------------------

#Mutating joins transfer vaiarbales across tables afer mtaching observations by key
#

flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)

#Here we match each airline code to its full name
flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
left_join(x, y)
  #Joins differ on how they treat cases where a key is present in one table but not another
  #INNER JOINS simply match observations that share a key
  #They drop obssrvations in either table that are not matched
  x %>% 
  inner_join(y, by = "key")
  
  #OUTER JOINS preserve observations that appear in at leasat one table.
  #Left joins keep those in x, right in y, full in both
  ##Any variables htat lack matches get filled with NA
  ##
  ##Problems arise with duplicate keys. They can be useful in one-to-many relationships

  x <- tribble(
    ~key, ~val_x,
    1, "x1",
    2, "x2",
    2, "x3",
    1, "x4"
  )
  y <- tribble(
    ~key, ~val_y,
    1, "y1",
    2, "y2"
  )
  left_join(x, y, by = "key") #Creates 2 rows for key 2, one x2 and y2, one x3 and y2
  
  #If both tables have duped keys, joins yield Cartesian product (all possible row combos.
  #This is not desriable
  #
  x <- tribble(
    ~key, ~val_x,
    1, "x1",
    2, "x2",
    2, "x3",
    3, "x4"
  )
  y <- tribble(
    ~key, ~val_y,
    1, "y1",
    2, "y2",
    2, "y3",
    3, "y4"
  )
  left_join(x, y, by = "key") #Key 2 pegged to all combinations of x2, x3, y2, and y3
  
  #"Natural" joins simply use all common columns as keys, 
  #
  flights2 %>% 
  left_join(weather) #Not good; year var has different meanings in tables
  
  #Or select varaibles to match by
  flights2 %>% 
    left_join(planes, by = "tailnum") #Preserves different year vars
  
  #Or match a variabele in x to one of a different name in y
  flights2 %>% 
    left_join(airports, c("dest" = "faa"))
  

# Filtering Joins ---------------------------------------------------------

#Filtering joins operate on observations, not variables
#
#Semi-joins keep all OBSERRVATIONS in x that have a mtach in y.
#Thye yare good for matching filtered sumamry tables to original datasets  
  top_dest <- flights %>%
    count(dest, sort = TRUE) %>%
    head(10)
  top_dest
  
  flights %>% 
    semi_join(top_dest) #Retians only rows in x whose value in join column matches y
  
  #Anti-joins keep rows that lack matches
  
  flights %>%
    anti_join(planes, by = "tailnum") %>%
    count(tailnum, sort = TRUE) #How many flights lack a match in planes?
  
  #GEenral workflow:
  #1. Determine each able's priamry key. Use understanding of data, not experimentation, which may find relationships
  #that don't genralize.
  #2. Ensure primary keys lack missing values.
  #3. Emsure your primary key matches another table's foreign key with anti_join