## This is the scrpt to explore the data

library(dplyr)

load(url("https://stat.duke.edu/~mc301/data/movies.Rdata"))

View(movies)

movies %>% 
  filter(studio == "20th Century Fox") %>% 
  select(audience_score, studio, genre) %>%  View()