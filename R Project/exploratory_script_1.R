## This is the scrpt to explore the data

library(dplyr)
library(ggplot2)
library(car)

load(url("https://stat.duke.edu/~mc301/data/movies.Rdata"))

View(movies)

# Part 1: EDA:

## Random exploration: 

names(movies)

# [1] "title"            "audience_score"   "type"            
# [4] "genre"            "runtime"          "year"            
# [7] "mpaa_rating"      "studio"           "imdb_num_votes"  
# [10] "critics_score"    "critics_rating"   "best_pic_nom"    
# [13] "best_pic_win"     "best_actor_win"   "best_actress_win"
# [16] "best_dir_win"     "top200_box"       "audience_rating" 
# [19] "director"         "actor1"           "actor2"          
# [22] "actor3"           "actor4"           "actor5"          
# [25] "imdb_url"         "rt_url"           "imdb_id"

### How many movies do we have and what's the score distribution?  

dim(movies)[1]  # 456

qplot(audience_score, data=movies, geom = 'histogram')
qplot(critics_score, data=movies, geom = 'histogram')

# qplot(audience_score, data=movies, geom = 'density')

qplot(audience_score, data=movies, geom = 'boxplot')

summary(movies$audience_score)

## which is the worst movie? 

movies %>% 
  filter(audience_score %in% c(max(audience_score), min(audience_score))) %>% View()

## how many votes do they receive? 

qplot(audience_score, data=movies, geom = 'histogram')


# Part 2: Inference 

### Qestion: What's the difference between a Oscar nom movie and a non-nom one? 

table(movies$best_pic_nom)

movies %>% 
  select(audience_score, critics_score, best_pic_nom) %>% 
  group_by(best_pic_nom) %>% 
  summarise(mean_aud_score = mean(audience_score), 
            means_crt_score = mean(critics_score))

# best_pic_nom mean_aud_score means_crt_score
# (fctr)          (dbl)           (dbl)
# 1           no       59.50787        52.22472
# 2          yes       79.18182        86.18182

# Is this difference significant? 

movies %>% 
  select(audience_score, critics_score, best_pic_nom) %>% 
  group_by(best_pic_nom) %>% 
  summarise(mean_aud_score = mean(audience_score), 
            means_crt_score = mean(critics_score)) %>% 

t.test(audience_score ~ best_pic_nom, alternative='less', data = movies) # yes, significantly smaller
t.test(critics_score ~ best_pic_nom, alternative='less', data = movies) # even more significantly different


scatterplot.matrix(~audience_score+critics_score+runtime+imdb_num_votes+critics_rating, movies)

## Part 3: Building Multiple Linear Regression for prediction

fit = lm(audience_score ~ critics_score + type*genre + runtime 
         + as.factor(year) + log(imdb_num_votes) + best_pic_nom + best_pic_win
         + best_actress_win + best_actor_win + best_dir_win + top200_box
         + critics_rating + mpaa_rating, data=movies)











