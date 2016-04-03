## This is the scrpt to explore the data

require(dplyr)
require(ggplot2)
require(car)
require(tidyr)
require(magrittr)
require(lubridate)
require(stringr)

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

fit = lm(audience_score ~ critics_score + type*genre + runtime + audience_rating
         + as.factor(year) + log(imdb_num_votes) + best_pic_nom + best_pic_win
         + best_actress_win + best_actor_win + best_dir_win + top200_box
         + critics_rating + mpaa_rating, data=movies)

summary(fit)

## concatante actors to engineer 

movies %<>% 
  unite(actors, actor1:actor5, sep=",")

# Create a frequency table of actors

act_tb_1 = as.data.frame(table(movies$actor1))
act_tb_2 = as.data.frame(table(movies$actor2))
act_tb_3 = as.data.frame(table(movies$actor3))
act_tb_4 = as.data.frame(table(movies$actor4))
act_tb_5 = as.data.frame(table(movies$actor5))
act_tb = rbind(act_tb_1, act_tb_2, act_tb_3, act_tb_4, act_tb_5)

act_tb %>% 
  group_by(Var1) %>% 
  summarise(freq = sum(Freq)) %>% 
  arrange(desc(freq)) %>% 
  filter(freq>2) %>% 
  select(Var1) -> act_freq

t = as.vector(act_freq$Var1)


mymat = matrix(nrow=651, ncol=83)

for(i in 1:651)  # for each row
{
  for(j in 1:83) 
  {
  mymat[i,j] = as.numeric(str_detect(movies_test$actors[i], t[j])) # assign values based on position: product of two indexes
  }
}

t1 = paste0("dm_", tolower(gsub(" ", "_", t)))

dummy_actors = as.data.frame(mymat)

names(dummy_actors) = t1

model_movie = cbind(movies, dummy_actors)

### ad actors to predictors

fit = lm(audience_score ~ critics_score + title_type*genre + runtime + audience_rating
         + as.factor(thtr_rel_year) + log(imdb_num_votes) + best_pic_nom + best_pic_win
         + best_actress_win + best_actor_win + best_dir_win + top200_box
         + critics_rating + mpaa_rating + dm_robert_de_niro + dm_jeff_bridges + dm_matt_damon 
         + dm_nicolas_cage + dm_charlize_theron + dm_christopher_walken + dm_eddie_murphy 
         + dm_geoffrey_rush + dm_sean_penn + dm_susan_sarandon + dm_aaron_eckhart + dm_adam_sandler
         + dm_bill_pullman + dm_colin_firth + dm_gene_hackman + dm_jason_statham + dm_jessica_biel
         + dm_joaquin_phoenix + dm_maximilian_schell + dm_michael_caine + dm_nicole_kidman 
         + dm_matthew_broderick + dm_adrien_brody + dm_alec_baldwin + dm_bette_midler 
         + dm_bill_murray + dm_bradley_cooper + dm_bryce_dallas_howard + dm_cameron_diaz 
         + dm_charlton_heston + dm_claire_danes 
         + dm_daryl_hannah + dm_emma_stone + dm_gary_sinise + dm_jack_nicholson + dm_jamie_foxx 
         + dm_jamie_kennedy + dm_jennifer_tilly + dm_john_leguizamo + dm_john_travolta + dm_katie_holmes 
         + dm_keira_knightley + dm_kevin_bacon + dm_laurence_fishburne + dm_meryl_streep 
         + dm_reese_witherspoon + dm_richard_gere + dm_robert_englund + dm_robert_john_burke
         + dm_sean_astin + dm_sean_connery + dm_toni_collette + dm_uma_thurman + dm_alan_cumming + dm_charlotte_rampling 
         + dm_chloe_sevigny + dm_guy_pearce + dm_james_earl_jones + dm_jessica_lange + dm_joan_allen + dm_jude_law 
         + dm_kelly_preston + dm_maria_bello + dm_martin_short + dm_mena_suvari + dm_morgan_freeman + dm_patrick_swayze 
         + dm_sam_neill + dm_tara_reid + dm_william_h._macy + dm_adam_brody + dm_cedric_the_entertainer + dm_hector_elizondo 
         + dm_jack_thompson + dm_joe_pantoliano + dm_louise_fletcher + dm_rufus_sewell + dm_will_patton + dm_alex_rocco, data=model_movie)

base_formular = "audience_score ~ critics_score + type + genre + runtime + audience_rating + year + imdb_num_votes + best_pic_nom + best_pic_win + best_actress_win + best_actor_win + best_dir_win + top200_box+ critics_rating + mpaa_rating + "

actor_formular = str_c(t1, collapse = " + ")

final_formular = as.formula(str_c(base_formular, actor_formular, collapse = ""))




