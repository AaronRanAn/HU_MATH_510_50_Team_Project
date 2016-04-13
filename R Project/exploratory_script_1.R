## This is the scrpt to explore the data

require(dplyr)
require(ggplot2)
require(car)
require(tidyr)
require(magrittr)
require(lubridate)
require(stringr)
require(caret)
require(knitr)

load(url("https://stat.duke.edu/~mc301/data/movies.Rdata"))

View(movies)

# Part 1: EDA:

## Random exploration: 

names(movies)

# [1] "title"            "title_type"       "genre"            "runtime"          "mpaa_rating"     
# [6] "studio"           "thtr_rel_year"    "thtr_rel_month"   "thtr_rel_day"     "dvd_rel_year"    
# [11] "dvd_rel_month"    "dvd_rel_day"      "imdb_rating"      "imdb_num_votes"   "critics_rating"  
# [16] "critics_score"    "audience_rating"  "audience_score"   "best_pic_nom"     "best_pic_win"    
# [21] "best_actor_win"   "best_actress_win" "best_dir_win"     "top200_box"       "director"        
# [26] "actor1"           "actor2"           "actor3"           "actor4"           "actor5"          
# [31] "imdb_url"         "rt_url"   

### How many movies do we have and what's the score distribution?  

dim(movies)[1]  # 456

qplot(audience_score, data=movies, geom = 'histogram')
qplot(critics_score, data=movies, geom = 'histogram')

# qplot(audience_score, data=movies, geom = 'density')

summary(movies$audience_score)

## which is the worst movie? 

movies %>% 
  filter(audience_score %in% c(max(audience_score), min(audience_score))) %>% kable()

## how many votes do they receive? 

qplot(audience_score, data=movies, geom = 'histogram')


# Part 2: Inference 

### Qestion: What's the difference between a Oscar nom movie and a non-nom one? 

table(movies$best_pic_nom)

movies %>% 
  select(audience_score, critics_score, best_pic_nom) %>% 
  group_by(best_pic_nom) %>% 
  summarise(mean_aud_score = mean(audience_score), 
            means_crt_score = mean(critics_score)) %>% 

# best_pic_nom mean_aud_score means_crt_score
# (fctr)          (dbl)           (dbl)
# 1           no       59.50787        52.22472
# 2          yes       79.18182        86.18182

# Is this difference significant? 

movies %>% 
  select(audience_score, critics_score, best_pic_nom) %>% 
  group_by(best_pic_nom) %>% 
  summarise(mean_aud_score = mean(audience_score), 
            means_crt_score = mean(critics_score)) %>% kable()

t.test(audience_score ~ best_pic_nom, alternative='less', data = movies) # yes, significantly smaller
t.test(critics_score ~ best_pic_nom, alternative='less', data = movies) # even more significantly different


scatterplot.matrix(~audience_score+critics_score+runtime+imdb_num_votes+critics_rating, movies)

################## Start to Engineering Features  #########################

# Create director, studio and gener frequency table to quantify the impact of these features

dir_mv_freq = as.data.frame(table(movies$director))
names(dir_mv_freq) = c("director", "dir_mv_freq")

std_mv_freq = as.data.frame(table(movies$studio))
names(std_mv_freq) = c("studio", "std_mv_freq")

gr_mv_freq = as.data.frame(table(movies$genre))
names(gr_mv_freq) = c("genre", "gr_mv_freq")

## count words in title 

movies %>% 
  mutate(title_ws_ct = str_count(title, " ") + 1, 
         runtime_gt_90 = as.numeric(runtime>90),
         thtr_rel_date = ymd(str_c(thtr_rel_year, thtr_rel_month, thtr_rel_day, sep="-")),
         dvd_rel_date = ymd(str_c(dvd_rel_year, dvd_rel_month, dvd_rel_day,  sep="-")),
         thtr_rel_wkd = as.numeric(wday(thtr_rel_date) %in% c(4,5)),
         thtr_rel_summer = as.numeric(thtr_rel_month %in% c(6,7)),
         thtr_rel_holiday = as.numeric(thtr_rel_month %in% c(10, 11, 12)),
         dvd_rel_holiday = as.numeric(dvd_rel_month %in% c(10, 11, 12)),
         thtr_rel_dump = as.numeric(thtr_rel_month %in% c(1, 2, 8, 9)),
         thtr_dvd_rel_diff = as.numeric(dvd_rel_date-thtr_rel_date)
         ) %>% 
  unite(actors, actor1:actor5, sep=",") %>% 
  left_join(dir_mv_freq, by="director") %>% 
  left_join(std_mv_freq, by="studio") %>% 
  left_join(gr_mv_freq, by="genre") -> movie_model

movie_model$actors %>% 
  str_c(collapse = ",") %>% 
  str_split(",") %>% 
  table(., exclude = "NA") %>% 
  as.data.frame() %>%
  arrange(desc(Freq)) -> act_mv_freq

movie_model$director %>% 
  table() %>% 
  as.data.frame() %>%
  arrange(desc(Freq)) -> dir_mv_freq_ls

act_mv_freq %>% 
  filter(Freq>4) %>% 
  select(.[1]) -> act_gt1_list

dir_mv_freq_ls %>% 
  filter(Freq>2) %>% 
  select(.[1]) -> dir_gt1_list

#############################

t = as.vector(act_gt1_list$.)
mymat = matrix(nrow=651, ncol=37)

for(i in 1:651)  # for each row
{
  for(j in 1:37) # for every actors with 
  {
    mymat[i,j] = as.numeric(str_detect(movie_model$actors[i], t[j]))
  }
}

t1 = paste0("dm_", tolower(gsub(" ", "_", t)))
t2 = str_replace_all(t1, fixed(".", T), "_")
t2 = str_replace_all(t2, fixed("'", T), "_")
t2 = str_replace_all(t2, fixed("-", T), "_")

##########################

d = as.vector(dir_gt1_list$.)
mymat2 = matrix(nrow=651, ncol=17)

for(i in 1:651)  # for each row
{
  for(j in 1:17) # for every actors with 
  {
    mymat2[i,j] = as.numeric(str_detect(movie_model$director[i], t[j]))
  }
}

d1 = paste0("dm_", tolower(gsub(" ", "_", d)))
d2 = str_replace_all(d1, fixed(".", T), "_")
d2 = str_replace_all(d2, fixed("'", T), "_")
d2 = str_replace_all(d2, fixed("-", T), "_")

dummy_dir = as.data.frame(mymat2)
names(dummy_dir) = d2

dummy_actors = as.data.frame(mymat)
names(dummy_actors) = t2

model_movie = cbind(movie_model, dummy_actors, dummy_dir)

## Part 3: Building Multiple Linear Regression for prediction

base_formular = "audience_score ~ title_type +  genre + runtime + mpaa_rating + studio + thtr_rel_year + thtr_rel_month + imdb_num_votes + critics_rating + critics_score + best_pic_nom + best_pic_win + best_actor_win + best_actress_win + best_dir_win + top200_box + as.factor(director) + title_ws_ct + runtime_gt_90 + thtr_rel_wkd + thtr_rel_summer + thtr_rel_holiday + dvd_rel_holiday +  thtr_rel_dump + thtr_dvd_rel_diff + dir_mv_freq + std_mv_freq + gr_mv_freq + "

base_formular = "audience_score ~ title_type +  genre + runtime + mpaa_rating + thtr_rel_month + thtr_rel_year + imdb_num_votes + critics_rating + critics_score + best_pic_nom + best_pic_win + best_actor_win + best_actress_win + best_dir_win + top200_box + title_ws_ct + runtime_gt_90 + thtr_rel_wkd + thtr_rel_summer + thtr_rel_holiday + dvd_rel_holiday +  thtr_rel_dump + thtr_dvd_rel_diff + dir_mv_freq + std_mv_freq + gr_mv_freq + "

actor_formular = str_c(t2, collapse = " + ")

dir_formular = str_c(names(dm_dir_df), collapse = " + ")

final_formular = as.formula(str_c(base_formular, actor_formular, collapse = ""))

fit = lm(final_formular, data = model_movie)

fit = lm(final_formular, data = model_movie)




fit = lm(  audience_score ~ title_type +  genre + runtime + mpaa_rating  + thtr_rel_month + imdb_num_votes + critics_rating + critics_score + 
           best_pic_nom + best_pic_win + best_actor_win + best_actress_win + best_dir_win + top200_box + title_ws_ct + runtime_gt_90 + thtr_rel_wkd + thtr_rel_summer + 
           thtr_rel_holiday + dvd_rel_holiday +  thtr_rel_dump + thtr_dvd_rel_diff + dir_mv_freq + std_mv_freq + gr_mv_freq, data=model_movie)

summary(fit)









### ad actors to predictors

fit = lm(audience_score ~ critics_score + title_type*genre + runtime
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
         + dm_jack_thompson + dm_joe_pantoliano + dm_louise_fletcher + dm_rufus_sewell + dm_will_patton + dm_alex_rocco + test + as.factor(director)*best_dir_win, data=model_movie)

base_formular = "audience_score ~ critics_score + type + genre + runtime + audience_rating + year + imdb_num_votes + best_pic_nom + best_pic_win + best_actress_win + best_actor_win + best_dir_win + top200_box+ critics_rating + mpaa_rating + "

actor_formular = str_c(t1, collapse = " + ")

final_formular = as.formula(str_c(base_formular, actor_formular, collapse = ""))



model_movie$test = rowSums(model_movie[29:111], na.rm = FALSE, dims = 1)














# Create a frequency table of actors

# act_tb_1 = as.data.frame(table(movies$actor1))
# act_tb_2 = as.data.frame(table(movies$actor2))
# act_tb_3 = as.data.frame(table(movies$actor3))
# act_tb_4 = as.data.frame(table(movies$actor4))
# act_tb_5 = as.data.frame(table(movies$actor5))
# act_tb = rbind(act_tb_1, act_tb_2, act_tb_3, act_tb_4, act_tb_5)
# 
# act_tb %>% 
#   group_by(Var1) %>% 
#   summarise(freq = sum(Freq)) %>% 
#   arrange(desc(freq)) %>% 
#   filter(freq>2) %>% 
#   select(Var1) -> act_freq






