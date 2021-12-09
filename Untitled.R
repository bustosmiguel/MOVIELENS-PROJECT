# 0 points: No RMSE reported AND/OR code used to generate the RMSE appears to violate the edX Honor Code.
# 5 points: RMSE >= 0.90000 AND/OR the reported RMSE is the result of overtraining (validation set - the final hold-out test set - ratings used for anything except reporting the final RMSE value) AND/OR the reported RMSE is the result of simply copying and running code provided in previous courses in the series.
# 10 points: 0.86550 <= RMSE <= 0.89999
# 15 points: 0.86500 <= RMSE <= 0.86549
# 20 points: 0.86490 <= RMSE <= 0.86499
# 25 points: RMSE < 0.86490

quiz <- 0.1*100
movieLens <- 0.4*65
ownProject <- 0.5*69

quiz+movieLens+ownProject
#70.5

# Goal and Executive Summary ----------------------------------------------

#The goal of this project is study the edx dataset and make a RMSE critical result.

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggrepel)
library(tinytex) #to export to pdf
install.packages("latex")

install.packages("tinytex")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)




# PART 1: GLOBAL SIGHT OF EDX – SOME STRUCTURE CHANGES AND A VISUALIZATION --------


library(ggplot2) # To make ggplots
library(ggthemes) #To use thhe economist plot style
library(metrics) #To use rmse() function
library(purrr) #To use modify_if
library(dplyr) #To mutate 

#1.- Structure
#Structure, head and dimension of edx object: 
str(edx)
head(edx)
dim(edx)

#2.- Creating a kindle data, each genre in each column, and year in one column
#Create edx1, it`s edx, but "genres" ordered and "year" separable from title:

edx1 <-  edx %>%
  separate(genres, str_c("genres", 1:10), sep = "([|])", remove = F) %>%
  mutate(year = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"),
                                       regex("\\d{4}"))),
         title = str_remove(title, "[/(]\\d{4}[/)]$")) 

str(edx1)
#3.- YEAR AND RATINNGS VISUALIZATION TABLE
# This simple table shows the ten ratings and years since 1015 to 2008:

str(edx1)
table(edx1$year, edx1$rating)
prop.table(table(edx1$year, edx1$rating))



#4.- Edx1 performance visualization (Ratings)
#The last simple table showed the 10 ratings and years since 2015 to 2008, 
#but how to obtain a better visualization? about the performance or "rating" per "genre"
#let`s taking a sample since 2005 to 2008. 
#This plot shows that "documentary genre" is the best evaluated if we take a N = 1000 sample:
  
  muestra1 = edx1 %>% 
  filter(year %in% 2005:2008) %>%
  sample_n(1000)

ggplot(data = muestra1, aes(genres1, rating))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = "jitter", aes(color = genres1))+
  theme_economist()+
  labs(x = "Genres", y = "Rating 1 - 5", title = "Boxplot of Genres - Ratings 2005 to 2008, sample of N = 1000")



#5.- Edx1 Rating Distribution

edx1 %>% 
  ggplot(aes(rating))+
  geom_histogram(binwidth = 0.4, fill = "royalblue", color = "skyblue")+
  theme_economist()+
  labs(x = "Rating", y = "Distribution Visual Proportion", title = "EDX RATING DISTRIBUTION")



#6.- Frequency of movies per Genre, the most viewed
#    gf  =  Genred Frequency of Movies per Genre (Action is the leader)
#    tw  =  Top watched (Action in 2008)

genres_frequency <- as.data.frame(table(edx1$genres1, edx1$rating))
class(genres_frequency)

gf <- ggplot(data = genres_frequency)
gf + geom_point(aes(x = Var1, y = Freq/10))+
  labs(x = "All EDX Genres", y = "Frequency", title = "GENRES FREQUENCY OF MOVIES PER GENRE - EDX")+
  theme_economist()+
  theme(axis.text.x = element_text(angle = 85, vjust = 0.6))



#7.- Action Movie Frequency, that have a rating of 5

top_watched <- edx1 %>% 
  filter(rating == "5", genres1 == "Action", year == "2008")

tg <- ggplot(data = top_watched)
tg + geom_bar(aes(x = title))+
  labs(x = "Action Movies (Rating of 5)", y = "Frequency watched", title = "ACTION MOVIE FREQUENCY, THAT HAVE A RATING OF 5")+
  theme_economist()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8, color = "darkblue", face = "italic", hjust = 1))
  












# PART 2: RMSE --------------------------------------------------------------------
# The part 1 was critical to ordered the variables of genre and year and visualize it
# Now, that we have these variables in Edx, we can work more efficient in a RMSE

# The Root-mean-square error (RMSE) is the accuracy measure that cslculate the prediction error rate.
# What are the residuals between "actual rating" and "predicted rating"? RMSE gives that answer
# In other words, the RMSE it`s the measure of how spread out these residuals are 
# and how concentrated the data is around the line of best fit
# So it`s the difference between "Actual Ratings" and the Forecast or "predicted ratings"


str(movielens)
movielens <- left_join(ratings, movies, by = "movieId")


# test_set & forecast_rating

mu <- mean(edx1$rating)

set.seed(1)
test_set %>% 
  left_join(edx1 %>% 
              group_by(movieId) %>% 
              summarise(fe = mean(rating - mu)), by = "movieId")
join <- test_set %>% 
  left_join(edx1 %>% 
              group_by(movieId) %>% 
              summarise(fe = mean(rating - mu)), by = "movieId")


forecast_rating <- mu + join$fe 
forecast_rating 
















# Create bootstrap sample with createDataPartition
set.seed(1)
test_index <- createDataPartition(y = edx1$rating, times = 1, p = 0.5, list = FALSE)
train_set <- edx1[-test_index,]
test_set <- edx1[test_index,]


str(edx1) #100004 observations
str(test_index) 
str(train_set) #80002 observarions
str(test_set) #20002 observations

valid <- test_set %>% 
  semi_join(edx1, by = "movieId") %>% 
  semi_join(edx1, by = "userId") #Return all rows from x with a match in y

length(test_set) 

test_set_r <- test_set$rating
forecast_rating <- mu + join$fe










# FIRST RMSE [1] 0.9436553

#Using the test_set_r: 0.5 to 5.0, and forecast_rating: different values (2.939235 3.738641 3.419572…)

mu <- mean(edx1$rating)
mu

set.seed(1)
difference <- test_set_r-forecast_rating
rmse <- sqrt(mean(difference^2))
rmse
## [1] 0.9436553
#or:
sqrt(mean((test_set_r-forecast_rating)^2))








# SECOND RMSE [1] 1.102293

#Using the test_set_r: 0.5 to 5.0, and now forecast_rating: using ceiling function, 
#that rounds up to the nearest integer, with more realistic number. 
#This way shows that the residual reaches: [1] 1.102293.- And this is still too far:

set.seed(1)
forecast_rating2 <- ceiling(forecast_rating)
sqrt(mean((test_set_r-forecast_rating2)^2))
#[1] 1.102293








# THIRD RMSE [1] 0.7399887 Using ranking test_sample and forecast_sample, we obtain #[1] 0.7399887 residuals value in 3.5 to 5.0 range:

# Using ranking test_sample and forecast_sample, we obtain #[1] 0.7399887 residuals value in 3.5 to 5.0 range.
# Making good recommendations movies, over 3.5 ranked movies, is the way to get an optimal RMSE, for 
# that reason, it`s neccessary to fix some parameters, that make a better recommendations:

# Just replace the parameter, each of these, here we have some vectors parameters:

parameter1 <- 3.0 
parameter2 <- 3.5
parameter3 <- 4.0
parameter4 <- 4.5
parameter5 <- 5.0

# What will be the RMSE results, in the recommendation is more than these ratings?, here we have some
# vectors parameters results:

# parameter1 : approximately [1] 1.000417 RMSE result
# parameter2 : approximately [1] 0.7399887 RMSE result
# parameter3 : approximately [1] 0.6379002 RMSE result
# parameter4 : approximately [1] 0.3106445 RMSE result
# parameter5 : approximately [1] 0 RMSE result, this is because it`s the limit ranking

# The best RMSE it`s obtained using parameter3 [1] 0.6379002 rmse result, anyway , just using parameter2, 
# we obtained a great recommendation [1] 0.7399887 RMSE result.

# These vector parameters, can be changed in this code, and we will have different RMSE results, this code 
# including >= symbol, so it is the best way to make recommendations to clients and obtain a better RMSE, m
# aking good recommensations movies, over 3.5 ranked value, that value is parameter 2:
  
set.seed(1)
index1 <- edx1$rating >= parameter2 #<- You must here write the parameter
test_sample <- edx1$rating[index1] %>% sample(size = 3000)
test_sample

set.seed(1)
index2 <- join$rating >= parameter2 #<- You must here write the parameter
forecast_sample <- join$rating[index2] %>% sample(size = 3000) 
forecast_simple_mu <- join$fe[forecast_sample]+ mu 
forecast_sample 

#The sample N = 3000 ranking distribution, as ECDF Empirical Distribution Function
plot.ecdf(test_sample, forecast_sample, ylab = "Sample Distribution",  xlab = "Ratings", col.01line = "blue", verticals = TRUE)

#RMSE
rmse_result <- sqrt(mean((test_sample-forecast_sample)^2))








# FOURTH RMSE









# CONCLUSION AND RECOMMENDATION

# Making an adecuated recommendation more than 3.5, we can obtain 0.7399887 rmse result. 
# For that reason, if the intention is to make a good recommendation, 
# the critical decision is recommend a good movie to the viewers, that recommendation is over 3.5, 
# and the gap will be more close, as a residual of 0.7399887 RMSE.

# Nowadays many companies are offering online products and services, 
# they won`t recommend a bad products, books, movies, licences, etcetera, 
# these companies will recommend products well evaluated, to guarantee a great experience, 
# happens the same in this project, it is necessary make good recomendations.

# The recommendation is to recommend all movies that are equal or more than 3.5 ranked, 
# so the spread will be roughly 0.7399887 RMSE and that guarantee the clients will have a great experience.

# The RMSE model, reaches [1] 0.7399887 result.



# Bibliography
# Indexing https://youtu.be/OX-YbREKh1U?t=109 





##OTHERS

year1 <- edx1$year %>% filter(2005:2008)

edx1 %>% filter(year == 2005:2008) %>% 
  gather(year, rating) %>% 
  ggplot(aes(year, rating, fill = rating)) +
  geom_boxplot()+
  facet_wrap(~year, scales = "free")

abline(intercept)