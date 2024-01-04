#Author: @Addison Ji, @Alisa Liu, @Keani Schuller, @Yusen Tang @Zhiming Zhang
#Course: MGSC 661-IMDB Blockbuster Prediction
#Date: Oct 26th
#Objective: Build predictive model using R to predict the 12 upcoming movieds using IMDb datasets
#Code File 2: Explore variable relationships and non-linearity test

#The dependent variable is imdb_score, the preditors are categorized into four groups based on its nature
IMDB_data=read.csv("IMDB_data_Fall_2023.csv")
attach(IMDB_data)
#Category 1 - Movie Details: release_day, release_month, release_year, language, country, duration, maturity rating, genres(dummy variables for all), plot of keywords

##Regression
reg_day = lm(imdb_score~release_day)
summary(reg_day)

reg_month = lm(imdb_score~release_month)
summary(reg_month)

reg_year = lm(imdb_score~release_year)
summary(reg_year)

reg_duration = lm(imdb_score~duration)
summary(reg_duration)

reg_language = lm(imdb_score~language)
summary(reg_language)

reg_country = lm(imdb_score~country)
summary(reg_country)

reg_rating = lm(imdb_score~maturity_rating)
summary(reg_rating)

reg_keyword = lm(imdb_score~plot_keywords)
summary(reg_keyword)

###genres(dummy variables for all)
reg_action=lm(imdb_score~action)
summary(reg_action)

reg_adventure=lm(imdb_score~adventure)
summary(reg_adventure)

reg_scifi=lm(imdb_score~scifi)
summary(reg_scifi)

reg_thriller=lm(imdb_score~thriller)
summary(reg_thriller)

reg_romance=lm(imdb_score~romance)
summary(reg_romance)

reg_western=lm(imdb_score~western)
summary(reg_western)

reg_sport=lm(imdb_score~sport)
summary(reg_sport)

reg_horror=lm(imdb_score~horror)
summary(reg_horror)

reg_drama=lm(imdb_score~drama)
summary(reg_drama)

reg_war=lm(imdb_score~war)
summary(reg_war)

reg_animation=lm(imdb_score~animation)
summary(reg_animation)

reg_crime=lm(imdb_score~crime)
summary(reg_crime)

##heteroskedasticity
###non-constant variance test to determine heteroskedasticity
ncvTest(reg_day)
ncvTest(reg_month)
ncvTest(reg_year)
ncvTest(reg_duration)
ncvTest(reg_language)
ncvTest(reg_country)
ncvTest(reg_rating)

ncvTest(reg_action)
ncvTest(reg_adventure)
ncvTest(reg_scifi)
ncvTest(reg_thriller)
ncvTest(reg_romance)
ncvTest(reg_western)
ncvTest(reg_sport)
ncvTest(reg_horror)
ncvTest(reg_drama)
ncvTest(reg_war)
ncvTest(reg_animation)
ncvTest(reg_crime)

##non-linearity
residualPlots(reg_day)
residualPlots(reg_month)
residualPlots(reg_year)
residualPlots(reg_duration)
residualPlots(reg_language)
residualPlots(reg_country)
residualPlots(reg_rating)
residualPlots(reg_keyword)

residualPlots(reg_action)
residualPlots(reg_adventure)
residualPlots(reg_scifi)
residualPlots(reg_thriller)
residualPlots(reg_romance)
residualPlots(reg_western)
residualPlots(reg_sport)
residualPlots(reg_horror)
residualPlots(reg_drama)
residualPlots(reg_war)
residualPlots(reg_animation)
residualPlots(reg_crime)


#Category 2 - Marketing Visibility & Popularity: nb_faces, nb_news_articles, distributor, movie_meter_IMDBpro

##Regression
reg_face=lm(imdb_score~nb_faces)
summary(reg_face)

reg_article=lm(imdb_score~nb_news_articles)
summary(reg_article)

reg_distributor=lm(imdb_score~distributor)
summary(reg_distributor)

reg_movie_meter=lm(imdb_score~movie_meter_IMDBpro)
summary(reg_movie_meter)

##heteroskedasticity
ncvTest(reg_face)
ncvTest(reg_article)
ncvTest(reg_distributor)
ncvTest(reg_movie_meter)

##non-linearity
residualPlots(reg_face)
residualPlots(reg_article)
residualPlots(reg_distributor)
residualPlots(reg_movie_meter)


#Category 3 - Cast Team: actor1, actor1_star_meter,actor2, actor2_star_meter,actor3, actor3_star_meter, director, cinematographer

##Regression
reg_actor1=lm(imdb_score~actor1)
summary(reg_actor1)

reg_actor1_meter=lm(imdb_score~actor1_star_meter)
summary(reg_actor1_meter)

reg_actor2=lm(imdb_score~actor2)
summary(reg_actor2)

reg_actor2_meter=lm(imdb_score~actor2_star_meter)
summary(reg_actor2_meter)

reg_actor3=lm(imdb_score~actor3)
summary(reg_actor3)

reg_actor3_meter=lm(imdb_score~actor3_star_meter)
summary(reg_actor3_meter)

reg_director=lm(imdb_score~director)
summary(reg_director)

reg_cinematographer=lm(imdb_score~cinematographer)
summary(reg_cinematographer)

##heteroskedasticity
ncvTest(reg_actor1)
ncvTest(reg_actor1_meter)
ncvTest(reg_actor2)
ncvTest(reg_actor2_meter)
ncvTest(reg_actor3)
ncvTest(reg_actor3_meter)
ncvTest(reg_director)
ncvTest(reg_cinematographer)

##non-linearity
residualPlots(reg_actor1)
residualPlots(reg_actor1_meter)
residualPlots(reg_actor2)
residualPlots(reg_actor2_meter)
residualPlots(reg_actor3)
residualPlots(reg_actor3_meter)
residualPlots(reg_director)
residualPlots(reg_cinematographer)


#Category 4 - Movie Production: movie_budget, colour_film, aspect_ratio, production_company

##Regression
reg_budget=lm(imdb_score~movie_budget)
summary(reg_budget)

reg_color=lm(imdb_score~colour_film)
summary(reg_color)

reg_ratio=lm(imdb_score~aspect_ratio)
summary(reg_ratio)

reg_company=lm(imdb_score~production_company)
summary(reg_company)

##heteroskedasticity
ncvTest(reg_budget)
ncvTest(reg_color)
ncvTest(reg_ratio)
ncvTest(reg_company)

##non-linearity
residualPlots(reg_budget)
residualPlots(reg_color)
residualPlots(reg_ratio)
residualPlots(reg_company)


#Collinearity for all quantitative predictors
## To run correlation matrix on all quantitative predictors
install.packages("psych")
require(psych)
require(car)

install.packages("corrplot")
library(corrplot)

quantvars=movie_data[, c(5, 6, 8, 9, 13, 15,18,20,22,25,27,28,29,30,31,32,33,34,35,36,37,38,39,40)]
corr_matrix=cor(quantvars)
round(corr_matrix,2)
corrplot(corr_matrix, method = "color")

quantvars=movie_data[, c(27,28,29,30,31,32,33,34,35,36,37,38,39)]
corr_matrix=cor(quantvars)
round(corr_matrix,2)
corrplot(corr_matrix, method = "color")
