---
title: "IMBD"
output: html_document
date: "2023-10-24"
---

library(ggplot2)
library(splines)

imdb=read.csv("IMDB_data_Fall_2023.csv")

## Building the model
# consider the genre because genre using the muti-variable linear regression
line_genre <- lm(imdb$imdb_score ~ imdb$action+imdb$adventure+imdb$scifi+imdb$thriller+imdb$musical+imdb$romance+imdb$western+imdb$sport+imdb$horror+imdb$drama+imdb$war+imdb$animation+imdb$crime)
summary(line_genre)

# exclude the variable with a p-value > 0.05 （"adventure","scific","sport", and "animation"）
line_genre_updated <- lm(imdb$imdb_score ~ imdb$action+imdb$thriller+imdb$musical+imdb$romance+imdb$western+imdb$horror+imdb$drama+imdb$war+imdb$crime)
summary(line_genre_updated)

# consider the score with the movie_meter_IMBDpro
plot<-ggplot(imdb, aes(y=imdb_score, x=movie_meter_IMDBpro))+geom_point()
plot

# most of the data are centered between 0-100000 so treat others as outliers and discard them
plot<-ggplot(imdb, aes(y=imdb_score, x=movie_meter_IMDBpro))+geom_point()
plot+coord_cartesian(xlim = c(0, 100000))

# a lot of data are between 0-12500 with a trend of imbd score increasing as the ranking decrease
plot<-ggplot(imdb, aes(y=imdb_score, x=movie_meter_IMDBpro))+geom_point()
plot+coord_cartesian(xlim = c(0, 12500))

# try linear regression
imdb_12500 <- imdb[imdb$movie_meter_IMDBpro < 12500,]
line2 <- lm(imdb_12500$imdb_score~imdb_12500$movie_meter_IMDBpro)
summary(line2)

plot<-ggplot(imdb_12500, aes(y=imdb_score, x=movie_meter_IMDBpro))
line_2 <- geom_smooth(method = "lm", formula = y~x )
plot+geom_point()+line_2

# curve trend in the scatter plot with no clear knot, use polynomial regression
poly_rank <- lm(imdb_12500$imdb_score ~ poly(imdb_12500$movie_meter_IMDBpro,4))
summary(poly_rank)

plot<-ggplot(imdb_12500, aes(y=imdb_score, x=movie_meter_IMDBpro))
line_3 <- geom_smooth(method = "lm", formula = y~poly(x,4 ))
plot+geom_point()+line_3

# combine the previous model with specific genres with the imdb pro ranking 
line_genre_ranking <- lm(imdb_12500$imdb_score ~ imdb_12500$action+imdb_12500$thriller+imdb_12500$musical+imdb_12500$romance+imdb_12500$western+imdb_12500$horror+imdb_12500$drama+imdb_12500$war+imdb_12500$crime+poly(imdb_12500$movie_meter_IMDBpro,3))
summary(line_genre_ranking)

# ANOVA test to find the best degree for polynomial regression of ranking
reg1 <- lm(imdb_12500$imdb_score ~ imdb_12500$action+imdb_12500$thriller+imdb_12500$musical+imdb_12500$romance+imdb_12500$western+imdb_12500$horror+imdb_12500$drama+imdb_12500$war+imdb_12500$crime+poly(imdb_12500$movie_meter_IMDBpro,1))
reg2 <- lm(imdb_12500$imdb_score ~ imdb_12500$action+imdb_12500$thriller+imdb_12500$musical+imdb_12500$romance+imdb_12500$western+imdb_12500$horror+imdb_12500$drama+imdb_12500$war+imdb_12500$crime+poly(imdb_12500$movie_meter_IMDBpro,2))
reg3 <- lm(imdb_12500$imdb_score ~ imdb_12500$action+imdb_12500$thriller+imdb_12500$musical+imdb_12500$romance+imdb_12500$western+imdb_12500$horror+imdb_12500$drama+imdb_12500$war+imdb_12500$crime+poly(imdb_12500$movie_meter_IMDBpro,3))
reg4 <- lm(imdb_12500$imdb_score ~ imdb_12500$action+imdb_12500$thriller+imdb_12500$musical+imdb_12500$romance+imdb_12500$western+imdb_12500$horror+imdb_12500$drama+imdb_12500$war+imdb_12500$crime+poly(imdb_12500$movie_meter_IMDBpro,4))
anova(reg1, reg2, reg3, reg4)

# choose degree 3 according to the result
reg <- lm(imdb_12500$imdb_score ~ imdb_12500$action+imdb_12500$thriller+imdb_12500$musical+imdb_12500$romance+imdb_12500$western+imdb_12500$horror+imdb_12500$drama+imdb_12500$war+imdb_12500$crime+poly(imdb_12500$movie_meter_IMDBpro,3))
summary(reg)

# create dummy variables for film colors. 
imdb_12500$black = ifelse(imdb_12500$colour_film == "Black and White", 1, 0)
imdb_12500$color = ifelse(imdb_12500$colour_film == "Color", 1, 0)

# add a linear regression for the film color
imdb_score <- imdb_12500$imdb_score
action <- imdb_12500$action
thriller <- imdb_12500$thriller
musical <- imdb_12500$musical
romance <- imdb_12500$romance
western <- imdb_12500$western
horror <- imdb_12500$horror
drama <- imdb_12500$drama
war <- imdb_12500$war
crime <- imdb_12500$crime
movie_meter_IMDBpro <- imdb_12500$movie_meter_IMDBpro
black = imdb_12500$black
color = imdb_12500$color

# FINAL MODEL
reg_final <- lm(imdb_score ~ action+thriller+musical+romance+western+horror+drama+war+crime+poly(movie_meter_IMDBpro,3)+black+color)
summary(reg_final)

## Testing
# load the test data. 
test_data = read.csv("test_data_IMDB_Fall_2023.csv")
test_data$black = ifelse(test_data$colour_film == "Black and White", 1, 0)
test_data$color = ifelse(test_data$colour_film == "Color", 1, 0)

# select the columns used to predict the model
library(dplyr)
selected_columns <- test_data %>%
  select('action','thriller','musical','romance','western','horror','drama','war','crime','movie_meter_IMDBpro','black','color')

# calculate the predictions based on our model
predictions <- predict(reg_final, newdata = selected_columns)
predictions

# calculate the r-squarde based on our predicted value
actual_values <-c(8.6
,6.7
,8.3
,6.1
,6
,8.5
,7.4)
predicted_values <- c(4.669633
,6.325256
,7.655658
,7.735894
,7.349640
,6.138990
,7.443147)

residuals <- actual_values - predicted_values
sse <- sum((actual_values - mean(actual_values))^2)
sst <- sum(residuals^2)
rsquared <- 1 - (sse / sst)
rsquared

library(caTools)
require(splines)
require(methods)
library(boot)

# test the out-of-sample performance by K-fold cross validation
fit=glm(imdb_score ~ action+thriller+musical+romance+western+horror+drama+war+crime+poly(movie_meter_IMDBpro,3)+black+color)
mse=cv.glm(imdb_12500, fit, K=10)$delta[1]
mse



