#Author: @Addison Ji, @Alisa Liu, @Keani Schuller, @Yusen Tang @Zhiming Zhang
#Course: MGSC 661-IMDB Blockbuster Prediction
#Date: Oct 26th
#Objective: The code is broken into 3 files with the codes of data exploration, regression and tests for issues, modeling and prediction
#Code File 1: Data Explorations & Understand the Variables

#Read Data
IMDB_data=read.csv("IMDB_data_Fall_2023.csv")
attach(IMDB_data)
View(IMDB_data)

#Load packages
library(ggplot2)
require(methods)
install.packages("lmtest")
install.packages("plm")
require(lmtest)
require(plm)
install.packages("ggpubr")
library(ggpubr)
install.packages("dplyr")
library(dplyr)
install.packages("stargazer")
library(stargazer)

#Distribution of Dependent variable
########################################################################
#################### 1:Distribution of Dependent variable###############
########################################################################

#### Histogram of imdb_score (Appendix Figure 1)
ggplot(IMDB_data, aes(x=imdb_score)) + 
  geom_histogram(binwidth=0.5, fill="#7F7F7F", color="black") +
  labs(title="Distribution of IMDb Scores", x="IMDb Score", y="Number of Movies") +
  theme_minimal() +
  theme(
    text = element_text(color="black", size=10),
    title = element_text(face="bold", size=12),
    axis.title = element_text(size=10),
    axis.text = element_text(size=10),)

#Distribution of Predictors
####################################################################### 
##################### 2:Distribution of Predictors#####################
#######################################################################

##Predictors for Movie Details

### Histogram of release year, month, day (Appendix Figure 2)
par(mfrow=c(1,3), mar=c(5,5,4,2) + 0.1) 

#### Plot for release year
hist(IMDB_data$release_year, col="#7F7F7F", border="black", main="Release Year", xlab="", ylab="Number of Movies", xlim=c(min(IMDB_data$release_year), max(IMDB_data$release_year)))

#### Plot for release month
IMDB_data$release_month=as.factor(IMDB_data$release_month)
hist(as.numeric(IMDB_data$release_month), col="#7F7F7F", border="black", main="Release Month", xlab="", ylab="", breaks=12, xlim=c(1,12), axes=FALSE)
axis(1, at=1:12, labels=levels(IMDB_data$release_month))

#### Plot for release day
hist(IMDB_data$release_day, col="#7F7F7F", border="black", main="Release Day", xlab="", ylab="", xlim=c(1,31), breaks=31)

### Histogram of language (Appendix Figure 3)
####Use bar chart to display the results is more clear for this predictor
language_freq=table(IMDB_data$language)
barplot(language_freq, col="#7F7F7F", border="black", las=1, 
        main="Distribution of Movies by Language", ylab="Number of Movies", xlab="")
#### Add the x-axis title manually to control its position so the axis lable is not overlap with the x-axis
title(xlab="Language", line=3)  

### Histogram of country (Appendix Figure 4)
#####Use bar chart to display the results is more clear for this predictor
country_freq=table(IMDB_data$country)
barplot(country_freq, col="#7F7F7F", border="black", las=1, 
        main="Distribution of Movies by Country", ylab="Number of Movies", xlab="")
title(xlab="Country", line=3) 

### Histogram and boxplot of Duration (Appendix Figure 5)
par(mfrow=c(1,2), mar=c(5,5,4,2) + 0.1)
#### Histogram for duration
hist(IMDB_data$duration, col="#7F7F7F", border="black", main="Histogram of Duration", xlab="Duration (minutes)", ylab="Number of Movies")
#### Boxplot for duration
boxplot(IMDB_data$duration, horizontal=TRUE, col="#7F7F7F", border="black", main="Boxplot of Duration", xlab="Duration (minutes)")

### Histogram of Maturity Ratings (Appendix Figure 6)
####Use bar chart to display the results is more clear for this predictor
maturity_rating_freq=table(IMDB_data$maturity_rating)
#### Bar plot for maturity ratings
barplot(maturity_rating_freq, col="#7F7F7F", border="black", las=2, 
        main="Distribution of Movies by Maturity Rating", ylab="Number of Movies", xlab="")

### Histogram of Genres (dummy variable for all genres)
par(mfrow=c(3,5), mar=c(5,5,2,1))
par(pin=c(1, 2))
#### Histograms for each genre
hist(action, col="#7F7F7F", main="Action", xlab="")
hist(adventure, col="#7F7F7F", main="Adventure", xlab="")
hist(scifi, col="#7F7F7F", main="Sci-Fi", xlab="")
hist(thriller, col="#7F7F7F", main="Thriller", xlab="")
hist(musical, col="#7F7F7F", main="Musical", xlab="")
hist(romance, col="#7F7F7F", main="Romance", xlab="")
hist(western, col="#7F7F7F", main="Western", xlab="")
hist(sport, col="#7F7F7F", main="Sport", xlab="")
hist(horror, col="#7F7F7F", main="Horror", xlab="")
hist(drama, col="#7F7F7F", main="Drama", xlab="")
hist(war, col="#7F7F7F", main="War", xlab="")
hist(animation, col="#7F7F7F", main="Animation", xlab="")
hist(crime, col="#7F7F7F", main="Crime", xlab="")

###Collinearity matrix between Genres (Appendix Figure 13)
install.packages("psych")
require(psych)
require(car)
install.packages("corrplot")
library(corrplot)

genres=IMDB_data[, c(27,28,29,30,31,32,33,34,35,36,37,38,39)]
corr_matrix=cor(genres)
round(corr_matrix,2)
corrplot(corr_matrix, method = "color")

### Cloud for plot of key words (Appendix Figure 7)
install.packages("tidyverse")
install.packages("wordcloud")
install.packages("tidytext")
library(tidyverse)
library(wordcloud)
library(tidytext)
#### Sample data
data=data.frame(plot_keywords = IMDB_data$plot_keywords)
#### Splitting the keywords and counting their frequency
keyword_count <- data %>%
  unnest_tokens(keyword, plot_keywords, token = "regex", pattern = "\\|") %>%
  count(keyword, sort = TRUE)
#### Displaying top N keywords
top_n <- 10
top_keywords <- head(keyword_count, top_n)
print(top_keywords)
#### Bar chart of top N keywords
ggplot(top_keywords, aes(x = reorder(keyword, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Keywords", x = "Keywords", y = "Frequency")
#### Word cloud
wordcloud(words = keyword_count$keyword, freq = keyword_count$n, min.freq = 2)

##Predictors for Marketing Visility

### Histogram and box plots of number of faces (Appendix Figure 8)
par(mfrow=c(1,2), mar=c(5,5,4,2) + 0.1)
hist(IMDB_data$nb_faces, col="#7F7F7F", border="black", main="Number of Faces in Poster", xlab="Number of Faces", ylab="Number of Movies")
boxplot(IMDB_data$nb_faces, horizontal=TRUE, col="#7F7F7F", border="black", main="Number of Faces in Poster", xlab="Number of Faces")

###Histogram of count of the news article
par(mfrow=c(1,2), mar=c(5,5,4,2) + 0.1)
#### Histogram for duration
hist(IMDB_data$nb_news_articles, col="#7F7F7F", border="black", main="Number of news/articles", xlab="Number of new/articles", ylab="Number of Movies")
#### Boxplot for duration
boxplot(IMDB_data$nb_news_articles, horizontal=TRUE, col="#7F7F7F", border="black", main="Number of news/articles", xlab="Number of news/articles")

### Plot distributors
table(IMDB_data$distributor)
num_distributors=length(unique(IMDB_data$distributor))
print(paste("Total number of unique distributors:", num_distributors))
#### Calculate the top 10 distributors and their respective counts
distributor_counts=table(IMDB_data$distributor)
top_distributors=head(sort(distributor_counts, decreasing = TRUE), 10)
print(top_distributors)
####Regression by grouping top 4 distributors
IMDB_data$top_4_distributor=ifelse(IMDB_data$distributor %in% c('Warner Bros.', 'Universal Pictures', 'Paramount Pictures','Twentieth Century Fox'), 1, 0)
model=lm(imdb_score ~ top_4_distributor, data=IMDB_data)
stargazer(model, type="html")

##Predictors for Cast Team

### BoxPlot of actor meter 1 2 3
par(mfrow = c(1, 3))
par(pin=c(3, 3))
boxplot(IMDB_data$actor1_star_meter, col = "#7F7F7F", main = "Main Actor Ranking", xlab = "Star Meter")
boxplot(IMDB_data$actor2_star_meter, col = "#7F7F7F", main = "Second Main Actor Ranking", xlab = "Star Meter")
boxplot(IMDB_data$actor3_star_meter, col = "#7F7F7F", main = "Third Main Actor Ranking", xlab = "Star Meter")

###Plot main,second and third main actor, director and Cinematographer and calculate how many movies asscoiated with the top of these people to understand the distribution spread

#### Calculate the number of unique values for main actor
num_unique_actor1=length(unique(IMDB_data$actor1))
print(paste("Total number of unique actors (actor1 column):", num_unique_actor1))
#### Top 5 main actors and the movies they have
actor1_counts=table(IMDB_data$actor1)
top_actor1=head(sort(actor1_counts, decreasing = TRUE), 5)
print("Top 5 actors (actor1 column) and their movie counts:")
print(top_actor1)

#### Calculate the number of unique values for second main actor
num_unique_actor2=length(unique(IMDB_data$actor2))
print(paste("Total number of unique actors (actor2 column):", num_unique_actor2))
#### Top 5 second main actors and the movies they have
actor2_counts=table(IMDB_data$actor2)
top_actor2=head(sort(actor2_counts, decreasing = TRUE), 5)
print("Top 5 actors (actor2 column) and their movie counts:")
print(top_actor2)

#### Calculate the number of unique values for third main actor
num_unique_actor3=length(unique(IMDB_data$actor3))
print(paste("Total number of unique actors (actor3 column):", num_unique_actor3))
#### Top 5 third main actors and the movies they have
actor3_counts=table(IMDB_data$actor3)
top_actor3=head(sort(actor3_counts, decreasing = TRUE), 5)
print("Top 5 actors (actor3 column) and their movie counts:")
print(top_actor3)

#### Calculate the number of unique values for 'director'
num_unique_directors=length(unique(IMDB_data$director))
print(paste("Total number of unique directors:", num_unique_directors))
#### Top 5 directors and the movies they have
director_counts= table(IMDB_data$director)
top_directors=head(sort(director_counts, decreasing = TRUE), 5)
print("Top 5 directors and their movie counts:")
print(top_directors)

#### Calculate the number of unique values for 'cinematographer'
num_unique_cinematographers=length(unique(IMDB_data$cinematographer))
print(paste("Total number of unique cinematographers:", num_unique_cinematographers))
#### Top 5 cinematographer and the movies they have
cinematographer_counts=table(IMDB_data$cinematographer)
top_cinematographers=head(sort(cinematographer_counts, decreasing = TRUE), 5)
print("Top 5 cinematographers and their movie counts:")
print(top_cinematographers)

##Predictors for Movie Production

###Histogram for production company and the number of movies they produced
par(mfrow = c(1, 1))
##### Calculate the counts for each production company
production_company_counts=table(IMDB_data$production_company)
####Visualize the distribution of movie counts for all production companies
hist(production_company_counts, 
     main="Distribution of Movie Counts by Production Company", 
     xlab="Number of Movies", 
     ylab="Number of Production Companies", 
     col="#7F7F7F", 
     border="white")
#### Calculate the top 10 distributors and their respective counts
top_production_company=head(sort(production_company_counts, decreasing = TRUE), 10)
print(top_production_company)
####Regression by grouping top 4 distributors
IMDB_data$top_4_production_company=ifelse(IMDB_data$distributor %in% c('Universal Pictures', 'Paramount Pictures','Columbia Pictures Corporation','Warner Bros. '), 1, 0)
model2=lm(imdb_score ~ top_4_production_company, data=IMDB_data)
summary(model2)

###Histogram and Boxplots for movie budget
par(mfrow=c(1,2))
par(pin=c(3, 3))
####Histogram
hist(IMDB_data$movie_budget, col="#7F7F7F", border="black", main="Movie Budget", xlab="Movie Budget", ylab="Number of Movies")
####Box plots
boxplot(IMDB_data$movie_budget, col = "#7F7F7F", main = "Movie Budget", xlab = "Movie Budget")

###Histogram and Boxplots for aspect_ratio
par(mfrow=c(1,2), mar=c(5,5,4,2) + 0.1)
####Histogram
hist(IMDB_data$aspect_ratio, col="#7F7F7F", border="black", main="Aspect ratio of Movie Image", xlab="Aspect Ratio", ylab="Number of Movies")
####Box plots
boxplot(IMDB_data$aspect_ratio, col = "#7F7F7F", main = "Aspect ratio of Movie Image", xlab = "Aspect Ratio")

###Bar chart for distribution of the movie colors
par(mfrow=c(1,1), mar=c(4,4,3,2))
par(pin=c(3, 3))
library(ggplot2)
ggplot(IMDB_data, aes(x = colour_film)) +
  geom_bar(aes(fill = colour_film)) +
  labs(title = "Comparison of Film Types",
       x = "Film Type",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Color" = "black", "Black and White" = "grey"))


