# tidytext-tutorial

# learnr 

# Introduction and Narrative
#### 1.	Introduction
Sentiment analysis is the process of detecting positive or negative sentiment in textual data. It is a common natural language processing (NLP) technique used to help business track customer satisfaction, understand customer needs, facilitate customer segmentation and more.
The tidytext package contains a bundle of functions that allow us to perform NLP tasks, including sentiment analysis, more conveniently in conjunction with tidyverse. In particular, the package contains collections of words in association with their sentiment orientation, known as Sentiment Lexicons. In today’s exercises we will practice performing a sentiment analysis with the help of these Sentiment Lexicons.

#### 2.	Data
The dataset “Disneyland Reviews” contains 42,000 reviews of 3 Disneyland branches - Paris, California and Hong Kong, posted by visitors on Trip Advisor. It is freely available at https://www.kaggle.com/datasets/arushchillar/disneyland-reviews. As downloading the dataset requires a Kaggle account, you can download the data [here].

#### 3.	Objective
In today’s tutorial, we will first revisit some of the Alice in Wonderland exercises to familiarise ourselves with functions provided by the tidytext package and see how these functions can help us to perform the same tasks more efficiently. We will then find out the most and least popular Disneyland attractions as well as differences in sentiment of visitors from different countries. By the end of this exercise, we present the results as a [    ] similar to figure 1.


# Alice in Wonderland

## An easier way to reach wonderland: Cleaning text data from Alice in Wonderland (general tutorial)

## Graph, faceted by sentiment, for most common words

# Disneyland

For this exercise, the following packages are required:
```{r}
library(tidytext)
library(tidyverse)
library(tm)
library(stringr)
library(tidyr)
library(dplyr)
library(textdata)
```
### Import the disney review data.
(1) Import data as tibbles  
(2) Reformat the location names of reviewers  
(3) Remove NAs (hint: NA values are indicated as "missing")  

(1) Import
```{r}
disney <- read_csv("DisneylandReviews.csv.zip",
                      na = "NA")
```
(2) As some location names of the reviewers are not correctly formatted, we will change them:
```{r}
disney$Reviewer_Location <- iconv(disney$Reviewer_Location,
  from = "UTF-8", to = "ASCII//TRANSLIT"
)
```
(3) Remove NAs
```{r}
disney <- disney |>
  na.omit() |>
  filter(Year_Month != "missing")
```

### Clean text field (this might take some time)
```{r}
clean <- disney$Review_Text |>
  removeNumbers() |>
  tolower() |>
  removePunctuation() |>
  removeWords(stop_words$word)
disney <- disney |>
  mutate(clean_text = clean)
```

### Text Tokenization for NLP
Text tokenization is the process of converting a data frame (containing strings) into a data frame that has one token (word) per string (from the initial data frame) per row. To do this, tidytext package is needed.
```{r}
disney_unnest <- disney |>
  unnest_tokens(word, clean_text)
```

### Sentiment Analysis of Review: Comparing Rating to Lexicon score by AFINN
AFINN is a lexicon used for general sentiment analysis on English-language texts, assigning words a numeric score between -5 (most negative) to 5 (most positive).

Append a column that summarizes the total sentiment score (according to the AFINN lexicon) per review
```{r}
afinn <- disney_unnest |>
  inner_join(get_sentiments("afinn")) |>
  group_by(index = Review_ID, Rating, Year_Month, Reviewer_Location, Branch) |>
  summarise(sentiments = sum(value)) |>
  mutate(method = "AFINN") |>
  glimpse()
```

To compare, ensure that the `Rating` column is converted to factors.
```{r}
disney <- disney |>
  mutate(Rating = as.factor(Rating))
```

(will clean this up later)
Plot a graph to show the relationship between ratings and `AFINN`
```{r}
afinn |>
  ggplot(aes(Rating, sentiments)) +
  geom_boxplot()
```

Find the countries that provided more than 100 reviews.
```{r}
location <- disney |>
  group_by(Reviewer_Location) |>
  count() |>
  filter(n > 100) |>
  pull(Reviewer_Location)
```

Filter data to the countries found in location.
```{r}
afinn_loc <- afinn |>
  filter(Reviewer_Location %in% location,
         Year_Month != "missing")

afinn_loc |>
  ggplot(aes(Rating, sentiments)) +
  geom_boxplot()

# log sentiments
afinn_loc |>
  ggplot(aes(Rating, log(sentiments))) +
  geom_boxplot()
```

Singapore data
```{r}
afinn_loc |>
  filter(Reviewer_Location == 'Singapore') |>
  ggplot(aes(log(sentiments))) +
  geom_bar() # need to change the bin_width

afinn_loc |>
  filter(Reviewer_Location == 'Singapore') |>
  ggplot(aes(Rating, log(sentiments))) +
  geom_boxplot()
```

For futher analysis, we will only focus on the countries that provided more than 1000 reviews.
```{r}
location_1000 <- disney |>
  group_by(Reviewer_Location) |>
  count() |>
  filter(n > 1000) |>
  pull(Reviewer_Location)

disney_filtered <- disney_unnest |>
  filter(Reviewer_Location %in% location_1000)
```

### Sentiment Analysis: Through nrc
**nrc** categorizes words as positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise and trust. Find the top 20 positive and negative words in the reviews.
```{r}
disney_nrccounts <- disney_filtered |>
  inner_join(get_sentiments("nrc")) |>
  filter(sentiment %in% c("positive", "negative")) |>
  count(word, sentiment, sort = TRUE) |>
  ungroup()

disney_nrccounts |>
  group_by(sentiment) |>
  top_n(20) |>
  ungroup() |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  ggtitle("Words' Contribution to Sentiment in Disney Review", subtitle = "Using the NRC Lexicon")
```
## Facet by park, average rating by time

## Facet by country, positive reviews
Find the top 20 positive words in the reviews, facet by country.
```{r, fig.height=14, fig.width=7}
nrccounts_c <- disney_filtered |>
  inner_join(get_sentiments("nrc")) |>
  filter(sentiment %in% c("positive", "negative")) |>
  group_by(Reviewer_Location) |>
  count(word, sentiment, sort = TRUE) |>
  ungroup()
# positive
nrccounts_c |>
  filter(sentiment == 'positive') |>
  group_by(Reviewer_Location) |>
  top_n(20) |>
  ungroup() |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Reviewer_Location, scales = "free_y") +
  coord_flip() +
  ggtitle("Words' Contribution to Positive Sentiment in Disney Review", subtitle = "Using the NRC Lexicon")
```
## Facet by country, negative reviews
Find the top 20 negative words in the reviews, facet by country.
```{r, fig.height=14, fig.width=7}
# negative
nrccounts_c |>
  filter(sentiment == 'negative') |>
  group_by(Reviewer_Location) |>
  top_n(20) |>
  ungroup() |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Reviewer_Location, scales = "free_y") +
  coord_flip() +
  ggtitle("Words' Contribution to Negative Sentiment in Disney Review", subtitle = "Using the NRC Lexicon")
```

### Which emotions are most common, facet by countries (need to re-order).
```{r, fig.height=14, fig.width=7}
emotions <- disney_filtered |>
  inner_join(get_sentiments("nrc") |>
               filter(sentiment %in% c("positive", "negative", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"))) |>
  count(sentiment, word, Reviewer_Location, sort = TRUE) |>
  ungroup()

emotions |>
  group_by(Reviewer_Location) |>
  mutate(sentiment = reorder(sentiment, n)) |>
  ggplot(aes(sentiment, n, fill = Reviewer_Location)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Reviewer_Location, scales = "free_y") +
  coord_flip() +
  ggtitle("General sentiments of Reviewers", subtitle = "Using the NRC Lexicon")
```

## Facet by sentiment, (just negative and positive), what are people complaining about? And what are people happy about?
