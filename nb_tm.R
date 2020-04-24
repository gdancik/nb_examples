library(tm)
library(naivebayes)

# make up positive and negative tweets
pos_tweets <- c("that is a happy dog",
            "i am happy today",
            "this song is so happy")

neg_tweets <- c("that man is mean",
            "what a mean thing to say",
            "i am not happy at all")


# create a vector of all tweets
all_tweets <- c(pos_tweets, neg_tweets)

# convert all tweets to a corpus (so that we can use DocumentTermMatrix)
corpus <- SimpleCorpus(VectorSource(all_tweets))

# create a document term matrix for our corpus
dtm <- DocumentTermMatrix(corpus)

# view the dtm; this includes a table where 
#   - each row is a document (a tweet in our case)
#   - each column is a unique word
inspect(dtm)

# convert dtm to data frame (needed for naive_bayes)
dtm_df <- data.frame(as.matrix(dtm))

# create a vector of labels (sentiments)
sentiments <- factor(c("positive", "positive", "positive",
                "negative", "negative", "negative"))


# convert all columns of the data frame to factors (needed for naive_bayes)
for (i in 1:ncol(dtm_df)) {
  dtm_df[,i] <- as.factor(dtm_df[,i] >0)
}

# train naive_bayes
nb <- naive_bayes(dtm_df, sentiments)


# proportions of the word "happy" in positive/negative tweets
# 'happy' was found in 100% of positive tweets and 33% of negative tweets
nb$tables$happy

# make predictions on the training set
predict(nb)

# let's create new data
new_df <- dtm_df[1,]

# convert all values to FALSE, since none are equal to 3
#   (would correspond to a tweet that does not contain any of the words)
new_df <- new_df == 3

# update row corresponding to the tweet "He is happy"
new_df[,'happy'] <- TRUE

# classification
predict(nb, new_df)

# show probabilities
predict(nb, new_df, type = "prob")
