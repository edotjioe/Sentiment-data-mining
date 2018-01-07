source("functions.R")
source("preparation.R")

# Train model with first dataset from kaggle
# take out all the words from the train data 
corpus <- corpus_freq(tokens, corpus_size=3000)

my_features <- get_feature_vectors(tokens, corpus = corpus)

my_features <- add_targets(my_features, train)
my_features$sentiment <- as.factor(my_features$sentiment)

# Formula for each model
form <- as.formula(paste("sentiment~", paste(setdiff(names(my_features), c("sentiment")), collapse="+")))
# train the support vector machine with the train data
m_svm <- svm(form, data=my_features, type="C")

# Prepare the first dataset
my_features_imdb <- get_feature_vectors(tokens_imdb, corpus = corpus)

my_features_imdb <- add_targets(my_features_imdb, imdb_review[2000:3000,])
my_features_imdb$sentiment <- as.factor(my_features_imdb$sentiment)
# Predict the sentiment of the reviews from first dataset
pred_svm <- predict(m_svm, my_features_imdb)

table(my_features_imdb$sentiment, pred_svm)

sensitivity(table(my_features_imdb$sentiment, pred_svm))

# Setup and predict second dataset
my_features_polarity <- get_feature_vectors(tokens_polarity, corpus = corpus)

my_features_polarity <- add_targets(my_features_polarity, polarity_review[1000:2000,])
my_features_polarity$sentiment <- as.factor(my_features_polarity$sentiment)

pred_svm <- predict(m_svm, my_features_polarity)

table(my_features_polarity$sentiment, pred_svm)

sensitivity(table(my_features_polarity$sentiment, pred_svm))

# Setup and predict own dataset
my_features_own_review <- get_feature_vectors(tokens_own_review, corpus = corpus)

my_features_own_review <- add_targets(my_features_own_review, own_review[,])
my_features_own_review$sentiment <- as.factor(my_features_own_review$sentiment)

pred_svm <- predict(m_svm, my_features_own_review)

pred_svm
own_review$sentiment

table(my_features_own_review$sentiment, pred_svm)

sensitivity(table(my_features_own_review$sentiment, pred_svm))

m_nbayes <- naiveBayes(form, data=my_features, laplace=1000, threshold=.5)
pred_nbayes <- predict(m_nbayes, my_features_polarity, threshold=.5, laplace=1000)
table(my_features_polarity$sentiment, pred_nbayes)
sensitivity(table(my_features_polarity$sentiment, pred_nbayes))
