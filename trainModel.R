source("import.R")
source("dataMiningFunctions.R")

size = 1500

# Train model with first dataset from kaggle
#take out all the words from the train reviews 
tokens <- tokenize(train[1:size,3])

corpus <- corpus_freq(tokens, corpus_size=3000)

my_features <- get_feature_vectors(tokens, corpus = corpus)

my_features <- add_targets(my_features, train[1:size,])
my_features$sentiment <- as.factor(my_features$sentiment)

train_set <- sample_frac(my_features, .8)
test_set <- setdiff(my_features, train_set)
test_set <- sample_frac(test_set, 1)

    #Formula for each model
form <- as.formula(paste("sentiment~", paste(setdiff(names(test_set), c("sentiment")), collapse="+")))

m_svm <- svm(form, data=train_set, type="C")

pred_svm <- predict(m_svm, test_set)

table(test_set$sentiment, pred_svm)

sensitivity(table(test_set$sentiment, pred_svm))

# Setup and predict second dataset
tokens_polarity <- tokenize(polarity_review[,2])

my_features_polarity <- get_feature_vectors(tokens_polarity, corpus = corpus)

my_features_polarity <- add_targets(my_features_polarity, polarity_review[,])
my_features_polarity$sentiment <- as.factor(my_features_polarity$sentiment)

test_set_polarity <- sample_frac(my_features_polarity)

pred_svm <- predict(m_svm, test_set_polarity)

table(test_set_polarity$sentiment, pred_svm)

sensitivity(table(test_set_polarity$sentiment, pred_svm))

# Setup and predict own dataset
tokens_own_review <- tokenize(own_review[,3])

my_features_own_review <- get_feature_vectors(tokens_own_review, corpus = corpus)

my_features_own_review <- add_targets(my_features_own_review, own_review[,])
my_features_own_review$sentiment <- as.factor(my_features_own_review$sentiment)

test_set_own_review <- sample_frac(my_features_own_review)

pred_svm <- predict(m_svm, test_set_own_review)

table(test_set_own_review$sentiment, pred_svm)

sensitivity(table(test_set_own_review$sentiment, pred_svm))
