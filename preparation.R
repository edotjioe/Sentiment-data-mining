# install.packages("tidytext")
# install.packages("readtext")

library("tidytext")
library("readtext")

# First data set
# Read tsv files
imbd_review <- read.delim("data/labeledTrainData.tsv", sep = "\t", stringsAsFactors = FALSE)

# Fill the train dataset with 1000 observations from imdb dataset
train <- imbd_review[1:2000, c(2,3)]

#Second dataset
# Reading all of the positive reviews from multiple files
path <- "review_polarity/pos/"
polarity_review <- ""
file.names <- dir(path, pattern =".txt")
for(i in 1:length(file.names)){
  file <- readtext(paste0(path, file.names[i]))
  file$sentiment <- 1
  polarity_review <- rbind(polarity_review, file)
}

# Removing first row due to readtext method 
polarity_review <- polarity_review[-1,]

# Reading all of the negative reviews from multiple files
path <- "review_polarity/neg/"
file.names <- dir(path, pattern =".txt")
for(i in 1:length(file.names)){
  file <- readtext(paste0(path, file.names[i]))
  file$sentiment <- 0
  polarity_review <- rbind(polarity_review, file)
}
# Change the column type of sentiment from chr to int
polarity_review$sentiment <- as.integer(polarity_review$sentiment)
row.names(polarity_review) <- NULL

# Change order and names of columns to the same as imbd_review
polarity_review <- polarity_review[, c(1,3,2)]
colnames(polarity_review) <- colnames(imbd_review)

# Shuffle all the polarity_reviews
polarity_review <- polarity_review[sample(nrow(polarity_review)),]

train <- rbind(train, polarity_review[1:1000, c(2,3)])

# Own data set
own_review <- read.delim("own_reviews.txt", sep = "\t", stringsAsFactors = FALSE)

# Create tokenized versions of the datasets
tokens <- tokenize(train[,2])
tokens_imdb <- tokenize(imbd_review[2000:3000, 3])
tokens_polarity <- tokenize(polarity_review[1000:2000,3])
tokens_own_review <- tokenize(own_review[,3])

# Remove unneeded variables
rm(file, i, file.names, path, file.names, size)
