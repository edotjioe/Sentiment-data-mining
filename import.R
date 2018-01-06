# install.packages("tidytext")
# install.packages("readtext")

library("dplyr")
library("tidytext")
library("readtext")
# First data set
# Read tsv files
train <- read.delim("data/labeledTrainData.tsv", sep = "\t", stringsAsFactors = FALSE)
unlabeled <- read.delim("data/unlabeledTrainData.tsv", sep = "\t", stringsAsFactors = FALSE)
test <- read.delim("data/testData.tsv", sep = "\t", stringsAsFactors = FALSE)

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
row.names(polarity_review) <- NULL

# Own data set
own_review <- read.delim("own_reviews.txt", sep = "\t", stringsAsFactors = FALSE)
rm(file, i, file.names, path)
