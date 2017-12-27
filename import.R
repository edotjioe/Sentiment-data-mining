#install.packages("tidytext")

library("dplyr")
library("tidytext")


# Read tsv files
labeled <- read.delim("data/labeledTrainData.tsv", sep = "\t", stringsAsFactors = FALSE)
unlabeled <- read.delim("data/unlabeledTrainData.tsv", sep = "\t", stringsAsFactors = FALSE)
test <- read.delim("data/testData.tsv", sep = "\t", stringsAsFactors = FALSE)