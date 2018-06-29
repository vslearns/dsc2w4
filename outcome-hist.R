outcome <- read.csv("pa3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

names(outcome)

outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])