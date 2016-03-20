# intro to nlp
# http://parikshit-joshi.com/use-r/machine-learning-sentimental-analysis/
library(tm)
library(rpart)
labeledtrain <- read.csv('./data/labeledTrainData.tsv', quote='', sep='\t')
corpus <- Corpus(VectorSource(labeledtrain$review))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.999)

dtmsparse <- as.data.frame(as.matrix(dtm))

train <- dtmsparse[1:15000,]
test <- dtmsparse[15001:25000,]
train$sentiment <- labeledtrain$sentiment[1:15000]
test$sentiment <- labeledtrain$sentiment[15001:25000]

model <- rpart(sentiment ~ ., data=train, method='class')
ptr <- predict(model, train)
pts <- predict(model, test)
sum(as.numeric(ptr[,2] > 0.5) == train$sentiment)/nrow(train)
sum(as.numeric(pts[,2] > 0.5) == test$sentiment)/nrow(test)

plot(dtm)
