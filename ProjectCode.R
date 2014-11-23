library(Hmisc)
library(caret)
library(randomForest)
library(foreach)
library(doParallel)
set.seed(998)

training.file   <- 'pml-training.csv'
test.cases.file <- 'pml-test.csv'
test.cases.url  <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
training.url    <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'

download.file(training.url, training.file)
download.file(test.cases.url,test.cases.file )

training.df   <-read.csv(training.file, na.strings=c("NA","#DIV/0!", ""))
test.cases.df <-read.csv(test.cases.file , na.strings=c("NA", "#DIV/0!", ""))
training.df<-training.df[,colSums(is.na(training.df)) == 0]
test.cases.df <-test.cases.df[,colSums(is.na(test.cases.df)) == 0]


training.df   <-training.df[,-c(1:7)]
test.cases.df <-test.cases.df[,-c(1:7)]

inTraining.matrix    <- createDataPartition(training.df$classe, p = 0.75, list = FALSE)
training.data.df <- training.df[inTraining.matrix, ]
testing.data.df  <- training.df[-inTraining.matrix, ]

registerDoParallel()
classe <- training.data.df$classe
variables <- training.data.df[-ncol(training.data.df)]

rf <- foreach(ntree=rep(250, 4), .combine=randomForest::combine, .packages='randomForest') %dopar% {
  randomForest(variables, classe, ntree=ntree) 
}


training.predictions <- predict(rf, newdata=training.data.df)
confusionMatrix(training.predictions,training.data.df$classe)

testing.predictions <- predict(rf, newdata=testing.data.df)
confusionMatrix(testing.predictions,testing.data.df$classe)

feature_set <- colnames(training.df)
newdata     <- test.cases.df

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

x <- read.csv("pml-test.csv")
x <- x[feature_set[feature_set!='classe']]

pml_write_files(answers)
