###########################################################################################
############################       INSTALLATION       #####################################
###########################################################################################

install.packages("RWeka")
install.packages("tm")

###########################################################################################
###########################    TRAIN CLASSIFIER       #####################################
###########################################################################################

data <- read.csv(file = "PATH//LabelledData.txt",sep = "\r",header = FALSE)
data <- data.frame(do.call('rbind', strsplit(as.character(data$V1),',,,',fixed=TRUE)))

data$X1 <- gsub("^\\s+|\\s+$", "", data$X1)
data$X2 <- gsub("^\\s+|\\s+$", "", data$X2)

preprocess <- function(doc){
  
  doc <- gsub(pattern = "[[:punct:]]+",replacement = " ",x = tolower(doc))
  
  library(tm)
  
  docs <- Corpus(VectorSource(doc))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, PlainTextDocument)
  
  return(docs)
}

library(RWeka)

Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))

library(tm)
dtm <- DocumentTermMatrix(preprocess(data$X1),control = list(tokenize=Tokenizer))
dt <- removeSparseTerms(dtm, 0.99)
dt <- as.matrix(dt)

dt <- data.frame(ifelse(dt>0,1,0))
dt$CATEGORY <- data$X2

model <- svm(CATEGORY ~ .,data=dt,type="C-classification")
result <- data.frame(CATEGORY=dt$CATEGORY, PREDICTED=predict(model,dt))
result$MATCH <- ifelse(result$PREDICTED==result$CATEGORY,1,0)

accuracy <- sum(result$MATCH)/nrow(result)

###########################################################################################
###########################    PREDICT CATEGORY       #####################################
###########################################################################################

predict.Category <- function(question){
  dtm1 <- DocumentTermMatrix(preprocess(c(question)),control = list(tokenize=Tokenizer))
  dt1 <- as.matrix(dtm1)
  
  temp <- dt[1,]
  temp <- ifelse(temp>=0,0,-1)
  
  if(length(intersect(colnames(dt1),colnames(dt)))==0){
    return("unknown")
  }
  temp[,intersect(colnames(dt1),colnames(dt))] <- 1
  return(as.character(predict(model,temp)))
}

interact <- function(){
  QUESTION <- readline("Question: ")
  cat(paste("Type:",predict.Category(QUESTION)))
}

interact()
