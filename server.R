library(shiny)
require(data.table)
require(stringr)



# global values that need to be computed only once
# read the four tables
wf_B <- read.table(file("bigrams.txt","r"),header=TRUE,sep=" ",stringsAsFactors=FALSE)
wf_T <- read.table(file("trigrams.txt","r"),header=TRUE,sep=" ",stringsAsFactors=FALSE)
tri_SGT_DT <- read.table(file("triSGT.txt","r"),header=TRUE,sep=" ",stringsAsFactors=FALSE)
bi_SGT_DT <- read.table(file("biSGT.txt","r"),header=TRUE,sep=" ",stringsAsFactors=FALSE)
cat("loaded all the files\n")
predict <- function(sentence) {
  # given a sentence/phrase, extract the last two words
  sl <- unlist(str_split(sentence," "))
  len <- length(sl)
  bigram <- paste(sl[len-1],sl[len])

  # get the subset of the trigram data table witha matching bigram start

  swf_T <- wf_T[wf_T$start == bigram,]

  #check if bigram was found in the trigram table
  if(nrow(swf_T) > 0) {
    # use the counts in the Simple GT table to extract the probability
    swf_T$p <- sapply(swf_T$count,FUN=function(x) tri_SGT_DT$p[tri_SGT_DT$r==x])

    # order by probability
    #swf_T <- swf_T[with(swf_T,order(-p))]
    # find the largest probability
    maxP <-max(swf_T$p) 
    #get the end words with the highest probability
    predictList <- swf_T$end[swf_T$p == maxP]
    predictions <- vector()
    for(i in 1:length(predictList)) {
      predictions[i] <- paste(sentence,predictList[i])
    }
    predictDF <- data.frame(words=predictions)
    if(length(predictions)>10) {return(predictDF[1:10],)}
    return(predictDF)
#     pl_T <- data.frame(words=swf_T$end,probs=swf_T$p)
#     if(nrow(pl_T) > 10) { return(pl_T[1:10,])}
#     return(pl_T)
#     
  } else {
    print(paste("No match for bigram",bigram,"in",sentence,"--looking for unigram match"))
    unigram <- sl[len]
    swf_B <- wf_B[wf_B$start == unigram,]
    if(nrow(swf_B) > 0) {
      # use the counts in the Simple GT table to extract the probability
      swf_B$p <- sapply(swf_B$count,FUN=function(x) bi_SGT_DT$p[bi_SGT_DT$r==x])
      # order by probability
      #swf_B <- swf_B[with(swf_B,order(-p))]
      # find the largest probability
      maxP <-max(swf_B$p)
      #get the end words with the highest probability
      predictList <- swf_B$end[swf_B$p == maxP]
      predictions <- vector()
      for(i in 1:length(predictList)) {
        predictions[i] <- paste(sentence,predictList[i])
      }
      predictDF <- data.frame(words=predictions)
      if(length(predictions)>10) {return(predictDF[1:10],)}
      return(predictDF)
#       pl_B <- data.frame(words=swf_B$end,probs=swf_B$p)
#       if(nrow(pl_B) > 10) { return(pl_B[1:10,])}
#       return(pl_B)
    } else {
      print(paste("No match for unigram",unigram,"in",sentence))
    }
  }
  
}


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
   output$WordsWithProbs <- renderTable({
      cat(input$sentence)
      cat("\n")
      print(predict(input$sentence))
   })
})
