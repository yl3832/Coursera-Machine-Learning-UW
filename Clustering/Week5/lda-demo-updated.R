library(lda)
library(ggplot2)
library(reshape2)
data(cora.documents)
data(cora.vocab)
data(cora.titles)
theme_set(theme_bw())
set.seed(1)
K <- 8## Num clusters

result <- lda.collapsed.gibbs.sampler(cora.documents, K,  ## Num clusters
                                      vocab=cora.vocab,
                                      num.iterations=100,## Num iterations
                                      alpha=0.1,
                                      eta=0.1,
                                      compute.log.likelihood=TRUE) 

## Get the top words in the cluster
top.words <- top.topic.words(result$topics, 5, by.score=TRUE)
top.words
## Number of documents to display
N <- 10
topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
topic.proportions <-topic.proportions[sample(1:dim(topic.proportions)[1], N),]
topic.proportions[is.na(topic.proportions)] <-  1 / K
colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")
topic.proportions.df <- melt(cbind(data.frame(topic.proportions), document=factor(1:N)),variable.name="topic",id.vars = "document")  

ggplot(topic.proportions.df, aes(x=topic,y = value,fill=document)) +geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  facet_wrap(~ document, ncol=5)
head(cora.titles,10)
