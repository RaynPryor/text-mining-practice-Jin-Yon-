load("~/R Code/59487.RData")
#※語意主題多樣性以及視覺化

library(Rwordseg) 
library(rJava) 
library(tm) 
library(slam) 
library(topicmodels) 
library(igraph)


# csv<- read.csv("神雕俠侶.csv",colClasses="character",encoding = "UTF-8")
# seg_words <- lapply(csv$content, segmentCN)
# 
# doc.list <- strsplit(as.character(seg_words), split=" ")
# 
# dg.corpus <- gsub("'", "", doc.list) 
# dg1.corpus<-gsub("\\b\\w{1,1}\\b","",doc.list) 
# dg2.corpus <- gsub("[[:punct:]]", " ", dg1.corpus) 
# dg3.corpus <- gsub("[[:cntrl:]]", " ", dg2.corpus) 
# dg4.corpus <- gsub("^[[:space:]]+", "", dg3.corpus) 
# dg5.corpus <- gsub("[[:space:]]+$", "", dg4.corpus)
# dg6.corpus <- gsub("[[0-9]]", " ", dg5.corpus)
wordcorpus <- VCorpus(VectorSource(dg6.corpus))

tdm <- TermDocumentMatrix(wordcorpus, control = list(wordLengths = c(2, Inf))) 
dtm<-DocumentTermMatrix(wordcorpus, control = list(wordLengths = c(2, Inf)))

term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
l1=term_tfidf >= quantile(term_tfidf, 0.99)
dtm <- dtm[,l1]
dtm = dtm[row_sums(dtm)>0, ]; dim(dtm) 
summary(col_sums(dtm))

k = 30
SEED <- 2003 
jss_TM2 <- list(
  VEM = LDA(dtm, k = k, control = list(seed = SEED)),
  VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)), Gibbs = LDA(dtm, k = k, method = "Gibbs",
                                                                                                control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)), CTM = CTM(dtm, k = k,
                                                                                                                                                                                control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))) ) 
save(jss_TM2, file = paste(getwd(), "/jss_TM2.Rdata", sep = ""))
# save(jss_TM, file = paste(getwd(), "/jss_TM1.Rdata", sep = ""))

termsForSave1<- terms(jss_TM2[["VEM"]], 5) 
termsForSave2<- terms(jss_TM2[["VEM_fixed"]], 5) 
termsForSave3<- terms(jss_TM2[["Gibbs"]], 5) 
termsForSave4<- terms(jss_TM2[["CTM"]], 5)

write.csv(as.data.frame(t(termsForSave1)),
          paste(getwd(), "/topic-document_", "_VEM_", k, "_2.csv", sep=""), fileEncoding = "UTF-8")

write.csv(as.data.frame(t(termsForSave2)),
          paste(getwd(), "/topic-document_", "_VEM_fixed_", k, "_2.csv", sep=""), fileEncoding = "UTF-8")

write.csv(as.data.frame(t(termsForSave3)),
          paste(getwd(), "/topic-document_", "_Gibbs_", k, "_2.csv", sep=""),
          fileEncoding = "UTF-8")

write.csv(as.data.frame(t(termsForSave4)),
          paste(getwd(), "/topic-document_", "_CTM_", k, "_2.csv", sep=""), fileEncoding = "UTF-8")

tfs = as.data.frame(termsForSave3, stringsAsFactors = F); tfs[,1] 
adjacent_list = lapply(1:30, function(i) embed(tfs[,i], 2)[, 2:1])
edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F) 
topic = unlist(lapply(1:30, function(i) rep(i, 4)))

edgelist$topic = topic
g <-graph.data.frame(edgelist,directed=T ) 
l<-layout.fruchterman.reingold(g)
edge.color="black"
nodesize = centralization.degree(g)$res
V(g)$size = log( centralization.degree(g)$res )

nodeLabel = V(g)$name
E(g)$color = unlist(lapply(sample(colors()[26:137], 10), function(i) rep(i, 9))); unique(E(g)$color)

png(paste(getwd(), "/topic_graph_gibbs_", g, "XX",".png", sep=''),width=5, height=5, units="in", res=700)
plot(g, vertex.label= nodeLabel, edge.curved=TRUE, vertex.label.cex =0.5, edge.arrow.size=0.2, layout=l )
dev.off()

