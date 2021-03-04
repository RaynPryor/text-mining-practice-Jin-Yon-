library("tm")
library("tmcn")
library("rJava")
library("Rwordseg")
library("SnowballC")
library("slam")

Sys.setlocale(category = "LC_ALL", locale = "UTF-8") # 避免中文亂碼

csv<- read.csv("神雕俠侶.csv",colClasses="character",encoding = "UTF-8")

data(NTUSD)

positive_tradition <- NTUSD[[3]]
negtive_tradition <- NTUSD[[4]]
insertWords(positive_tradition)
insertWords(negtive_tradition)

dir <- read.table("sdxl_wordlist.txt",encoding = "UTF-8")
insertWords(dir$V1)

#insertWords(tokens)

role <- read.table("role_list.txt",encoding = "UTF-8",header = T)
insertWords(role)

stopwords <- read.table("cn_stopwords.txt",encoding = "UTF-8")
stopword <- stopwordsCN(stopwords = stopwords$V1, useStopDic = TRUE)

seg_words <- segmentCN(as.character(csv), returnType = "tm")
doc.list <- strsplit(as.character(seg_words), split=" ")

dg.corpus <- gsub("'", "", doc.list)
dg1.corpus <- gsub("[[:punct:]]", " ", dg.corpus) 
dg2.corpus <- gsub("[[:cntrl:]]", " ", dg1.corpus) 
dg3.corpus <- gsub("^[[:space:]]+", "", dg2.corpus) 
dg4.corpus <- gsub("[[:space:]]+$", "", dg3.corpus)
dg5.corpus <- gsub("[[0-9]]", " ", dg4.corpus)
dg6.corpus <- gsub("[[0-9]]", "", dg5.corpus)
wordcorpus <- VCorpus(VectorSource(dg5.corpus))

tdm <- TermDocumentMatrix(wordcorpus, control = list(wordLengths = c(2, Inf)))
tdm.tfidf<-weightTfIdf(tdm, normalize = T) 
dtm<-as.matrix(tdm.tfidf)
v<-sort(rowSums(dtm), decreasing = T) 
d<-data.frame(word=names(v),tfidf=v)

write.csv(d, file = "test.csv",fileEncoding = "UTF-8")

#處理亂碼
#為了讓結果漂亮點，只能改了
rn = rownames(dtm)
rn
# [1] "3016"   "3017"   "5239"   "5450"   "562d"  
# [6] "59f9"   "5f11"   "64c0"   "6c0a"   "7081"  
# [11] "7f4e"   "8552"   "98c3"   "99e1"   "一人"  

rn[1:5] <- c("〖","〗","刹那","吶喊","嘭")
rn[6:10] <- c("姹","弑","擀","氊","炁")
rn[11:14] <- c("罎","蕒","飃","駡道")

rownames(dtm) <- rn
tdm[["dimnames"]][["Terms"]] <-rn
names(v) <-rn

#※詞彙彼此間重要程度關聯
#依據第一部份TF-IDF的權重值來做dendrogram的圖示

dtm <- DocumentTermMatrix(wordcorpus, control = list(wordLengths = c(2, Inf))) 
dtm01 <- weightTfIdf(dtm)

#可以在全部資料構成的矩陣中，手動剔除一些權重值較低的字詞
#為了方便觀看設作0.87，設更高則怕文字會太擁擠
dtm02 <- removeSparseTerms(dtm01, 0.895) 

#不知為啥還是有亂碼
dtm02$dimnames$Terms[1:3] = c("剎那","吶喊","駡道")

tdm = as.TermDocumentMatrix(dtm02) 
tdm <- weightTfIdf(tdm)
mydata.df <- as.matrix(tdm) 
mydata.df.scale <- scale(mydata.df)
d<- dist(mydata.df.scale)

fit <- hclust(d)
plot(fit)

save.image("~/R Code/59487.RData")
