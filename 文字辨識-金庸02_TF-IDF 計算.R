###TF-IDF 計算
# 
# 最簡單的詞頻分析就屬TF-IDF了！
# 
# 某一特定文件內的高詞語頻率，以及該詞語在整個文件集合中的低文件頻率，可以產生出高權重的TF-IDF。因此，TF-IDF傾向於過濾掉常見的詞語，保留重要的詞語。
# 
#※ TF :  該詞在文件d中的出現次數 / 在文件d中所有字詞的出現次數和
#※ IDF : log(語料庫中的文件總數 / 包含詞語 t 的文件數目)


library("tm")
library("tmcn")
library("rJava")
library("Rwordseg")
library("SnowballC")
library("slam")

#Sys.setlocale(category = "LC_ALL", locale = "UTF-8-BOM") # 避免中文亂碼
#說是這麼說，跑出亂碼的次數也不下N遍...(以下略

Sys.setlocale(category = "LC_ALL", locale = "UTF-8-BOM") 


csv<- read.csv("神雕俠侶.csv",colClasses="character",encoding = "UTF-8")


#導入台灣大學定義的字典到系統中，該字典中含有正面及負面的簡體詞和繁體詞共22173個。這邊我們只用繁體的部分。
data(NTUSD)
positive_tradition <- NTUSD[[3]]
negtive_tradition <- NTUSD[[4]]
insertWords(positive_tradition)
insertWords(negtive_tradition)

#導入自訂辭典
dir <- read.csv("sdxl_wordlist.txt", encoding="UTF-8", sep="")
insertWords(dir$X.U.FEFF.value)

#insertWords(tokens)

role <- read.csv("role_list.txt", encoding="UTF-8", sep="")
insertWords(role$X.U.FEFF.角色名)

#導入指定停止詞
stopwords <- read.csv("cn_stopwords.txt" ,encoding = "UTF-8", sep="")
stopword <- stopwordsCN(stopwords = stopwords$X.U.FEFF.停止詞, useStopDic = TRUE)

#※分割測試
#
test = "楊過隨著小龍女穿越甬道，奔出古墓，大喜無已。"
test01 <- segmentCN(test, analyzer = "default",returnType = "tm")
test02 <- segmentCN(test, analyzer = "hmm",returnType = "tm")
test03 <- segmentCN(test, analyzer = "jiebaR",returnType = "tm")
#後來證實只有"jiebaR"的分析器才能接收自訂詞彙跟停止詞
#我才不會承認因為不知道這件事而鬼打牆了大半天......

seg_words <- segmentCN(as.character(csv), analyzer = "jiebaR", returnType = "tm")
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

#輸出tf-idf表
write.csv(d, file = "result.csv",fileEncoding = "big-5")


#※詞彙彼此間重要程度關聯
#依據第一部份TF-IDF的權重值來做dendrogram的圖示

dtm <- DocumentTermMatrix(wordcorpus, control = list(wordLengths = c(2, Inf))) 
dtm01 <- weightTfIdf(dtm)

#可以在全部資料構成的矩陣中，手動剔除一些權重值較低的字詞
#為了方便觀看設作0.09，設更高則怕文字會太擁擠
dtm02 <- removeSparseTerms(dtm01, 0.09) 


tdm = as.TermDocumentMatrix(dtm02) 
tdm <- weightTfIdf(tdm)
mydata.df <- as.matrix(tdm) 
mydata.df.scale <- scale(mydata.df)
d<- dist(mydata.df.scale)

fit <- hclust(d)
plot(fit)

save.image("~/R Code/59487.RData")
