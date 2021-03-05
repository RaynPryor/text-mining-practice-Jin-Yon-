#文字探勘(Text Mining)實作
# 
# －－之別問我為啥挑金庸
#因為我上課常看(誤)


#系統參數設定
Sys.setlocale(category = "LC_ALL", locale = "UTF-8") # 避免中文亂碼

require(dplyr)
require(tidytext)
require(jiebaR)
require(gutenbergr)
library(stringr)
library(wordcloud)
library(wordcloud2)
library(ggplot2)
library(tidyr)
library(scales)

###Part 1 ：章節分析
#先對一個代表性章節做初步分析
#

# Jieba套件基本使用
# 初始化斷詞引擎
# # 使用默認參數初始化一個斷詞引擎
jieba_tokenizer = worker()
# 基本斷詞
# 預設好斷詞引擎後，可以使用不同方式進行斷詞。

# 以預設參數進行斷詞

#本文輸入
Eagle_part <- file('神鵰俠侶 第十三回 武林盟主.txt')
Eagle_part = readLines(Eagle_part)


segment(Eagle_part, jieba_tokenizer)
# [183] "在"   "大廳"     "中"       "站立"     "不住"     "一步步"   "退到"    
# [190] "了"   "天井"     "之中"     "　"       "　"       "黃蓉見"   "楊過"    
# [197] "與"   "小龍女"   "並肩"     "坐在"     "柱旁"     "離"       "惡鬥"    
# [204] "的"   "二人"     "不過"     "丈余余"   "自行"     "喁"       "喁"        "與"   
# .....
# "忽然間"   "筆法"    
# [463] "又"    "變"       "運筆"     "不似"     "寫字"     "卻"       "如"      
# [470] "拿"  "了"       "斧斤"     "在"       "石頭"     "上"       "鑿"      
# [477] "打"    "一般"     "　"       "　"       "這"       "一節"     "郭"      
# [484] "芙"    "也"       "瞧"       "出來"     "了"       "問道"     "朱"      
# [491] "伯伯"  "在"       "刻字"     "麼"       "黃蓉笑"   "道"       "我"    

## 以參數形式手動加入 ----
#用函數或者外部資料新增詞彙，兩種方式
# 動態新增自訂詞彙
#  
new_user_word(jieba_tokenizer, c("郭靖","黃蓉","楊過","郭芙","小龍女","魯有腳","金輪法王","霍都","達爾巴","點蒼漁隱","狂風迅雷功","一陽","一陽指法","金杵","打狗棒法","打狗棒","古墓派","大理國","眾目睽睽","爾乃蠻夷","小畜生"))
# [1] TRUE

#如果數量過多，也可以選用外部輸入
# jieba_tokenizer <- worker(user="sdxl_wordlist.txt")

segment(Eagle_part, jieba_tokenizer)
# "忽然間"     "筆法"      
# [457] "又"     "變"         "運筆"       "不似"       "寫字"       "卻"        
# [463] "如"     "拿"         "了"         "斧斤"       "在"         "石頭"      
# [469] "上"     "鑿"         "打"         "一般"       "　"         "　"        
# [475] "這"     "一節"       "郭芙"       "也"         "瞧"         "出來"      
# [481] "了"     "問道"       "朱"         "伯伯"       "在"         "刻字"      
# [487] "麼"    "黃蓉"       "笑道"       "我"         "的"         "女兒"   



## 停用詞使用
# 使用自定義Vector作為停用詞參數
# 動態新增停用詞
tokens <- segment(Eagle_part, jieba_tokenizer)
tokens

stop_words <- c("在","的","下","個","來","至","座","亦","與","或","日","月","年","週","了","也","是","不","和","這","一個")
res <- filter_segment(tokens, stop_words)
#filter_segment:如果tokens吻合stop_words就移除
res
tokens = res

# 外部寫入
# jieba_tokenizer <- worker(user="sdxl_wordlist.txt", stop_word = "cn_stopwords.txt")
# tokens <- segment(Eagle_part, jieba_tokenizer)

##清除雜數
# res <- str_remove_all(res, "[0-9a-zA-Z]+?") #本資料檔不含數字，故不需要
# res

# 將詞彙長度為1的詞清除
tokens <- tokens[nchar(tokens)>1]
tokens
# [1] "金輪法王"   "眼時"     "開時"       "眼前"       "戰局"       "不在意"    
# [7] "實則"      "一切"       "看得"       "清清楚楚"   "眼見"       "霍都"      
# [13] "已處"     "下風"       "突然"       "說道"       "阿古斯金"   "得兒"      
# [19] "哈斯登"    "七兒"      "七兒呼"     "人不知"     "幾句"       "藏語"      
# [25] "說些"     "甚麼"       "霍都"       "卻知"       "師父"       "提醒"      
# [31] "自己"     "不可"       "一味"       "堅守"       "須使"       "狂風迅雷功"


## 2.文字雲(Word Cloud) ----
# 完成了斷詞之後，才是真正的開始，通常第2步驟就是計算詞彙的頻率，通過詞彙的頻率我們就可以直接使用文字雲的套件wordcloud來視覺化文章的重點了！
# 計算詞彙頻率
txt_freq <- freq(tokens)
# 由大到小排列
txt_freq <- arrange(txt_freq, desc(freq))
# 檢查前10名
head(txt_freq, n = 10)
# char freq
# 1      楊過  236
# 2      霍都  113
# 3    小龍女   75
# 4    達爾巴   75
# 5  金輪法王   71
# 6      武功   56
# 7      師父   50
# 8      黃蓉   47
# 9      郭靖   41
# 10     自己   39

# 文字雲套件主要有兩個，wordcloud套件是文字雲的基本款，主要輸出靜態的圖片；wordcloud2顧名思義就是前一個套件的進階版，主要提供互動式的圖片，非常適用在Shiny等網頁中。然而需要注意的是，我認為一般wordcloud的參數比較完整，且兩者參數的命名不盡相同，注意不要混淆了。

par(family=("Microsoft YaHei")) #一般wordcloud需要定義字體，不然會無法顯示中文

# 一般的文字雲 (pkg: wordcloud)
wordcloud(txt_freq$char, txt_freq$freq, min.freq = 5, random.order = F, ordered.colors = F, colors = rainbow(nrow(txt_freq)))

# 互動式文字雲 (pkg: wordcloud2)
wordcloud2(filter(txt_freq, freq > 5), 
           minSize = 2, fontFamily = "Microsoft YaHei", size = 1)

#------------------------------------------------------------------------------

###Part 2 : 全文分析


# Jieba套件基本使用
# 初始化斷詞引擎
# # 使用默認參數初始化一個斷詞引擎
jieba_tokenizer = worker()
# 基本斷詞
# 預設好斷詞引擎後，可以使用不同方式進行斷詞。

# 以預設參數進行斷詞
Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8") # 避免中文亂碼
#但後面還是有亂碼......


#本文輸入:

Eagle <- file('神雕俠侶neo.txt',encoding = "UTF-8-BOM")#感謝R友-阿賢提供 encoding="UTF-8-BOM"解決亂碼問題.
Eagle = readLines(Eagle)


segment(Eagle, jieba_tokenizer)


### 以外部檔案形式加入
# 使用使用者自訂字典、停用詞參數
jieba_tokenizer <- worker(user="sdxl_wordlist.txt", stop_word = "cn_stopwords.txt")
tokens <- segment(Eagle, jieba_tokenizer)
##注意：寫入之文字檔必須為UTF-8編碼


##清除雜數
# res <- str_remove_all(res, "[0-9a-zA-Z]+?") #本資料檔不含數字，故不需要
# res

# 將詞彙長度為1的詞清除
tokens <- tokens[nchar(tokens)>1]
#tokens

### 2.文字雲(Word Cloud) ----
# 完成了斷詞之後，才是真正的開始，通常第2步驟就是計算詞彙的頻率，通過詞彙的頻率我們就可以直接使用文字雲的套件wordcloud來視覺化文章的重點了！
# 計算詞彙頻率
txt_freq <- freq(tokens)
# 由大到小排列
txt_freq <- arrange(txt_freq, desc(freq))
# 檢查前5名
head(txt_freq, n = 20)
# char freq
# 1    楊過 5947
# 2  小龍女 2141
# 3    說道 1460
# 4    郭靖 1425
# 5    黃蓉 1414
# 6  李莫愁 1020
# 7    一個  983
# 8    武功  939
# 9    郭芙  860
# 10   郭襄  762
# 11   一聲  755
# 12   法王  726
# 13   叫道  720
# 14   不知  699
# 15   二人  687
# 16   師父  608
# 17   蒙古  604
# 18 周伯通  573
# 19 陸無雙  558
# 20   突然  553

par(family=("Microsoft YaHei")) #一般wordcloud需要定義字體，不然會無法顯示中文

# 一般的文字雲 (pkg: wordcloud)
wordcloud(txt_freq$char, txt_freq$freq, min.freq = 10, random.order = F, ordered.colors = F, colors = rainbow(nrow(txt_freq)))

# 互動式文字雲 (pkg: wordcloud2)
wordcloud2(filter(txt_freq, freq > 10), 
           minSize = 2, fontFamily = "Microsoft YaHei", size = 1)


# extra: 

##慣用詞分析
role = read.table("role_list.txt",encoding = "UTF-8",header = T)
txt_wordFreq <- txt_freq[ !(txt_freq$char %in% role$X.U.FEFF.角色名), ]
wordcloud2(filter(txt_wordFreq, freq > 200), 
           minSize = 2, fontFamily = "Microsoft YaHei", size = 1)

##比較角色出場頻率
role = read.table("role_list.txt",encoding = "UTF-8",header = T)
role_freq <- filter(txt_freq,char %in% role$X.U.FEFF.角色名)

wordcloud2(filter(role_freq, freq > 60), 
           minSize = 2, fontFamily = "Microsoft YaHei", size = 1)


library(ggplot2)

role_freq60 = filter(role_freq,freq>60)
df = data.frame(term=role_freq60$char, freq=role_freq60$freq)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("角色") + ylab("登場次數") + coord_flip()


readr::write_csv(txt_freq, "txt_freq.csv",encoding = "UTF-8")

#結論：在這類敘述性文字較高的武俠類小說裡，通常是人名(尤其主要角色)的出現頻率偏高
#這要說是理所當然好像也對啦......
#還有，前作主角(郭靖黃蓉)竟然比本作的主要反派(李莫愁、金輪法王)的出現次數加總還高，這是不是搞錯什麼了......

