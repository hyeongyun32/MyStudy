library(dplyr)
library(readr)
library(stringr)
library(textclean)
library(tidytext)
library(ggplot2)
library(KoNLP)
library(tidyr)
library(tidygraph)
library(ggraph)
library(widyr)

#redlight.csv를 sangjae에 저장
sangjae <- read_csv("redlight.csv",locale=locale('ko',encoding='euc-kr'))
#sangjae에 한글외에 문자들 모두 제거하고 id 고유번호 추가한 후 sangjae_comment에 저장
sangjae_comment <- sangjae %>% mutate(comment = str_replace_all(comment,"[^가-힣]"," "),
                            comment = str_squish(comment),
                            id = row_number())
#unnest_tokens의 token = SimplePos22를 통해 22개의 품사로 구분하고 comment_pos에 저장
comment_pos <- sangjae_comment %>% unnest_tokens(input = comment, output = word, token = SimplePos22,drop = F)
#separate_rows를 통해 +가 나올때마다 행을 나눠준다.
comment_pos <- comment_pos %>% separate_rows(word, sep = "[+]")
# 파이계수
#\n이 붙어있는 단어 /로 시작하는 모든 문자를 제거한다.
noun <- comment_pos %>% filter(str_detect(word, "/n")) %>% mutate(word = str_remove(word,"/.*$"))
#동사/pv,형용사/pa 붙어있는 단어를 추출하여 단어뒤에 태그대신 다를 붙여준다.
pvpa <- comment_pos %>% filter(str_detect(word,"/pv|/pa")) %>% mutate(word = str_replace(word,"/.*$","다"))
#filter(str_count(word)>=2)를 통해 두글자 이상의 단어만 남긴다.
comment <- bind_rows(noun, pvpa) %>% filter(str_count(word)>=2) %>% arrange(id)

#add_count()를 통해 원자료에 빈도를 나타낸 변수를 추가한다.
#pairwise_count()로 한단어를 기준으로 다른 모든단어빈도를 계산한다.
word_cors <- comment %>% add_count(word) %>% filter(n>=2) %>% pairwise_cor(item = word, feature = id, sort = T)

#correlation이 0.3이상인것들만 as_tbl_graph()로 네트워크 그래프데이터로 변환하여 변수에 저장
graph_cors <- word_cors %>% filter(correlation >= 0.3) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(),
                                                                                                 group = as.factor(group_infomap()))
set.seed(1234)
#geom_edge_link가 간선, geom_node_point가 노드, geom_node_text가 텍스트, theom_graph() 배경제거
ggraph(graph_cors, layout = "fr") + geom_edge_link(color = "gray50", alpha = 0.5) + geom_node_point(color = "lightcoral",size = 5) +
  geom_node_text(aes(label = name), repel = T, size = 5) + theme_graph()


#바이그램
#명사와, 동사 그리고 형용사 모두 추출하고 두글자 이상만 남깁니다.
comment_new <- comment_pos %>% filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$","다"),
                       str_remove(word,"/.*$"))) %>%
  filter(str_count(word) >= 2) %>% arrange(id)
# 댓글 하나가 하나의 행이 되도록 결합합니다.
line_comment <- comment_new %>% group_by(id) %>% summarise(sentence = paste(word, collapse = " "))
#unnest_tokens를 이용하여 바이그램으로 토큰화합니다.
bigram_comment <- line_comment %>% unnest_tokens(input = sentence, output = bigram, token = "ngrams", n = 2)
#separate()로 바이그램의 두단어를 서로다른 변수로 분리합니다.
bigram_separated <- bigram_comment %>% separate(bigram, c("word1","word2"), sep = " ")
#한단어로된 문장은 바이그램 토큰화 하면 NA가 되고 na.omit()으로 NA를 제거하고 단어쌍 빈도를 구한다.
pair_bigram <- bigram_separated %>% count(word1, word2, sort = T) %>% na.omit()

set.seed(1234)

graph_bigram <- pair_bigram %>% filter(n >= 1) %>% as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),group = as.factor(group_infomap()))

ggraph(graph_bigram, layout = "fr") + geom_edge_link(color = "gray50", alpha = 0.5) + 
  geom_node_point(aes(size = centrality, color = group), show.legend = F) + scale_size(range = c(4,8)) +
  geom_node_text(aes(label = name), repel = T, size = 5) + theme_graph()

