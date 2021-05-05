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

#redlight.csv¸¦ sangjae¿¡ ÀúÀå
sangjae <- read_csv("redlight.csv",locale=locale('ko',encoding='euc-kr'))
#sangjae¿¡ ÇÑ±Û¿Ü¿¡ ¹®ÀÚµé ¸ğµÎ Á¦°ÅÇÏ°í id °íÀ¯¹øÈ£ Ãß°¡ÇÑ ÈÄ sangjae_comment¿¡ ÀúÀå
sangjae_comment <- sangjae %>% mutate(comment = str_replace_all(comment,"[^°¡-ÆR]"," "),
                            comment = str_squish(comment),
                            id = row_number())
#unnest_tokensÀÇ token = SimplePos22¸¦ ÅëÇØ 22°³ÀÇ Ç°»ç·Î ±¸ºĞÇÏ°í comment_pos¿¡ ÀúÀå
comment_pos <- sangjae_comment %>% unnest_tokens(input = comment, output = word, token = SimplePos22,drop = F)
#separate_rows¸¦ ÅëÇØ +°¡ ³ª¿Ã¶§¸¶´Ù ÇàÀ» ³ª´²ÁØ´Ù.
comment_pos <- comment_pos %>% separate_rows(word, sep = "[+]")
# ÆÄÀÌ°è¼ö
#\nÀÌ ºÙ¾îÀÖ´Â ´Ü¾î /·Î ½ÃÀÛÇÏ´Â ¸ğµç ¹®ÀÚ¸¦ Á¦°ÅÇÑ´Ù.
noun <- comment_pos %>% filter(str_detect(word, "/n")) %>% mutate(word = str_remove(word,"/.*$"))
#µ¿»ç/pv,Çü¿ë»ç/pa ºÙ¾îÀÖ´Â ´Ü¾î¸¦ ÃßÃâÇÏ¿© ´Ü¾îµÚ¿¡ ÅÂ±×´ë½Å ´Ù¸¦ ºÙ¿©ÁØ´Ù.
pvpa <- comment_pos %>% filter(str_detect(word,"/pv|/pa")) %>% mutate(word = str_replace(word,"/.*$","´Ù"))
#filter(str_count(word)>=2)¸¦ ÅëÇØ µÎ±ÛÀÚ ÀÌ»óÀÇ ´Ü¾î¸¸ ³²±ä´Ù.
comment <- bind_rows(noun, pvpa) %>% filter(str_count(word)>=2) %>% arrange(id)

#add_count()¸¦ ÅëÇØ ¿øÀÚ·á¿¡ ºóµµ¸¦ ³ªÅ¸³½ º¯¼ö¸¦ Ãß°¡ÇÑ´Ù.
#pairwise_count()·Î ÇÑ´Ü¾î¸¦ ±âÁØÀ¸·Î ´Ù¸¥ ¸ğµç´Ü¾îºóµµ¸¦ °è»êÇÑ´Ù.
word_cors <- comment %>% add_count(word) %>% filter(n>=2) %>% pairwise_cor(item = word, feature = id, sort = T)

#correlationÀÌ 0.3ÀÌ»óÀÎ°Íµé¸¸ as_tbl_graph()·Î ³×Æ®¿öÅ© ±×·¡ÇÁµ¥ÀÌÅÍ·Î º¯È¯ÇÏ¿© º¯¼ö¿¡ ÀúÀå
graph_cors <- word_cors %>% filter(correlation >= 0.3) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(),
                                                                                                 group = as.factor(group_infomap()))
set.seed(1234)
#geom_edge_link°¡ °£¼±, geom_node_point°¡ ³ëµå, geom_node_text°¡ ÅØ½ºÆ®, theom_graph() ¹è°æÁ¦°Å
ggraph(graph_cors, layout = "fr") + geom_edge_link(color = "gray50", alpha = 0.5) + geom_node_point(color = "lightcoral",size = 5) +
  geom_node_text(aes(label = name), repel = T, size = 5) + theme_graph()


#¹ÙÀÌ±×·¥
#¸í»ç¿Í, µ¿»ç ±×¸®°í Çü¿ë»ç ¸ğµÎ ÃßÃâÇÏ°í µÎ±ÛÀÚ ÀÌ»ó¸¸ ³²±é´Ï´Ù.
comment_new <- comment_pos %>% filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$","´Ù"),
                       str_remove(word,"/.*$"))) %>%
  filter(str_count(word) >= 2) %>% arrange(id)
# ´ñ±Û ÇÏ³ª°¡ ÇÏ³ªÀÇ ÇàÀÌ µÇµµ·Ï °áÇÕÇÕ´Ï´Ù.
line_comment <- comment_new %>% group_by(id) %>% summarise(sentence = paste(word, collapse = " "))
#unnest_tokens¸¦ ÀÌ¿ëÇÏ¿© ¹ÙÀÌ±×·¥À¸·Î ÅäÅ«È­ÇÕ´Ï´Ù.
bigram_comment <- line_comment %>% unnest_tokens(input = sentence, output = bigram, token = "ngrams", n = 2)
#separate()·Î ¹ÙÀÌ±×·¥ÀÇ µÎ´Ü¾î¸¦ ¼­·Î´Ù¸¥ º¯¼ö·Î ºĞ¸®ÇÕ´Ï´Ù.
bigram_separated <- bigram_comment %>% separate(bigram, c("word1","word2"), sep = " ")
#ÇÑ´Ü¾î·ÎµÈ ¹®ÀåÀº ¹ÙÀÌ±×·¥ ÅäÅ«È­ ÇÏ¸é NA°¡ µÇ°í na.omit()À¸·Î NA¸¦ Á¦°ÅÇÏ°í ´Ü¾î½Ö ºóµµ¸¦ ±¸ÇÑ´Ù.
pair_bigram <- bigram_separated %>% count(word1, word2, sort = T) %>% na.omit()

set.seed(1234)

graph_bigram <- pair_bigram %>% filter(n >= 1) %>% as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),group = as.factor(group_infomap()))

ggraph(graph_bigram, layout = "fr") + geom_edge_link(color = "gray50", alpha = 0.5) + 
  geom_node_point(aes(size = centrality, color = group), show.legend = F) + scale_size(range = c(4,8)) +
  geom_node_text(aes(label = name), repel = T, size = 5) + theme_graph()

