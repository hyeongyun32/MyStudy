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

#redlight.csv�� sangjae�� ����
sangjae <- read_csv("redlight.csv",locale=locale('ko',encoding='euc-kr'))
#sangjae�� �ѱۿܿ� ���ڵ� ��� �����ϰ� id ������ȣ �߰��� �� sangjae_comment�� ����
sangjae_comment <- sangjae %>% mutate(comment = str_replace_all(comment,"[^��-�R]"," "),
                            comment = str_squish(comment),
                            id = row_number())
#unnest_tokens�� token = SimplePos22�� ���� 22���� ǰ��� �����ϰ� comment_pos�� ����
comment_pos <- sangjae_comment %>% unnest_tokens(input = comment, output = word, token = SimplePos22,drop = F)
#separate_rows�� ���� +�� ���ö����� ���� �����ش�.
comment_pos <- comment_pos %>% separate_rows(word, sep = "[+]")
# ���̰��
#\n�� �پ��ִ� �ܾ� /�� �����ϴ� ��� ���ڸ� �����Ѵ�.
noun <- comment_pos %>% filter(str_detect(word, "/n")) %>% mutate(word = str_remove(word,"/.*$"))
#����/pv,�����/pa �پ��ִ� �ܾ �����Ͽ� �ܾ�ڿ� �±״�� �ٸ� �ٿ��ش�.
pvpa <- comment_pos %>% filter(str_detect(word,"/pv|/pa")) %>% mutate(word = str_replace(word,"/.*$","��"))
#filter(str_count(word)>=2)�� ���� �α��� �̻��� �ܾ �����.
comment <- bind_rows(noun, pvpa) %>% filter(str_count(word)>=2) %>% arrange(id)

#add_count()�� ���� ���ڷῡ �󵵸� ��Ÿ�� ������ �߰��Ѵ�.
#pairwise_count()�� �Ѵܾ �������� �ٸ� ���ܾ�󵵸� ����Ѵ�.
word_cors <- comment %>% add_count(word) %>% filter(n>=2) %>% pairwise_cor(item = word, feature = id, sort = T)

#correlation�� 0.3�̻��ΰ͵鸸 as_tbl_graph()�� ��Ʈ��ũ �׷��������ͷ� ��ȯ�Ͽ� ������ ����
graph_cors <- word_cors %>% filter(correlation >= 0.3) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(),
                                                                                                 group = as.factor(group_infomap()))
set.seed(1234)
#geom_edge_link�� ����, geom_node_point�� ���, geom_node_text�� �ؽ�Ʈ, theom_graph() �������
ggraph(graph_cors, layout = "fr") + geom_edge_link(color = "gray50", alpha = 0.5) + geom_node_point(color = "lightcoral",size = 5) +
  geom_node_text(aes(label = name), repel = T, size = 5) + theme_graph()


#���̱׷�
#����, ���� �׸��� ����� ��� �����ϰ� �α��� �̻� ����ϴ�.
comment_new <- comment_pos %>% filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$","��"),
                       str_remove(word,"/.*$"))) %>%
  filter(str_count(word) >= 2) %>% arrange(id)
# ��� �ϳ��� �ϳ��� ���� �ǵ��� �����մϴ�.
line_comment <- comment_new %>% group_by(id) %>% summarise(sentence = paste(word, collapse = " "))
#unnest_tokens�� �̿��Ͽ� ���̱׷����� ��ūȭ�մϴ�.
bigram_comment <- line_comment %>% unnest_tokens(input = sentence, output = bigram, token = "ngrams", n = 2)
#separate()�� ���̱׷��� �δܾ ���δٸ� ������ �и��մϴ�.
bigram_separated <- bigram_comment %>% separate(bigram, c("word1","word2"), sep = " ")
#�Ѵܾ�ε� ������ ���̱׷� ��ūȭ �ϸ� NA�� �ǰ� na.omit()���� NA�� �����ϰ� �ܾ�� �󵵸� ���Ѵ�.
pair_bigram <- bigram_separated %>% count(word1, word2, sort = T) %>% na.omit()

set.seed(1234)

graph_bigram <- pair_bigram %>% filter(n >= 1) %>% as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),group = as.factor(group_infomap()))

ggraph(graph_bigram, layout = "fr") + geom_edge_link(color = "gray50", alpha = 0.5) + 
  geom_node_point(aes(size = centrality, color = group), show.legend = F) + scale_size(range = c(4,8)) +
  geom_node_text(aes(label = name), repel = T, size = 5) + theme_graph()

