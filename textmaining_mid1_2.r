#���̺귯��
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
#sondia.txt������ �о� sondia������ ����
sondia = readLines("sondia.txt",encoding = "UTF-8")
#sondia�� Ƽ�� ������ �ٲ��� �� mutate�� singer��� ��� �ش���� son���� ������ְ�, ����son�� ����
son <- sondia %>% as_tibble() %>% mutate(singer = "son")
#redlight.txt������ �о�, sangjae��� ������ ����
sangjae = readLines("redlight.txt",encoding = "UTF-8")
#sangjae�� Ƽ������ �ٲ� �� ���������� singer��� ��� �ش����� ���� sangjae�� �����, ���� sang�� ����
sang <- sangjae %>% as_tibble() %>% mutate(singer = "sangjae")

#bind_rows�Լ��� ��������� ���� �� select���� ���� ���� ������ �ٲ�
bind_comment <- bind_rows(son, sang) %>% select(singer, value)
#bind_comment�� str_replace_all�� ���� �ѱ��ܹ̿��� ���� �������� �� �� str_squish�� ��������
comment_music <- bind_comment %>% mutate(value = str_replace_all(value, "[^��-�R]"," "), value = str_squish(value))
#unnest_tokens�� ���� ���ȭ �ϸ� extractNoun���� ��縸 ����
comment_music <- comment_music %>% unnest_tokens(input = value, output = word, token = extractNoun)

#count�Լ��� singer,word �ܾ� �� ���ϰ� filter�Լ��� ���� word�� 1 �̻��ΰ͸� ����������
freq_comment <- comment_music %>% count(singer, word) %>% filter(str_count(word)>1)
#group_by,slice_max�Լ��� ���� �׷캰�� ��� �ܾ� 10�� ����
top10_music <- freq_comment %>% group_by(singer) %>% slice_max(n, n=10,with_ties = F)

#ggplot �׷���, reorder_within �ܾ�󵵼����� ���� singer ����
#facet_wrap singer�������� �׷����� ����, scale_x_reordered �׸��̸�����, labs ��������
ggplot(top10_music, aes(x=reorder_within(word, n, singer),y=n,fill=singer)) + geom_col() +
  coord_flip() + facet_wrap(~singer, scales = "free_y") + scale_x_reordered() + labs(x = NULL)

#((�� �ܾ��+1)/(��ü�ܾ��+1))/((�� �ܾ��+1)/(��ü�ܾ��+1))�� ����� ���
#1�� �����ִ� ������ �󵵰� 0�ϰ�� ���� 0���� ������ �Ǿ� 1�� ������
#arrange�� ������������ ����
freq_wide <- freq_comment %>% pivot_wider(names_from = singer, values_from = n, values_fill = list(n=0))
freq_wide <- freq_wide %>% mutate(ratio_sang = ((sangjae+1)/(sum(sangjae+1))),ratio_son = ((son+1)/(sum(son+1))))
freq_wide <- freq_wide %>% mutate(odds_ratio = ratio_sang/ratio_son)
freq_wide %>% arrange(-odds_ratio)
