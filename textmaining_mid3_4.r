library(dplyr)
library(readr)
library(stringr)
library(textclean)
library(tidytext)
library(ggplot2)
library(KoNLP)
library(tidyr)

#dic ������ �������� ����
dic <- read_csv("knu_sentiment_lexicon.csv")
#sangjae������ ��������
sangjae <- read_csv("redlight.csv",locale=locale('ko',encoding='euc-kr'))
#row_number()�� ������ȣ ���� ��,str_squish(replace_html(comment)�� Ư������ ����
sangjae_comment <- sangjae %>% mutate(id=row_number(),comment = str_squish(replace_html(comment)))

#sangjae_comment�� words�������� ��ūȭ
word_comment <- sangjae_comment %>% unnest_tokens(input = comment, output = word, token = "words",drop = F)
#word�������� ���������� ������ dic������ left_join���� ���ļ� �������� �ο�
word_comment <- word_comment %>% left_join(dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity),0,polarity))
#���ǹ��� �̿��Ͽ� ���������� 2���̸� pos, -2���̸� neg -1~1���� neu �ο�
word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg","neu")))

#id��comment�� score�� ������ ����Ŀ� score_comment������ ����
score_comment <- word_comment %>% group_by(id, comment) %>% summarise(score = sum(polarity)) %>% ungroup()
#score_comment�� ������ 1�� �̻��̸� pos, -1�������̸� neg, ������ neu�� ����
score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos",ifelse(score <= -1, "neg","neu")))

#freq_score�� score_comment$sentiment�� ī�����Ͽ� n/sum(n)*100���� ���
freq_score <- score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
freq_score

#new_dic ������ ���������� �Ҹ�, ������ ������ 2������ �ٲ�
new_dic <- dic %>% mutate(polarity = ifelse(word %in% c("�Ҹ�","������","����"),2,polarity))

#-polarity�� polarity���� ��� ������ ������������ ���ļ� �����ο�
new_word_comment <- word_comment %>% select(-polarity) %>% left_join(new_dic,by="word") %>% mutate(polarity = ifelse(is.na(polarity),0,polarity))

#id�� comment�� ���������� score�� ������ ����Ŀ� new_score_comment������ ����
new_score_comment <- new_word_comment %>% group_by(id, comment) %>% summarise(score = sum(polarity)) %>% ungroup()
#new_score_comment�� ������ 1�� �̻��̸� pos, -1�������̸� neg, �������� neu�� ����
new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos",ifelse(score <= -1, "neg","neu")))

#new_freq_score�� new_score_comment$sentiment�� ī�����Ͽ� n/sum(n)*100���� ���
new_freq_score <- new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
new_freq_score
