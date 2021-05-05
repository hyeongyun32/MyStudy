library(dplyr)
library(readr)
library(stringr)
library(textclean)
library(tidytext)
library(ggplot2)
library(KoNLP)
library(tidyr)

#dic 변수에 감정사전 저장
dic <- read_csv("knu_sentiment_lexicon.csv")
#sangjae변수에 파일저장
sangjae <- read_csv("redlight.csv",locale=locale('ko',encoding='euc-kr'))
#row_number()로 고유번호 만든 후,str_squish(replace_html(comment)로 특수문자 제거
sangjae_comment <- sangjae %>% mutate(id=row_number(),comment = str_squish(replace_html(comment)))

#sangjae_comment를 words기준으로 토큰화
word_comment <- sangjae_comment %>% unnest_tokens(input = comment, output = word, token = "words",drop = F)
#word기준으로 감정사전을 저장한 dic변수와 left_join으로 합쳐서 감정점수 부여
word_comment <- word_comment %>% left_join(dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity),0,polarity))
#조건문을 이용하여 감정점수가 2점이면 pos, -2점이면 neg -1~1점은 neu 부여
word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg","neu")))

#id와comment를 score에 점수를 계산후에 score_comment변수에 저장
score_comment <- word_comment %>% group_by(id, comment) %>% summarise(score = sum(polarity)) %>% ungroup()
#score_comment에 점수가 1점 이상이면 pos, -1점이하이면 neg, 나머진 neu로 설정
score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos",ifelse(score <= -1, "neg","neu")))

#freq_score에 score_comment$sentiment를 카운터하여 n/sum(n)*100으로 계산
freq_score <- score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
freq_score

#new_dic 변수에 감정사전에 소름, 더럽게 싫은을 2점으로 바꿈
new_dic <- dic %>% mutate(polarity = ifelse(word %in% c("소름","더럽게","싫은"),2,polarity))

#-polarity로 polarity뺴고 모두 수정한 감정사전으로 합쳐서 점수부여
new_word_comment <- word_comment %>% select(-polarity) %>% left_join(new_dic,by="word") %>% mutate(polarity = ifelse(is.na(polarity),0,polarity))

#id와 comment를 마찬가지로 score에 점수를 계산후에 new_score_comment변수에 저장
new_score_comment <- new_word_comment %>% group_by(id, comment) %>% summarise(score = sum(polarity)) %>% ungroup()
#new_score_comment에 점수가 1점 이상이면 pos, -1점이하이면 neg, 나머지는 neu로 설정
new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos",ifelse(score <= -1, "neg","neu")))

#new_freq_score에 new_score_comment$sentiment를 카운터하여 n/sum(n)*100으로 계산
new_freq_score <- new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
new_freq_score
