#라이브러리
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
#sondia.txt파일을 읽어 sondia변수에 저장
sondia = readLines("sondia.txt",encoding = "UTF-8")
#sondia를 티블 구조로 바꿔준 후 mutate로 singer라는 행과 해당행들 son으로 만들어주고, 변수son에 저장
son <- sondia %>% as_tibble() %>% mutate(singer = "son")
#redlight.txt파일을 읽어, sangjae라는 변수에 저장
sangjae = readLines("redlight.txt",encoding = "UTF-8")
#sangjae를 티블구조로 바꾼 후 마찬가지로 singer라는 행과 해당행을 전부 sangjae로 만들고, 변수 sang에 저장
sang <- sangjae %>% as_tibble() %>% mutate(singer = "sangjae")

#bind_rows함수로 행방향으로 결합 후 select으로 보기 좋게 순서를 바꿈
bind_comment <- bind_rows(son, sang) %>% select(singer, value)
#bind_comment에 str_replace_all을 통해 한글이외문자 전부 공백으로 한 후 str_squish로 공백제거
comment_music <- bind_comment %>% mutate(value = str_replace_all(value, "[^가-힣]"," "), value = str_squish(value))
#unnest_tokens를 통해 토근화 하며 extractNoun으로 명사만 추출
comment_music <- comment_music %>% unnest_tokens(input = value, output = word, token = extractNoun)

#count함수로 singer,word 단어 빈도 구하고 filter함수를 통해 word가 1 이상인것만 변수에저장
freq_comment <- comment_music %>% count(singer, word) %>% filter(str_count(word)>1)
#group_by,slice_max함수를 통해 그룹별로 고빈도 단어 10개 추출
top10_music <- freq_comment %>% group_by(singer) %>% slice_max(n, n=10,with_ties = F)

#ggplot 그래프, reorder_within 단어빈도순으로 정렬 singer 기준
#facet_wrap singer기준으로 그래프를 나눔, scale_x_reordered 항목이름제거, labs 제목제거
ggplot(top10_music, aes(x=reorder_within(word, n, singer),y=n,fill=singer)) + geom_col() +
  coord_flip() + facet_wrap(~singer, scales = "free_y") + scale_x_reordered() + labs(x = NULL)

#((각 단어빈도+1)/(전체단어빈도+1))/((각 단어빈도+1)/(전체단어빈도+1))로 오즈비를 계산
#1을 더해주는 이유는 빈도가 0일경우 계산시 0으로 나누게 되어 1을 더해줌
#arrange로 내림차순으로 정렬
freq_wide <- freq_comment %>% pivot_wider(names_from = singer, values_from = n, values_fill = list(n=0))
freq_wide <- freq_wide %>% mutate(ratio_sang = ((sangjae+1)/(sum(sangjae+1))),ratio_son = ((son+1)/(sum(son+1))))
freq_wide <- freq_wide %>% mutate(odds_ratio = ratio_sang/ratio_son)
freq_wide %>% arrange(-odds_ratio)
