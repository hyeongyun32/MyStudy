#¶óÀÌºê·¯¸®
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
#sondia.txtÆÄÀÏÀ» ÀĞ¾î sondiaº¯¼ö¿¡ ÀúÀå
sondia = readLines("sondia.txt",encoding = "UTF-8")
#sondia¸¦ Æ¼ºí ±¸Á¶·Î ¹Ù²ãÁØ ÈÄ mutate·Î singer¶ó´Â Çà°ú ÇØ´çÇàµé sonÀ¸·Î ¸¸µé¾îÁÖ°í, º¯¼öson¿¡ ÀúÀå
son <- sondia %>% as_tibble() %>% mutate(singer = "son")
#redlight.txtÆÄÀÏÀ» ÀĞ¾î, sangjae¶ó´Â º¯¼ö¿¡ ÀúÀå
sangjae = readLines("redlight.txt",encoding = "UTF-8")
#sangjae¸¦ Æ¼ºí±¸Á¶·Î ¹Ù²Û ÈÄ ¸¶Âù°¡Áö·Î singer¶ó´Â Çà°ú ÇØ´çÇàÀ» ÀüºÎ sangjae·Î ¸¸µé°í, º¯¼ö sang¿¡ ÀúÀå
sang <- sangjae %>% as_tibble() %>% mutate(singer = "sangjae")

#bind_rowsÇÔ¼ö·Î Çà¹æÇâÀ¸·Î °áÇÕ ÈÄ selectÀ¸·Î º¸±â ÁÁ°Ô ¼ø¼­¸¦ ¹Ù²Ş
bind_comment <- bind_rows(son, sang) %>% select(singer, value)
#bind_comment¿¡ str_replace_allÀ» ÅëÇØ ÇÑ±ÛÀÌ¿Ü¹®ÀÚ ÀüºÎ °ø¹éÀ¸·Î ÇÑ ÈÄ str_squish·Î °ø¹éÁ¦°Å
comment_music <- bind_comment %>% mutate(value = str_replace_all(value, "[^°¡-ÆR]"," "), value = str_squish(value))
#unnest_tokens¸¦ ÅëÇØ Åä±ÙÈ­ ÇÏ¸ç extractNounÀ¸·Î ¸í»ç¸¸ ÃßÃâ
comment_music <- comment_music %>% unnest_tokens(input = value, output = word, token = extractNoun)

#countÇÔ¼ö·Î singer,word ´Ü¾î ºóµµ ±¸ÇÏ°í filterÇÔ¼ö¸¦ ÅëÇØ word°¡ 1 ÀÌ»óÀÎ°Í¸¸ º¯¼ö¿¡ÀúÀå
freq_comment <- comment_music %>% count(singer, word) %>% filter(str_count(word)>1)
#group_by,slice_maxÇÔ¼ö¸¦ ÅëÇØ ±×·ìº°·Î °íºóµµ ´Ü¾î 10°³ ÃßÃâ
top10_music <- freq_comment %>% group_by(singer) %>% slice_max(n, n=10,with_ties = F)

#ggplot ±×·¡ÇÁ, reorder_within ´Ü¾îºóµµ¼øÀ¸·Î Á¤·Ä singer ±âÁØ
#facet_wrap singer±âÁØÀ¸·Î ±×·¡ÇÁ¸¦ ³ª´®, scale_x_reordered Ç×¸ñÀÌ¸§Á¦°Å, labs Á¦¸ñÁ¦°Å
ggplot(top10_music, aes(x=reorder_within(word, n, singer),y=n,fill=singer)) + geom_col() +
  coord_flip() + facet_wrap(~singer, scales = "free_y") + scale_x_reordered() + labs(x = NULL)

#((°¢ ´Ü¾îºóµµ+1)/(ÀüÃ¼´Ü¾îºóµµ+1))/((°¢ ´Ü¾îºóµµ+1)/(ÀüÃ¼´Ü¾îºóµµ+1))·Î ¿ÀÁîºñ¸¦ °è»ê
#1À» ´õÇØÁÖ´Â ÀÌÀ¯´Â ºóµµ°¡ 0ÀÏ°æ¿ì °è»ê½Ã 0À¸·Î ³ª´©°Ô µÇ¾î 1À» ´õÇØÁÜ
#arrange·Î ³»¸²Â÷¼øÀ¸·Î Á¤·Ä
freq_wide <- freq_comment %>% pivot_wider(names_from = singer, values_from = n, values_fill = list(n=0))
freq_wide <- freq_wide %>% mutate(ratio_sang = ((sangjae+1)/(sum(sangjae+1))),ratio_son = ((son+1)/(sum(son+1))))
freq_wide <- freq_wide %>% mutate(odds_ratio = ratio_sang/ratio_son)
freq_wide %>% arrange(-odds_ratio)
