#라이브러리 설치
library(tidyverse)
library(dplyr)
library(patchwork)
library(showtext)
library(tidyquant)
showtext_auto()


rm(list=ls())
plot.new()
frame()
options(warn=-1) # 경고메세지 무시하기

df <- tibble::tribble(
  ~희석비율, ~측정횟수, ~`0mm`, ~`30mm`, ~`60mm`, ~`90mm`, ~`120mm`, ~`150mm`, ~`180mm`, ~`210mm`, ~`240mm`, ~`270mm`, ~`300mm`,
  "15%",    1L,   26.5,    24.5,      24,    24.5,       25,       26,     25.5,     26.5,       25,     25.5,       26,
  NA,    2L,   26.5,    25.5,      25,      25,       25,     25.5,     25.5,     25.5,       26,     25.5,     26.5,
  NA,    3L,   25.5,      25,      25,    25.5,       26,       25,       25,     25.5,       26,       26,     25.5,
  "30%",    1L,     19,      NA,      21,      14,       NA,     16.5,       NA,       16,       17,       NA,       18,
  NA,    2L,     19,      NA,    21.5,      19,       NA,     18.5,       NA,     18.5,       20,       NA,     20.5,
  NA,    3L,     20,      NA,    20.5,    21.5,       NA,     20.5,       NA,       20,       20,       NA,       21,
  "45%",    1L,   14.5,    13.5,    13.5,      13,       13,     12.5,     13.5,       13,     12.5,     12.5,     12.5,
  NA,    2L,   13.5,    13.5,    13.5,      13,       13,     13.5,       13,       12,     12.5,       13,       13,
  NA,    3L,   13.5,    13.5,      13,    12.5,       13,       13,     12.5,     12.5,     12.5,       13,     13.5,
  "60%",    1L,    9.5,      NA,     9.5,     9.5,       NA,      9.5,       NA,      9.5,      9.5,       NA,      9.5,
  NA,    2L,    9.5,      NA,      10,      10,       NA,        9,       NA,     10.5,     10.5,       NA,     10.5,
  NA,    3L,   10.5,      NA,      11,    10.5,       NA,       10,       NA,       11,       11,       NA,       11
) %>% fill(희석비율, .direction = "down")

library(gt)
clipr::write_clip(df)
df %>% gt()

means <-df %>% fill(희석비율, .direction = "down") %>% pivot_longer(cols = `0mm`:`300mm`, names_to = "position", values_to = "Hardness") %>% na.omit() %>%
  group_by(희석비율) %>% summarise(avg = mean(Hardness))

means %>% 
  ggplot(aes(x=희석비율, y=avg, fill=(희석비율))) + 
  geom_col()+
  geom_text(aes(label = round(avg)), vjust = -0.5)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(col = "희석비율(%)",
       title = "희석 비율에 따른  Si-pad  경도", 
       y="Shore 평균 경도",
       x= "희석 비율(%)")


df %>% fill(희석비율, .direction = "down") %>% pivot_longer(cols = `0mm`:`300mm`, names_to = "position", values_to = "Hardness") %>% na.omit() %>% 
  ggplot(aes(x=희석비율, y=Hardness , fill= 희석비율))+geom_boxplot()+
  labs(col = "희석비율(%)",
       title = "희석 비율에 따른  Si-pad  경도", 
       y="Shore 경도",
       x= "희석 비율(%)")+
  scale_fill_tq()+
  scale_color_tq()+
  theme_tq()+
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE) + 
  geom_text(data = means, aes(label = paste0("avg=",round(avg)), y = avg + 2))


df %>% fill(희석비율, .direction = "down") %>% pivot_longer(cols = `0mm`:`300mm`, names_to = "position", values_to = "Hardness") %>% na.omit() %>% 
  mutate(희석비율 = as.numeric(gsub("%", "", 희석비율))) %>% 
  ggplot(aes(x=희석비율, y=Hardness ))+geom_point()+geom_smooth(method='lm')+
  labs(col = "희석비율(%)",
       title = "희석 비율에 따른  Si-pad  경도", 
       y="Shore 경도",
       x= "희석 비율(%)")+
  scale_fill_tq()+
  scale_color_tq()+
  theme_tq()



df %>% fill(희석비율, .direction = "down") %>% pivot_longer(cols = `0mm`:`300mm`, names_to = "position", values_to = "Hardness") %>% na.omit() %>% 
  mutate(희석비율 = as.numeric(gsub("%", "", 희석비율))) %>% 
  mutate(position = as.numeric(gsub("mm", "", position))) %>% 
  ggplot(aes(x=position, y=Hardness, col=as.factor(희석비율) ))+geom_point()+geom_smooth()+
  labs(col = "희석비율(%)",
       title = "위치에 따른 Si-pad  경도", 
       y="Shore 경도",
       x= "x position (mm)")+
  scale_fill_tq()+
  scale_color_tq()+
  theme_tq()


