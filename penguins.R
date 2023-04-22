install.packages(c("tidyverse", "palmerpenguins"))
library(tidyverse)
library(palmerpenguins)

head(penguins)


# 단순계산
15+23
1+(21*100)/4
2^4
sqrt(16)

# TRUE, FALSE
1==2
1!=2
TRUE+FALSE
T+T
F+F

# 벡터
vec <- c(100,200,300,400,500)
vec
vec[1]
vec[c(2:3)]
vec[c(1,3)]

names(vec)
names(vec) <- c('a','b','c','d','e')
vec
unname(vec)
unname(vec)[1]

x <- c(100,200,NA,400,500)
mean(x)
mean(x, na.rm = T)
sd(x, na.rm = T)
var(x, na.rm = T)

which(x>200)
x[which(x>200)]


# 데이터프레임

df <- iris
str(df)
df[,1]
df$Sepal.Length
df[,c(1:2)]
df[,c(1,3)]
df[1,]
df[c(1:5),]

str(df)
df$tmp1 <- 1
str(df)
nrow(df)
ncol(df)
summary(df)
df[c(1:75),]$tmp1 <- 1
df[c(76:150),]$tmp1 <- 0
df$tmp_1 <- as.factor(df$tmp1)
str(df)

summary(df)
df$tmp2 <- ifelse(df$Sepal.Width > 3, 'Y','N')
str(df)
df$tmp2 <- as.factor(df$tmp2)
df$tmp3 <- as.factor(ifelse(df$Sepal.Width < 2.3 , "S", 
                            ifelse(df$Sepal.Width < 3 , "M", "L")))
summary(df)



str(df)
df[,1]
df$Sepal.Length
df[,c(1:2)]
df[,c(1,3)]
df[1,]
df[c(1:5),]
df$tmp_1 <- as.factor(df$tmp1)



# data handling
library(dplyr)
df <- df

df %>% filter(Species == 'setosa')
df %>% filter(Species == 'setosa' & Sepal.Length > 5.4)
df %>% filter(Species == 'setosa' | Sepal.Length > 5.4)
df %>% filter(Species == 'setosa' & Sepal.Length > 5.4) %>% select(c('Species','Sepal.Length'))
df %>% select(-'Species')
df %>% group_by(Species) %>% summarize(avg=mean(Sepal.Width))
df %>% group_by(Species) %>% summarize(avg=mean(Sepal.Width), max = max(Sepal.Width))
df %>% filter(Species == 'setosa') %>% arrange(Sepal.Width)
df %>% filter(Species == 'setosa') %>% arrange(Sepal.Width, desc(Sepal.Length))
df %>% slice(c(1:3))


# mean, max, etc.
mean(df[,1])
mean(df[,1], na.rm = T)
max(df[,1])
min(df[,1])
sd(df[,1])
var(df[,1])


# NA
df <- airquality
str(df)
summary(df)

colSums(df)
is.na(df)
colSums(is.na(df))


#rbind, cbind
df <- df
str(df)
df1 <- df %>% slice(c(1:75))
df2 <- df %>% slice(c(76:150))
rbind(df1, df2)

df1 <- df %>% select(c(Sepal.Length,Sepal.Width))
df2 <- df %>% select(c(Petal.Length,Petal.Width))
df3 <- df %>% select('Species')

df <- cbind(df1,df2,df3)
str(df)

df <- read.csv('')
write.csv('')

################################# 코딩예제제
library(tidyverse)
library(palmerpenguins)

#Q1
df <- penguins
colSums(is.na(df))
#Q2
penguin <- penguins %>%  rename( bill_length = bill_length_mm,
                                 bill_depth = bill_depth_mm,
                                 flipper_length = flipper_length_mm)
head(penguin)

#Q3
penguin %>%
  filter(species =="Adelie") %>%
  summarise("부리길이"=mean(bill_length, na.rm=T))
#Q4
penguin %>%
  group_by(species) %>%
  summarise("부리길이"=mean(bill_length, na.rm=T))
#Q5
penguin %>%
  count(species)
#Q6
penguin %>%
  select(species, bill_length, bill_depth) %>%
  head()
#Q7
penguin %>% slice(5:10)

#Q8
penguin %>%
  mutate(bill_ratio=bill_length/bill_depth) %>%
  head()
#Q9
penguin %>%
  mutate(bill_ratio=bill_length/bill_depth) %>%
  na.omit() %>%
  head()


### 시각화 ###
#STEP1
penguins %>%
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g))
#STEP2
penguins %>%
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g)) +
  geom_point(aes(color = species,
                 shape = species),
             size = 3, alpha = 0.8)

#STEP3
ggplot(data = penguins,
       aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point(aes(color = species, shape = species),
             size = 3, alpha = 0.8) +
  scale_color_brewer(palette="Dark2")

#STEP4
penguins %>%
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g)) +
  geom_point(aes(color = species, shape = species),
             size = 3,alpha = 0.8) +
  scale_color_brewer(palette="Dark2") +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species")

#STEP5
penguins %>%
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g)) +
  geom_point(aes(color = species, shape = species),
             size = 3,alpha = 0.8) +
  scale_color_brewer(palette="Dark2") +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species")+
  geom_smooth(method = "lm", se=F,aes(color = species))

#STEP6
penguins %>%
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g)) +
  geom_point(aes(color = species, shape = species),
             size = 3,alpha = 0.8) +
  scale_color_brewer(palette="Dark2") +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species")+
  facet_wrap(~species)


#STEP7
penguins %>% ggplot(aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
  geom_jitter(aes(color = species), alpha = 0.5, show.legend = FALSE,
              position = position_jitter(width = 0.2, seed = 0))+
  scale_fill_brewer(palette="Dark2")+
  labs(title = "Box plot of Penguins",
       subtitle = "Flipper length and body mass for Penguins",
       x = "Species",
       y = "flipper_length (mm)")

#STEP8
penguins %>% ggplot(aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), alpha = 0.5,
                 position = "identity") +
  scale_color_brewer(palette="Dark2") +
  labs(x = "Flipper length (mm)",
       y = "Frequency",
       title = "Penguin flipper lengths")


#Photo
library(tidyverse)

library(patchwork)

library(showtext)

showtext_auto()

cd <- read.csv("./data/cd.csv")

head(cd)

str(cd)

cd %>% mutate_if(is.character, as.factor) %>%
  
  ggplot(aes(x=exp, y=cd, col=as.factor(dev)))+geom_point()+
  
  facet_wrap(material~Tpr, nrow=2, labeller = label_both)+
  
  theme(strip.text.x = element_text(size = 15))+
  
  theme(text = element_text(size = 15))




p1 <- cd %>% ggplot(aes(x=exp, y=cd))+               #x,y 변수 지정

 geom_boxplot(aes(fill=material),alpha=0.5)+         #boxplot 그래프 설정, 재료로 색깔 나누기, 투명도 설정

 geom_hline(yintercept=40, linetype='dashed', color='red', linewidth=1)+ #target 두께 수평선 표시

 facet_wrap(Tpr ~ dev, ncol=3, labeller = label_context) + #Tpr, dev 으로 그래프 배열하고, 그래프 제목은 변수 내용으로 표시하기

 ggtitle("Box Plot of CD")+                                # 제목 달기

 xlab('exp[mJ]') +                                         # x축 이름

 ylab('CD[um]') +                                          # y축 이름

 theme_bw()+                                               # 배경 테마 지정

 theme(title = element_text(size=20),                      # 제목 글자 크기

      axis.title=element_text(size=20),                    # x,y 축 라벨 크기

      axis.text.y = element_text(size=15),                 # y축 숫자 크기

      axis.text.x = element_text(size=15),                 # x축 숫자 크기

      legend.text=element_text(size=12))+                  # 범례 글자 크기

 theme(strip.text = element_text(size = 15, colour = "blue"))  # 가로/세로 그래프 제목 글자 크기
p1



tk <- read.csv("./data/tk.csv")

p2 <- tk %>% ggplot(aes(x=exp, y=Tk))+
  
  geom_boxplot(aes(fill=material), alpha=0.5)+
  
  geom_hline(yintercept=2.0, linetype='dashed', color='red', linewidth=1)+
  
  facet_wrap(Tpr ~ dev, ncol=3, labeller = label_context) +
  
  ggtitle("Box Plot of Tk")+
  
  xlab('exp[mJ]') +
  
  ylab('Tk[um]') +
  
  theme_bw()+
  
  theme(axis.title=element_text(size=20),
        
        title = element_text(size=20),
        
        axis.text.y = element_text(size=15),
        
        axis.text.x = element_text(size=15),
        
        legend.text=element_text(size=12))+
  
  theme(strip.text = element_text(size = 15, colour = "blue"))

p2



p1+p2

ggsave("pdl_test.png")


## Dry

dry <- read.csv("./data/dry_etch.csv")
dry %>% group_by(recipe) %>%
  summarise(avt=mean(cd),
            uniormity=(max(cd)-min(cd))/(2*mean(cd)))


dry <- read.csv("./data/dry_etch.csv")
summary <-
  dry %>%
  group_by(recipe) %>%
  summarise(avg=mean(cd),
            stdev = sd(cd),
            uniormity=(max(cd)-min(cd))/(2*mean(cd)))
summary


p1<- dry %>%
  ggplot(aes(x=recipe, y=cd))+
  geom_boxplot(aes(fill=recipe))+
  geom_hline(yintercept=3.5,
             linetype='dashed',
             color='red', size=1)

p1
p1+
  geom_text(data = summary,
            aes(label = paste0("avg=", round(avg,1)),
                y=avg,nudge_y = -.2))


p2<- dry %>% ggplot(aes(x=recipe, y=cd))+geom_violin(aes(fill=recipe))+
  geom_hline(yintercept=3.5, linetype='dashed', color='red', size=1)+
  geom_jitter(width=0.2, aes(alpha=0.5))

p2


p3<- dry %>%
  ggplot(aes(x=cd)) +
  geom_histogram(binwidth=0.2,
                 aes(fill=recipe))+
  facet_wrap(recipe ~., ncol=5)

p3


p4<- dry %>%
  ggplot(aes(x=cd)) +
  geom_density(aes(col=recipe))+
  facet_wrap(recipe ~., ncol=5)

p4

p5<- dry%>%
  ggplot(aes(x1,y1) ) +
  geom_raster(aes(fill = cd)) +
  scale_fill_continuous(low = "#BFE1B0", high = "#137177")+
  facet_grid(. ~ recipe, labeller = label_value)

p5
