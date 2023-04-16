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
df
df <- df
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

# merge
id_name <- data.frame(id = c("s1","s2","s3","s4","s5", "s6", "s7"), 
                      last_name = c("David", "Jane", "Choi", "Park", "Kim" , "Lee", "Jane"))
id_number <- data.frame(id = c("s3", "s4", "s5", "s6", "s7", "s8", "s9"), 
                        number = c(1, 4, 5, 0, 1, 8, 5))
id_df <- merge(id_name, id_number, by = 'id')
id_df
id_df1 <- merge(id_name, id_number, by = 'id', all.x = T) # all.y = T, all = T
id_df1

# mean, max, etc.
mean(df[,1])
mean(df[,1], na.rm = T)
max(df[,1])
min(df[,1])
sd(df[,1])
var(df[,1])


# preProcess
library(caret)
df_sc <- preProcess(df, method = c("center","scale"))
df_sc_df <- predict(df_sc, df)
head(df_sc_df)

df_sd <- preProcess(df, method = c('range'))
df_sd_df <- predict(df_sd, df)
head(df_sd_df)


# NA
df <- airquality
str(df)
summary(df)

colSums(df)
is.na(df)
colSums(is.na(df))

df1 <- df %>% filter(!is.na(Ozone))
summary(df1)
df$new1 <- ifelse(is.na(df$Ozone), mean(df$Ozone, na.rm = T), df$Ozone)
summary(df)

imp <- preProcess(df, method = 'bagImpute')
df2 <- predict(imp, df)
summary(df2)

# outlier
summary(df2)
fivenum(df2[,1])

low <- fivenum(df2[,1])[2] - 1.5*IQR(df2[,1])
upp <- fivenum(df2[,1])[4] + 1.5*IQR(df2[,1])

fivenum(df2[,1])
df2[,1] <- ifelse(df2[,1] > upp, mean(df2[,1], na.rm = T), df2[,1])
fivenum(df2[,1])



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
