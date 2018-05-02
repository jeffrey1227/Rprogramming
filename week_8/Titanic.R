library('gridExtra')
library('ggplot2') 
library('ggthemes')
library('scales')
library('dplyr')
library('mice') 
library('randomForest')


# 讀檔（訓練集及測試集）
train.data <- read.csv("titanicTrain.csv", stringsAsFactors = F, na.strings = c(""))
test.data <- read.csv("titanicQuestion.csv", stringsAsFactors = F, na.strings = c(""))
train.data <- train.data[1:1000, ]
test.data <- test.data[1:309, ]
full <- rbind(train.data, test.data)
str(full)


full$survived <- as.factor(full$survived)
full$pclass <- as.factor(full$pclass)
full$sex <- as.factor(full$sex)

train <- full[1:1000,]
# 生還人數
ggplot(train, aes(x = survived, fill = survived)) +
  geom_bar(stat = 'count') +
  labs(x = 'Survival') +
  geom_label(stat = 'count',aes(label = ..count..), size = 6) +
  theme_grey(base_size = 16) +
  coord_polar("x", start=0)

# 男女小孩生還比較
MF <- ggplot(train, aes(x = sex, fill = survived)) +
  geom_bar(stat = 'count') +
  labs(x = 'Survival of Male and Female') +
#  geom_label(stat = 'count',aes(label = ..count..), size = 7) +
  theme_grey(base_size = 16) +
  coord_polar("x", start=0)

kids.survival <- train[(!is.na(train$age)) & (train$age < 10), ]
KS <- ggplot(kids.survival, aes(x = survived, fill = survived)) +
  geom_bar(stat = 'count') +
  labs(x = 'Survival of Kids aged under 10') +
#  geom_label(stat = 'count',aes(label = ..count..), size = 7) +
  theme_grey(base_size = 16) +
  coord_polar("x", start=0)


#多圖組合
grid.arrange(MF,KS, nrow = 1)
#可以看出男人的生還率最低，女人及小孩的生還率較高


#依照座艙等級分類
ggplot(train, aes(x = pclass, fill = survived)) +
  geom_bar(stat = 'count') +
  labs(x = 'Survival of Different Class Levels') +
#  geom_label(stat = 'count',aes(label = ..count..), size = 7) +
  theme_grey(base_size = 16) +
  coord_polar("x", start=0)
# 可以發現座艙等級越高，生還率越高



# 上船
full[!is.na(full$boat), "onboat"] <- "1"
full[is.na(full$boat), "onboat"] <- "0"


ggplot(full, aes(x = onboat, fill = onboat)) +
  geom_bar(stat = 'count') +
  labs(x = '\nON or NOT onboat') +
  theme_grey(base_size = 16) +
  coord_polar("x", start=0)

ggplot(full[1:1000,], aes(x = onboat, fill = survived)) +
  geom_bar(stat = 'count') +
  labs(x = '\nSurvival of ON or NOT onboat') +
  theme_grey(base_size = 16) 
#  coord_polar("x", start=0)
# 上船者有很大的機率逃過一劫，反之則難逃一劫


# ----------分析上船者大多是什麼人----------#
MF.OB <- ggplot(full, aes(x = onboat, fill = sex)) +
  geom_bar(stat = 'count') +
  labs(x = 'Sex of those who were and weren\'t Onboat') +
  theme_grey(base_size = 16) +
  coord_polar("x", start=0)
MF.OB
# 上船者有一大部分是女人，沒上船的有一大部分是男人

CL.OB <- ggplot(full, aes(x = onboat, fill = pclass)) +
  geom_bar(stat = 'count') +
  labs(x = 'Sex of those who were and weren\'t Onboat') +
  theme_grey(base_size = 16) +
  coord_polar("x", start=0)
CL.OB
# 上船者有一大部分是座艙等級最高的，沒上船的有一半是座艙等級最低的


full$age <- floor(full$age)
p3 <- ggplot(full[!is.na(full$age) & (full$age <= 10),], aes(x = onboat, fill = onboat)) +
  geom_bar(stat = 'count') +
  labs(x = 'Kids ON or NOT onboat') +
  theme_grey(base_size = 16) +
  coord_polar("x", start=0)
# 小孩大多有上船

p4 <- ggplot(full[!is.na(full$age) & (full$age >= 65),], aes(x = onboat, fill = onboat)) +
  geom_bar(stat = 'count') +
  labs(x = 'Elders ON or NOT onboat') +
  theme_grey(base_size = 16) +
  coord_polar("x", start=0)
# 老人大多沒上船

p5 <- ggplot(full[!is.na(full$age) & (full$age < 65) & (full$age > 10),], aes(x = onboat, fill = onboat)) +
  geom_bar(stat = 'count') +
  labs(x = 'Others ON or NOT onboat') +
  theme_grey(base_size = 16) +
  coord_polar("x", start=0)


grid.arrange(p3, p4, p5, nrow = 1)


#---------------- 觀察姓名變量 ----------------#
# 從乘客名字中提取頭銜 
full$title <- gsub('(.*, )|(\\..*)', '', full$name) 
# 查看按照性別劃分的頭銜數量
table(full$sex, full$title)
# 對於那些出現頻率較低的頭銜合併為一類
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer', 'Mme')
full$title[full$title == 'Mlle']<- 'Miss' 
full$title[full$title == 'Ms'] <- 'Miss'
full$title[full$title %in% rare_title] <- 'Rare Title'
table(full$sex, full$title)

ggplot(full[1:1000,]) + 
  geom_histogram(aes(x = title, fill = survived), stat="count")
# 可得知女性生還率較高

# 最後從乘客姓名中，提取姓氏 
full$surname <- sapply(full$name, function(x) strsplit(x, split = '[,.]')[[1]][1])



#---------------- 觀察家庭變量 ----------------#
full$familycount <- full$sibsp + full$parch + 1

ggplot(full[1:1000, ], aes(x = familycount, fill = survived)) + 
  geom_bar(stat='count', position='dodge') + 
  scale_x_continuous(breaks=c(1:8)) + labs(x = 'Family Size') + 
  theme_gray()
# 一個人的罹難率高，家庭人數二到四（小家庭）的生存率最高，家庭人數四個以上（大家庭）生存率低

full$familysize[full$familycount == 1] <- 'single' 
full$familysize[full$familycount < 5 & full$familycount > 1]<- 'small' 
full$familysize[full$familycount > 4] <- 'large'


ggplot(full[1:1000, ], aes(x = familysize, fill = survived)) +
  geom_bar(stat = 'count', position = 'fill') +
  labs(x = 'Survival of Different Family Size') +
  theme_grey(base_size = 16)

#full$familyID <- paste(as.character(full$familysize), as.character(full$surname), sep = "")
#full[as.numeric(full$familycount) <= 1, "familyID"] <- 'Small'
#full$familyID <- as.factor(full$familyID)

#-----------填補缺失值----------#
sum(is.na(full$embarked))
full[is.na(full$embarked),]
#169, 285

# 估計對於有相同艙位等級（passenger class）和票價（Fare）的乘客也許有著相同的登船港口位置embarkment
ggplot(full, aes(x = embarked, y = fare, fill = factor(pclass))) + 
  geom_boxplot() + 
  geom_hline(aes(yintercept=80), colour='red', linetype='dashed', lwd=2) + 
  scale_y_continuous(labels=dollar_format()) + 
  theme_gray()

# 港口Ｃ的票價的中位數為80，因此我們可以把乘客169, 285的出發港口缺失值替換為‘Ｃ’
full[c(169, 285),"embarked"] <- 'C'

sum(is.na(full$fare))
# 有一個缺失值
full[is.na(full$fare),]
# 1226

ggplot(full[full$pclass == '3' & full$embarked == 'S', ], aes(x = fare)) + 
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(fare, na.rm=T)),colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) + 
  theme_few()

median(full[!is.na(full$fare) & (full$embarked == 'S') & (full$pclass == '3'), "fare"])

full[1226,"fare"] <- median(full[!is.na(full$fare) & (full$embarked == 'S') & (full$pclass == '3'), "fare"])

# 年齡缺失值
sum(is.na(full$age))

# 使因子變量因子化 
factor_vars <- c('pclass', 'sex', 'embarked', 'title', 'surname', 'familysize') 
full[factor_vars] <- lapply(full[factor_vars],function(x) as.factor(x)) 
# 設置隨機種子 
set.seed(2018)  
# 執行多重插補法，剔除一些沒什麼用的變量: 
mice.age <- mice(full[, !names(full) %in% c('name', 'ticket', 'cabin', 'surname', 'survived', 'home.dest', 'body')], method='rf') 
# 保存完整輸出 
mice.output <- complete(mice.age)

# 繪製年齡分布圖 
par(mfrow=c(1,2)) 
hist(full$age, freq=F, main='Age: Original Data', col='orange', ylim=c(0,0.04)) 
hist(mice.output$age, freq=F, main='Age: MICE Output', col='red', ylim=c(0,0.04))

# 對原年齡數據進行更換
full$age <- mice.output$age
# 確認已全數填補
sum(is.na(full$age))




#-----------機器學習-預測-----------#
train <- full[1:1000, ]
test <- full[1001:1309,]

set.seed(2018)
rf_model <- randomForest(survived ~ pclass + sex + age + sibsp 
                         + parch + fare + embarked + title + familysize + onboat, data = train)

par(mfrow=c(1,1))
plot(rf_model, ylim=c(0,0.36)) 
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

prediction.train <- predict(rf_model, train)
correction.rate <- sum(prediction.train == full[1:1000, "survived"]) / 1000
correction.rate


prediction <- predict(rf_model, test)
ID <- c(1001:1309)
answer <- data.frame(survived = prediction )

ggplot(answer, aes(x = survived, fill = survived)) +
  geom_bar(stat = 'count') +
  labs(x = 'Prediction of Survival') +
  theme_grey(base_size = 16)

write.csv(answer, file = "predict1.csv", row.names = FALSE)


