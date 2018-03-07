### hw_1_question


########################################################### Task 1

# 查看內建資料集: 鳶尾花(iris)資料集
iris

# 使用dim(), 回傳iris的列數與欄數
dim(iris)

# 使用head() 回傳iris的前六列
head(iris)

# 使用tail() 回傳iris的後六列
tail(iris)

# 使用str() 
str(iris)

# 使用summary() 查看iris敘述性統計、類別型資料概述。
summary(iris)

########################################################### Task 2

# 使用for loop 印出九九乘法表
# Ex: (1x1=1 1x2=2...1x9=9 ~ 9x1=9 9x2=18... 9x9=81)
for(i in 1:9){
  for(j in 1:9){
    print(paste(i,"x",j,"=",i*j))
  }
}


########################################################### Task 3

# 使用sample(), 產出10個介於10~100的整數，並存在變數 nums
#nums <- sample(c(10:100),size= 10)
nums <- sample(10:100, size = 10)


# 查看nums
nums

# 1.使用for loop 以及 if-else，印出大於50的偶數，並提示("偶數且大於50": 數字value)
# 2.特別規則：若數字為66，則提示("太66666666666了")並中止迴圈。

for(i in nums){
  if(i == 66){
    print("太66666666666了")
  }else if(i %% 2 == 0 & i > 50){
    print(i)
    print(paste("偶數且大於50:", i))
  }
}
  
  
  



########################################################### Task 4

# 請寫一段程式碼，能判斷輸入之西元年分 year 是否為閏年
n <- readline(prompt = "Enter the year: ")
n <- as.integer(n)
#is.numeric(n)
if(n %% 400 == 0 | (n %% 100 != 0 & n %% 4 == 0)){
  print("閏年")
}else{
  print("非閏年")
}





########################################################### Task 5

# 猜數字遊戲
# 1. 請寫一個由電腦隨機產生不同數字的四位數(1A2B遊戲)
# 2. 玩家可重覆猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

#Get random number
while (1) {
  answer <- sample(1000:9999, size = 1)
  test <- answer
  d <- test %% 10
  test <- floor(test / 10)
  c <- test %% 10
  test <- floor(test / 10)
  as.integer(test)
  b <- test %% 10
  test <- floor(test / 10)
  as.integer(test)
  a <- test %% 10
  print(paste(a,b,c,d))
  if ((a != b & a != c & a!= d) & (b != c & b!= d) & (c != d)){
    break
  }
}


#Guessing
while (1) {
  num <- readline(prompt = "Enter your guess: ")
  num <- as.integer(num)
  test <- num
  D <- test %% 10
  test <- floor(test / 10)
  C <- test %% 10
  test <- floor(test / 10)
  as.integer(test)
  B <- test %% 10
  test <- floor(test / 10)
  as.integer(test)
  A <- test %% 10
  countA <- 0
  countB <- 0
  if (A == a){
    countA <- countA + 1
  }
  if (B == b){
    countA <- countA + 1
  }
  if (C == c){
    countA <- countA + 1
  }
  if (D == d){
    countA <- countA + 1
  }
  if (A == b | A == c | A == d){
    countB <- countB + 1
  }
  if (B == a | B == c | B == d){
    countB <- countB + 1
  }
  if (C == a | C == b | C == d){
    countB <- countB + 1
  }
  if (D == a | D == b | D == c){
    countB <- countB + 1
  }
  if(countA == 4){
    print("You've got it!")
    break
  }else{
    print(paste(countA,"A",countB,"B"))
  }
}





