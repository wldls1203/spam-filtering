install.packages('kernlab')
install.packages("rJava")
source("https://install-github.me/talgalili/installr")
installr::install.java()
library(rJava)
install.packages('KoNLP')
source("https://install-github.me/talgalili/installr")
installr::install.java()
library(KoNLP)
library(stringr)
library(tm)
library(kernlab)


useSejongDic()

##파일 불러오기
ding <- readLines("C:/Users/Yang jiin/Desktop/Dingdong.txt")
head(ding)


##중복제거, 영어,특수기호 제거
ding <- unique(ding)
ding2 <- str_replace_all(ding,"[^[:alpha:][:blank:]]"," ")
head(ding2)

##단어로 분리후 영어제거 -> 이렇게 해야 확인이 확, 인 으로 나누어 지지 않는다.
ding2 <- extractNoun(ding2)
ding2 <-lapply(ding2,function(x){gsub("[^가-힣, ]"," ",x)})
head(ding2)

##공백제거
ding2 <- lapply(ding2, function(x){
  Filter(function(y){nchar(y)<=10&nchar(y)>1},x)
})
head(ding2)

##tm에 넘겨주기 위한 작업
ding3 <- format(ding2,justify = "none")
####ding3 <- Filter(function(y){nchar(y)<=10&nchar(y)>1},ding3)
ding3 <- gsub("\\,","",ding3)
head(ding3)

##tm작업 시작
ding4 <- Corpus(VectorSource(ding3))

##Term-Doc Matrix
ding5 <- DocumentTermMatrix(ding4, control = list(weighting = function(x)
  weightTfIdf(x, normalize = F)))
dingmat <- as.matrix(ding5)
View(ding5)
###Term sparse  ##단어 86개
library(class)
cleandtm = removeSparseTerms(ding5,0.99)
cleandtm$dimnames

cleanmat_notlabel <- as.matrix(cleandtm)
cleanframe <- as.data.frame(cleanmat_notlabel)
cleandata <- cbind("Class" = c(rep("spam",152),rep("ham",80)), cleanframe)
View(cleandata)
# 트레이닝 데이터와 테스트 데이터 분류
library(caret)
inTrain <- createDataPartition(cleandata$Class,p=0.60,list=FALSE)
svmtrain <- cleandata[inTrain,]
svmtest <- cleandata[-inTrain,]
#svm실행
library(e1071)
install.packages("gmodels")
library(gmodels)
svm.linear <- svm(Class~., data=svmtrain,cost=100,  kernel='linear')
pred.linear <- predict(svm.linear, svmtest[,-1])

confusionMatrix(pred.linear,svmtest$Class)

