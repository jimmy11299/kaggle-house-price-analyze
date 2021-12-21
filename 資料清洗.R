#資料清洗
library(dplyr)
library(purrr)
library(readr)
library(data.table)
test <- read.csv("C:/test.csv")
test_1<-as.data.table(test)
## create na replacement function
na.replace <- function(x){
  ifelse(is.na(x), tail(names(sort(table(x))), 1), x)
}

## get the character columns
cols1 <- names(Filter(is.character, test_1))

## replace the values
test_1[, (cols1) := lapply(.SD, na.replace), .SDcols = cols1]

#######################################
## get the numeric columns
cols2 <- names(Filter(is.numeric, test_1))

## replace the values
test_1[, (cols2) := lapply(.SD, na.replace), .SDcols = cols2]

View(test_1)
sum(is.na(test_1))
length(test_1[1])

############################################
#2.移除不重要的colume(condition1,condition2,BsmtQual),並組合每個房屋的價錢。
library(readr)
sample_submission <- read_csv("C:/sample_submission.csv")
sample_submission1<-as.data.table(sample_submission)
test_1<-cbind(test_1,sample_submission1$SalePrice)
test_2<-test_1[,-c(1,10,14,15,31)]
View(test_2)

############################################
#透過建立線性模型移除outlier
#############lm模型1(67,402,752)
lm1<-lm(test_2$V2 ~ ., data = test_2)
summary(lm1)
length(test_2[1])
AIC(lm1); BIC(lm1)
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm1,4)

#############lm模型2(236,401,644)
test_3<-test_2[-c(67,752),-1]
lm2<-lm(test_3$V2 ~ .,data = test_3)
summary(lm2)
AIC(lm2); BIC(lm2)
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm2,4)
plot(lm2)