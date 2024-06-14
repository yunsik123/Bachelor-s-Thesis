#arragne는 오름차순
#df%>% distinct(AGE,GENDER)
#열에 적용되는함수  mutate(새로열만듬) select(변수선택) relocate(순서변경) rename(이름변경)
#count대신 group_by사용가능
#summary대신 summarise()사용가능능
# rowSums(), rowMeans() 변수합또는 평균을 구함
rm(list=ls())


library(tidyverse)
#데이터 로드하기
df <- read.csv('Car_Insurance_Claim.csv')
df<-as.tibble(df)
class(df)
head(df)
tail(df,3)
dim(df)# 10000 19
str(df)
glimpse(df)#정보 11개의 수치형 8개의 범주형
summary(df)#음수는 안보임

#결측치확인
is.na(df)
colSums(is.na(df))# ANNUAL_MILEAGE와 CREDIT_SCORE값이  각각 957개 982개씩 존재함
#겹치는 결측치 값을 확인
tmp1 <- df %>%
  filter(is.na(ANNUAL_MILEAGE) & is.na(CREDIT_SCORE)) %>%
  nrow()
tmp1 #88개개

#결측치의 합집합인 행을 구했을때의 개수
tmp2 <- df %>%
  filter(is.na(ANNUAL_MILEAGE) | is.na(CREDIT_SCORE)) %>%
  nrow()
tmp2 #1851개개

#결측치의 개수가 많음으로 결측치를 이후에 대체할것이다.




glimpse(df)
#8개의범주형
df %>% count(AGE) #4개
df %>% count(GENDER) #2개 거의반반
df %>% count(RACE)#2개 10대1정도
df %>% count(DRIVING_EXPERIENCE) #4개 갈수록 줄어듬
df %>% count(EDUCATION)#3개애매
df %>% count(INCOME)#개 뭐...그냥 있음
df %>% count(VEHICLE_YEAR)#2개  2대1정도
df %>% count(VEHICLE_TYPE)#2개 세단이 앞도적 스포츠카보다
#추가로 수치형중에 범주형으로 볼수있는것들 확인
df %>% count(VEHICLE_OWNERSHIP)# 0과1임
df %>% count(MARRIED)#0과 1 두가지임
df %>% count(CHILDREN)#0과 1 두가지임
df %>% count(DUIS)#7가지밖에없음 음주운전횟수로 라벨인코딩이다.
df %>% count(PAST_ACCIDENTS)#15가지이나 라벨인코딩느낌으로 그냥두자
df %>% count(OUTCOME)#0과 1임 2대1정도로 불균형데이터임을 알 수 있음
df %>% count(SPEEDING_VIOLATIONS) %>% print(n = nrow(.))#21이고 순서존재하니 label로두자
df %>% count(POSTAL_CODE)#4개임
df %>% count(ANNUAL_MILEAGE) %>% print(n = nrow(.)) #22개로 이역시 label로두자

#범주형 변수에 순서가 고려되기에 라벨인코딩(순서를 고려한채로 그냥둘수있으나)
#순서에 따라 중요도가 동일하게 증가된다고 생각되지 않아 
#위의 몇개 변수들을 factor화시켜줬다.

#관련되서보고싶은것
df %>% count(VEHICLE_TYPE,INCOME)
df %>% filter(GENDER=="male" & VEHICLE_TYPE=="sports car")


#factor화 필요한 변수들 진행해줌
df$AGE<-as.factor(df$AGE)
df$GENDER<-as.factor(df$GENDER)
df$RACE<-as.factor(df$RACE)
df$DRIVING_EXPERIENCE<-as.factor(df$DRIVING_EXPERIENCE)
df$EDUCATION<-as.factor(df$EDUCATION)
df$INCOME<-as.factor(df$INCOME)
df$VEHICLE_YEAR<-as.factor(df$VEHICLE_YEAR)
df$VEHICLE_TYPE<-as.factor(df$VEHICLE_TYPE)
df$VEHICLE_OWNERSHIP<-as.factor(df$VEHICLE_OWNERSHIP)
df$MARRIED<-as.factor(df$MARRIED)
df$CHILDREN<-as.factor(df$CHILDREN)
df$OUTCOME<-as.factor(df$OUTCOME)
df$POSTAL_CODE<-as.factor(df$POSTAL_CODE)






#ID는 불필요한 변수이기에 drop시킴
df<-dplyr::select(df,-ID)

#ANNUAL_MILEAGE와 CREDIT_SCORE둘다 결측치가 있는 행 88개를 제거한다
df3 <- df %>%
  filter(!is.na(ANNUAL_MILEAGE) | !is.na(CREDIT_SCORE))

#OUTCOME은 잠시빼자
df4<-dplyr::select(df3,-OUTCOME)

#전체적인결측치를 뺴자
df5<-na.omit(df4)

glimpse(df5)

#DUIS PAST SPEEDING ANNUAL 그리고 CREDIT_SCORE가지고 cor구함
df2 <- df5 %>%dplyr:: select(DUIS, PAST_ACCIDENTS, SPEEDING_VIOLATIONS, ANNUAL_MILEAGE, CREDIT_SCORE)
cor(df2)
model1<-lm(CREDIT_SCORE ~ ., data = df2);summary(model1)# 변수는 유의하지만 R^2값이 너무 낮다. 선형보간법을 사용하지 못한다.


#그룹평균보간법을 사용해보자

#CREDIT_SCORE 제외 연속변수제거한 데이터프레임을 가져온다.
df6 <- df5 %>% dplyr::select(-DUIS, -PAST_ACCIDENTS, -SPEEDING_VIOLATIONS,-ANNUAL_MILEAGE)
model2 <- step(lm(CREDIT_SCORE ~ ., data = df6), direction = "backward");summary(model2)#GENDER INCOME POSTAL_CODE #이것도 backward와 stepwise방법똑같음.
# 그룹별 평균 계산
group_means <- df3 %>%
  group_by(GENDER, INCOME, POSTAL_CODE) %>%
  summarise(mean_credit_score = mean(CREDIT_SCORE, na.rm = TRUE))
# 결측치를 그룹별 평균으로 채워넣기
df_filled <- df3 %>%
  left_join(group_means, by = c("GENDER", "INCOME", "POSTAL_CODE")) %>%
  mutate(CREDIT_SCORE = ifelse(is.na(CREDIT_SCORE), mean_credit_score, CREDIT_SCORE)) %>%
  dplyr::select(-mean_credit_score)

#ANNUAL_MILEAGE 제외 연속변수제거한 데이터프레임을 가져온다.
df6 <- df5 %>% dplyr::select(-DUIS, -PAST_ACCIDENTS, -SPEEDING_VIOLATIONS,-CREDIT_SCORE)
model2 <- step(lm(ANNUAL_MILEAGE ~ ., data = df6), direction = "both");summary(model2)#DRIVING_EXPERIENCE INCOME VEHICLE_OWNERSHIP MARRIED CHILDREN POSTAL_CODE
# 그룹별 평균 계산
group_means <- df3 %>%
  group_by(DRIVING_EXPERIENCE ,INCOME ,VEHICLE_OWNERSHIP, MARRIED ,CHILDREN ,POSTAL_CODE) %>%
  summarise(mean_ANNUAL_MILEAGE = mean(ANNUAL_MILEAGE, na.rm = TRUE))
# 결측치를 그룹별 평균으로 채워넣기
df_filled2 <- df_filled %>%
  left_join(group_means, by = c("DRIVING_EXPERIENCE" ,"INCOME" ,"VEHICLE_OWNERSHIP", "MARRIED" ,"CHILDREN" ,"POSTAL_CODE")) %>%
  mutate(ANNUAL_MILEAGE = ifelse(is.na(ANNUAL_MILEAGE), mean_ANNUAL_MILEAGE, ANNUAL_MILEAGE)) %>%
  dplyr::select(-mean_ANNUAL_MILEAGE)



summary(df_filled2) #그룹이 만들어지지않은 값이6 개존재한다.
#이를제거하자
df<-na.omit(df_filled2)




#EDA 
#연속형변수 #CREDIT_SCORE ANNUAL_MILEAGE SPEEDING_VIOLATIONS DUIS PAST_ACCIDENTS 
glimpse(df)
#CREDIT_SCORE---- 
par(mfrow=c(1,1))
plot(df$OUTCOME,df$CREDIT_SCORE)
ggplot(data = df, mapping = aes(x = CREDIT_SCORE)) +
  geom_density(bins = 100)
#등분산검정
group1 <- df$CREDIT_SCORE[df$OUTCOME == 0]
group2 <- df$CREDIT_SCORE[df$OUTCOME == 1]
result1 <- var.test(group1, group2)
print(result1)#0.05보다 작음으로 이분산이다.
#이분산 평균차이검정(Welch-t검정)
result2 <- t.test(group1, group2, var.equal = FALSE)
print(result2)#평균차이가 있다.
#CREDIT_SCORE는 자동차보험여부에 유의미한차이를 보이는 변수다.
#정규분포모양을 따르기도하고 신용점수에 0~1사이임을 보이면 이상치처리 할필요없다.

summary(df)
glimpse(df)

#ANNUAL_MILEAGE 
par(mfrow=c(1,1))
plot(df$OUTCOME,df$ANNUAL_MILEAGE)
ggplot(data = df, mapping = aes(x = ANNUAL_MILEAGE)) +
  geom_density(bins = 100)
#등분산검정
group1 <- df$ANNUAL_MILEAGE[df$OUTCOME == 0]
group2 <- df$ANNUAL_MILEAGE[df$OUTCOME == 1]
result1 <- var.test(group1, group2)
print(result1)#0.05보다 작음으로 이분산이다.
#이분산 평균차이검정(Welch-t검정)
result2 <- t.test(group1, group2, var.equal = FALSE)
print(result2)#평균차이가 있다.


#SPEEDING_VIOLATIONS
par(mfrow=c(1,1))
plot(df$OUTCOME,df$SPEEDING_VIOLATIONS)
ggplot(data = df, mapping = aes(x = SPEEDING_VIOLATIONS)) +
  geom_density(bins = 100)
#등분산검정
group1 <- df$SPEEDING_VIOLATIONS[df$OUTCOME == 0]
group2 <- df$SPEEDING_VIOLATIONS[df$OUTCOME == 1]
result1 <- var.test(group1, group2)
print(result1)#0.05보다 작음으로 이분산이다.
#이분산 평균차이검정(Welch-t검정)
result2 <- t.test(group1, group2, var.equal = FALSE)
print(result2)#평균차이가 있다.


#DUIS 
par(mfrow=c(1,1))
plot(df$OUTCOME,df$DUIS)
ggplot(data = df, mapping = aes(x = DUIS)) +
  geom_density(bins = 100)
#등분산검정
group1 <- df$DUIS[df$OUTCOME == 0]
group2 <- df$DUIS[df$OUTCOME == 1]
result1 <- var.test(group1, group2)
print(result1)#0.05보다 작음으로 이분산이다.
#이분산 평균차이검정(Welch-t검정)
result2 <- t.test(group1, group2, var.equal = FALSE)
print(result2)#평균차이가 있다.


#PAST_ACCIDENTS 
par(mfrow=c(1,1))
plot(df$OUTCOME,df$PAST_ACCIDENTS)
ggplot(data = df, mapping = aes(x = PAST_ACCIDENTS)) +
  geom_density(bins = 100)
#등분산검정
group1 <- df$PAST_ACCIDENTS[df$OUTCOME == 0]
group2 <- df$PAST_ACCIDENTS[df$OUTCOME == 1]
result1 <- var.test(group1, group2)
print(result1)#0.05보다 작음으로 이분산이다.
#이분산 평균차이검정(Welch-t검정)
result2 <- t.test(group1, group2, var.equal = FALSE)
print(result2)#평균차이가 있다.







#AGE GENDER RACE DRIVING_EXPERIENCE EDUCATION INCOME VEHICLE_OWNERSHIP VEHICLE_YEAR MARRIED CHILDREN POSTAL_CODE 
#VEHICLE_TYPE SPEEDING_VIOLATIONS DUIS PAST_ACCIDENTS OUTCOME


#범주형변수 탐자
#AGE 유의함
tmp<-table(df$OUTCOME, df$AGE);tmp
chi_square_test <- chisq.test(tmp)
print(chi_square_test)
#GENDER 유의함
tmp<-table(df$OUTCOME, df$GENDER);tmp
chi_square_test <- chisq.test(tmp)
print(chi_square_test)
#RACE 안유의함
tmp<-table(df$OUTCOME, df$RACE);tmp
chi_square_test <- chisq.test(tmp)
print(chi_square_test)
#DRIVING_EXPERIENCE 유의함
tmp<-table(df$OUTCOME, df$DRIVING_EXPERIENCE);tmp
chi_square_test <- chisq.test(tmp)
print(chi_square_test)
#EDUCATION 유의함
tmp<-table(df$OUTCOME, df$EDUCATION);tmp
chi_square_test <- chisq.test(tmp)
print(chi_square_test)
#INCOME 유의함
tmp<-table(df$OUTCOME, df$INCOME);tmp
chi_square_test <- chisq.test(tmp)
print(chi_square_test)
#VEHICLE_OWNERSHIP 유의함
tmp<-table(df$OUTCOME, df$VEHICLE_OWNERSHIP);tmp
chi_square_test <- chisq.test(tmp)
print(chi_square_test)
#VEHICLE_YEAR 유의함
tmp<-table(df$OUTCOME, df$VEHICLE_YEAR);tmp
chi_square_test <- chisq.test(tmp)
print(chi_square_test)
#MARRIED 유의함
tmp<-table(df$OUTCOME, df$MARRIED);tmp
chi_square_test <- chisq.test(tmp)
print(chi_square_test)
#CHILDREN 유의함
tmp<-table(df$OUTCOME, df$CHILDREN);tmp
chi_square_test <- chisq.test(tmp)
print(chi_square_test)
#POSTAL_CODE 유의함
tmp<-table(df$OUTCOME, df$POSTAL_CODE);tmp
chi_square_test <- chisq.test(tmp)
print(chi_square_test)
#VEHICLE_TYPE 안유의함
tmp<-table(df$OUTCOME, df$VEHICLE_TYPE);tmp
chi_square_test <- chisq.test(tmp)
print(chi_square_test)


#VEHICLE_TYPE이랑 VEHICLE_YEAR 유의하지 않네?

#OUTCOME counterplot 
ggplot(df, aes(x = OUTCOME)) +
  geom_bar() 

#데이터를 7:3으로 나눈다.
library(caret)
set.seed(101)
a <- createDataPartition(df$OUTCOME, p = 0.7, list = FALSE, times = 1)
df_train <- df[a, ]
df_test <- df[-a, ]
str(df_train)
str(df_test)

library(MASS)
# 후진선택, 스텝와이즈 방식을 적용합니다.
backward <- stepAIC(glm(OUTCOME ~ ., data = df_train, family = binomial), direction = "backward")
stepwise <- stepAIC(glm(OUTCOME ~ ., data = df_train, family = binomial), direction = "both")
summary(backward)
summary(stepwise)


fit <- glm(OUTCOME ~ GENDER+DRIVING_EXPERIENCE+VEHICLE_OWNERSHIP+VEHICLE_YEAR+MARRIED+POSTAL_CODE+ANNUAL_MILEAGE,data = df_train, family = binomial)
summary(fit)



df_train$OUTCOME
#변수별십분위분석
df_train$OUTCOME <- as.numeric(as.character(df_train$OUTCOME) )
df_test$OUTCOME  <- as.numeric(as.character(df_test$OUTCOME) )
detach("package:tidyverse", unload = TRUE)
# 예측 확률을 계산합니다.
df_train$predicted_prob <- predict(fit, type = "response")

# 십분위 분석을 위한 함수를 정의합니다.
perform_decile_analysis <- function(df, predicted_col, actual_col) {
  # 예측 확률을 기준으로 데이터를 십분위로 나눕니다.
  df <- df %>%
    mutate(decile = ntile(!!sym(predicted_col), 10)) %>%
    group_by(decile) %>%
    summarise(
      count = n(),
      actual_mean = mean(!!sym(actual_col), na.rm = TRUE)
    ) %>%
    arrange(desc(decile))
  
  return(df)
}

# 각 변수별로 십분위 분석을 수행합니다.
decile_analysis_results <- list()
variables <- c("GENDER", "DRIVING_EXPERIENCE", "VEHICLE_OWNERSHIP", 
               "VEHICLE_YEAR", "MARRIED", "POSTAL_CODE", "ANNUAL_MILEAGE")

for (var in variables) {
  # 변수별 모델을 적합합니다.
  formula <- as.formula(paste("OUTCOME ~", var))
  model <- glm(formula, data = df_train, family = binomial)
  
  # 예측 확률을 계산합니다.
  df_train[[paste0("predicted_prob_", var)]] <- predict(model, type = "response")
  
  # 십분위 분석을 수행합니다.
  decile_results <- perform_decile_analysis(df_train, paste0("predicted_prob_", var), "OUTCOME")
  
  # 결과를 리스트에 저장합니다.
  decile_analysis_results[[var]] <- decile_results
}

# 결과를 확인합니다.
decile_analysis_results





#학습용데이터와 테스트용데이터를 통한 십분위분석비교
# 예측 확률 계산
df_train$predicted_prob <- predict(fit, type = "response")
df_test$predicted_prob <- predict(fit, newdata = df_test, type = "response")

# 십분위 분석을 위한 함수 정의
decile_analysis <- function(data, predicted_col) {
  data %>%
    mutate(decile = ntile(!!sym(predicted_col), 10)) %>%
    group_by(decile) %>%
    summarise(
      count = n(),
      predicted_prob = mean(!!sym(predicted_col), na.rm = TRUE),
      actual_mean = mean(OUTCOME, na.rm = TRUE)
    ) %>%
    arrange(desc(decile))
}

# 훈련 데이터와 테스트 데이터에 대해 십분위 분석 수행
train_deciles <- decile_analysis(df_train, "predicted_prob")
test_deciles <- decile_analysis(df_test, "predicted_prob")

# 결과 출력
print(train_deciles)
print(test_deciles)




#ROC커브

library(pROC)
predicted_probs <- predict(fit, newdata = df_test, type = "response")

# 실제 OUTCOME 값과 예측 확률을 사용하여 ROC 객체를 생성합니다.
roc_obj <- roc(df_test$OUTCOME, predicted_probs)

# ROC 곡선을 그립니다.
plot(roc_obj)
# AUC 값을 추가합니다.
auc(roc_obj)


library(PRROC)
#PR curve
# 검증 데이터 세트에 대한 예측 확률을 계산합니다.
predicted_probs <- predict(fit, newdata = df_test, type = "response")

# 실제 OUTCOME 값과 예측 확률을 사용하여 PR 곡선 객체를 생성합니다.
pr_obj <- pr.curve(scores.class0 = predicted_probs, weights.class0 = df_test$OUTCOME == 1,curve=TRUE)

# 모든 그래픽 장치를 닫습니다.
graphics.off()

# PR 곡선을 다시 그립니다.
plot(pr_obj)


# PR AUC 값을 계산합니다.
pr_auc <- pr_obj$auc.integral

print(paste("PR AUC:", pr_auc))





