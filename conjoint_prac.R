#############conjoint#############
##################################
#############conjoint#############
##############################################################################3

library(dplyr)
#install.packages("ggplot2")
library(ggplot2)

hotel = read.csv("roomselect.csv")
hotel$choice = ifelse(hotel$choice == 1, TRUE, FALSE) #1이면 true, 아니면 false
#factor형 변환 시켜주기
hotel$review = as.factor(hotel$review)
#hotel$price = as.factor(hotel$price) #문자형은 함수에서 자동으로 변형되는데 숫자는 숫자로 인식한다.
head(hotel)

#visualization(eda)
hotel %>% 
  group_by(choice) %>%
  filter(choice == TRUE) %>%
  ggplot(aes(roomsize, fill = review)) +
  geom_bar()

hotel %>% 
  group_by(location) %>%
  filter(choice == TRUE) %>%
  ggplot(aes(review, fill = location)) +
  geom_bar()


#choice based
library(mlogit)

hotel2 <- mlogit.data(hotel, id.var = "resp_id", shape = "long", #long 포맷
                      choice = "choice", alt.var = "alt")


m1 <- mlogit(choice ~ 0 + review + roomsize + location + price, data=hotel2)

summary(m1)

exp(m1$coefficients)

-coef(m1)/coef(m1)[6] #willingness to pay

#check the interaction effect
m2 <- mlogit(choice ~ 0 + review + location + price + roomsize*price, data=hotel2)

summary(m2)

lrtest(m1, m2) #model comparison #유의미하지 않다. m1으로 가자

#해석방법
exp(m1$coefficients) #= 계수는 효용정도를 나타냅니다.

-coef(m1)/coef(m1)[6] #지불용의 -> 효용정도(변수들의 계수)/ 가격의 효용(가격의 계수)

#market share 측정 방법

#1. 임의의, 가상의 상품들을 만들고 가변수들 구성
#2. 가변수들 * m1에서 구한 효용 = 효용정도
#3. share = exp(utility)/sum(exp(utility)) 이것이 preference rule
#point!, 가상의 상품을 만들때, client의 요구나 도메인의 핵심 이슈를 반영해서 만들기


#market share

#1. 가상의 상품 만들기 (정답은 없습니다!)
hotel = read.csv("roomselect.csv")

#필터링
h_chosen = subset(hotel,hotel$choice == TRUE)

h_chosen$price = as.factor(h_chosen$price)

h_chosen %>% 
  group_by(roomsize) %>%
  filter(choice == TRUE) %>%
  ggplot(aes(roomsize, fill = price)) +
  geom_bar()
#300$의 방의 실적이 저조하다.

#logistic regression
g1 = glm(choice ~ price, data = hotel, family = binomial) #....
summary(g1)
#가격이 높을수록 선택확률은 감소함을 확인


#위 결과를 바탕으로 가상의 옵션 구성
#idea = (300$ 상품을 어떻게 구성하면 좋을까?) (300$라는 높은 가격을 어떻게 보완할 수 있을까)
products300 <- data.frame(roomsize = factor(c("suit", "suit", "suit", "regular","regular","regular"), levels=c("regular", "suit")), 
                          location = factor(c("airport", "attraction", "conference","airport", "attraction", "conference"), levels=c("airport", "attraction","conference")), 
                          price = c(300, 300, 300,300,300,300)
)


#모델에서 review 빼기
m11 <- mlogit(choice ~ 0 + roomsize + location + price, data=hotel2)
summary(m11)

data300.model = model.matrix(m11$formula,
                             data = products300)

products300

data300.model

utility = data300.model[,-1] %*%m11$coefficients 

utility

share = exp(utility)/sum(exp(utility)) #preference rule을 이용해 점유율 추정하기

shares = cbind(share, products300)
shares #점유율 추정

