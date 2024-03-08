rm(list = ls())
library(tidyverse)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(MASS)
library(dplyr)
library(janitor)
library(tidyr)
evals.gender <- evals %>% 
  dplyr::select(gender, age)
ggplot(data = evals.gender, aes(x = gender, y = age, fill = gender)) + 
  geom_boxplot() + 
  labs(x = "Gender", y = "Age") + 
  theme(legend.position = "none")
model <- glm(gender ~ age, data = evals.gender, family = binomial(link = "logit"))
model %>%
  summary()
summ(model)#更加美观,jtools
mod1coefs <- round(coef(model), 2)#提取coefficient
mod1coefs
confint(model)#生成其置信区间

mod.coef.logodds <- model %>%
  summary() %>%
  coef()#一样的计算系数，只是不同方法

plot_model(model, show.values = T, transform = NULL, title = "Log-Odds(Male instructor)", show.p = F)
#The log-odds of age for male instructors.transform = NULL才显示log-odds

evals.gender <- evals.gender %>%
  mutate(logodds.male = predict(model))#添加估计值，用log-odds值表示

model %>%
  coef() %>%
  exp()#求幂来求odds，相比于log-odds更好解释

plot_model(model, show.values = T, axis.lim = c(1, 1.5),
           title = "Odds (Male instructor)", show.p = F)

evals.gender <- evals.gender %>%
  mutate(odds.male = exp(logodds.male))#添加估计值，用odds值表示

p.num <- exp(mod.coef.logodds["(Intercept)", "Estimate"] + mod.coef.logodds["age", "Estimate"] * 52)
p.denom <- 1 + p.num
p.num / p.denom

plogis(mod.coef.logodds["(Intercept)", "Estimate"] + mod.coef.logodds["age", "Estimate"] * 52)
#简略计算方法

evals.gender <- evals.gender %>%
  mutate(probs.male = fitted(model))#用fitted可以一步到位计算全部的概率

ggplot(data = evals.gender, aes(x = age, y = probs.male)) + 
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),
              se = F) + 
  labs(x = "Age", y = "Probability of instructor being male")

#plot_model(model, type = "pred", title = "",
           #axis.title = c("Age", "Prob. of instructor being male"))有问题，还不知道如何修改


#现在选取二元变量作为解释变量
evals.ethnic <- evals %>%
  select(gender, ethnicity)

evals %>%
  tabyl(ethnicity, gender) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns()#janitor包里的东西，把二元变量的所占比例展示出来

ggplot(evals, aes(x = gender, y = ..prop.., group = ethnicity, fill = ethnicity)) + 
  geom_bar(position = "dodge", stat = "count") + 
  labs(y = "Proportion")#画成柱形图来体现

model.ethnic <- glm(gender ~ ethnicity, data = evals.ethnic, family = binomial(link = "logit"))
model.ethnic %>%
  summary()

confint(model.ethnic) %>%#计算95%CI
  kableExtra::kable()
#计算过程
mod.ethnic.coef.logodds <- model.ethnic %>%
  summary() %>%
  coef()
ethnic.logodds.lower <- mod.ethnic.coef.logodds["ethnicitynot minority", "Estimate"] - 
  1.96 * mod.ethnic.coef.logodds["ethnicitynot minority", "Std. Error"]
ethnic.logodds.upper <- mod.ethnic.coef.logodds["ethnicitynot minority", "Estimate"] + 
  1.96 * mod.ethnic.coef.logodds["ethnicitynot minority", "Std. Error"]

plot_model(model.ethnic, show.values = TRUE, transform = NULL,
           title = "Log-Odds (Male instructor)", show.p = FALSE)

evals.ethnic <- evals.ethnic %>%
  mutate(logodds.male = predict(model.ethnic))

model.ethnic %>%
  coef() %>%
  exp()

exp(confint(model.ethnic))
plot_model(model.ethnic, show.values = TRUE,
           title = "Odds (Male instructor)", show.p = FALSE)

evals.ethnic <- evals.ethnic %>%
  mutate(odds.male = exp(logodds.male))

evals.ethnic <- evals.ethnic %>%
  mutate(probs.male = fitted(model.ethnic))#并上概率值

plot_model(model.ethnic, type = "pred", title = "",
           axis.title = c("Ethnicity", "Prob. of instructor being male"))






##Further Tasks
rm(list = ls())
yanny <- read.csv("yanny.csv")
yanny <- yanny %>%
  select(hear, gender, age)
yanny$hear <- as.factor(yanny$hear)
yanny$gender <- as.factor(yanny$gender)

ggplot(data = yanny, aes(x = hear, y = age, fill = hear)) + 
  geom_boxplot() + 
  labs(x = "What do you hear?", y = "Age") + 
  theme(legend.position = "none")

yanny %>%
  tabyl(gender, hear) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns()

ggplot(data = yanny, aes(x = hear, y = ..prop.., group = gender, fill = gender)) + 
  geom_bar(position = "dodge") + 
  labs(x = "What do you hear?", y = "Proportion")

mod.yanny <- glm(hear ~ age + gender, data = yanny, family = binomial(link = "logit"))

mod.yanny %>%
  summary()

plot_model(mod.yanny, show.values = T, 
           title = "Odds(Age)", show.p = T)

#plot_model(mod.yanny, type = "pred", title = "",
          #axis.title = c("Age", "Probability of hearing Yanny"))


titanic <- read.csv("titanic.csv")
titanic <- titanic %>%
  select(survived, age, gender, passenger.class)
titanic$survived <- as.factor(titanic$survived)
levels(titanic$survived) <- c("Died", "Survived")
titanic$gender <- as.factor(titanic$gender)
titanic$passenger.class <- as.factor(titanic$passenger.class)

ggplot(data = titanic, aes(x = survived, y = age, fill = survived)) + 
  geom_boxplot() + 
  labs(x = "Survived the Titanic?", y = "Age") + 
  theme(legend.position = "none")

titanic %>%
  tabyl(gender, survived) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns()

ggplot(data = titanic, aes(x = survived, group = gender)) + 
  geom_bar(aes(y = ..prop.., fill = gender), stat = "count", position = "dodge") + 
  labs(x = "Survived the Titanic?", y = "Proportion")
titanic %>%
  tabyl(passenger.class, survived) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns() 

ggplot(data = titanic, aes(x = survived, group = passenger.class)) + 
  geom_bar(aes(y = ..prop.., fill = passenger.class),
           stat = "count", position = "dodge") + 
  labs(x = "Survived the Titanic?", y = "Proportion")

mod.titanic <- glm(survived ~ gender + passenger.class + age, data = titanic, family = binomial(link = "logit"))
mod.titanic %>%
  summary()

plot_model(mod.titanic, show.values = T,
           title = "", show.p = F, value.offset = 0.25)












