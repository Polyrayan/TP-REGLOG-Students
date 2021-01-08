library(ggplot2)
library(broom)
library(forestmodel)
library(effects)
library(ggeffects)
library(stats)
library(caret)
library(e1071)

students <- read.csv(file = 'dataset/students.csv', sep = ";", header=TRUE)
head(students)

class(students$admit) 
class(students$gre) 
class(students$gpa) 
class(students$rank)

#boxplot variables gre gpa and rank
ggplot(students) + geom_boxplot(aes(factor(admit), gre))
ggplot(students) + geom_boxplot(aes(factor(admit), gpa))
ggplot(students) + geom_boxplot(aes(factor(admit), rank))


#adding boolean variable : admitBool
students$admitBool[students$admit == 1] <- TRUE
students$admitBool[students$admit == 0] <- FALSE


regLog <- glm(admitBool ~ gre + gpa + rank, data = students, family = binomial(logit))
summary(regLog)

#trust interval
exp(confint(regLog))
# Odds ratio
exp(coef(regLog))

tmp <- tidy(regLog, conf.int = TRUE, exponentiate = TRUE)

ggplot(tmp) + aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high) +
  geom_vline(xintercept = 1) +
  geom_errorbarh() +
  geom_point() +
  scale_x_log10()

plot(allEffects(regLog))

students_pred <- predict(regLog, type = "response", newdata = students)

# Confusion Matrix
students_y <- students_pred > 0.5
caret::confusionMatrix(data = as.factor(as.numeric(students_y)),
                       reference = as.factor(students$admit),
                       positive = "1")

# AUC
pROC::auc(students$admitBool, students_pred)
