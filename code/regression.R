library(dplyr)
library(reshape2) 
library(caret)
library(pROC)
library(car)

load("./data/dat.RData") # dat 
head(dat)
dat$shift_or_not = as.factor(dat$shift_or_not)
dat = dat[,-c(1,2)]

inTraining <- createDataPartition(dat$shift_or_not, p = .80, list = FALSE)
training <- dat[inTraining,]
testing  <- dat[-inTraining,]
training = na.omit(training)
model.null = glm(shift_or_not ~ 1,
                 data=training,
                 family = binomial(link="logit"))
summary(model.null)
model.full = glm(shift_or_not ~ atop_number+bicc_gmi+bicc_hw+bicc_milexp+bicc_milper+br_regch+fh_polity2+gle_rgdpc+icrg_qog, 
                 data=training,
                 family = binomial(link="logit"))
summary(model.full)
test_prob = predict(model.full, newdata = testing, type = "response")
test_roc = roc(testing$shift_or_not ~ test_prob, plot = TRUE, print.auc = TRUE) # AUC: 0.8896
test_roc

## use AIC to select model 
step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=training)
dat.final = training %>% dplyr::select(shift_or_not, fh_polity2, atop_number)
# Call:  glm(formula = shift_or_not ~ vdem_egaldem + gle_rgdpc + atop_number, 
#            family = binomial(link = "logit"), data = training)
# 
# Coefficients:
#   (Intercept)  vdem_egaldem     gle_rgdpc   atop_number  
# 4.838e+00    -1.005e+01     9.589e-05    -1.079e-01  

# model.final = glm(formula = shift_or_not ~ fh_polity2 + atop_number + bicc_milper, 
#                   family = binomial(link = "logit"), data = training)
# summary(model.final)
# test_prob = predict(model.final, newdata = testing, type = "response")
# test_roc = roc(testing$shift_or_not ~ test_prob, plot = TRUE, print.auc = TRUE) # AUC: 0.9444
# test_roc


model.final = glm(formula = shift_or_not ~ vdem_egaldem + gle_rgdpc + atop_number, 
                  family = binomial(link = "logit"), data = training)
summary(model.final)
test_prob = predict(model.final, newdata = testing, type = "response")
test_roc = roc(testing$shift_or_not ~ test_prob, plot = TRUE, print.auc = TRUE) # AUC: 0.9444
test_roc

library(pROC)
roc_full <- roc(dat$shift_or_not, model.final$fitted.values)
auc_full <- auc(roc_full)
# For each predictor variable, remove it from the model and calculate the AUC for the reduced model. 
# This reduced model will be without the variable you want to assess.
# Assuming "covariate_to_remove" is the variable you want to assess
reduced_model <- glm(response ~ . - covariate_to_remove, data = data, family = "binomial")
roc_reduced <- roc(data$response, reduced_model$fitted.values)
auc_reduced <- auc(roc_reduced)
# The AUC contribution of a variable is the difference between the AUC of the full model and the AUC of the reduced model without that variable.
auc_contribution <- auc_full - auc_reduced

## use VIF to detect multicolineaity 
vif_values <- vif(model.final)
vif_values


##### visulize coefficients ####
model = glm(formula = shift_or_not ~ vdem_egaldem + gle_rgdpc + atop_number, 
            family = binomial(link = "logit"), data = dat)
fitted=predict(model, type="response")

x = dat$gle_rgdpc
y = dat$shift_or_not
sunflowerplot(x, y, rotate = F,  cex = 5, cex.fact = 1,  pch = 1,  yaxt = 'n', xlab='', ylab='', 
              cex.axis=1.4,  col = "maroon",  seg.col = "maroon",  seg.lwd = 3)
title(xlab='Predictor', ylab='Outcome', line=2, cex.lab=1.4)
# line=2 determines axis title's distance from axis
axis(2, at= c(0,1), las = 1, labels=c('No', 'Yes'), cex.axis=1.4, cex.lab=1.4)
lines(x, fitted(model),col= "maroon", lwd = 1.7)

# Alt ways to get fitline
lines (lowess(y~x), lwd=2, col= "red")
lines (scatter.smooth(y~x), lwd=2)


### 
library(ggplot2)
library(stringr)
df = data.frame(var = c("level of democracy","number of alliance", "military personnel index"),
                AUC = c(0.212, 0.325, 0.189))
df$len = c(0.212, 0.325, 0.189)
df$sd = c(0.076, 0.065,0.074)
p<-ggplot(data=df, aes(x=var, y=AUC)) +
  geom_bar(stat="identity",fill="darkgray",width=0.5)+
  theme_classic()+
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=0,
                position=position_dodge(3), linewidth=1)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  labs(y= " ", x = " ", title="AUC contribution of each predictor")
p + coord_flip()

## 
df = data.frame(var = c("level of democracy","number of alliance", "military personnel index"),
                coefficient =c(-0.55038,-0.06679,0.79085))
p = ggplot(data=df, aes(x=var, y=coefficient))+
  geom_bar(stat="identity",fill="darkgray",width=0.5)+theme_classic()+
  labs(y= " ", x = " ", title=" ") + geom_hline(yintercept = 0)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
p + coord_flip()
