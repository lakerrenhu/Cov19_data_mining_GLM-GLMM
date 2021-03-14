##STA5505_ term project

#####Read dataset############
#####################################
cov2019=read.csv(file.choose(),header=T)
dim(cov2019)  
str(cov2019)   
names(cov2019) 
na(cov2019)
attr()
cov2019$agegroup1=as.factor(cov2019$agegroup)
str(cov2019) # gender and agegroup1 are factors

#####independence test############
#####################################
logit1=glm(Death~County,family=binomial,data=cov2019)
summary(logit1) # county is not significant with large p-value >0.9
logit2=glm(Death~Age,family=binomial,data=cov2019)
summary(logit2) # age is significant with small p-value=<2e-16 <0.05
logit3=glm(Death~Gender,family=binomial,data=cov2019)
summary(logit3) # gender is significant with small p-value=9.14e-10 <0.05
logit4=glm(Death~Days_after_first_case,family=binomial,data=cov2019)
summary(logit4) # Days_after_first_case is significant with small p-value=2e-16 <0.05
logit5=glm(Death~agegroup1,family=binomial,data=cov2019)
summary(logit5)  # agegroup as categorical is insignificant
logit6=glm(Death~agegroup,family=binomial,data=cov2019)
summary(logit6)  # agegroup as ordinal is significant with small p-value=2e-16 <0.05

#########Odds ratio#######
logistic.regression.or.ci <- function(regress.out, level=0.95)
{
  ################################################################
  #                                                              #
  #  This function takes the output from a glm                   #
  #  (logistic model) command in R and provides not              #
  #  only the usual output from the summary command, but         #
  #  adds confidence intervals for all coefficients and OR's.    #
  #                                                              #
  #  This version accommodates multiple regression parameters    #
  #                                                              #
  ################################################################
  usual.output <- summary(regress.out)
  z.quantile <- qnorm(1-(1-level)/2)
  number.vars <- length(regress.out$coefficients)
  OR <- exp(regress.out$coefficients[-1])
  temp.store.result <- matrix(rep(NA, number.vars*2), nrow=number.vars)
  for(i in 1:number.vars)
  {
    temp.store.result[i,] <- summary(regress.out)$coefficients[i] +
      c(-1, 1) * z.quantile * summary(regress.out)$coefficients[i+number.vars]
  }
  intercept.ci <- temp.store.result[1,]
  slopes.ci <- temp.store.result[-1,]
  OR.ci <- exp(slopes.ci)
  output <- list(regression.table = usual.output, intercept.ci = intercept.ci,
                 slopes.ci = slopes.ci, OR=OR, OR.ci = OR.ci)
  return(output)
}

logistic.regression.or.ci(logit2) #Age: 1.090416
logistic.regression.or.ci(logit3) #GenderMale: 1.70677
logistic.regression.or.ci(logit4) #Days_after_first_case:0.9254203
logistic.regression.or.ci(logit6) #agegroup:2.425343

#####logit model fitting############
#####################################

# gender, agegroup1 are factors
logit.cov=glm(Death~Age+Gender+Days_after_first_case+agegroup1,
              family=binomial(link="logit"),data=cov2019)
summary(logit.cov) 
# wald tets shows agegroup1 as a categoircal is not significant
# AIC=4014.9, Deviance=3988.9

drop1(logit.cov,test="Chisq") 
# LRT shows county and agegroup1 as a categoircal is not significant

#use the agegroup as an ordinal variable , remove county and test again
logit.cov1=glm(Death~Age+Gender+Days_after_first_case+agegroup,
               family=binomial(link="logit"),data=cov2019)
summary(logit.cov1)
# wald test shows that age,gender,Days after 1st case, agegroup are significant
# AIC: 4008.4, Deviance= 3998.4
drop1(logit.cov1,test="Chisq")
#                      Df Deviance    AIC     LRT  Pr(>Chi)    
#<none>                     3998.4 4008.4                      
#Age                    1   4019.7 4027.7  21.278 3.972e-06 ***
#Gender                 1   4038.0 4046.0  39.586 3.139e-10 ***
#Days_after_first_case  1   4196.8 4204.8 198.372 < 2.2e-16 ***
#agegroup               1   4002.4 4010.4   3.989    0.0458 *  

#see if there are collinearities between indepdendent variables
cor(cov2019$Age,cov2019$agegroup)
cor(cov2019$Age,cov2019$Days_after_first_case)
cor(cov2019$Days_after_first_case,cov2019$agegroup)
# the correlaton coefficient of age and agegroup r=0.9884039,
# so remove agegroup or age to avoid collinearity
  
logit.cov2=glm(Death~Age+Gender+Days_after_first_case,
              family=binomial(link="logit"),data=cov2019)
summary(logit.cov2)
# AIC=4010.4, Deviance=4002.4
drop1(logit.cov2,test="Chisq")
#                      Df Deviance    AIC     LRT  Pr(>Chi)    
#<none>                     4002.4 4010.4                      
#Age                    1   5150.6 5156.6 1148.24 < 2.2e-16 ***
#Gender                 1   4043.2 4049.2   40.78 1.707e-10 ***
#Days_after_first_case  1   4202.7 4208.7  200.34 < 2.2e-16 ***

# see if there are interactions between age,gender and days infected
logit.cov3=glm(Death~Age+Gender+Days_after_first_case+Age:Gender:Days_after_first_case
               +Age:Gender+Gender:Days_after_first_case+Age:Days_after_first_case,
               family=binomial(link="logit"),data=cov2019)
summary(logit.cov3) # a two-way interaction of Age:Days_after_first_case is significant
#                                       Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                          -12.558442   2.052907  -6.117 9.51e-10 ***
#Age                                    0.168142   0.026953   6.238 4.42e-10 ***
#GenderMale                             4.893470   2.457210   1.991  0.04643 *  
#Days_after_first_case                  0.069372   0.061955   1.120  0.26283    
#Age:GenderMale                        -0.062271   0.032731  -1.902  0.05711 .  
#GenderMale:Days_after_first_case      -0.112728   0.075273  -1.498  0.13424    
#Age:Days_after_first_case             -0.002175   0.000811  -2.681  0.00733 ** 
#Age:GenderMale:Days_after_first_case   0.001653   0.000999   1.655  0.09793 .  
# AIC: 4007.6, Deviance=3991.6
drop1(logit.cov3,test="Chisq") # none of the interactions is significant
logit.cov4=glm(Death~Age+Gender+Days_after_first_case+Age:Days_after_first_case,
               family=binomial(link="logit"),data=cov2019)
summary(logit.cov4)
anova(logit.cov2,logit.cov4) # deviance=5.6862
1-pchisq(5.6862,1) #p-value=0.01709884 means to reject the null hypothesis and keep the interaction

# see if there are significant quadratic terms for age, and Days_after_first_case
logit.cov5=glm(Death~Age+I(Age^2)+Gender+Days_after_first_case+I(Days_after_first_case^2)
                 +Age:Days_after_first_case,
               family=binomial(link="logit"),data=cov2019)
summary(logit.cov5) # it shows the quadratic term of Days infected is significant
drop1(logit.cov5,test="Chisq")
logit.cov50=glm(Death~Age+I(Age^2)+Gender+Days_after_first_case
               +Age:Days_after_first_case,
               family=binomial(link="logit"),data=cov2019)
summary(logit.cov50)

logit.cov51=glm(Death~Age+Gender+Days_after_first_case
                +Age:Days_after_first_case:Gender,
                family=binomial(link="probit"),data=cov2019)
summary(logit.cov51)

logit.cov55=glm(Death~Age+Gender+Days_after_first_case+I(Days_after_first_case^2)
               +Age:Days_after_first_case,
               family=binomial(link="logit"),data=cov2019)
summary(logit.cov55)
anova(logit.cov4,logit.cov55) # deviance=-0.026461
1-pchisq(-0.026461,2) #p-value=1, so fails to reject the null hypothesis and remove the quadratic terms

# final logit model
# Death ~ Age + Gender + Days_after_first_case + Age:Days_after_first_case
logit.cov6=logit.cov4
summary(logit.cov6)  # AIC=4006.7  Deviance=3996.7 
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)               -9.7220123  1.1324582  -8.585  < 2e-16 ***
#Age                        0.1278606  0.0152660   8.376  < 2e-16 ***
#GenderMale                 0.5819371  0.0930833   6.252 4.06e-10 ***
#Days_after_first_case     -0.0045288  0.0350570  -0.129   0.8972    
#Age:Days_after_first_case -0.0011096  0.0004709  -2.356   0.0185 *  

drop1(logit.cov6,test="Chisq")
#Model:
#Death ~ Age + Gender + Days_after_first_case + Age:Days_after_first_case
#                           Df Deviance   AIC    LRT  Pr(>Chi)    
#<none>                         3996.7 4006.7                     
#Gender                     1   4037.1 4045.1 40.379 2.092e-10 ***
#Age:Days_after_first_case  1   4002.4 4010.4  5.686    0.0171 *


#####probit model fitting############
#####################################

# the correlaton coefficient of age and agegroup r=0.9884039,
# so remove agegroup or age to avoid collinearity
probit.cov=glm(Death~Age+Gender+Days_after_first_case,
               family=binomial(link="probit"),data=cov2019)
summary(probit.cov) # wald test shows age, gender,Days are significant
#AIC=4022.3, Deviance=4014.3
drop1(probit.cov,test="Chisq") # remove county

#see if there are significant interactions
probit.cov1=glm(Death~Age+Gender+Days_after_first_case+Age:Gender:Days_after_first_case
                +Age:Gender+Gender:Days_after_first_case+Age:Days_after_first_case,
               family=binomial(link="probit"),data=cov2019)
summary(probit.cov1) # a two-way interaction Age:Days_after_first_case is significant
# AIC=4014.3, Deviance=3998.3  
#                                       Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                          -6.4214784  0.9124146  -7.038 1.95e-12 ***
#Age                                   0.0843401  0.0125065   6.744 1.54e-11 ***
#GenderMale                            2.2958418  1.1025640   2.082 0.037317 *  
#Days_after_first_case                 0.0512837  0.0269839   1.901 0.057363 .  
#Age:GenderMale                       -0.0290985  0.0153464  -1.896 0.057946 .  
#GenderMale:Days_after_first_case     -0.0581524  0.0332227  -1.750 0.080053 .  
#Age:Days_after_first_case            -0.0013053  0.0003691  -3.536 0.000406 ***
#Age:GenderMale:Days_after_first_case  0.0008372  0.0004612   1.815 0.069469 .  

drop1(probit.cov1,test="Chisq") # it shows no significant interactions
probit.cov11=glm(Death~Age+Gender+Days_after_first_case+Age:Days_after_first_case,
                family=binomial(link="probit"),data=cov2019)
summary(probit.cov11)
anova(probit.cov11,probit.cov) #Deviance=-16.02, df=-4
1-pchisq(16.02,4) # p-value=0.002992444 rejects the null hypothesis, no interactions exist

#see if there are signficant quadratic terms
probit.cov2=glm(Death~Age+I(Age^2)+Gender+Days_after_first_case+I(Days_after_first_case^2)
                +Age:Days_after_first_case,
                family=binomial(link="probit"),data=cov2019)
summary(probit.cov2) # wald test shows both quadratic terms are significant

probit.cov3=glm(Death~Age+Gender+Days_after_first_case
                +Age:Days_after_first_case,
                family=binomial(link="probit"),data=cov2019)
anova(probit.cov3,probit.cov2) # deviance=11.345,df =2
1-pchisq(11.345,2) # p-value=0.003439256, reject the null hypothesis and keep both quadratic terms

probit.cov22=glm(Death~Age+I(Age^2)+Gender+Days_after_first_case
                +Age:Days_after_first_case,
                family=binomial(link="probit"),data=cov2019)
anova(probit.cov3,probit.cov22)
1-pchisq(11.369,1)  # p-value=0.0007468007, reject the null hypothesis and keep both quadratic terms
#final probit model
probit.cov4=probit.cov2
summary(probit.cov4) # AIC=4004.6, deviance=3990.6
drop1(probit.cov4,test="Chisq") # the quadratic of days is not significant
#Model:Death ~ Age + I(Age^2) + Gender + Days_after_first_case + I(Days_after_first_case^2) + 
#  Age:Days_after_first_case
#                            Df Deviance    AIC    LRT  Pr(>Chi)    
#<none>                          3990.6 4004.6                     
#I(Age^2)                    1   4002.0 4014.0 11.368  0.000747 ***
#Gender                      1   4033.2 4045.2 42.537 6.937e-11 ***
#I(Days_after_first_case^2)  1   3990.6 4002.6  0.000  1.000000    
#Age:Days_after_first_case   1   4006.7 4018.7 16.047 6.180e-05 ***

probit.cov5=glm(Death~Age+I(Age^2)+Gender+Days_after_first_case
                +Age:Days_after_first_case,
                family=binomial(link="probit"),data=cov2019)
summary(probit.cov5) # AIC=4002.6, deviance=3990.6
anova(probit.cov5,probit.cov4)
1-pchisq(-0.023474,1) # p-value=1 fails to reject the null hypothesis
# the final model will be probit.cov5 as the AIC is smaller than probit.cov4


######Compare##################
#logit(Death)=Age + Gender + Days_after_first_case + Age:Days_after_first_case
summary(logit.cov6)  # AIC=4006.7  Deviance=3996.7 
#probit(Death)= Age + I(Age^2) + Gender + Days_after_first_case + Age:Days_after_first_case
summary(probit.cov5) # AIC=4002.6, deviance=3990.6
# both models have very close AIC and Deviance

# Odds ratios of logit model

logistic.regression.or.ci(logit.cov6)
#Coefficients:
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)               -9.7220123  1.1324582  -8.585  < 2e-16 ***
#Age                        0.1278606  0.0152660   8.376  < 2e-16 ***
#GenderMale                 0.5819371  0.0930833   6.252 4.06e-10 ***
#Days_after_first_case     -0.0045288  0.0350570  -0.129   0.8972    
#Age:Days_after_first_case -0.0011096  0.0004709  -2.356   0.0185 *  
#$OR
#Age              GenderMale    Days_after_first_case  Age:Days_after_first_case 
#1.1363946       1.7895016         0.9954815                 0.9988910 
#$OR.ci
#[,1]      [,2]
#[1,] 1.1028965 1.1709102
#[2,] 1.4910749 2.1476559
#[3,] 0.9293786 1.0662860
#[4,] 0.9979695 0.9998134

#check by step():backward elimination
logit.cov7=glm(Death~Age+Gender+Days_after_first_case
                +Age:Gender+Gender:Days_after_first_case+Age:Days_after_first_case+
                 I(Age^2)+I(Days_after_first_case^2),
                family=binomial(link="logit"),data=cov2019)
step(logit.cov7,test="Chisq") # same results shown as did by drop1(),anova()

probit.cov6=glm(Death~Age+Gender+Days_after_first_case
               +Age:Gender+Gender:Days_after_first_case+Age:Days_after_first_case+
                 I(Age^2)+I(Days_after_first_case^2),
               family=binomial(link="probit"),data=cov2019)
step(probit.cov6,test="Chisq") # same results shown as did by drop1(),anova()

#see AUC/ROC
library(pROC)
#AUC of logit model
rocplot.logit=roc(Death~fitted(logit.cov7),data=cov2019)
plot.roc(rocplot.logit,legacy.axes=TRUE)
auc(rocplot.logit)
#Area under the curve: 0.8833

#AUC of probit model
rocplot.prbit=roc(Death~fitted(probit.cov6),data=cov2019)
plot.roc(rocplot.prbit,legacy.axes=TRUE)
auc(rocplot.prbit)
#Area under the curve: 0.8836

#conclusion: the probit model slightly outperforms the logit model
# logit model:   AIC=4006.7, Deviance=3996.7, AUC=0.8833
# probit model:  AIC=4002.6, Deviance=3990.6, AUC=0.8836

#Logit(death)= - 9.722 + 0.12786 * age + 0.581937 * gender(male) - 0.0045288 * DAYS - 0.0011096 * age * DAYS. 
#Probit(death) = - 4.169 + 0.03495 * age + 0.0002439 * age *age + 0.2817 *gender(male) + 0.01572 * DAYS - 0.0008174 * age * DAYS.


#group data
library(dplyr)

cov2019 %>% 
  group_by(Death,Gender) %>%
  summarise(freq=n())
## Groups:   Death [2]
#Death Gender  freq
#<int> <fct>  <int>
#  1     0 Female 10565
#2     0 Male   10745
#3     1 Female   212
#4     1 Male     368

cov2019b=cov2019 %>% 
  group_by(Death,Gender,agegroup1) %>%
  summarise(freq=n())
summary(cov2019$Days_after_first_case)
#create days group 
days=cov2019$Days_after_first_case
daysgroup=days

i<-1
while(i<=dim(cov2019)[1])
{
  if (days[i]>=1 & days[i]<=7)
  {
    daysgroup[i]=1
  }
  if (days[i]>7 & days[i]<=14)
  {
    daysgroup[i]=2
  }
  if (days[i]>14 & days[i]<=21)
  {
    daysgroup[i]=3
  }
  if (days[i]>21 & days[i]<=28)
  {
    daysgroup[i]=4
  }
  if (days[i]>28)
  {
    daysgroup[i]=5
  }
  i=i+1
}

cov2019$daysgroup=daysgroup

#grouped data
cov2019b=cov2019 %>% 
  group_by(Death,Gender,agegroup1,daysgroup) %>%
  summarise(freq=n())
cov2019b

################poisson model##########
poisson.reg=glm(Death~Age+Gender+Days_after_first_case
                +Age:Gender+Gender:Days_after_first_case+Age:Days_after_first_case+Age:Days_after_first_case:Gender+
                  I(Age^2)+I(Days_after_first_case^2),
                family=poisson(link="log"),data=cov2019)
summary(poisson.reg) 
step(poisson.reg,test="Chisq")
drop1(poisson.reg,test="Chisq")
#age,gender,days,days^2 are kept, AIC=4105.7,Deviance=2925.7

poisson.reg1=glm(Death~Age+Gender+Days_after_first_case
                ,
                family=poisson(link="log"),data=cov2019)
summary(poisson.reg1) #AIC: 4104.1, Deviance=2936.1

poisson.reg2=glm(Death~Age+Gender+Days_after_first_case+I(Days_after_first_case^2)
                 ,
                 family=poisson(link="log"),data=cov2019)
summary(poisson.reg2) #AIC: 4106.2,deviance=2936.2
anova(poisson.reg1,poisson.reg2)
1-pchisq(-0.031174,1) # fails to reject H0,that's to say, poisson.reg1 is better.

##quasi poisson
quasipo.reg=glm(Death~Age+Gender+Days_after_first_case,
                 family=quasipoisson(link="log"),data=cov2019)
summary(quasipo.reg) 
step(quasipo.reg,test="Chisq")
drop1(quasipo.reg,test="Chisq")

#########negative binomial########
library(MASS)
neg.bio=glm.nb(Death~Age+Gender+Days_after_first_case,data=cov2019)
summary(neg.bio)

neg.bio1=glm.nb(Death~Age+Gender+Days_after_first_case
               +Age:Gender+Gender:Days_after_first_case+Age:Days_after_first_case+Age:Days_after_first_case:Gender+
                 I(Age^2)+I(Days_after_first_case^2),data=cov2019)
summary(neg.bio1) 
#AIC: 4082.1,deviance: 2895.8  #I(Days_after_first_case^2) is significant
step(neg.bio1,test="Chisq")   #Deviance: 2896 	AIC: 4082
drop1(neg.bio1,test="Chisq")

neg.bio2=glm.nb(Death~Age+Gender+Days_after_first_case+I(Days_after_first_case^2),data=cov2019)
summary(neg.bio2) #AIC: 4080.6,deviance: 2905.7
anova(neg.bio,neg.bio2) 
#LR test:chi-square=27.81515,p-value=2.343688e-06, reject H0,so neg,bio2 is better

library(pROC)
#AUC of poisson model
rocplot.poisson=roc(Death~fitted(poisson.reg1),data=cov2019)
plot.roc(rocplot.poisson,legacy.axes=TRUE)
auc(rocplot.poisson)
#Area under the curve: 0.8827
#log(death)=-6.971681+0.084142*Age+0.513622*Gender(Male)-0.074541*DAYS

#AUC of negative binomial model
rocplot.neg.bio2=roc(Death~fitted(neg.bio2),data=cov2019)
plot.roc(rocplot.neg.bio2,legacy.axes=TRUE)
auc(rocplot.neg.bio2)
#Area under the curve: 0.885
#log(death)=-9.6504962+0.0846064*Age+0.5132698*Gender(Male)+0.1204673*DAYS-0.0033714*DAYS*DAYS

#conclusion: logit,probit,poisson,negative binomial models
# logit model:   AIC=4006.7, Deviance=3996.7, AUC=0.8833
# probit model:  AIC=4002.6, Deviance=3990.6, AUC=0.8836
# poisson model: AIC=4104.1, Deviance=2936.1, AUC=0.8827
# negative binomial model: AIC= 4080.6,Deviance= 2905.7, AUC=0.8850

library(lme4)
head(cov2019)

# glm model of cov2019
cov.glm=glm(Death~ Age+Gender+Days_after_first_case+Age:Days_after_first_case,
                         data=cov2019,family=binomial(link=logit))
summary(cov.glm)

#glmm model of cov2019 considering county as a random effect
cov.GLMM <-glmer(Death~ Age+Gender+Days_after_first_case+Age:Days_after_first_case +Age:Gender
                 +Gender:Days_after_first_case+Age:Days_after_first_case:Gender+I(Age^2)+I(Days_after_first_case^2)
                 + (1|County),
                  data=cov2019,family=binomial(link=logit))

summary(cov.GLMM)  #I(Age^2)  is insignificant
cov.GLMM1 <--glmer(Death~ Age+Gender+Days_after_first_case+Age:Days_after_first_case +Age:Gender
                   +Gender:Days_after_first_case+Age:Days_after_first_case:Gender+I(Days_after_first_case^2)
                   + (1|County),
                   data=cov2019,family=binomial(link=logit))
summary(cov.GLMM1) # error

cov.GLMM2 <-glmer(Death~ Age+Gender+Days_after_first_case+Age:Days_after_first_case +Age:Gender
                  +Gender:Days_after_first_case+I(Days_after_first_case^2)
                  + (1|County),
                  data=cov2019,family=binomial(link=logit))
summary(cov.GLMM2) # error
anova(cov.GLMM2,cov.GLMM1)
cov.GLMM3 <-glmer(Death~ Age+Gender+Days_after_first_case+Age:Days_after_first_case +Age:Gender
                  +Gender:Days_after_first_case
                  + (1|County),
                  data=cov2019,family=binomial(link=logit))
summary(cov.GLMM3)
cov.GLMM4 <-glmer(Death~ Age+Gender+Days_after_first_case+Age:Days_after_first_case +Age:Gender
                 
                  + (1|County),
                  data=cov2019,family=binomial(link=logit))
summary(cov.GLMM4) # error
cov.GLMM5 <-glmer(Death~ Age+Gender+Days_after_first_case+Age:Days_after_first_case 
                  
                  + (1|County),
                  data=cov2019,family=binomial(link=logit))
summary(cov.GLMM5)
library(pROC)
rocplot.GLMM5=roc(Death~fitted(cov.GLMM5),data=cov2019)
plot.roc(rocplot.GLMM5,legacy.axes=TRUE)
auc(rocplot.GLMM5) #Area under the curve: 0.8885

anova(cov.GLMM5,cov.GLMM3) #p-value=0.3039, fails to reject cov.GLMM5
1-pchisq(2.3818,2) 
anova(cov.GLMM5,cov.GLMM)  #p-value=2.853e-05, rejects cov.GLMM5
1-pchisq(28.54,5) 

step(cov.GLMM,test="Chisq")
drop1(cov.GLMM,test="Chisq")

ranef(cov.GLMM)

##random effect test by LRT test
2*(logLik(cov.GLMM) - logLik(cov.glm))  #'log Lik.' 21.68271 (df=6)
0.5*(1-pchisq(21.68271,df=6))   
#p-value=0.0006910166, reject the null hypothesis that the random effect is not significant

# AUC of GLMM
library(pROC)
rocplot.GLMM=roc(Death~fitted(cov.GLMM),data=cov2019)
plot.roc(rocplot.GLMM,legacy.axes=TRUE)
auc(rocplot.GLMM)
#Area under the curve: 0.8912

##conclusion: logit,probit,poisson,negative binomial and GLMM models
# logit model:   AIC=4006.7, Deviance=3996.7, AUC=0.8833
# probit model:  AIC=4002.6, Deviance=3990.6, AUC=0.8836
# poisson model: AIC=4104.1, Deviance=2936.1, AUC=0.8827
# negative binomial model: AIC= 4080.6,Deviance= 2905.7, AUC=0.8850
# GLMM1 model:    AIC=3968.5, Deviance=3946.5, AUC=0.8912
# GLMM5 model:    AIC=3987, Deviance=3975, AUC=0.8885