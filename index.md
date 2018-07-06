---
title: "Class Notes"
author: "Faraz Ahmad"
output: html_document
---


#Data Preprocessing
```{r}
advertise = read.csv("E:\\Faraz\\Data Science\\Machine Learning\\Advertising.csv")
names(advertise)
#View(advertise)
colSums(is.na(advertise))
nrow(advertise)*0.8
```

#Identify percentage of missing values
```{r}
colSums(is.na(advertise))/nrow(advertise) *100
```

#Checking Outliers
```{r}
boxplot(advertise$TV)
boxplot(advertise$radio)
boxplot(advertise$newspaper) #2 outliers
boxplot(advertise$sales)
boxplot(advertise)
```


#Method 1
```{r}
df_training = advertise[1:162,]
df_testing = advertise[163:200,]
```
#Method 2
```{r}
sample(seq(1,200),162)
sample(seq(1:nrow(advertise)),162)
df_training = advertise[sample(seq(1:nrow(advertise)),162),]
df_testing = advertise[sample(seq(1:nrow(advertise)),38),]
```
#Both training and testing have input and output labels
```{r}
dim(df_training)
```
#Feature Selection
#Use all Input variables --> TV, Newspaper, Radio

#Fit a Model
```{r}
adv_model = lm(sales~TV+radio+newspaper, data = df_training)
adv_model
```
#Mathematical Equation

#Predict Sales= 0.045336TV + 0.18250radio + 0.002084newspaper + 3.009651

#Predict model for Testing Data Set
```{r}
df_testing$sales_pred = predict(adv_model,df_testing[,c('TV','radio','newspaper')])
```


#Calculate Error Row Wise
```{r}
df_testing$error = df_testing$sales - df_testing$sales_pred
View(df_testing)
```

#Calculate Squared Error
```{r}
df_testing$sqr_error = df_testing$error ^2
View(df_testing)

sum(df_testing$sqr_error)
{{plot(df_testing$sales, type = 'l')
  lines(df_testing$sales_pred, col = 'red')}}
```


#BANK DATASET

```{r}
bank = read.csv("E:\\Data Science\\Machine Learning\\bank.csv",sep = ';')
View(bank)
```

#outcome is a Categorical column so we will go for classification analysis
```{r}
nrow(bank)*80/100
bank_training = bank[sample(seq(1:nrow(bank)),3616),]
bank_testing = bank[sample(seq(1:nrow(bank)),4521-3616),]

library(tree)
bank_model = tree(y~.,data = bank_training)
bank_model
{{plot(bank_model)
  text(bank_model)}}
library(dplyr)
probs = as.data.frame(predict(bank_model,bank_testing %>% select(-y)))
probs$predict = if_else(probs$no > probs$yes, 'no','yes')
bank_testing$predicted = probs$predict
bank_testing

sum(bank_testing$y == bank_testing$predicted) #sum of TRUE prediction
sum(bank_testing$y == bank_testing$predicted)/nrow(bank_testing)*100 #percentage of correct prediction
```


```{r}
iris
table(iris)
library(dplyr)
iris_new  =  iris %>% select(-Species)       
View(iris_new)
iris_model = kmeans(iris_new, centers=3)
iris_model$cluster
length(iris_model$cluster)
table(iris_model$cluster)
iris_model = kmeans(iris_new, centers=4)
table(iris_model$cluster)
iris_model


```
#Fit a Model
```{r}
adv_model = lm(sales~TV, data = df_training)
adv_model2 = lm(sales~., data = df_training)
adv_model
```

```{r}
m = 0.01
c = 1

sales_pred = m * df_training$TV + c  #y = mx + c

m = 0.02
c = 1

m = 0.03
c = 1

m = 0.04
c = 1

m = 0.05
c = 1

m = 0.06
m = 0.07
m = 0.08
m = 0.09
m = 0.01:0.10
```

#Mean Squared Error (MSE)
```{r}
error = sum((df_training$sales-sales_pred)^2)/nrow(df_training)
error
```

```{r}
m = seq(0,1, length.out = 100)
#OR
seq.int(0,1,length.out = 100)

#Creating a for loop for predicting 100 Error Values...

for (i in m){
  sales_pred = i * df_training$TV +c
  error = sum((df_training$sales-sales_pred)^2)/nrow(df_training)
  print(error)
}

```
```{r}
#----------------------------------------------------------------------#
#Plotting Error Values
for (i in m){
  sales_pred = i * df_training$TV +c
  error = sum((df_training$sales-sales_pred)^2)/nrow(df_training)
  print(error)

{{plot(df_training$TV,df_training$sales)
  lines(df_training$TV,sales_pred)}}
}  

#----------------------------------------------------------------------#
m = seq(-1,1, length.out = 100)
e = c()

for (i in m){
  sales_pred = i * df_training$TV +c
  error = sum((df_training$sales-sales_pred)^2)/nrow(df_training)
  print(error)
  e = c(e,error)
}

plot(e)

min(e)

which(e==min(e))
m[55]
```

#Now values of "c" will change
```{r}

m = seq(-1,1, length.out = 10)
c = seq(-10,10, length.out = 10)

for (i in m){
  for (j in c){
    print(c(i,j))
  }
}


m = seq(-1,1, length.out = 100)
c = seq(-10,10, length.out = 100)

m_rep = c()
c_rep = c()
e = c()
for (i in m){
  for (j in c){
  sales_pred = i * df_training$TV + j
  error = sum((df_training$sales-sales_pred)^2)/nrow(df_training)
  m_rep = c(m_rep,i)
  c_rep = c(c_rep,j)
  e = c(e,error)
  }
}

which(e == min(e))

models = data.frame(slope = m_rep, intercept = c_rep, mse = e)
models
dim(models)
View(models)
library(dplyr)
#Row in which minimum error is there
models %>% arrange(mse) %>% head(1)

```
#Presenting Data in 3D Graph
```{r}
library(plotly)

mspace = m
cspace = c
zspace = matrix(e,nrow = length(m), ncol = length(c))

plot_ly(x = mspace, y = cspace, z = zspace) %>% add_surface()

```

```{r}
m1 = seq(-1,1, length.out = 10)
m2 = seq(-1,1, length.out = 10)
c1 = seq(-10,10, length.out = 10)

m1_rep = c()
m2_rep = c()
c1_rep = c()
e = c()
for (i in m1){
  for (j in m2){
    for (k in c1){
  sales_pred = i * df_training$TV + j*df_training$radio +k
  error = sum((df_training$sales-sales_pred)^2)/nrow(df_training)
  m1_rep = c(m1_rep,i)
  m2_rep = c(m2_rep,j)
  c_rep = c(c1_rep,k)
  e = c(e,error)
  }
 }
}

```

#GRADIENT DESCENT

```{r}
x = rnorm(100)
y = 0.05*x
df_xy = data.frame(x=x, y=y)
plot(x,y)
cor(x,y)

library(dplyr)

df_xy = df_xy %>% mutate(xy = x*y)
df_xy2 = df_xy %>% mutate(mx2 = m*(x^2))
```

```{r}
m=1000
alpha = 0.2
n_iterations = 1000
errors = c()
m_vals = c()
for (i in seq(1,n_iterations)) {
  m_vals = c(m_vals,m)
  curr_err = sum((y-(m*x))^2)/length(x)
  errors = c(errors,curr_err)
  df_xy = df_xy %>% mutate(xy=x*y)
  df_xy = df_xy %>% mutate(mx2 = m*(x^2))
  df_xy = df_xy %>% mutate(xy_mx2 = xy-mx2)
  sigma_xy_mx2 = sum(df_xy$xy_mx2)
  m_gradient = -2/length(x)*sigma_xy_mx2
  m = m-alpha*m_gradient
}
print(m)
plot(errors)


{{plot(m_vals,errors)
  lines(m_vals,errors)}}
```

#ADVERTISING DATASET
```{r}
advertise = read.csv("E:\\Faraz\\Data Science\\Machine Learning\\Advertising.csv")
adv_training = advertise[1:162,]
adv_testing = advertise[163:200,]
lm(sales~TV, data = adv_training)

x = scale(adv_training$TV)
y = adv_training$sales
df_xy = data.frame(x=scale(x),y=y)
m=10
alpha = 0.01
n_iterations = 100
c = 10
errors = c()
m_vals = c()
for(i in seq(1,n_iterations)) {
  m_vals = c(m_vals,m)
  curr_err = sum((y-(m*x))^2)/length(x)
  errors = c(errors,curr_err)
  df_xy = df_xy %>% mutate(xy=x*y)
  df_xy = df_xy %>% mutate(mx2 = m*(x^2))
  df_xy = df_xy %>% mutate(xy_mx2 = xy-mx2)
  df_xy = df_xy %>% mutate(y_mx_c = y-m*x-c)
  sigma_xy_mx2 = sum(df_xy$xy_mx2)
  sigma_y_mx_c = sum(df_xy$y_mx_c)
  m_gradient = -2/length(x)*sigma_xy_mx2
  c_gradient = -2/length(x)*sigma_y_mx_c
  m = m-alpha*m_gradient
  c = c-alpha*c_gradient
}
print(m)
print(c)
```

#Code for Surface Plot


```{r}
m = -10
c=0
a=0.01
error_vals = c()
m_vals = c()
c_vals = c()
x = adv$TV
y = adv$sales
test = data.frame(x = scale(x),y=y)

for (i in seq(1,1000)){
  df_xy = mutate(test, yhat2 = (y-m*x-c)^2)
  error_vals = c(error_vals,1/length(x)*sum(test$yhat2))
  m_vals = c(m_vals,m)
  c_vals = c(c_vals,c)
  df_xy = df_xy %>% mutate(xy=x*y)
  df_xy = df_xy %>% mutate(mx2 = m*(x^2))
  df_xy = df_xy %>% mutate(xy_mx2 = xy-mx2)
  df_xy = df_xy %>% mutate(y_mx_c = y-m*x-c)
  sigma_xy_mx2 = sum(df_xy$xy_mx2)
  sigma_y_mx_c = sum(df_xy$y_mx_c)
  m_gradient = -2/length(x)*sigma_xy_mx2
  c_gradient = -2/length(x)*sigma_y_mx_c
  m = m-alpha*m_gradient
  c = c-alpha*c_gradient
}
```
```{r}
library(plotly)
library(rgl)
open3d()
plot3d(x=m_vals,y=c_vals,z=error_vals)

plotly(x=m_vals,y=c_vals,z=error_vals)





#install.packages("rgl")
library(rgl)
open3d()

plot3d(x=m_vals,y=c_vals,z=errors)


###################
m<-seq(-1,1,length.out = 100)
c<-seq(0,10,length.out = 100)
e<-c()
m_rep=c()
c_rep=c()
for (i in m) {
for (j in c) {
sales_predicted<-i*adv_training$TV+j
error = sum((adv_training$sales - sales_predicted)^2) / nrow(adv_training)
m_rep = c(m_rep,i)
c_rep = c(c_rep,j)
e=c(e,error)  
}
}
open3d()
plot3d(x=m_rep,y=c_rep,z=e,col=rainbow(100))

```

#Decision Trees
```{r}
library(tree)
model_tree = tree(sales~TV, data=advertise)
{{plot(model_tree)
  text(model_tree)}}

```


```{r}
adv %>% filter(TV<122.05) %>% summarise(mean(sales))
adv %>% filter(TV>122.05) %>% summarise(mean(sales))
sort(adv$TV)
adv
adv %>% filter(TV<134.75) %>% summarise(count = n())
adv %>% filter(TV<134.75) %>% summarise(mean(sales))
adv %>% filter(TV>134.75) %>% summarise(mean(sales))
```

#Getting Values of Cuts
```{r}
TV_uniqs = sort(unique(adv$TV))
for (i in seq(1, length(TV_uniqs)-1)){
  curr_cut = (TV_uniqs[i]+TV_uniqs[i+1])/2
  print(curr_cut)
}
```
```{r}
cuts = c()
mses = c()
TV_uniqs = sort(unique(adv$TV))
for (i in seq(1, length(TV_uniqs)-1)){
  curr_cut = (TV_uniqs[i]+TV_uniqs[i+1])/2
  cuts = c(cuts,curr_cut)
  samples_left = adv %>% filter(TV<curr_cut)
  samples_right = adv %>% filter(TV>curr_cut)
  avg_left = mean(samples_left$sales)
  avg_right = mean(samples_right$sales)
  adv$predicted_sales = if_else(adv$TV<curr_cut,avg_left,avg_right)
  curr_mse = sum((adv$sales-adv$predicted_sales)^2)/nrow(adv)
  mses = c(mses,curr_mse)
}

models_perf = data.frame(TV_cut = cuts,MSE = mses)
models_perf %>% arrange(MSE) %>% head(1) #value with the least error is 122.05
```

#Getting <>Cuts with filtered Cut of 122.05
```{r}
adv_temp = adv %>% filter(TV<122.05)
cuts = c()
mses = c()
TV_uniqs = sort(unique(adv$TV))
for (i in seq(1, length(TV_uniqs)-1)){
  curr_cut = (TV_uniqs[i]+TV_uniqs[i+1])/2
  cuts = c(cuts,curr_cut)
  samples_left = adv_temp %>% filter(TV<curr_cut)
  samples_right = adv_temp %>% filter(TV>curr_cut)
  avg_left = mean(samples_left$sales)
  avg_right = mean(samples_right$sales)
  adv_temp$predicted_sales = if_else(adv_temp$TV<curr_cut,avg_left,avg_right)
  curr_mse = sum((adv_temp$sales-adv_temp$predicted_sales)^2)/nrow(adv)
  mses = c(mses,curr_mse)
}

models_perf = data.frame(TV_cut = cuts,MSE = mses)
models_perf %>% arrange(MSE) %>% head(1)
```

```{r}
adv_temp = adv %>% filter(TV>122.05)
cuts = c()
mses = c()
TV_uniqs = sort(unique(adv$TV))
for (i in seq(1, length(TV_uniqs)-1)){
  curr_cut = (TV_uniqs[i]+TV_uniqs[i+1])/2
  cuts = c(cuts,curr_cut)
  samples_left = adv_temp %>% filter(TV<curr_cut)
  samples_right = adv_temp %>% filter(TV>curr_cut)
  avg_left = mean(samples_left$sales)
  avg_right = mean(samples_right$sales)
  adv_temp$predicted_sales = if_else(adv_temp$TV<curr_cut,avg_left,avg_right)
  curr_mse = sum((adv_temp$sales-adv_temp$predicted_sales)^2)/nrow(adv)
  mses = c(mses,curr_mse)
}

models_perf = data.frame(TV_cut = cuts,MSE = mses)
models_perf %>% arrange(MSE) %>% head(1)
```

#_--------------------------------------------------_#
#07-05-2018

##Decision Trees with more than one input predictor
```{r}
adv = read.csv("E:\\Faraz\\Data Science\\Machine Learning\\Advertising.csv")
model = tree(sales~., data = adv)
{{plot(model)
  text(model)}}
```

##Cuts for TV
```{r}
TV_uniqs = sort(unique(adv$TV))
length(TV_uniqs)
TV_uniqs[1:10]
cut = (0.7+4.1)/2

cuts_TV = (TV_uniqs[1:length(TV_uniqs)-1]+
             TV_uniqs[2:length(TV_uniqs)])/2
length(cuts_TV)
```
##Cuts for Radio
```{r}
Radio_uniqs = sort(unique(adv$radio))
length(Radio_uniqs)


Radio_cuts = (Radio_uniqs[1:length(Radio_uniqs)-1]+
             Radio_uniqs[2:length(Radio_uniqs)])/2
length(Radio_cuts)
```
##Cuts for Newspaper
```{r}
Newspaper_uniqs = sort(unique(adv$newspaper))
length(Newspaper_uniqs)


Newspaper_cuts = (Newspaper_uniqs[1:length(Newspaper_uniqs)-1]+
             Newspaper_uniqs[2:length(Newspaper_uniqs)])/2
length(Newspaper_cuts)
```
#For each cut calculate MSE value
#Method1
```{r}
tv_cuts_mse = c()
library(dplyr)
temp=adv%>% filter(TV > 122.05 & 194.55,radio>26.85)
for (cut in cuts_TV){
  samples_left = adv %>% filter(TV<cut)
  samples_right = adv %>% filter(TV>cut)
  pred_left = mean(samples_left$sales)
  pred_right = mean(samples_right$sales)
  temp$pred = ifelse(temp$TV<cut,pred_left,pred_right)
  curr_mse = sum((temp$sales-temp$pred)^2)/nrow(temp)
  tv_cuts_mse = c(tv_cuts_mse,curr_mse)
}

radio_cuts_mse = c()
library(dplyr)
#temp=adv
for (cut in Radio_cuts){
  samples_left = adv %>% filter(radio<cut)
  samples_right = adv %>% filter(radio>cut)
  pred_left = mean(samples_left$sales)
  pred_right = mean(samples_right$sales)
  temp$pred = ifelse(temp$radio<cut,pred_left,pred_right)
  curr_mse = sum((temp$sales-temp$pred)^2)/nrow(temp)
  radio_cuts_mse = c(radio_cuts_mse,curr_mse)
}

newspaper_cuts_mse = c()
library(dplyr)
#temp=adv
for (cut in Newspaper_cuts){
  samples_left = adv %>% filter(newspaper<cut)
  samples_right = adv %>% filter(newspaper>cut)
  pred_left = mean(samples_left$sales)
  pred_right = mean(samples_right$sales)
  temp$pred = ifelse(temp$newspaper<cut,pred_left,pred_right)
  curr_mse = sum((temp$sales-temp$pred)^2)/nrow(temp)
  newspaper_cuts_mse = c(newspaper_cuts_mse,curr_mse)
}

result_TV = data.frame(column = rep('TV',length(cuts_TV)),
                       cut = cuts_TV,
                       mse = tv_cuts_mse)

result_Radio = data.frame(column = rep('Radio',length(Radio_cuts)),
                       cut = Radio_cuts,
                       mse = radio_cuts_mse)

result_Newspaper = data.frame(column = rep('Newspaper',length(Newspaper_cuts)),
                       cut = Newspaper_cuts,
                       mse = newspaper_cuts_mse)

result = rbind(result_TV,result_Radio,result_Newspaper)
result

result %>% arrange(mse) %>% head(1) #cut for TV 122.05 is the best
#radio with cut 26.85 is the best split

nrow(adv %>% filter(TV<122.05)) #83
nrow(adv %>% filter(TV>122.05)) #117



```

#Method2
```{r}
cuts = c(cuts_TV,Radio_cuts,Newspaper_cuts)
predictors = c(rep('TV',length(cuts_TV)),rep('Radio',length(Radio_cuts)),
                 rep('Newspaper',length(Newspaper_cuts)))
result = data.frame(predictors,cuts)

temp = adv
cuts_mse = c()
for (i in seq(1, length(cuts))){
  cut = cuts[i]
  curr_col = predictors[i]
  samples_left = temp[temp[, curr_col]<cut,]  
  samples_right = temp[temp[, curr_col]>cut,]
  pred_left = mean(samples_left$sales)
  pred_right = mean(samples_right$sales)
  temp$pred = ifelse(temp[,curr_col]<cut, pred_left, pred_right)
  curr_mse = sum((temp$sales-temp$pred)^2)/nrow(temp)
  cuts_mse = c(cuts_mse, curr_mse)
}
result$mse = cuts_mse
View(result)
result %>% arrange(mse) %>% head(1)
View(temp[, curr_col]<cut)
```
```{r}
temp = adv #%>% filter(TV>194.55 & radio >26.85)
df = data.frame(TV=temp$TV, radio=temp$radio, y=temp$sales)
tv_uniqs = sort(unique(df$TV))
tv_cuts = (tv_uniqs[1:length(tv_uniqs)-1] + tv_uniqs[2:length(tv_uniqs)]) / 2
radio_uniqs = sort(unique(df$radio))
radio_cuts = (radio_uniqs[1:length(radio_uniqs)-1] + radio_uniqs[2:length(radio_uniqs)]) / 2

predictors = c(rep('TV', length(tv_cuts)),
               rep('radio', length(radio_cuts)))
cuts = c(tv_cuts, radio_cuts)
vardev_cuts = c()
for (i in seq(1, length(cuts))){
  cut = cuts[i]
  samples_left = df[df[,predictors[i]]<cut, ]
  samples_right = df[df[,predictors[i]]>cut, ]
  avg_left = mean(samples_left$y)
  avg_right = mean(samples_right$y)
  
  curr_vardev = var(df$y) - (nrow(samples_left)/nrow(df))*var(samples_left$y) -
    (nrow(samples_right)/nrow(df))*var(samples_right$y)
  vardev_cuts = c(vardev_cuts, curr_vardev)
}
result = data.frame(predictor=predictors, cuts=cuts, vardev=vardev_cuts)
View(result)
result %>% arrange(-vardev) %>% head(1)
```
##Classification Techniques

```{r,fig.width=11,fig.height=6}
hr = read.csv("E:\\Faraz\\Data Science\\Machine Learning\\HR Analytics.csv")
library(rattle)
hr$Attrition = as.factor(hr$Attrition)
hr_train = hr[1:(0.7*nrow(hr)),]
hr_test = 1-hr_train

model = tree(Attrition~.,data=hr_train)

{{plot(model)
  text(model)}}
library(rpart)
model = rpart(Attrition~OverTime, data=hr)
fancyRpartPlot(model)
```

#--------------------------------------------------------------------------------------#
#08-05-2018

#GINI IMPURITY....
###Categorical with two classes

```{r}
nrow(hr_train)
left_overtime = hr_train %>% filter(OverTime=='Yes')
right_overtime = hr_train %>% filter(OverTime=='No')
nrow(left_overtime)
nrow(right_overtime)
table(left_overtime$Attrition)
1-(96/303)^2 - (207/303)^2
table(right_overtime$Attrition)
1-(76/726)^2 - (650/726)^2
```

```{r}
View(hr_train)
left_gender = hr_train %>% filter(Gender=='Male')
right_gender = hr_train %>% filter(Gender=='Female')
nrow(left_gender) #598
nrow(right_gender) #431
table(left_gender$Attrition)
1-(105/598)^2 - (493/598)^2
table(right_gender$Attrition)
1-(67/431)^2 - (364/431)^2

(598/1029)*0.28 + (431/1029)*0.26

```
#For Whole Dataset
```{r}
table(hr_train$Attrition)
1-(857/nrow(hr_train))^2 - (172/nrow(hr_train))^2

model = tree(Attrition~OverTime+Gender,data = hr_train)
{{plot(model)
  text(model)}}
```

#Marital Status Combination
```{r}
marital_status_uniq = unique(hr_train$MaritalStatus)

for(status in marital_status_uniq){
  print(status)
  samples_left = hr_train %>% filter(MaritalStatus==status)
  samples_right = hr_train %>% filter(MaritalStatus!=status)
}



```
```{r}
marital_status_uniq = unique(hr_train$MaritalStatus)

for(status in marital_status_uniq){
  print(status)
  samples_left = hr_train %>% filter(MaritalStatus==status)
  samples_right = hr_train %>% filter(MaritalStatus!=status)
  p_0_left = nrow(samples_left %>% filter(Attrition==0))/nrow(samples_left)
  p_1_left = nrow(samples_left %>% filter(Attrition==1))/nrow(samples_left)
  gi_left = 1-p_0_left^2 - p_1_left^2
  
  p_0_right = nrow(samples_right %>% filter(Attrition==0))/nrow(samples_right)
  p_1_right = nrow(samples_right %>% filter(Attrition==1))/nrow(samples_right)
  gi_right = 1-p_0_right^2 - p_1_right^2
  
  gi_status = nrow(samples_left)/nrow(hr_train)*gi_left +
    nrow(samples_right)/nrow(hr_train) * gi_right
  temp = marital_status_uniq[marital_status_uniq!=status]
  print('left node')
  print('status')
  print('right node')
  print(temp)
  print(gi_status)
  print('--------------------------------')
}
```

#Job Category Combination
```{r}
x = c('a','b','c','d')
combn(x,2,simplify = F)


jobs_uniq = unique(hr_train$JobRole) 
combinations_left = c()
combinations_right = c()
gi_all = c()
for (n in c(1,2,3,4)){
  comb_n = combn(jobs_uniq, n, simplify = F)
  for(i in seq(1,length(comb_n))){
    comb_left = comb_n[[i]]
    comb_right = jobs_uniq[!jobs_uniq %in% comb_left]
    
    samples_left = hr_train %>% filter(JobRole %in% comb_left)
    samples_right = hr_train %>% filter(JobRole %in% comb_right)
    
    p0_left = nrow(samples_left %>% filter(Attrition==0))/nrow(samples_left)
    p1_left = nrow(samples_left %>% filter(Attrition==1))/nrow(samples_left)
    gi_left = 1 - p0_left^2 - p1_left^2
    
    p0_right = nrow(samples_right %>% filter(Attrition==0))/nrow(samples_right)
    p1_right = nrow(samples_right %>% filter(Attrition==1))/nrow(samples_right)
    gi_right = 1 - p0_right^2 - p1_right^2
    
    gi_status = nrow(samples_left)/nrow(hr_train)*gi_left + nrow(samples_right)/nrow(hr_train) * gi_right
    
    combinations_left = c(combinations_left, paste0(comb_left,collapse=','))
    combinations_right = c(combinations_right, paste0(comb_right, collapse = ','))
    gi_all = c(gi_all, gi_status)
  }  
}


result = data.frame(left=combinations_left, right=combinations_right, gi=gi_all)

result %>% arrange(gi) %>% head(1)
model = rpart(Attrition~JobRole, data=hr_train)
levels(hr_train$JobRole)
{{plot(model)
  text(model)}}
comb_right

```

```{r}
month<-sort(unique(hr_train$MonthlyIncome))
split<-(month[1:length(month)-1]+month[2:length(month)])/2
splits<-c()
gi<-c()
for (cut in split) {
  left<-hr_train%>%filter(MonthlyIncome<cut)
  right<-hr_train%>%filter(MonthlyIncome>cut)
  p0_left<-nrow(left%>%filter(Attrition==0))/nrow(left)
  p1_left<-nrow(left%>%filter(Attrition==1))/nrow(left)
  gi_left<-1-p0_left^2-p1_left^2
  p0_right<-nrow(right%>%filter(Attrition==0))/nrow(right)
  p1_right<-nrow(right%>%filter(Attrition==1))/nrow(right)
  gi_right<-1-p0_right^2-p1_right^2
  gi_status=nrow(left)/nrow(hr_train)*gi_left+nrow(right)/nrow(hr_train) *gi_right
  splits<-c(splits,cut)
  gi<-c(gi,gi_status)
}
result<-data.frame(splits,gi)
result
result%>%arrange(gi)%>%head(1)

m2<-rpart(Attrition~MonthlyIncome,data = hr_train)
{{plot(m2)
  text(m2)}}
fancyRpartPlot(m2)

```
```{r}
table(hr_train$Attrition)
samples_left = hr_train %>% filter(OverTime=='No')
samples_right = hr_train %>% filter(OverTime=='Yes')
nrow(samples_left)
nrow(samples_right)
```

------------------------------------------------
#14-05-2018

job_uniqs = unique(hr$JobRole)
temp = hr
for (n in seq(1,4)){
for (i in combn(job_uniqs,n,simplify=F)){
jobs_left = i[[1]]
samples_left = temp %>% filter(JobRole %in% jobs_left)
samples_left = temp %>% filter(!JobRole %in% jobs_left)
Nl = nrow(samples_left)
Nr = nrow(samples_right)

GI_left = 1-(nrow(samples_left %>% filter(Attrition==0))/Nl)^2-
nrow(samples_left %>% filter(Attrition==1)/Nr)^2

GI_right =  1-(nrow(samples_right %>% filter(Attrition==0))/Nl)^2-
nrow(samples_right %>% filter(Attrition==1)/Nr)^2

GI = (Nl/N*GI_left)+(N2/N*GI_right)
GIs = c(GIs,GI)
jobs_left_all = c(jobs_left_all, paste0)
}
}

----------------------------------------------------------------------
#15-05-2018


```{r}
hr = read.csv("E:\\Faraz\\Data Science\\Machine Learning\\HR Analytics.csv")
hr$Attrition = as.factor(hr$Attrition)
model = rpart(Attrition~Gender+MonthlyIncome+OverTime,data=hr)
fancyRpartPlot(model)

```

```{r}
hr = read.csv("E:\\Faraz\\Data Science\\Machine Learning\\HR Analytics.csv")
hr$Attrition = as.factor(hr$Attrition)
hr_train = hr[sample(seq(1,nrow(hr)),(0.7*nrow(hr))),]
hr_test = hr[sample(seq(1,nrow(hr)),(0.3*nrow(hr))),]
dim(hr_train)
dim(hr_test)
model = rpart(Attrition~Gender+MonthlyIncome+OverTime,data=hr_train,method="class")
fancyRpartPlot(model)
result =as.data.frame(predict(model,hr_test))


hr_test$predict=ifelse(result$`0`>0,0.1)
View(result)
hr[1189,c('Gender','MonthlyIncome','OverTime','Attrition')]
fancyRpartPlot(model)
View(result)
hr_test$predict= ifelse(result$'0'>0.5,0,1)
View(hr_test[,c('Attrition','predict')])

hr_test %>% filter(predict==Attrition) %>% nrow()  #370
nrow(hr_test) #441

Accuracy = 370/nrow(hr_test) #0.8390023

table(hr$Attrition)
```

```{r}
table(hr_test$predict,hr_test$Attrition)
hr_test %>% filter(Attrition==1,predict==1) %>% nrow()

hr_test$predict = as.factor(hr_test$predict)
#Confusion Matrix
cm = confusionMatrix(hr_test$predict,hr_test$Attrition,positive = '1')
cm
```

```{r}
model = rpart(Attrition~JobRole,data = hr)
model
fancyRpartPlot(model)

model = rpart(Attrition~Gender+MonthlyIncome+OverTime,data=hr, control = rpart.control(cp=-1))
fancyRpartPlot(model)
```

```{r}
model = rpart(Attrition~Gender+MonthlyIncome+OverTime,data=hr, control = rpart.control(cp=-1,minsplit = 2,minbucket = 1))
fancyRpartPlot(model)
```
----------------------------------------------------------------------
#16-05-2018

##Random Forest##
```{r}
model= rpart(Attrition~., data = hr_train)
result = as.data.frame(predict(model,hr_test))
hr_test$predict = ifelse(result$'0'>0.5,0,1)
hr_test$predict = as.factor(hr_test$predict)
confusionMatrix(hr_test$predict, hr_test$Attrition, positive = '1')

table(hr_train$Attrition)
877/nrow(hr_train) #0.85228389
nrow(hr_train)
```

```{r}
mtry = round(sqrt(length(colnames(hr_train))-1))
model_rf = randomForest(Attrition~., data = hr_train, ntree = 400,mtry = mtry)

hr_test$predicted = predict(model_rf, hr_test)
cm = confusionMatrix(hr_test$predicted, hr_test$Attrition, positive = '1')

accuracy = cm$overall['Accuracy']*100

cm$byClass['Sensitivity']*100

```

```{r}
input_predictors = colnames(hr_train %>% select(-Attrition))
length(input_predictors) #34
mtry = round(sqrt(length(input_predictors))) #6
#ntree = 10
ntree = 400
result = data.frame(actual = hr_test$Attrition)
for (i in seq(1,ntree)){
  sample_predictors = input_predictors[sample(1:length(input_predictors),mtry)]
  samples_index = sample(seq(1,nrow(hr_train)),(0.6*nrow(hr_train)))
  sample_data = hr_train[samples_index,c(sample_predictors,'Attrition')]
  curr_model = rpart(Attrition~., data = sample_data)
  result[,paste0('tree_',i,collapse = '')] = predict(curr_model,hr_test %>%
                                                        select(sample_predictors),type = 'class')
}
#input_predictors[sample(1:length(input_predictors),mtry)]
#View(hr_test %>% select(sample_predictors))
View(result)

result$count_0 = rowSums((result %>% select(-actual))==0)
result$count_1 = rowSums((result %>% select(-actual))==1)
result$final = ifelse(result$count_0>result$count_1,0,1)

dim(result)
table(result$final,result$actual)
```

```{r}
model_rf
```
------------------------------------------------------------------
#17-05-2018

##K Nearest Algorithm

```{r}
iris_train= iris[sample(seq(1,nrow(iris)),(0.7*nrow(iris))),]
iris_test=iris[sample(seq(1,nrow(iris)),(0.3*nrow(iris))),]
k = 11
predictors = c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")
iris_train$dist = 0
iris_test$pred = 'random'
for (i in seq(1, nrow(iris_test))){
  p1 = iris_test[i,predictors]
  for (j in seq(1,nrow(iris_train))){
    p2 = iris_train[j,predictors]
    calc_dist = dist(rbind(p1,p2))
    iris_train[j,'dist']= calc_dist
  }
   nn = iris_train %>% arrange(dist) %>% head(k)
   nn_poll = table(nn$Species)
   iris_test[i,'pred'] = names(nn_poll)[which.max(nn_poll)]
  
}

sum(iris_test$Species==iris_test$pred)/nrow(iris_test)
```

KNN

```{r}
hr = read.csv("E:\\Faraz\\Data Science\\Machine Learning\\HR Analytics.csv")
library(caret)

##Convert categorical columns to Numerical columns
dummy_obj = dummyVars(~., data = hr %>% select(-Over18))
hr_new = data.frame(predict(dummy_obj, newdata = hr))

#Normalizing
library(BBmisc)
hr_norm = normalize(hr_new, method = 'range', range = c(0,1))
hr_train = hr_norm[sample(seq(1,nrow(hr_norm)),(0.7*nrow(hr_norm))),]
hr_test = hr_norm[sample(seq(1,nrow(hr_norm)),(0.3*nrow(hr_norm))),]

#Model
hr_test$predict = knn(hr_train, hr_test, cl = as.factor(hr_train$Attrition),k=1)
hr_test$Attrition = as.factor(hr_test$Attrition)
hr_test$predict = as.factor(hr_test$predict)
confusionMatrix(hr_test$predict, hr_test$Attrition, positive = '1')


```

```{r}
hr_train = hr_norm[sample(seq(1,nrow(hr_norm)),(0.7*nrow(hr_norm))),]
hr_test = hr_norm[sample(seq(1,nrow(hr_norm)),(0.3*nrow(hr_norm))),]
nn_accuracy = c()
nn_error_rate = c()
k_trials = seq(1,50)
for (k in k_trials){
pred_class = knn(hr_train %>% select (-Attrition),
                 hr_test %>% select(-Attrition),
                 cl = as.factor(hr_train$Attrition),
                 k=k)
hr_test$Attrition = as.factor(hr_test$Attrition)
pred_class = as.factor(pred_class)
cm = confusionMatrix(pred_class,hr_test$Attrition, positive = '1')
nn_accuracy = c(nn_accuracy, cm$overall['Accuracy'])
e_rate = 1-cm$overall['Accuracy']
nn_error_rate = c(nn_error_rate,e_rate)
}
plot(k_trials, nn_error_rate, type = 'l')

```

#Naive Bayes
```{r}
library(e1071)
hr = read.csv("E:\\Data Science\\Machine Learning\\HR Analytics.csv")
hr_train = hr[sample(seq(1,nrow(hr_norm)),(0.7*nrow(hr_norm))),]
model = naiveBayes(Attrition~Gender, data = hr_train)
View(predict(model,hr_test,type = 'raw'))
set.seed(100)
hr_train %>% filter(Attrition==1,Gender == 'Female') %>% nrow()
```

```{r}
library(e1071)
hr = read.csv("E:\\Data Science\\Machine Learning\\HR Analytics.csv")
hr_train = hr[sample(seq(1,nrow(hr_norm)),(0.7*nrow(hr_norm))),]
model = naiveBayes(Attrition~JobRole, data = hr_train)
View(predict(model,hr_test,type = 'raw'))
set.seed(100)
hr_train %>% filter(Attrition==1,Gender == 'Female') %>% nrow()
unique(hr_train$JobRole)
hr_train %>% filter(Attrition==1) %>% nrow()
```

----------------------------------------------------------------------
#18-05-2018

##Clustering
```{r}
odi = read.csv("E:\\Data Science\\EDA\\eda assignment 2\\dataset\\odi-batting.csv")
odi$century = ifelse(odi$Runs>99, 1,0)
odi$ducks = ifelse(odi$Runs==0,1,0)
odi$above_150 = ifelse(odi$Runs>149,1,0)
odi$fifties = ifelse(odi$Runs>49&odi$Runs<100,1,0)
odi$missed_centuries = ifelse(odi$Runs>90&odi$Runs<100,1,0)
View(odi)
```

```{r}
players_summary = odi %>% group_by(Player) %>% summarise(
  matches = n(),
  total_runs=sum(Runs, na.rm=T),
  avg_runs=mean(Runs, na.rm=T),
  centuries=sum(century, na.rm=T),
  ducks=sum(ducks, na.rm=T),
  fifties=sum(fifties, na.rm=T),
  above_150=sum(above_150, na.rm=T),
  missed_centuries=sum(missed_centuries, na.rm=T)
)
View(players_summary)
```

```{r}
top_players = players_summary %>% arrange(-total_runs) %>% head(100)
data_kmeans = top_players %>% select(-Player)
data_norm = normalize(data_kmeans,method = 'range',range = c(0,1))
model_kmeans = kmeans(data_norm,centers = 10)
top_players$cluster = model_kmeans$cluster
barplot(table(top_players$cluster))
View(data_norm)

```
#Cluster Characteristics

```{r}
model_kmeans$centers
```
#Between square sums
```{r}
model_kmeans$withinss
model_kmeans$betweenss
model_kmeans$totss
```

```{r}
View(data_norm)
dim(data_norm)
dim(as.matrix(dist(data_norm)))
data_norm_2d = cmdscale(dist(data_norm))
plot(data_norm_2d)
```

```{r}
data_norm_2d = as.data.frame(data_norm_2d)
data_norm_2d$cluster = model_kmeans$cluster
ggplot(data_norm_2d, aes(x = V1,y = V2,color = cluster))+geom_point()

```
```{r}
hr_sub=hr %>% select(c(Age,MonthlyIncome))
hr_norm=normalize(hr_sub)
model_hr=kmeans(hr_norm,centers = 3)
hr_sub$cluster=model_hr$cluster

ggplot(data = hr_sub,aes(x=Age,y=MonthlyIncome,color=cluster))+geom_point()

```

#Hierarchial Clustering
```{r}
names(data_norm)
hclust_model = hclust(dist(data_norm))
plot(hclust_model)
```

##Get cluster labels
```{r}
data_norm_2d$cluster=cutree(hclust_model,k=4)
ggplot(data_norm_2d,aes(x=V1,y=V2,color=cluster))+geom_point()

```

```{r}
library(corrplot)
cor_players = cor(t(data_norm %>% head(10)))
corrplot(cor_players,order = 'hclust', addrect = 2)
```

-------------------------------------------------------------------------------
#21-05-2018

##                                 Market Basket Analysis                                   ##
#Recommenders
```{r}
data("Groceries")
class(Groceries) #transactions

inspect(Groceries[1]) #citrus fruit,semi-finished bread,margarine,ready soups
inspect(Groceries[2]) #tropical fruit,yogurt,coffee
Groceries

```

```{r}
model = apriori(Groceries,parameter = list(support = 0.01,confidence = 0.2))
inspect(model[1:50],by = 'lift')
```

```{r}
baskets = list(c('a','b','c'),c('a','c','d'),c('e','a'),c('a','b'))
baskets_trans = as(baskets,'transactions')
baskets_trans
summary(baskets_trans)
```
```{r}
itemFrequencyPlot(Groceries)
```

```{r}
itemFrequencyPlot(Groceries,topN = 10)
```

```{r}
itemFrequencyPlot(baskets_trans)
```

```{r}
model = apriori(baskets_trans,parameter = list(support = 0,confidence = 0))
```

```{r}
summary(model)
```

```{r}
inspect(model)
```

```{r}
model = apriori(Groceries,parameter = list(support = 0.01,confidence = 0.2))
inspect(sort(model,decreasing = T,by = 'lift')[1:10])
```
```{r}
movies = read.csv("E:\\Data Science\\Machine Learning\\movies\\movies.csv")
ratings = read.csv("E:\\Data Science\\Machine Learning\\movies\\ratings.csv")
library(knitr)
kable(head(movies))
kable(head(ratings))
length(unique(ratings$movieId))
library(reshape2)
rating_matrix = dcast(ratings,userId~movieId, value.var = 'rating')

dim(rating_matrix)
cor(t(rating_matrix[1:2,]))

library(LSAfun)
cosine(t(rating_matrix[1:2,]))
View(rating_matrix[1:5,1:10])
View(rating_matrix[1:15,1:15])
library(recommenderlab)
rank_matrix = as(as.matrix(rating_matrix[,-1]),'realRatingMatrix')
rank_matrix

model = Recommender(rank_matrix,method = "UBCF",param = list(method="Cosine",nn=30))
#nn --> no. of neighbours to check
summary(model)

#Predict for 1st User:
result = predict(model,rank_matrix[1,],n=10)#1st row vs all columns, top 10 movies
as(result,'list') #predicted Movie IDs for User1
```
#Predicting the title of the movie for a User
```{r}
movies_rec = as.numeric(as(result,'list')[[1]])
movies %>% filter(movieId %in% movies_rec) %>% select(title)
```

#Creating a function for predicting movies for every User
```{r}
recommend_movies = function(model,userid){
  result = predict(model,rank_matrix[userid,],n=10)
  movies_rec = as.numeric(as(result,'list')[[1]])
  return_movies = movies %>% filter(movieId %in% movies_rec) %>% select(title)
  return(return_movies)
}
recommend_movies(model,90)
```
#Item Based Collaborative Filtering
```{r}
rating_matrix = dcast(ratings,movieId~userId, value.var = 'rating')
rank_matrix = as(as.matrix(rating_matrix[,-1]),'realRatingMatrix')
model = Recommender(rank_matrix,method = "IBCF",param = list(method="Cosine"))
result = predict(model,rank_matrix[1,],n=10)
movies_rec = as.numeric(as(result,'list')[[1]])
movies %>% filter(movieId %in% movies_rec) %>% select(title)
```

#20-06-2018

```{r}
##Random Forest##
library(randomForest)
library(dplyr)
hr$Attrition <- as.factor(hr$Attrition)
set.seed(100)
hr_train = hr[1:(0.7*nrow(hr)),]
hr_test = hr[(0.7*nrow(hr)+1):nrow(hr),]

model= randomForest(Attrition~., data = hr_train)
result = predict(model,hr_test, type= "prob")
hr_test$predict = ifelse(result[,2]>0.5,1,0)
hr_test$predict = as.factor(hr_test$predict)
confusionMatrix(hr_test$predict, hr_test$Attrition, positive = '1')

table(hr_train$Attrition)
877/nrow(hr_train) #0.85228389
nrow(hr_train)
```

```{r}
hr_train$Attrition = as.factor(hr_train$Attrition)
hr_test$Attrition = as.factor(hr_test$Attrition)
model_rf = randomForest(Attrition~., data = hr_train)
pred_probs = predict(model_rf,hr_test,type = 'prob')
#View(pred_probs)

hr_test$pred_class = as.factor(ifelse(pred_probs[,2]>0.5,1,0))
View(hr_test)

confusionMatrix(hr_test$pred_class,hr_test$Attrition, positive = '1')

```

##ROC Curves
```{r}
library(ROCR)
library(pROC)
x = roc(hr_test$Attrition, result[,2])
plot(x)
```

```{r}
x$thresholds #for parameter 1

```


```{r}
dummy_obj = dummyVars(~., data = hr %>% select(-Over18))
hr_new = data.frame(predict(dummy_obj, newdata=hr))
hr_norm = normalize(hr_new, method = 'range', range = c(0,1))
hr_train = hr[1:(0.7*nrow(hr)),]
hr_test = hr[(0.7*nrow(hr)+1):nrow(hr),]
hr_train$Attrition = as.factor(hr_train$Attrition)
hr_test$Attrition = as.factor(hr_test$Attrition)
library(class)
hr_test$predict = knn(hr_train,hr_test, cl = as.factor(hr_train$Attrition),k=1)
hr_test$Attrition = as.factor(hr_test$Attrition)
hr_test$predict = as.factor(hr_test$predict)
confusionMatrix(hr_test$predict,hr_test$Attrition,positive = "1")

nn_Accuracy = c()
nn_error_rate =c()
k_trials = seq(1,50)
for(k in k_trials){
  predict_class = knn(hr_train %>% select(-Attrition),hr_test %>% select(-Attrition), cl = as.factor(hr_train$Attrition),k=k)
  hr_test$Attrition = as.factor(hr_test$Attrition)
  predict_class = as.factor(predict_class)
  cm = confusionMatrix(predict_class,hr_test$Attrition,positive = "1")
  acc = cm$overall['Accuracy']
  nn_Accuracy = c(nn_Accuracy,acc)
  e_rate = 1 - acc
  nn_error_rate = c(nn_error_rate,e_rate)
}
plot(k_trials, nn_error_rate, type = 'l')
```

##KNN

```{r}
##Convert categorical column to numerical column

dummy_obj = dummyVars(~., data = hr %>% select(-Over18))
hr_new = data.frame(predict(dummy_obj, newdata=hr))

##Normalizing
hr_norm = normalize(hr_new, method = 'range', range = c(0,1))
```


```{r}

library(class)
hr = read.csv("E:\\Faraz\\Data Science\\Machine Learning\\HR Analytics.csv")


dummy_obj = dummyVars(~., data = hr %>% select(-Over18))
hr_new = data.frame(predict(dummy_obj, newdata=hr))

hr_norm = normalize(hr_new, method = 'range', range = c(0,1))

set.seed(100)
hr_train = hr_norm[1:(0.7*nrow(hr_norm)),]
hr_test = hr_norm[(0.7*nrow(hr_norm)+1):nrow(hr_norm),]
#hr$Attrition = as.numeric(hr$Attrition)
pred_probs_knn = knn(hr_train %>% select(-Attrition),
                     hr_test %>% select(-Attrition),
                     cl = as.factor(hr_train$Attrition),
                     k = 10,prob = T)


probs = data.frame(prob = attr(pred_probs_knn,'prob'),
                   class = pred_probs_knn)

probs[probs['class']==0,'prob'] = 1-probs[probs['class']==0,'prob']
roc_knn = roc(as.factor(hr_test$Attrition),probs$prob)
roc_rf = roc(hr_test$Attrition,pred_probs[,2])
{{plot(x)
  lines(roc_knn,col = 'red')
  }}





```
 
#Area under the Curve
```{r}
auc(roc_rf)
auc(roc_knn)

## finding the threshold

pred_obj = prediction(pred_probs[,2],as.factor(hr_test$Attrition))
cost.perf = performance(pred_obj,'cost')
y.values = cost.perf@y.values[[1]]
y.values = y.values[!y.values %in% c(Inf,-Inf)]
pred_obj@cutoffs[[1]][which.min(y.values)]
#y.values
```
 
## taking threshold 0.43 from above code
```{r}
hr_test$new_class = as.factor(ifelse(pred_probs[,2]>0.43,1,0))
confusionMatrix(hr_test$new_class,as.factor(hr_test$Attrition), positive="1")
```

### Calibrating probabilities

```{r}
library(lattice)
histogram(pred_probs[pred_probs[,2]>0.5,2])
```

```{r}
mushroom = read.csv("E:/Faraz/Data Science/Machine learning/mushroom_full.csv")
df = mushroom
```

```{r}
set.seed(100)
df_train = df[1:(0.7*nrow(df)),]
df_test = df[(0.7*nrow(df)+1):nrow(df),]
df_rf = randomForest(class~.,data=df_train)
pred_probs = predict(df_rf, df_test,type='prob')

histogram(pred_probs[pred_probs[,2]>0.5,2])
```

```{r}
pred_class = as.factor(ifelse(pred_probs[,2]>0.5,'POISONOUS','EDIBLE'))
confusionMatrix(pred_class,df_test$class,positive = 'POISONOUS')
```


```{r}
set.seed(100)
train = df[1:(0.7*nrow(df)),]
test = df[(0.7*nrow(df)+1):nrow(df),]

model <- randomForest(class~., data= train)
pred_probs <- predict(model, test, type = "prob")

colnames(pred_probs)
levels(df$class)
pred_class <- as.factor(ifelse(pred_probs[,2]>0.5,
                               "POISONOUS",
                               "EDIBLE"))
confusionMatrix(pred_class, test$class, positive = "POISONOUS")

histogram(pred_probs[pred_probs[,2]>0.5,2])

test$prob_pois = pred_probs[,2]

x_vals =c()
y_vals =c()

for(i in seq(0,1,0.05)){
  start_bin =i
  end_bin =i +0.05
  x_vals = c(x_vals, (start_bin+end_bin)/2)
  df_subset = test %>% filter(prob_pois>start_bin & prob_pois<=end_bin)
  curr_y = nrow(df_subset %>% filter(class=="POISONOUS"))/nrow(df_subset)
  y_vals= c(y_vals, curr_y)
}

plot(x_vals, y_vals, type="l")
```


#21-06-2018

##Probabilities Collaboration

```{r}
mushroom = read.csv("E:/Faraz/Data Science/Machine learning/mushroom_full.csv")
hr = read.csv("E:\\Faraz\\Data Science\\Machine Learning\\HR Analytics.csv")
df = hr
df$class = as.factor(df$Attrition)
df = df %>% select(-Attrition)
set.seed(100)
train = df[sample(seq(1,nrow(df)),(0.7*nrow(df))),]
test = df[sample(seq(1,nrow(df)),(0.3*nrow(df))),]
df_rf = randomForest(class~.,data = train) #Random Forest Model(df_rf)
pred_probs = predict(df_rf,test,type='prob')

train_probs = as.data.frame(predict(df_rf,train, type = 'prob'))
View(train_probs)
train_probs$class = train$class
colnames(train_probs) = c('prob_0','prob_1','class')
calib_model = glm(class~prob_1,data = train_probs,family = binomial)

```
```{r}
test_probs = as.data.frame(predict(df_rf,test,type='prob'))
colnames(test_probs) = c('prob_0','prob_1')
calib_model = glm(class~prob_1, data = train_probs,family = binomial)
calib_model
```
```{r}
test_probs = as.data.frame(predict(df_rf,test,type = 'prob'))
colnames(test_probs) = c('prob_0','prob_1')
View(test_probs)
test_probs$pred_class = as.factor(ifelse(test_probs$prob_1>0.5,1,0))
test_probs$pred_class_new = as.factor(ifelse(test_probs$prob_1_new>0.5,1,0))
confusionMatrix(test_probs$pred_class_new,test$class,positive = '1')
```

#Updated Threshold
```{r}
library(pROC)
library(ROCR)
pred_obj = prediction(test_probs[,'prob_1_new'],as.factor(test$class))
cost.perf = performance(pred_obj,'cost')
pred_obj@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
m2 = roc(test$class,test_probs$prob_1_new)
m1 = roc(test$class,test_probs$prob_1)
{{
  plot(m1)
  lines(m2,color = 'red')
}}
```

```{r}
library(caret)
library(dplyr)
seed = 7
control = trainControl(method="repeatedcv", number=10, repeats = 2)
metric = 'Accuracy'
tunegrid = expand.grid(.mtry=c(5,6,7))
rf_default = train(class~.,
                   data=df %>% select(-Over18),
                   method="rf",
                   metric=metric,
                   trControl=control,
                   tuneGrid = tunegrid)
rf_default$results
```
```{r}
pred_probs = as.data.frame(pred_probs)
pred_probs$class = as.factor(ifelse(pred_probs[,2]>0.5,1,0))

confusionMatrix(pred_probs$class, test$class)
```


##Multiclass Prediction

```{r}
car = read.csv("E:\\Faraz\\Data Science\\Machine Learning\\car_data.csv")
df = car
set.seed(100)
train.rows = createDataPartition(df$class, p=0.7, list = F)
train = df[train.rows, ]
test = df[-train.rows, ]
table(df$class)/nrow(df)
table(train$class)/nrow(train)
table(test$class)/nrow(test)
nrow(df)

model_rf = randomForest(class~., data=train)
test$pred_class = as.factor(predict(model_rf,test))
confusionMatrix(test$pred_class,test$class)

```
```{r}
test %>% filter(pred_class=='vgood' & class=='vgood') %>% nrow()
test %>% filter(class=='vgood') %>% nrow
```
***********************************************
#random forest
```{r}
md_rf1 <- randomForest(y=train$y, x = train[, -ncol(train)],
                       ytest = test$y, xtest = test[, -ncol(test)],
                       ntree = 100, mtry = 3, keep.forest = TRUE)
md_rf1
```

```{r}
md_rf2 <- randomForest(y=train$y, x = train[, -ncol(train)],
                       ytest = test$y, xtest = test[, -ncol(test)],
                       ntree = 200, mtry = 3, keep.forest = TRUE)
md_rf2
```

```{r}
md_rf3 <- randomForest(y=train$y, x = train[, -ncol(train)],
                       ytest = test$y, xtest = test[, -ncol(test)],
                       ntree = 300, mtry = 3, keep.forest = TRUE)
md_rf3
```

```{r}
rf_model<-randomForest(y ~.,data = test, importance=TRUE, ntree=1000)
rf_model
```
#knn
```{r}
dummy_obj=dummyVars(~.,data = bank %>% select(-y))
new = data.frame(predict(dummy_obj,newdata = bank %>% select(-y)))
new$y = bank$y


#normalising

nor = sapply(new %>% select(-y), function(x) (x-min(x))/(max(x)-min(x))) %>% as.data.frame()
nor$y = bank$y


set.seed(100) # Make sure that you run this to get similar results
train.rows = createDataPartition(nor$y, p=0.7, list=F)
train = nor[train.rows,]
test = nor[-train.rows, ]
sum(is.na(test))
predict=knn(train %>% select(-y),test %>% select(-y),cl=as.factor(train$y),k=10)

test$predict = as.factor(predict)
test$y=as.factor(test$y)

confusionMatrix(test$predict,test$y ,positive = '1')
confusionMatrix(test$predict,test$y, positive = 'yes')
```
#decision tree
```{r}
library(rpart)
model = rpart(y~., data=bank)
fancyRpartPlot(model)
```


#k MEANS
```{r}
num <- sapply(bank, function(x) is.numeric(x))
new_bank <- bank[,num]
View(new_bank)


data_norm <- normalize(new_bank,range = c(0,1),method = 'range')


model_kmeans <- kmeans(data_norm,centers = 5)

bank$cluster <- model_kmeans$cluster

model_kmeans$size

table(model_kmeans$cluster)

##total within
model_kmeans$tot.withinss

## withinsum of squares  this is distance of each clusters from the centroid , basically it should be less(close to each other)

model_kmeans$withinss


## between sum of squares (distance between 2 clusters hence this value should be ideally far or higher)

model_kmeans$betweenss


```


Hitters

#Prediciting Baseball Player Salaries Using Regression Trees

```{r}
data('Hitters')
reg.tree <- rpart(Salary ~ Years + Hits, data = Hitters)
reg.tree
rpart.plot(reg.tree, type = 4)
```
```{r}
library(rpart)
model = rpart(NewLeague~., data=Hitters)
model
fancyRpartPlot(model)
```

```{r}
reg.tree$variable.importance
```
```{r}
library(MASS)
library(rpart)
set.seed(1984)
train <- sample(1:nrow(Hitters), nrow(Hitters)/2)
tree.baseball <- rpart(Salary ~ Hits + HmRun + Runs + RBI + Walks + Years + Errors, subset = train, data = Hitters)
summary(tree.baseball)
```

```{r}
library(rpart.plot)
rpart.plot(tree.baseball)
```
```{r}
tree.baseball$variable.importance
```

#decision tree

```{r}
Hitters = na.omit(Hitters)
library(tree)
tree.fit <- tree(Salary~Hits+Years, data=Hitters)
summary(tree.fit)

```
```{r}
library(caret)
split <- createDataPartition(y=Hitters$Salary, p=0.7, list=FALSE)

trainH <- Hitters[split,]
testH <- Hitters[-split,]

#Create tree model
trees <- tree(Salary~., train)
plot(trees)
text(trees, pretty=0)
```









