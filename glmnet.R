require(glmnet)
require(survival)

# pr?paration des donn?es
diabete = read.csv2("base_diabete_ml_sauf_sexe.csv", header= T, sep=";")
diabete[is.na(diabete)] = 0

nr<-dim(diabete)
diabete <- diabete[sample.int(nr),]

y = diabete[which(diabete[,2]!=0),2]
status = diabete[which(diabete[,2]!=0),1]

x =  diabete[which(diabete[,2]!=0), -c(1,2)]
x =  x[, -which(colnames(x)=="CT_Group")]
x =  x[, -which(colnames(x)=="cortico")]
x =  x[, -which(colnames(x)=="anticorps")]


for(j in 1:ncol(x)) x[,j] = as.numeric(x[,j])
for(j in c(1:5)) x[,j] = as.factor(x[,j])
for(j in c(7:19)) x[,j] = as.factor(x[,j])
x <- model.matrix( ~ ., x)  

x_train <- x[1:floor(dim(x)[1]*0.66),];
x_test <-x[-(1:floor(dim(x)[1]*0.66)),];

y_train <- y[1:floor(length(y)*0.66)];
y_test <-y[-(1:floor(length(y)*0.66))];

status_train <- status[1:floor(length(status)*0.66)];
status_test <-status[-(1:floor(length(status)*0.66))];

#foldid=sample(1:3,size=length(Surv(y, status)),replace=TRUE)

folds <- lapply(c(1:10), function(e){ return(sample(1:3,size=length(y_train),replace=TRUE))})




#when alpha=1

cv1 <- cv.glmnet(x_train,Surv(y_train, status_train),family="cox",nfolds = 3,foldid=folds[[1]],alpha=0.5)

plot(cv1)

#lambda.min	value of lambda that gives minimum cvm.
#lambda.1se	largest value of lambda such that error is within 1 standard error of the minimum.

#cv1$lambda.min
cv1$lambda.1se

#We can check the active covariates in our model and see their coefficients.
#coef(cv1, s = "lambda.min")
#
coef(cv1, s = "lambda.1se")


#when alpha=0

#cv0 <- cv.glmnet(x,Surv(y, status),family="cox",nfolds = 3,paralle = TRUE,foldid=folds[[1]],alpha=0)

#plot(cv0)

#lambda.min	value of lambda that gives minimum cvm.
#lambda.1se	largest value of lambda such that error is within 1 standard error of the minimum.

#cv0$lambda.min
#cv0$lambda.1se

#We can check the active covariates in our model and see their coefficients.
#coef(cv0, s = "lambda.min")
#coef(cv0, s = "lambda.1se")

# ELASTIC NET WITH 0 < ALPHA < 1

alphas <- c(1:10) * 0.1
res <- lapply(alphas, function(alpha){
  return(sapply(folds, function(fold){
    cv <- cv.glmnet(x_train, Surv(y_train, status_train),family="cox", nfolds = 3,foldid=fold,alpha = alpha)
    return(cv)
  }))
})
sapply(res, function(element){
  cms <- apply(element, 2, function(l) return(l$cvm))
  cmsavg <- apply(cms, 1, mean)
  minlambdaIndex <- which.min(cmsavg);
  return(c(minlambdaIndex, min(cmsavg)));
})

res[[1]][,1]$lambda[15]
#second alpha ,1, is index of lamda then give best lambda

md3 <- glmnet(x_train,Surv(y_train, status_train),family="cox", lambda = 0.02870526, alpha = 0.1)
coef(md3)

x_test <- model.matrix( ~ ., x_test)  
predict <- predict.glmnet(object=md3, newx=x_test, type = "response")





#a <- seq(0.1, 0.9, 0.1)
#search <- foreach(i = a, .combine = rbind) %dopar% {
  #cv <- cv.glmnet(x,Surv(y, status),family="cox",nfolds = 3, paralle = TRUE,foldid=foldid,alpha = i)
  #data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
#}
#cv3 <- search[search$cvm == min(search$cvm), ]
#md3 <- glmnet(x,Surv(y, status),family="cox", lambda = cv3$lambda.1se, alpha = cv3$alpha)
#coef(md3)

#cv3$lambda.min
#cv3$lambda.1se
#cv3$alpha
#cv$cvm

#cox ph
cox <- coxph(Surv(y_train, status_train) ~ typeg1+typeg2+adiag+anais+mean_Pancreas_Tail, data = as.data.frame(x_train))
summary(cox)
