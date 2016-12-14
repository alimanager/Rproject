library(VIM)
require(VIM)
library(MASS)
library(klaR)
library(ipred)
library(randomForest)
library(caret)
library(ROCR)
library(rpart)
library(adabag)
library(e1071)

visa=read.table("/home/abdelali/Documents/ETU/Logiciel R/VisaPremier.txt", header=T,na.strings="NA")
str(visa)
summary(visa)
#depart , codeqlt,  agemvt , nbpaiecb"
# les variables à valeur manquents: depart , codeqlt,  agemvt , "nbpaiecb"
var_NA=c()
for(i in 1:ncol(visa)){
  if(levels(factor(visa[,i],exclude=NULL))=="NA" || levels(factor(visa[,i],exclude=NULL))=="." ){
    #is.na(visa[,i]))
    plot(x = table(visa[,i]))
    print(colnames(visa[i]))
    var_NA=c(var_NA,i)}
}

# remplacer les valeurs manquants avec NA :
for(i in var_NA){
  print(i)
  visa[visa[,i]==".",i]<-NA 
}

#imputed missing values :
visaK=kNN(visa,variable=c("departem" ,"codeqlt","agemvt" , "nbpaiecb"))
aggr(visaK,delimiter ="_imp",numbers=T,prop=c(T,F))


#les données netoyé:
visa.clean=visaK[,-c(ncol(visaK)-0:3)] # 4 variables Binaire géneré avec la fonction Aggr , définit la valeur imputé  des individus des 4 variables
k=c()
for(i in 1: ncol(visa.clean)){
  if(class(visa.clean[,i])=="factor" && nlevels(visa.clean[,i])>10){
    k=c(k,i)
    visa.clean[,i]<-as.integer(levels(visa.clean[,i]))[visa.clean[,i]]#convertit les k variables factor en integer
    names(visa.clean[k]) #"departem" "nbop" "sexer" 
  }
}


## colinérité de cartevp et cartevpr!! supprimer: retirer la variables cartevp recodé en binaire
#nbimpaye, mtbon , mteparlt , engageml : glm.fit: l'algorithme n'a pas convergé
var_out=c("nbimpaye","mtbon","mteparte","nbeparte","nbbon","cartevp")
sapply(visa.clean[var_out],function(x){table(x)})
out=which(names(visa.clean) %in% var_out)
visa.clean1= visa.clean[-out]
visa.clean1$cartevpr=as.factor(visa.clean1$cartevpr)
str(visa.clean1)
summary(visa.clean1)


#*****************************************************************************************
#regression logistiques: 
#*****************************************************************************************
visa.RL=glm(cartevpr ~ .,data=visa.clean1,family =binomial)
print(visa.RL)
summary(visa.RL)
pchisq(visa.RL$null.deviance - visa.RL$deviance,df =  visa.RL$df.null - visa.RL$df.residual,lower.tail = F)

#selection des variables - Optimisation du critère AIC: 
RL.BACK=stepAIC(visa.RL,scope=list(lower=cartevpr ~ 1, upper=visa.RL$formula),direction = "backward")
print(RL.BACK)
summary(RL.BACK)

#rapport de Vraisemblance : le rapport entre le modéle trivial et la déviance du modéle étudié
pchisq(RL.BACK$null.deviance - RL.BACK$deviance,df =  RL.BACK$df.null - RL.BACK$df.residual,lower.tail = F)


#Modéles avec les varibales siginificatives :
# les variables siginifiactives au seuil de 5%
RL.formula=as.formula(cartevpr ~ sexe + csp + codeqlt + mtrejet + nbopguic + engagemm + moycredi + nbop + mtfactur + mtvie +  nbeparlt + nbcb + nbcbptar + aveparfi)
RL2=glm(RL.formula,data=visa.clean1,family =binomial)
summary(RL2)
pchisq(RL2$null.deviance - RL2$deviance,df =  RL2$df.null - RL2$df.residual,lower.tail = F)


#Cross-Validation:K-fold 

#function d'evaluation 
taux_err=function(pre,y){
    Mat=table(pre,y$cartevpr)
    err= 1.0 - sum(diag(Mat))/sum(Mat)
    print(Mat)
    print(err)
    return(err)
}
# Manipulation : dans la variable CSP y avais une une modalité 'Pagri' avec un seul individus , ce qui produit 
# une erreur avec CV10, puisque, probable qui sera avec la partie test, est pas en apprentisage. en a remplacé avec 
# pinc, qui est aussi probable que cette erreur occure avec, ya seulement 2 individus , donc on a regroupé les deux 
# a la place de l'imputer avec la médiane du variable.


table(visa.clean1$csp)
visa.clean1[ visa.clean1$csp=="Pagri",]$csp<-'Pinc'
visa.clean1$csp<-droplevels(visa.clean1$csp)
table(visa.clean1$csp)


# K-fold & Taux d'erreur & Analyse de performance & courbe Roc & Erreur CV10:
#==================================================================================
  kfold=createFolds(visa.clean1$cartevpr,k = 10)  
  AUC=Err=tvp=fpr=c()
  for(i in seq(10)){
    print(i)
    Train= do.call(rbind.data.frame,lapply(kfold[-i],function(k){visa.clean1[k,]})) # TRAIN **do call transforme la list produit par lapply à data.frame
    Test= do.call(rbind.data.frame,lapply(kfold[i],function(kf){visa.clean1[kf,]}))  # TEST
    
    Modele = glm(cartevpr ~.,data =Train,family =binomial)

      print(Modele$deviance)
      pre=predict(Modele,newdata=Test,type = "response")
      pre.moda=factor(ifelse(pre>0.5,"1","0"))
      #pre=predict(Modele,newdata=Test,type = "terms")
#     res=apply(pre,1,function(x){names(x)[order(x, decreasing = TRUE)][1:3]})  l'importance des variables :
#     paste(res,collapse=";",sep="")
    
    Err[i]=taux_err(pre.moda,Test)
    prediction_modele=prediction(pre,Test$cartevpr)
    roc=performance(prediction_modele,"tpr","fpr")
    fpr[i]=performance(prediction_modele,"tpr","fpr")@x.values
    tvp[i]=performance(prediction_modele,"tpr","fpr")@y.values
    AUC[i]=performance(prediction_modele,"auc")@y.values[[1]]
    RL=plot(roc)
  }
RL_ROC=list(y=sort(unlist(tvp)),x=sort(unlist(fpr)))
plot(RL_ROC,type="l", main="courbe ROC syntithéique des CV10, du RL",xlab="TFP(1-specifité)",ylab="TVP(Rappel)")
RL_AUC=mean(AUC)
RL_Err=mean(Err)

  

#*****************************************************************************************
#Arbre de Décision : 
#*****************************************************************************************
library(rpart)
AR=rpart(cartevpr ~ .,visa.clean1)  
print(AR)
visa.clean1$cartevpr=factor(visa.clean1$cartevpr)
predict(AR,newdata = visa.clean1,type="class")


plot(AR);text(AR)
plot(AR$variable.importance)
summary(AR)

AR2=rpart(cartevpr ~ .,visa.clean1,control = rpart.control(minsplit = 10,minbucket = 1) )
plot(AR2);text(AR2)
predict(AR2,newdata = Test,type="class")

CP=AR$cptable[,1]

for(j in 1:length(CP)){
  ARTest=rpart(cartevpr ~ .,visa.clean1,control = rpart.control(cp=0.01,minsplit = 10,minbucket = 1))
  pre1=predict(ARTest,Test,type="class")
  taux_err(pre1,Test)
  }
#Cross-validation : 0.12.
errorest(cartevpr ~ .,data = visa.clean1,model=rpart,predict=mypredict.rpart,estimator="cv",cp=0.01)

# K-fold & Taux d'erreur & Analyse de performance & courbe Roc & Erreur CV10:
#==================================================================================
kfold=createFolds(visa.clean1$cartevpr,k = 10)  
AUC=Err=tvp=fpr=c()
for(i in seq(cv)){
  print(i)
  Train= do.call(rbind.data.frame,lapply(kfold[-i],function(k){visa.clean1[k,]})) # TRAIN **do call transforme la list produit par lapply à data.frame
  Test= do.call(rbind.data.frame,lapply(kfold[i],function(kf){visa.clean1[kf,]}))  # TEST
  
  Modele = rpart(cartevpr ~ .,Train,control = rpart.control(cp=0.01,minsplit = 10,minbucket = 1))

  preC=predict(Modele,newdata=Test,type = "class")
  pre.moda=factor(ifelse(pre>0.5,"1","0"))
  pre=predict(Modele,newdata=Test,type = "prob")[,2]

  Err[i]=taux_err(preC,Test)
  prediction_modele=prediction(pre,Test$cartevpr)
  roc=performance(prediction_modele,"tpr","fpr")
  fpr[i]=performance(prediction_modele,"tpr","fpr")@x.values
  tvp[i]=performance(prediction_modele,"tpr","fpr")@y.values
  AUC[i]=performance(prediction_modele,"auc")@y.values[[1]]
  plot(roc)
}
AR_ROC=list(y=sort(unlist(tvp)),x=sort(unlist(fpr)))
plot(AR_ROC,type="l", main="courbe ROC syntithéique des CV10, du AR",xlab="TFP(1-specifité)",ylab="TVP(Rappel)")
AR_AUC=mean(AUC);print(AR_AUC)
AR_Err=mean(Err);print(AR_Err)

#*****************************************************************************************
#random forest:
#*****************************************************************************************

RF=randomForest(cartevpr ~ .,visa.clean1[1:650,],ntree=100,keep.forest=T,importance=T,proximity=T)
print(RF)
varImpPlot(RF)
RF_pre=predict(RF,newdata =visa.clean1[651:nrow(visa.clean1),],type = "class")
taux_err(RF_pre,visa.clean1[651:nrow(visa.clean1),])

# plusieurs arbre plus de 20 
nb_arbre=c(5,10,20,30,40,50,60,70,80,90,100,200)
ERR_RF=c()
for(k in 1:length(nb_arbre)){
  ERR_RF[k]=errorest(cartevpr ~ .,data = visa.clean1,model=randomForest,predict=mypredict.rpart,estimator="cv",ntree=nb_arbre[k])$error
}
plot(ERR_RF,x=nb_arbre)
lines(mean(ERR_RF))

#fonction pour ERROREST : spécifier les modalité de prédiction pour le Modéle : AR et RF.
mypredict.rpart <- function(object, newdata){
  predict(object, newdata = newdata,type="class")
}

#Taux d'erreur de Random forest avec Errorest : 0.10 ( 0.0941  avec ntree50)
Te=errorest(cartevpr ~ .,data = visa.clean1,model=randomForest,predict=mypredict.rpart,estimator="cv",ntree=100)
print(Te)

#les importantes Var pertinantes de RF:
varImpPlot(RF)

# K-fold & Taux d'erreur & Analyse de performance & courbe Roc & Erreur CV10:
#==================================================================================
kfold=createFolds(visa.clean1$cartevpr,k = 10)  
AUC=Err=tvp=fpr=c()
for(i in seq(cv)){
  print(i)
  Train= do.call(rbind.data.frame,lapply(kfold[-i],function(k){visa.clean1[k,]})) # TRAIN **do call transforme la list produit par lapply à data.frame
  Test= do.call(rbind.data.frame,lapply(kfold[i],function(kf){visa.clean1[kf,]}))  # TEST
  
  Modele = randomForest(cartevpr ~ .,Train,ntree=60)
  
  preC=predict(Modele,newdata=Test,type = "class")
  pre.moda=factor(ifelse(pre>0.5,"1","0"))
  pre=predict(Modele,newdata=Test,type = "prob")[,2]
  
  Err[i]=taux_err(preC,Test)
  prediction_modele=prediction(pre,Test$cartevpr)
  roc=performance(prediction_modele,"tpr","fpr")
  fpr[i]=performance(prediction_modele,"tpr","fpr")@x.values
  tvp[i]=performance(prediction_modele,"tpr","fpr")@y.values
  AUC[i]=performance(prediction_modele,"auc")@y.values[[1]]
  plot(roc)
}
RF_Roc=list(y=sort(unlist(tvp)),x=sort(unlist(fpr)))
plot(sort(unlist(tvp)),x=sort(unlist(fpr)),type="l", main="courbe ROC syntithéique des CV10, du RF",xlab="TFP(1-specifité)",ylab="TVP(Rappel)")
RF_AUC=mean(AUC);print(RF_AUC)
RF_Err=mean(Err);print(RF_Err)



#*****************************************************************************************
#boosting : 
#*****************************************************************************************
library(adabag)
visa.clean1$cartevpr<- as.factor(visa.clean1$cartevpr)
boosting(cartevpr ~., data=Test,mfinal = 20,boos=F)

kfold=createFolds(visa.clean1$cartevpr,k = 10)  
AUC=Err=tvp=fpr=c()
for(i in seq(10)){
  print(i)
  Train= do.call(rbind.data.frame,lapply(kfold[-i],function(k){visa.clean1[k,]})) # TRAIN **do call transforme la list produit par lapply à data.frame
  Test= do.call(rbind.data.frame,lapply(kfold[i],function(kf){visa.clean1[kf,]}))  # TEST
  
  Modele = boosting(cartevpr ~., data=Train,mfinal=20,boos = F)
  
  preC=predict(Modele,newdata=Test)
  pre.moda=factor(ifelse(pre>0.5,"1","0"))
  pre=preC$prob[,2]
  
  Err[i]=taux_err(preC$class,Test)
  prediction_modele=prediction(pre,Test$cartevpr)
  roc=performance(prediction_modele,"tpr","fpr")
  fpr[i]=performance(prediction_modele,"tpr","fpr")@x.values
  tvp[i]=performance(prediction_modele,"tpr","fpr")@y.values
  AUC[i]=performance(prediction_modele,"auc")@y.values[[1]]
  plot(roc)
}
BO_ROC=list(y=sort(unlist(tvp)),x=sort(unlist(fpr)))
plot(BO_ROC,type="l", main="courbe ROC syntithéique des CV10, du Bo",xlab="TFP(1-specifité)",ylab="TVP(Rappel)")
BO_AUC=mean(AUC);print(BO_AUC)
BO_Err=mean(Err);print(BO_Err)


#*****************************************************************************************
#SVM
#*****************************************************************************************
library(e1071)
SVM=svm(cartevpr ~., data=visa.clean1[1:650,]) #50%
preS=predict(SVM,visa.clean1[651:1073,],type="class")# 40%
taux_err(preS,visa.clean1[651:1073,]) # 50% Erreur !!!!

SVM2=svm(cartevpr ~., data=visa.clean1[1:650,],kernel="polynomial") #50%
preS2=predict(SVM2,visa.clean1[651:1073,],descision.values=T)# 40%
taux_err(preS2,visa.clean1[651:1073,]) # 50%


ir.tune<-tune(svm, cartevpr ~., data=visa.clean1, kernel="polynomial",
              ranges=list(gamma=2^(-5:5)),cost=2^(-5:5),
              control = tune.control(sampling="cross", cross=10))
summary(ir.tune)


kfold=createFolds(visa.clean1$cartevpr,k = 10)  
AUC=Err=tvp=fpr=c()
for(i in seq(10)){
  print(i)
  Train= do.call(rbind.data.frame,lapply(kfold[-i],function(k){visa.clean1[k,]})) # TRAIN **do call transforme la list produit par lapply à data.frame
  Test= do.call(rbind.data.frame,lapply(kfold[i],function(kf){visa.clean1[kf,]}))  # TEST
  
  Modele =svm(cartevpr ~., data=Train, kernel="polynomial",gamma=0.25,degree=3,
          coef=0,cost=c(0.03125, 0.0625, 0.125, 0.25 ,0.5, 1, 2, 4, 8 ,16,32),probability=T)
  
  preS=predict(Modele,Test,descision.values=T)# 40%
  pre=attr(predict(Modele,Test,probability = T),"probabilities")[,1]
  
  Err[i]=taux_err(preS,Test)
  prediction_modele=prediction(pre,Test$cartevpr)
  roc=performance(prediction_modele,"tpr","fpr")
  fpr[i]=performance(prediction_modele,"tpr","fpr")@x.values
  tvp[i]=performance(prediction_modele,"tpr","fpr")@y.values
  AUC[i]=performance(prediction_modele,"auc")@y.values[[1]]
  plot(roc)
}
SVM_ROC=list(y=sort(unlist(tvp)),x=sort(unlist(fpr)))
plot(SVM_ROC,type="l", main="courbe ROC syntithéique des CV10, du SVM",xlab="TFP(1-specifité)",ylab="TVP(Rappel)")
SVM_AUC=mean(AUC);print(SVM_AUC)
SVM_Err=mean(Err);print(SVM_Err)



#*****************************************************************************************
# les courbes roc ,le taux d'erreur et l'importance des variables , AUC : l'aire sous les courbes! 
#*****************************************************************************************
# Comparaison des modéles: 
#================================================================================
plot(SVM_ROC,type="l", main ="Comparaison des modéles : RL VS SVM",xlab="TFP(1-specifité)",ylab="TVP(Rappel)")
#lines(RF_Roc,type="l",col="red")
#lines(BO_ROC,type="l",col="green")
lines(RL_ROC,type="l",col="purple")
lines(RL_ROC2,type="l",col="green")
#lines(AR_ROC,type="l",col="yellow")
legend(0.6,0.6,c('SVM_ROC','RL_ROC','RL_STEPAIC'),col=c('black','purple','green'),lwd=3)
data.frame(modéle=factor(c('SVM','Regression logistique', 'Regression logistique avec stepAIC')),Erreur_Test=factor(c(SVM_Err,RL_Err,RL_Err2)),AUC=factor(c(SVM_AUC,RL_AUC,RL_AUC2)))
