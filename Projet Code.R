#################################
########## Statistiques #########
########## Descriptives #########
#################################

# Suppression des données manquantes
#description ? 
learn2 <- na.omit(learn)
summary(learn2)

learn2$N <- row.names(learn2)
learn3 <- c(learn2$N)
learn3 <- as.integer(learn3)
#création d'une table miss avec les individus aux obs manquantes
miss <- learn[-learn3,]

# Présence de données manquantes 

x <- nrow(miss)/nrow(learn);x
if (x>0.05){
  print("Le pourcentage de données manquantes est supérieur à 5% on ne peut pas supprimer les observations manquantes")
  print(x*100)
} else {
  print("Le pourcentage de données manquantes est inférieur à 5% on peut supprimer les observations manquantes")
}

#Il y a trop de données manquantes pour les supprimer
# Imputation multiple


#################################
########## Imputations ##########
########### Multiple ############
#################################


# Imputation via le package mice
install.packages("mice")
library(mice)
  
  #Affiche les données manquantes
    md.pattern (learn)
  
  # La sortie nous dit que 4055 échantillons sont complets, 
  # 37 échantillons où il manquent seulement la variable X1, 
  # 42 échantillons manquent seulement la variable X2 et ainsi de suite
  # Au total il y a 1045 obs manquantes dont 43 en X1, 45 en X2,...
  
  #Visualisation des données manquantes
    install.packages("VIM")
    library(VIM)
    
    dev.off()#réinitialise les options graphiques par défaut 
    par()
    
    aggr_plot <- aggr(learn, col=c('blue','red'), numbers=TRUE, sortVars=TRUE, 
                      labels=names(learn),  cex.lab = 1, cex.axis=.8, gap=0.1, 
                      ylab=c("Histogram of missing data","Pattern"))
    
    #illisible ! divisé en 2 ou 3 X1 - X7 / X8 - X14 / X15 - X20 ? 
    learn_1 <- learn[,1:5]
    
    aggr_plot <- aggr(learn_1, col=c('blue','red'), numbers=TRUE, sortVars=TRUE, 
                      bars=T, combined=T, 
                      labels=names(learn_1), cex.lab=1, cex.axis=.5, cex.numbers=0.5 , 
                      ylab=c("Histogram of missing data","Pattern"))
    
  #Impution multiple :
    #On remplace chaque valeur manquante par M(>1) valeur tirées dans une distribution approprié
    #On considère que pour M = 5 on à de bon résultat
    #Maxit = on relance 10 fois l'imputation multiple 
    #seed = ???
    
    #Il existe différentes méthodes d'imputation dans la librairi R mice 
    
    #La méthode "Predictive mean matching" : pmm / convient à tout types de variables (num ou char)
    
    ImpData <- mice(learn,m=5,maxit=10,meth='pmm',seed=100)
    ImpData
    summary(ImpData)
    
    CompletedData <- complete(ImpData) #imputation
    


# Imputation via le package missForest
install.packages("missForest")
library(missForest)

#Beaucoup de résultats dépendent du hasard, 
#set.seed permet d'obtenir les mêmes résultats d'une fois sur l'autre

set.seed(1450)
mis.learn = missForest(learn, maxiter=5, ntree=100)


#selon la méthode d'imputation choisie, faire tourner une des deux commandes suivantes
learn = CompletedData   #MICE
learn = learn.MF  #missForest





#################################
########### Prévision ###########
###########     Y    ############
#################################

# ANOVAC Analyse de la covariance 
#R utilise lm peu importe le type des variables explicatives

#Séparation de la base en apprentissage (80%) et validation (20%)
library(caret)

app = createDataPartition(y=learn$Y, p=0.80, list=FALSE )
training = learn[app,]
baseTest = learn[-app,]
nrow(training) ; nrow(baseTest)



############## Modèle linéaire ############

reg = train(Y~., data=training, method="lm",
            preProc = c("center","scale"),
            list = FALSE,
            tr=trainControl(method = "repeatedcv", 
                            number=10, repeats = 10, p=0.7, returnResamp="all",
                            savePredictions = TRUE))
reg
reg$finalModel

#Resampling : Bootstrapped (25 reps)
summary(reg) #Il y a des variables non significatives -> à éliminer

plot(reg$finalModel$residuals)
reg$results


plot(reg$resample$Rsquared, type='l', main='R2',
     xlab='training numéro',
     ylab='R2')

plot(reg$resample$RMSE, type='l', main='RMSE',
     xlab='training numéro',
     ylab='RMSE')


#Perte quadratique 
predY = predict(reg, newdata=baseTest)
Test.mod <- cbind(baseTest,predY)
head(Test.mod)

#Taux d'erreur
risqueY = sum((predY-baseTest$Y)^2)/nrow(baseTest)
risqueY



################ CART ################
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

learnTree = rpart(Y~.,data=training, method="anova", 
                  control=rpart.control(minsplit=50,cp=0,xval=50))
plotcp(learnTree)
printcp(learnTree)

#erreurs apprentissage / Validation croisée
plot(x=learnTree$cptable[,2],y=learnTree$cptable[,3], type='l',col='red')
lines(learnTree$cptable[,4], type='l', col='blue')  #avec CV
legend("right",legend=c("Sans CV","Avec CV"), col=c("red","blue"),lwd=c(2,2))

learnOptimal = prune(learnTree, 
                     cp=learnTree$cptable[which.min(learnTree$cptable[,4]),1])
prp(learnOptimal, extra=1, cex=0.5)

#Prévision
predictY = predict(learnOptimal, baseTest)

#Risque empirique selon une perte quadratique
risqueTree = sum((predictY-baseTest$Y)^2)/nrow(baseTest)
risqueTree

res.tree=predictY-baseTest$Y
plot(res.tree)
plot(residuals(learnOptimal))
plot(predictY)


plot(predictY,baseTest$Y)
abline(a=0,b=1)




############### Random Forest ###########
library(caret)
learnRF <- train(Y ~ ., data = training, method = "rf",
                 tr = trainControl(method = "repeatedcv", number=10, repeats = 3))
learnRF
print(learnRF$finalModel)
plot(learnRF) # graphique pas normal je crois
plot(learnRF$finalModel)
plot(learnRF$resample$RMSE, type='l')
plot(learnRF$resample$Rsquared, type='l')

#Importance des variables
varImpPlot(learnRF$finalModel)
plot(Y~X4, data=training)
plot(Y~X16, data=training)

predictY <- predict(learnRF, newdata=baseTest)
#Risque empirique selon une perte quadratique
risqueTree = sum((predictY-baseTest$Y)^2)/nrow(baseTest)
risqueTree




############## Random Forest avec le package "randomForest" #######
#Apprentissage sans validation croisée
install.packages("randomForest")
library(randomForest)
learnRF2 <- randomForest(Y ~ ., training, ntree=5000, mtry=2)

print(learnRF2)

learnRF2$importance
varImpPlot(learnRF2)


plot(learnRF2$rsq, type='l')
#plot(learnRF2$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")

p=learnRF2$predicted
risqueRF2 = sum((p-baseTest$Y)^2)/nrow(baseTest)
risqueRF2




#################################
########### Prévision ###########
###########     Z    ############
#################################



#Création de Z
Z=ifelse(learn$Y<0,Z<-0,ifelse(learn$Y>=0 & learn$Y<=2,Z<-1,Z<-2))
Z=as.factor(Z)
learn2 = cbind(learn,Z)

#Suppression de la variable Y dans la table
learn2 = learn2[,-(ncol(learn2)-1)]
head(learn2)

#Séparation de la base en apprentissage (70%) et validation (30%)

app = createDataPartition(y=learn2$Z, p=0.80, list=FALSE )
training = learn2[app,]
baseTest = learn2[-app,]
nrow(training) ; nrow(baseTest)


########### Classifieur Bayésien Naif #########


#**************************************
# Aprrentissage sans validation croisée
#**************************************
install.packages("e1071")
library(e1071)

# Modélisation
NaiveBayes <- naiveBayes(Z ~ ., data = training)
NaiveBayes
# Probabilités conditionnelles négatives ???????????????

#La qualité du modèle dépend de sa capacité à bien classer dans le jeu de données test

Z.predict <- predict(object = NaiveBayes, newdata = baseTest)
confusionMatrix(data=Z.predict,reference=baseTest$Z)


#Taux d'erreur
Test.mod <- cbind(baseTest, Z.predict)
head(Test.mod, 5)

Confusion = table(Test.mod$Z, Test.mod$Z.predict)  # Taux de bien classé
Confusion
round(prop.table(Confusion), 2) #Pourcentage

error <- (1*Confusion[1,2]+Confusion[1,3]+
            1*Confusion[2,1]+1*Confusion[2,3]+
            1*Confusion[3,1]+1*Confusion[3,2])/sum(Confusion)

print(error) 


#**************************************
# Aprrentissage avec validation croisée
#**************************************

#method="nb"
#For classification using package klaR with tuning parameters:
  # Laplace Correction (fL, numeric) :
  # fL Factor for Laplace correction, default factor is 0, i.e. no correction.
  

# caret sans correction
NB = train(Z~., 
           data=training, 
           method="nb", 
           tr=trainControl(method="repeatedcv", number=10, repeats=10)
          )
#5 min 

NB


# La qualité du modèle
Z.predict.NB <- predict(object = NB, newdata = baseTest)

confusionMatrix(data=Z.predict.NB, reference=training$Z)

#Sensitivity: given that a result is truly an event, what is the probability that the model will predict an event results?
#Specificity: given that a result is truly not an event, what is the probability that the model will predict a negative results?

#Taux d'erreur
Test.mod <- cbind(training, Z.predict.NB)
Confusion = table(Test.mod$Z, Test.mod$Z.predict.NB)
Confusion
round(prop.table(Confusion), 2)

error <- (1*Confusion[1,2]+Confusion[1,3]+
            1*Confusion[2,1]+1*Confusion[2,3]+
            1*Confusion[3,1]+1*Confusion[3,2])/sum(Confusion)

print(error) 


############### Random Forest ###########


library(caret)
learnRF <- train(z ~ ., data = training, method = "rf",
                 tr = trainControl(method = "repeatedcv", number=10, repeats = 3))
learnRF
print(learnRF$finalModel)
plot(learnRF) # graphique pas normal je crois
plot(learnRF$finalModel)
plot(learnRF$resample$RMSE, type='l')
plot(learnRF$resample$Rsquared, type='l')

#Importance des variables
varImpPlot(learnRF$finalModel)
plot(Y~X4, data=training)
plot(Y~X16, data=training)

Z.predict.RF <- predict(learnRF, newdata=baseTest)
confusionMatrix(data=Z.predict.RF, reference=baseTest$Z)

#Taux d'erreur
Test.mod <- cbind(baseTest, Z.predict.RF)
Confusion = table(Test.mod$Z, Test.mod$Z.predict.RF)
Confusion
round(prop.table(Confusion), 2)

error <- (1*Confusion[1,2]+Confusion[1,3]+
            1*Confusion[2,1]+1*Confusion[2,3]+
            1*Confusion[3,1]+1*Confusion[3,2])/sum(Confusion)

print(error) 