#Packages n?cessaires
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("mice")
install.packages("missForest")
install.packages("VIM")
install.packages("leaps")
install.packages("randomForest")
install.packages("corrplot")
install.packages("e1071")
install.packages("klaR")
install.packages("MASS")




set.seed(1450)

#################################
#--------- Statistiques --------#
#--------- Descriptives --------#
#################################


#Analyse exploratoire
sapply(learn, class)
str(learn)
summary(learn)


#***************************************************#
#**************  Matrice de corr?lation  ***********#
#************** et analyse des variables ***********#
#**************       quantitatives      ***********#
#***************************************************#
library(caret)

#Matrice des variables quantitatives
learnQuanti = learn[,sapply(learn,is.numeric)]
head(learnQuanti)
#on voit que X6 et X18 sont en fait des variables binaires
learnQuanti = learnQuanti[-5]
learnQuanti = learnQuanti[-13]


#Visualisation des nuages de points (Y,X)
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(learnQuanti[,1:(ncol(learnQuanti)-1)], y = learnQuanti$Y, plot = "scatter", layout = c(3, 5))


#matrice de corr?lations pour les variables quantitatives
matCorr = cor(learnQuanti, use = "complete.obs")

#Visualisation des corr?lations
library(corrplot)
corrplot(matCorr, type="upper", tl.col="black", tl.srt=45, method='color')


#Utilisation des p-values pour une meilleure lisibilit? du graphique
# mat : matrice de donn?es
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Matrice de p-value de la corr?lation
p.mat <- cor.mtest(learnQuanti)
corrplot(matCorr, type="upper", p.mat = p.mat, sig.level = 0.01, method='color')


#Boxplot
boxplot(learnQuanti)





############################################
#---  Imputation des donn?es manquantes ---#
############################################


learn2 <- na.omit(learn)
summary(learn2)

learn2$N <- row.names(learn2)
learn3 <- c(learn2$N)
learn3 <- as.integer(learn3)
#cr?ation d'une table miss avec les individus aux obs manquantes
miss <- learn[-learn3,]

# Pr?sence de donn?es manquantes 

x <- nrow(miss)/nrow(learn);x
if (x>0.05){
  print("Le pourcentage de donn?es manquantes est sup?rieur ? 5%, on ne peut pas supprimer les observations manquantes")
  print(x*100)
} else {
  print("Le pourcentage de donn?es manquantes est inf?rieur ? 5%, on peut supprimer les observations manquantes")
}

#Il y a trop de donn?es manquantes pour les supprimer
# On met en place une imputation multiple


#################################
#--------- Imputation ----------#
#---------- Multiple -----------#
#################################

#**************************************#
#*** Imputation via le package mice ***#
#**************************************#

library(mice)

#Affiche les donn?es manquantes
md.pattern (learn)
# Au total il y a 1045 obs manquantes dont 43 en X1, 45 en X2,...

#Visualisation des donn?es manquantes
library(VIM)

dev.off()#r?initialise les options graphiques par d?faut 
par()

aggr_plot <- aggr(learn, col=c('blue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(learn),  cex.lab = 1, cex.axis=.8, gap=0.1, 
                  ylab=c("Histogram of missing data","Pattern"))


#Impution multiple :
#On remplace chaque valeur manquante par M(>1) valeur tir?es dans une distribution appropri?
#On consid?re que pour M = 5 on ? de bon r?sultat
#Maxit = on relance 10 fois l'imputation multiple 
#On utilise la m?thode "Predictive mean matching" : pmm / convient ? tout types de variables (num ou char)

ImpData <- mice(learn,m=5,maxit=10,meth='pmm',seed=100)
ImpData
summary(ImpData)

CompletedData <- complete(ImpData) #table imput?e


learn = CompletedData   #MICE




#################################
#---------- Prévision ----------#
#----------     Y     ----------#
#################################

#S?paration de la base en apprentissage (80%) et validation (20%)
#library(caret)
app = createDataPartition(y=learn$Y, p=0.80, list=FALSE )
training = learn[app,]
baseTest = learn[-app,]
nrow(training) ; nrow(baseTest)



################### Mod?le lin?aire #######################

#******** Mod?le complet ***********#

reg = train(Y~., data=training, method="lm",
            preProc = c("center","scale"),
            list = FALSE,
            tr=trainControl(method = "repeatedcv", 
                            number=10, repeats = 10, p=0.7, returnResamp="all",
                            savePredictions = TRUE))
reg
reg$finalModel
summary(reg) #Il y a des variables non significatives -> ? ?liminer

plot(reg$finalModel$residuals)
reg$results


#Risque avec perte quadratique sur l'?chantillon d'apprentissage
Yapp = predict(reg)
YappRisque = sum((Yapp-training$Y)^2)/nrow(training)
YappRisque

#Risque avec une perte quadratiquesur les donn?es de test
predY = predict(reg, newdata=baseTest)
Test.mod <- cbind(baseTest,predY)
risqueLMY = sum((predY-baseTest$Y)^2)/nrow(baseTest)
risqueLMY

risquesLM1 = c(YappRisque, risqueLMY)


#***** Relance de la r?gression avec variables significatives ? 1% *****#
#cr?ation de x20b
x20b=ifelse(learn$X20=='B',x20b<-1,x20b<-0)
learnx20b = cbind(learn,x20b)
#S?paration de la base en apprentissage (80%) et validation (20%)

app = createDataPartition(y=learnx20b$Y, p=0.80, list=FALSE )
trainingx20b = learnx20b[app,]
baseTestx20b = learnx20b[-app,]

#R?gression
regLM = train(Y~X1+X4+X5+X6+X8+X11+X12+X16+X17+x20b, data=trainingx20b, 
              method="lm",
              preProc = c("center","scale"),
              list = FALSE,
              tr=trainControl(method = "repeatedcv", 
                              number=10, repeats = 10, p=0.7, returnResamp="all",
                              savePredictions = TRUE))
summary(regLM)

#Risque avec perte quadratique sur l'?chantillon d'apprentissage
Yapp = predict(regLM)
YappRisque = sum((Yapp-trainingx20b$Y)^2)/nrow(trainingx20b)
YappRisque
#0.4946133

#Risque avec perte quadratique sur l'?chantillon test
YpredLM = predict(regLM, newdata=baseTestx20b)
risqueLMY = sum((YpredLM-baseTestx20b$Y)^2)/nrow(baseTestx20b)
risqueLMY
#0.5460654

risquesLM2 = c(YappRisque,risqueLMY)


#***************** Selection Backward **************#

library(leaps)

regBw = train(Y~., data=training, method="leapBackward",
              tr=trainControl(method = "repeatedcv", 
                              number=10, repeats = 10),
              tuneGrid=expand.grid(nvmax=c(1:24)))
regBw
summary(regBw)
regBw$results
regBw$bestTune #nvmax obtenu par validation crois?e
regBw$resample

par(mfrow=c(1,2))
plot(regBw$results$RMSE, type='b', main='RMSE pour Backward Selection', 
     xlab='Nombre de variables', ylab='RMSE')
plot(regBw$results$Rsquared, type='b',main='R2 pour Backward Selection', 
     xlab='Nombre de variables', ylab='R2')


#Risque avec perte quadratique sur l'?chantillon d'apprentissage
Yapp = predict(regBw)
YappRisqueBw = sum((Yapp-training$Y)^2)/nrow(training)
YappRisqueBw

#Risque avec perte quadratique sur l'?chantillon test 
predY = predict(regBw, newdata=baseTest)
risqueBwY = sum((predY-baseTest$Y)^2)/nrow(baseTest)
risqueBwY

risquesBw = c(YappRisqueBw, risqueBwY)


#******************Selection Stepwise*************#

regSw = train(Y~., data=training, method="leapSeq",
              list = FALSE, 
              tr=trainControl(method = "repeatedcv", 
                              number=10, repeats = 10, p=0.7, returnResamp="all",
                              savePredictions = TRUE),
              tuneGrid=expand.grid(nvmax=c(1:24)))
regSw
summary(regSw)
regSw$results
regSw$bestTune #nvmax
regSw$resample

par(mfrow=c(1,2))
plot(regSw$results$RMSE, type='b', main='RMSE pour Stepwise Selection', 
     xlab='Nombre de variables', ylab='RMSE')
plot(regSw$results$Rsquared, type='b',main='R2 pour Stepwise Selection', 
     xlab='Nombre de variables', ylab='R2')

#Risque avec perte quadratique sur l'?chantillon d'apprentissage
Yapp = predict(regSw)
YappRisqueSw = sum((Yapp-training$Y)^2)/nrow(training)
YappRisqueSw

#risque avec perte quadratique sur l'?chantillon de test
predY = predict(regSw, newdata=baseTest)
risqueSwY = sum((predY-baseTest$Y)^2)/nrow(baseTest)
risqueSwY

risquesSw = c(YappRisqueSw,risqueSwY)

#**********COMPARAISON DES MODELES*********#

#Comparaison des risques
risquesY = data.frame(Base = c("Apprentissage","Test"),
                      Full_Model = risquesLM1,
                      Var_sign = risquesLM2,
                      Backward = risquesBw,
                      Stepwise = risquesSw)
risquesY



#################### ARBRE DE DECISION ####################

library(rpart)
library(rpart.plot)

learnTree = rpart(Y~.,data=training, method="anova", 
                  control=rpart.control(cp=0,xval=50))
plotcp(learnTree)
printcp(learnTree)

#erreurs apprentissage / Validation crois?e
dev.off()
plot(x=learnTree$cptable[,2],y=learnTree$cptable[,3], type='l', ylab="erreur")
lines(learnTree$cptable[,4], type='l', lty=2)  #avec CV
legend("right",legend=c("Ensemble d'apprentissage","Validation Crois?e"), lty=c(1,2),
       cex=0.8,bty='n')

learnOptimal = prune(learnTree, 
                     cp=learnTree$cptable[which.min(learnTree$cptable[,4]),1])
prp(learnOptimal, extra=1, cex=0.5)


#Erreur sur la base d'apprentissage
erreurTree = sum((predict(learnOptimal)-training$Y)^2)/nrow(training)
erreurTree

#Pr?vision
predictY = predict(learnOptimal, baseTest)
#Risque empirique selon une perte quadratique
risqueTree = sum((predictY-baseTest$Y)^2)/nrow(baseTest)
risqueTree


risquesTree = c(erreurTree, risqueTree)


######################### Random Forest ##################

#*************************************************#
#*************** Avec validation crois?e *********#
#*************************************************#

#library(caret)
learnRFY <- train(Y ~ ., data = training, method = "rf",
                 tr = trainControl(method = "repeatedcv", number=10, repeats = 3))
learnRFY
print(learnRFY$finalModel)
plot(learnRFY)
plot(learnRFY$finalModel)


#Importance des variables
varImpPlot(learnRFY$finalModel)
plot(Y~X4, data=training)
plot(Y~X16, data=training)

#Erreur sur la base d'apprentissage
erreurRFY = sum((predict(learnRFY)-training$Y)^2)/nrow(training)
erreurRFY

predictY <- predict(learnRFY, newdata=baseTest)
#Risque sur la base de test
risqueRFY = sum((predictY-baseTest$Y)^2)/nrow(baseTest)
risqueRFY

risquesRFY = c(erreurRFY,risqueRFY)



################################## SVM ##########################
library(e1071)

# Avec validation crois?e : cross = 10 
#Kernel = "linear"
svm_model1 <- svm(Y ~ ., 
                  data=training,
                  type="eps-regression",
                  kernel = "linear", #param?tre par d?faut : cost = 1
                  cross = 10
)
summary(svm_model1)
pred1 <- predict(svm_model1,newdata=baseTest)

Test.mod <- cbind(baseTest, pred1) ; head(Test.mod)

#Risque
risqueSVM1 = sum((pred1-baseTest$Y)^2)/nrow(baseTest)
risqueSVM1

# Avec validation crois?e : cross = 10
# kernel = "polynomial"

svm_model2 <- svm(Y ~ ., 
                  data=training,
                  type="eps-regression",
                  kernel = "polynomial", #param?tre par d?faut degree = 3 , gamma = 1/data dimension , coef0 = 0 , cost = 1
                  cross = 10
)
summary(svm_model2)
pred2 <- predict(svm_model2,newdata=baseTest)

Test.mod <- cbind(baseTest, pred2) ; head(Test.mod)
#Risque
risqueSVM2 = sum((pred2-baseTest$Y)^2)/nrow(baseTest)
risqueSVM2

# AVec validation crois?e : cross = 10
# kernel = "radial"

svm_model3 <- svm(Y ~ ., 
                  data=training,
                  type="eps-regression",
                  kernel = "radial", #param?tre par d?faut gamma = 1/data dimension et cost = 1 
                  cross = 10
)

summary(svm_model3)
pred3 <- predict(svm_model3,newdata=baseTest)

Test.mod <- cbind(baseTest, pred3)
head(Test.mod)
#risque
risqueSVM3 = sum((pred3-baseTest$Y)^2)/nrow(baseTest)
risqueSVM3

# AVec validation crois?e : cross = 10
# kernel = "sigmoid"

svm_model4 <- svm(Y ~ ., 
                  data=training,
                  type="eps-regression",
                  kernel = "sigmoid", #param?tre par d?faut gamma = 1/data dimension
                  cross = 10
)

summary(svm_model4)
pred4 <- predict(svm_model4,newdata=baseTest)

Test.mod <- cbind(baseTest, pred4)
head(Test.mod)

#risque
risqueSVM4 = sum((pred4-baseTest$Y)^2)/nrow(baseTest)
risqueSVM4



#Comparaison des risques
risquesSVM = data.frame(linear = risqueSVM1,
                        Polynomial = risqueSVM2,
                        Radial = risqueSVM3,
                        Sigmoid = risqueSVM4)
risquesSVM

# CONCLUSION : en choissant un noyau de type radial on minimise l'erreur 




################### COMPARAISON DES METHODES ###################
risquesPredY = data.frame(Modele_lineaire = risqueBwY,
                          Arbre = risqueTree,
                          Random_Forest = risqueRFY,
                          SVM = risqueSVM3)
risquesPredY







#################################
########### Pr?vision ###########
###########     Z    ############
#################################



#Cr?ation de Z
Z=ifelse(learn$Y<0,Z<-0,ifelse(learn$Y>=0 & learn$Y<=2,Z<-1,Z<-2))
Z=as.factor(Z)
learn2 = cbind(learn,Z)

#Suppression de la variable Y dans la table
learn2 = learn2[,-(ncol(learn2)-1)]
head(learn2)

#S?paration de la base en apprentissage (70%) et validation (30%)

app = createDataPartition(y=learn2$Z, p=0.80, list=FALSE )
trainingZ = learn2[app,]
baseTestZ = learn2[-app,]
nrow(trainingZ) ; nrow(baseTestZ)


########### Classifieur Bay?sien Naif #########


#**************************************
# Aprrentissage sans validation crois?e
#**************************************
#library(e1071)

# Mod?lisation
NaiveBayes <- naiveBayes(Z ~ ., data = trainingZ)
NaiveBayes

#La qualit? du mod?le d?pend de sa capacit? ? bien classer dans le jeu de donn?es test

Z.predict <- predict(object = NaiveBayes, newdata = baseTestZ)
confusionMatrix(data=Z.predict,reference=baseTestZ$Z)


#Taux d'erreur
Test.mod <- cbind(baseTestZ, Z.predict)
Confusion = table(Test.mod$Z, Test.mod$Z.predict)  # Taux de bien class?
Confusion
round(prop.table(Confusion), 2) #Pourcentage

errorCBN1 <- (1*Confusion[1,2]+Confusion[1,3]+
                1*Confusion[2,1]+1*Confusion[2,3]+
                1*Confusion[3,1]+1*Confusion[3,2])/sum(Confusion)

print(errorCBN1) 


#**************************************
# Aprrentissage sans validation crois?e
#**************************************

#method="nb"
#For classification using package klaR with tuning parameters:
# Laplace Correction (fL, numeric) :
# fL Factor for Laplace correction, default factor is 0, i.e. no correction.

library(klaR) ; library(MASS)

NB = train(Z~., 
           data=trainingZ, 
           method="nb", 
           tr=trainControl(method="repeatedcv", number=10, repeats=3)
)
NB

# La qualit? du mod?le
Z.predict.NB <- predict(object = NB, newdata = baseTestZ)
confusionMatrix(data=Z.predict.NB, reference=baseTestZ$Z)

#Taux d'erreur
Test.mod <- cbind(baseTestZ, Z.predict.NB)
Confusion = table(Test.mod$Z, Test.mod$Z.predict.NB)
Confusion
round(prop.table(Confusion), 2)

errorCBN2 <- (1*Confusion[1,2]+Confusion[1,3]+
                1*Confusion[2,1]+1*Confusion[2,3]+
                1*Confusion[3,1]+1*Confusion[3,2])/sum(Confusion)

print(errorCBN2) 




###################### CART #######################



learnTreeZ = rpart(Z~.,data=trainingZ, control=rpart.control(minsplit=1,cp=0))
plotcp(learnTreeZ)
printcp(learnTreeZ)


#min cp = 0.002 environ
learnSimple = prune(learnTreeZ, cp=0.002)
prp(learnSimple)
predictZSimple = predict(learnSimple, baseTestZ, type="class")
table(baseTestZ$Z, predictZSimple)

#Si on cherche automatiquement le cp minimal
learnOptimal = prune(learnTreeZ, 
                     cp=learnTree$cptable[which.min(learnTree$cptable[,4]),1])
prp(learnOptimal, extra=1, cex=0.5) #arbre tr?s fourni !
#Pr?vision
predictZOptimal = predict(learnOptimal, baseTestZ, type="class")
Confusion = table(baseTestZ$Z, predictZOptimal)
Confusion

errorTree <- (1*Confusion[1,2]+Confusion[1,3]+
                1*Confusion[2,1]+1*Confusion[2,3]+
                1*Confusion[3,1]+1*Confusion[3,2])/sum(Confusion)

print(errorTree) 




#################### Random Forest ####################

learnRF <- train(Z ~ ., data = trainingZ, method = "rf",
                 tr = trainControl(method = "repeatedcv", number=10, repeats = 3))
learnRF
print(learnRF$finalModel)
plot(learnRF)
plot(learnRF$finalModel)

#Importance des variables
varImpPlot(learnRF$finalModel)
plot(Z~X4, data=trainingZ)
plot(Z~X16, data=trainingZ)

Z.predict.RF <- predict(learnRF, newdata=baseTestZ)
confusionMatrix(data=Z.predict.RF, reference=baseTestZ$Z)

#Taux d'erreur
Test.mod <- cbind(baseTestZ, Z.predict.RF)
Confusion = table(Test.mod$Z, Test.mod$Z.predict.RF)
Confusion
round(prop.table(Confusion), 2)

errorRF <- (1*Confusion[1,2]+Confusion[1,3]+
              1*Confusion[2,1]+1*Confusion[2,3]+
              1*Confusion[3,1]+1*Confusion[3,2])/sum(Confusion)

print(errorRF) 






############### SVM ###########
#library(e1071)


#Selection du bon noyau 
#Comparaison des m?thodes


# Avec validation crois?e : cross = 10
# kernel = "polynomial"

svm_model1 <- svm(Z ~ ., 
                  data=trainingZ,
                  type="C-classification",
                  kernel = "polynomial", #param?tre par d?faut degree = 3 et gamma = 1/data dimension et coef0 = 0 et cost = C dans le cours = 1
                  cross = 10
)
summary(svm_model1)
pred1 <- predict(svm_model1,newdata=baseTestZ)

Test.mod <- cbind(baseTestZ, pred1)
Confusion = table(Test.mod$pred1, Test.mod$Z)
Confusion

errorSVM1 <- (1*Confusion[1,2]+Confusion[1,3]+
                1*Confusion[2,1]+1*Confusion[2,3]+
                1*Confusion[3,1]+1*Confusion[3,2])/sum(Confusion)

print(errorSVM1)


# AVec validation crois?e : cross = 10
# kernel = "sigmoid"

svm_model2 <- svm(Z ~ ., 
                  data=trainingZ,
                  type="C-classification",
                  kernel = "sigmoid", #param?tre par d?faut gamma = 1/data dimension
                  cross = 10
)

summary(svm_model2)
pred2 <- predict(svm_model2,newdata=baseTestZ)

Test.mod <- cbind(baseTestZ, pred2)
Confusion = table(Test.mod$pred2, Test.mod$Z)
Confusion

errorSVM2 <- (1*Confusion[1,2]+Confusion[1,3]+
                1*Confusion[2,1]+1*Confusion[2,3]+
                1*Confusion[3,1]+1*Confusion[3,2])/sum(Confusion)

print(errorSVM2)


# AVec validation crois?e : cross = 10
# kernel = "linear"

svm_model3 <- svm(Z ~ ., 
                  data=trainingZ,
                  type="C-classification",
                  kernel = "linear", #param?tre par d?faut cost = 1
                  cross = 10
)

summary(svm_model3)
pred3 <- predict(svm_model3,newdata=baseTestZ)

Test.mod <- cbind(baseTestZ, pred3)
Confusion = table(Test.mod$pred3, Test.mod$Z)
Confusion

errorSVM3 <- (1*Confusion[1,2]+Confusion[1,3]+
                1*Confusion[2,1]+1*Confusion[2,3]+
                1*Confusion[3,1]+1*Confusion[3,2])/sum(Confusion)

print(errorSVM3)

# AVec validation crois?e : cross = 10
# kernel = "radial"

svm_model4 <- svm(Z ~ ., 
                  data=trainingZ,
                  type="C-classification",
                  kernel = "radial", #param?tre par d?faut gamma = 1/data dimension et cost = 1
                  cross = 10
)

summary(svm_model4)
pred4 <- predict(svm_model4,newdata=baseTestZ)

Test.mod <- cbind(baseTestZ, pred4)
Confusion = table(Test.mod$pred4, Test.mod$Z)
Confusion

errorSVM4 <- (1*Confusion[1,2]+Confusion[1,3]+
                1*Confusion[2,1]+1*Confusion[2,3]+
                1*Confusion[3,1]+1*Confusion[3,2])/sum(Confusion)

print(errorSVM4)

# CONCLUSION : en choissant un noyau de type radial on minimise l'erreur avec les param?tres par d?fauts

#Optimisation pour le kernel radial   

#optimisation du cout C et du gamma 

tune = tune.svm(Z~., data = trainingZ, kernet="radical",
                cost=10^(-3:3),gamma=c(0,0.5,1,2))
plot(tune)
summary(tune)

#On remarque qu'en tra?ant le graphique avec l'optimisation du cout uniquement 
#on retrouve un cost optimal = 10^0 = 1 
#difficilement lisible sur le graphique

#Param?tres optimaux  

pred <- predict(tune$best.model,newdata=baseTestZ)

Test.mod <- cbind(baseTestZ, pred)
Confusion = table(Test.mod$pred, Test.mod$Z)
Confusion

error <- (1*Confusion[1,2]+Confusion[1,3]+
            1*Confusion[2,1]+1*Confusion[2,3]+
            1*Confusion[3,1]+1*Confusion[3,2])/sum(Confusion)

print(error)


# Taux d'erreur plus ?lev? avec les param?tres optimaux 
# On d?cide de garder le mod?le qui nous donne le moins d'erreurs 
# soit celui qui nous donnera de meilleures pr?dictions a priori

# le mod?le s?l?ctionn? n'est pas le mod?le optimal au sens des param?tres du kernel s?lectionn?
# mais ses pr?dictions sont meilleures





################### COMPARAISON DES METHODES ###################
risquesPredY = data.frame(CBN = errorCBN2,
                          Arbre = errorTree,
                          Random_Forest = errorRF,
                          SVM = errorSVM4)
risquesPredY



###################################################
#---------- Application des mod?les --------------#
#----------     sur la base test    --------------#
###################################################


summary(test)
ImpData <- mice(test,m=5,maxit=10,meth='pmm',seed=100)
ImpData
summary(ImpData)

test <- complete(ImpData) #table imput?e


#Pr?vision de Y gr?ce ? random forest
Y_predicted = predict(learnRFY, newdata=test)
test_Y = cbind(test,Y_predicted)

write.csv2(test_Y,file="test_Y.csv")


#Pr?vision de Z gr?ce ? random forest
Z_predicted = predict(learnRF, newdata=test)
test_Z = cbind(test, Z_predicted)
