#il dataset da utilizzare è PimaIndiansDiabetes2  appartenente al package ' mlbench'.
#L'obiettivo è quello di classificare correttamente ciascun paziente nella positive 
#class (variabile' diabetes') contenuta nell'ultima colonna del dataframe.
library(mlbench)
data(PimaIndiansDiabetes2)
pi2<-PimaIndiansDiabetes2.

#Elimino i valori NA dal dataset sostituendoli con la media
##############Se invece volessi sosituire con "0":   pi2[is.na(pi2)] <- 0
for(i in 1:ncol(pi2)) {
  valore.medio <- round(mean(pi2[,i], na.rm=T))
  pi2[is.na(pi2[,i]),i] <- valore.medio
}


#Splittare, in modo casuale, il dataframe in un training set e in un test set utilizzando le proporzioni 75%-25%;
index.random <- sort(sample(nrow(pi2), size=nrow(pi2)*0.75))
pi2.train <- pi2[index.random,]
pi2.test <- pi2[-index.random,]


#apprendere un classificatore kNN sul training set
#utilizzare l'algoritmo di classificazione così appreso per classificare le istanze nel test set, scegliendo
#in modo euristico il valore ottimale di k;
library(class)
pred_k5  <- knn(pi2.train[,-9], pi2.test[,-9], pi2.train[,9], k = 5, prob=F)
pred_k4  <- knn(pi2.train[,-9], pi2.test[,-9], pi2.train[,9], k = 4, prob=F)
pred_k3  <- knn(pi2.train[,-9], pi2.test[,-9], pi2.train[,9], k = 3, prob=F)

pred_k8  <- knn(pi2.train[,-9], pi2.test[,-9], pi2.train[,9], k = 8, prob=F)


#costruire una matrice di confusione e valutare l'accuratezza della classificazione.
matrix_k5 <-table(pi2.train[1:192,9], pred_k5)
sum(diag(table(pi2.train[1:192,9], pred_k5)))/sum(table(pi2.train[1:192,9], pred_k5))

matrix_k4 <-table(pi2.train[1:192,9], pred_k4)
sum(diag(matrix_k4))/sum(matrix_k4)

matrix_k3 <-table(pi2.train[1:192,9], pred_k3)
sum(diag(matrix_k3))/sum(matrix_k3)

matrix_k8 <-table(pi2.train[1:192,9], pred_k8)
sum(diag(matrix_k8))/sum(matrix_k3)

