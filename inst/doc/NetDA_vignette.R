## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(NetDA)

## -----------------------------------------------------------------------------
data(WineData)
Y = WineData[,1] # the response
X = WineData[,2:14] # the predictors

## -----------------------------------------------------------------------------
D1 = WineData[which(Y==1),]
D2 = WineData[which(Y==2),]
D3 = WineData[which(Y==3),]

## -----------------------------------------------------------------------------
Train = rbind(D1[1:45,], D2[1:45,],D3[1:45,]) # user-specific training data
Test = rbind(D1[45:dim(D1)[1],],D2[45:dim(D2)[1],],D3[45:dim(D3)[1],]) # user-specific testing data

## -----------------------------------------------------------------------------
X = Train[,2:14]
Y = Train[,1]

## -----------------------------------------------------------------------------
X_test = Test[,2:14]
Y_test = Test[,1]

## ----cars---------------------------------------------------------------------
NetDA(X,Y,method=1,X_test) -> NetLDA
yhat_lda = NetLDA$yhat
Net_lda = NetLDA$Network

NetDA(X,Y,method=2,X_test) -> NetQDA
yhat_qda = NetQDA$yhat
Net_qda = NetQDA$Network

yhat_lda
round(Net_lda,3)

yhat_qda
round(Net_qda[[1]],3)

round(Net_qda[[2]],3)

round(Net_qda[[3]],3)

## -----------------------------------------------------------------------------
Metrics(yhat_lda,Y_test)
Metrics(yhat_qda,Y_test)

