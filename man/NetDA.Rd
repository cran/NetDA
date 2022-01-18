\name{NetDA}
\alias{NetDA}
\title{
Network-Based Discriminant Analysis Subject to Multi-Label Classes
}
\description{
Implementation of discriminant analysis with network structures in predictors accommodated to do classification and prediction.
}
\usage{
NetDA(X,Y, method,X_test)
}
\arguments{
  \item{X}{
an (n,p) ``matrix'' of the predictors from the training data.
}
  \item{Y}{
an n-dimensional ``vector'' of the response from the training data.
}

  \item{method}{
a ``scalar'' to determine the classification method. ``method = 1'' represents network-based linear discriminant analysis (NetLDA);
 ``method = 2'' represents network-based quadratic discriminant analysis (NetQDA).
}
  \item{X_test}{
an (m,p) ``matrix'' of the predictors from the testing data.
}
}
\details{
This function is used for the classification using discriminant analysis with network structures in predictors.
NetLDA is formulated by linear discriminant function with the corresponding estimated precision matrix obtained by pooling all subjects in the training data;
NetLDA is formulated by quadratic discriminant function with the estimated precision matrices determined by stratifying subjects from the associated classes.
}

\value{
\item{yhat}{a vector of predicted responses obtained by NetLDA or NetQDA.}
\item{Network}{the estimators of confusion matrices.}


}
\references{
  Chen, L.-P. (2022) Network-Based Discriminant Analysis for Multiclassification. Under revision.

  Friedman, J., Hastie, T., and Tibshirani, R. (2008). Sparse inverse covariance estimation with the graphical lasso. Biostatistics, 9, 432-441.
}

\author{
Chen, L.-P.
}



\examples{

data(WineData)

Y = WineData[,1]     # the response
X = WineData[,2:14]  # the predictors

D1 = WineData[which(Y==1),]
D2 = WineData[which(Y==2),]
D3 = WineData[which(Y==3),]

Train = rbind(D1[1:45,], D2[1:45,],D3[1:45,])   # user-specific training data
Test = rbind(D1[45:dim(D1)[1],], D2[45:dim(D2)[1],],D3[45:dim(D3)[1],]) # user-specific testing data

X = Train[,2:14]
Y = Train[,1]
X_test = Test[,2:14]
Y_test = Test[,1]

NetDA(X,Y,method=1,X_test) -> NetLDA

yhat_lda = NetLDA$yhat
Net_lda = NetLDA$Network

#############

NetDA(X,Y,method=2,X_test) -> NetQDA

yhat_qda = NetQDA$yhat
Net_qda = NetQDA$Network
}

\keyword{funtion}
\keyword{core}
