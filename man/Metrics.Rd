\name{Metrics}
\alias{Metrics}
\title{
Network-Based Discriminant Analysis Subject to Multi-Label Classes
}
\description{
Implementation of discriminant analysis with network structures in predictors accommodated to do classification and prediction.
}
\usage{
Metrics(yhat,Y_test)
}
\arguments{
  \item{yhat}{
an m-dimensional ``vector'' of the predicted values determined by NetDA.
}
  \item{Y_test}{
an m-dimensional ``vector'' of the response from the testing data.
}



}
\details{
This function aims to report the performance of classification results. The output includes confusion matrices and some commonly used criteria, such as
precision, recall, F-score, and ARI.
}

\value{
\item{Confusion matrix}{A confusion matrix based on predicted values and responses from the testing data}
\item{(PRE,REC,F-score)}{Values of precision (PRE), recall (REC), and F-score}
\item{ARI}{Values of the adjusted Rand index (ARI)}

}
\references{

Chen, L.-P., Yi, G. Y., Zhang, Q., and He, W. (2019). Multiclass analysis and prediction with network structured covariates. Journal of Statistical Distributions and Applications, 6:6.

Hubert, L. and Arabie, P. (1985). Comparing partitions. Journal of Classification, 2, 193-218.
}

\author{
Chen, L.-P.
}

\seealso{
  \code{\link{NetDA}}
%  \code{\link{glasso}}
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
Metrics(yhat_lda,Y_test)

#############

NetDA(X,Y,method=2,X_test) -> NetQDA
yhat_qda = NetQDA$yhat
Metrics(yhat_qda,Y_test)

}

\keyword{funtion}
%\keyword{core}
