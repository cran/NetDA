\name{WineData}
\docType{data}
\alias{WineData}
\title{Network-Based Discriminant Analysis Subject to Multi-Label Classes}
\description{
  The dataset, available at https://archive.ics.uci.edu/ml/datasets/wine,
  illustrates the usage of \code{\link{NetDA}} and \code{\link{Metrics}} functions.
  The dataset contains measurements of 178 units where the response is classified as three classes
  together with 13 relevant predictors.
}
\usage{data(WineData)}
\format{The dataset contains one response, 13 predictors, and 178 subjects.}

\examples{

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
}

\keyword{datasets}
