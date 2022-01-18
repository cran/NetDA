\name{NetDA-package}
\alias{NetDA-package}
\docType{package}
\title{
Network-Based Discriminant Analysis Subject to Multi-Label Classes
}
\description{
Implementation of discriminant analysis with network structures in predictors accommodated to do classification and prediction.
}
\details{
There are two functions in this package: NetDA and Metrics. NetDA aims to construct network-based linear discriminant function and
network-based quadratic discriminant function based on the training data, and then do classification for individuals in the testing data. Predicted values can be determined by NetDA.
The function Metrics provides a confusion matrix and some commonly used criteria to assess the performance of classification and prediction.
}

\author{
Chen, L.-P.

Maintainer: Li-Pang Chen <lchen723@nccu.edu.tw>
}
\references{
  Chen, L.-P. (2022) Network-Based Discriminant Analysis for Multiclassification. Under revision.

  Friedman, J., Hastie, T., and Tibshirani, R. (2008). Sparse inverse covariance estimation with the graphical lasso. Biostatistics, 9, 432-441.

}

\seealso{
  \code{\link[NetDA]{NetDA}}
  %\code{\link[Metrics]{Metrics}}
}
\keyword{ package }

