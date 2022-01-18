NetDA = function(X,Y, method,X_test) {  #method=1, LDA; method=2, QDA
#library(glasso)
rho = 0.01
class = max(Y)

n1 = dim(X_test)[1]
n = length(Y)
p = dim(X)[2]
pi = NULL             # prior
x = NULL   # predictors in each class
meanx = NULL
for(i in 1:class) {
pi = c(pi, length(which(Y==i))/n)
x[[i]] = X[which(Y==i),]
meanx[[i]] = as.vector(colMeans(x[[i]]))
}

#rho=0.1           # graphs based on different classes
Q_GLASSO = NULL
for(i in 1:class){
s<- as.matrix(var(x[[i]]))
Theta_glasso = glasso(s, rho=rho)$wi
#Theta_glasso[which(abs(Theta_glasso) < 0.1)] = 0
Q_GLASSO[[i]] = Theta_glasso
}

ls = as.matrix(var(X))   # graph without classes
L_GLASSO = glasso(ls, rho=rho)$wi
#L_GLASSO[which(abs(L_GLASSO) < 0.2)] = 0    # entries threhold
predClass = NULL

## Network-LDA
if(method == 1) {
EstPrecision = L_GLASSO
Cmatrix = NULL
for(i in 1:class) {
Cmatrix = cbind(Cmatrix,
log(pi[i]) - 0.5 * as.numeric(t(meanx[[i]]) %*% L_GLASSO %*% meanx[[i]]) + as.vector(meanx[[i]] %*% L_GLASSO %*% t(X_test)) )
}
for(j in 1:n1) {
predClass = c(predClass, which(Cmatrix[j,] == max(Cmatrix[j,]) )  )
}
    }

## Network-QDA
if(method == 2) {
EstPrecision = Q_GLASSO
Cmatrix = NULL
for(i in 1:class) {
Cmatrix = cbind(Cmatrix,
log(pi[i]) - 0.5* log(det(Q_GLASSO[[i]])) - 0.5 * diag(t(as.matrix(t(X_test)-meanx[[i]])) %*% Q_GLASSO[[i]] %*% (as.matrix(t(X_test)-meanx[[i]])))  )
}
for(j in 1:n1) {
predClass = c(predClass, which(Cmatrix[j,] == max(Cmatrix[j,]) )  )
}
    }

List = list("yhat" = predClass, "Network" = EstPrecision)
return(List)



}
