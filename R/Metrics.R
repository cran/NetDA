Metrics = function(yhat,Y_test) {

I = max(Y_test)

M = matrix(0,I,I) ## confusion table
for(i in 1:length(Y_test)) {
M[yhat[i], Y_test[i]] = M[yhat[i], Y_test[i]] + 1
}

###############################

TP_lda = diag(M)
FP_lda = rowSums(M) - TP_lda
FN_lda = colSums(M) - TP_lda

PRE_lda = sum(TP_lda) / sum(TP_lda + FP_lda)
REC_lda = sum(TP_lda) / sum(TP_lda + FN_lda)

F_lda = 2 * ((PRE_lda * REC_lda) / (PRE_lda + REC_lda))

###############################

d = dim(M)[1]

col = colSums(M)
row = rowSums(M)

a1 = sum(choose(M,2))
a2 = sum(choose(col,2))
a3 = sum(choose(row,2))
a4 = choose(sum(M),2)

ari = (a1 - ((a2*a3)/a4) ) / (  ((a2+a3)/2)  - ((a2*a3)/a4)        )

################################


List = list("Confusion matrix" = M, "(PRE,REC,F-score)" = c(PRE_lda,REC_lda,F_lda),
"ARI" = ari)

return(List)


}

