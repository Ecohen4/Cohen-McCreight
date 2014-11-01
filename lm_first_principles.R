elm<-function(X,Y) {
  X <- as.matrix(X)    # predictor set X
  Y <- as.matrix(Y)    # response vector Y
  Xaug <- cbind(1,X)  # augmented matrix
  Yhat <- Xaug %*% solve(t(Xaug) %*% Xaug) %*% t(Xaug) %*% Y # least-squares solution
  residuals <- Y - Yhat
#   hatm <- Xaug %*% solve(t(Xaug) %*% Xaug) %*% t(Xaug)
#   beta <- solve(t(Xaug) %*% Xaug) %*% t(Xaug) %*% Y
#   SST = sum((Y - mean(Y))^2)    # deviation of observed values from sample mean
#   SSR = sum((Yhat - mean(Y))^2) # deviation of fitted values from sample mean
#   return(list(beta, SST, SSR))
  return(data.frame(X=X, Y=Y, Yhat=Yhat, residuals=residuals))
}