# By Gaurav Yadav
# reading file and storing that in matrix form
X = as.matrix(read.table("Iris_data_norm_train.txt",sep=","))
in_size <- ncol(X)-1 # last column is class of that data
x <- X[,1:in_size]
xt <- t(X[,1:in_size])
Y <- X[,in_size+1]

B <- xt%*%x
#finding the weight vector for linear regression
w <- solve(B)%*%xt%*%Y

test <- as.matrix(read.table("iris_data_norm_test.txt",sep=","))
no_err <- 0
no_test <- nrow(test)
for(i in 1:no_test){
  y <- test[i,1:in_size]
  ex_out <- as.numeric(test[i,in_size+1])
  y <- as.numeric(y)
  result = y%*%w
  if(result<0 && ex_out == 1){
    no_err <- no_err+1
  }
  if(result>0 && ex_out == -1){
    no_err <- no_err+1
  }
}
acc <- (no_err/no_test)*100
w
