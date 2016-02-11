# By Gaurav Yadav
# reading file and storing that in matrix form
X = as.matrix(read.table("Iris_data_norm_train.txt",sep=","))
in_size <- ncol(X)-1

x <- X[,1:in_size]
Y <- X[,in_size+1]

nrows <- nrow(x)
x_new <- matrix(0,nrows,15)
for(i in 1:nrows){
  x_new[i,] <- c(1,x[i,1],x[i,2],x[i,3],x[i,4],x[i,1]*x[i,1],x[i,2]*x[i,2],x[i,3]*x[i,3],x[i,4]*x[i,4],
                    x[i,1]*x[i,2],x[i,1]*x[i,3],x[i,1]*x[i,4],x[i,2]*x[i,3],x[i,2]*x[i,4],x[i,3]*x[i,4])
}
x_newt <- t(x_new)
B <- x_newt%*%x_new
#finding the weight vector for linear regression
w <- solve(B)%*%x_newt%*%Y

test <- as.matrix(read.table("iris_data_norm_test.txt",sep=","))
no_err <- 0
test_row <- nrow(test)
for(i in 1:test_row){
  y <- test[i,1:in_size]
  ex_out <- as.numeric(test[i,in_size+1])
  y <- as.numeric(y)
  y_new <- c(1,y[1],y[2],y[3],y[4],y[1]*y[1],y[2]*y[2],y[3]*y[3],y[4]*y[4],
                    y[1]*y[2],y[1]*y[3],y[1]*y[4],y[2]*y[3],y[2]*y[4],y[3]*y[4])
  result = y_new%*%w
  if(result<0 && ex_out == 1){
    no_err <- no_err+1
  }
  if(result>0 && ex_out == -1){
    no_err <- no_err+1
  }
}
acc <- ((test_row-no_err)/test_row)*100
w
acc
