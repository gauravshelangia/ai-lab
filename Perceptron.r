# By Gaurav Yadav
# reading file and storing that in matrix form
X = as.matrix(read.table("Iris_data_norm_train.txt",sep=","))

H<- function(x){
  if(x > 0){
    r <- 1
  }
  else{
    r <- -1
  }
  return (r)
}
# initial random weight are runif(len,in,last)
# last column is class of that data
in_size <- ncol(X)-1
w <- runif(4,0,1)
rows <- nrow(X)
for(f in 1:100){
  for (i in 1:rows ){
    x <- X[i,1:in_size]
    x <- as.numeric(x)
    expected <- as.numeric(X[i,in_size+1])
    result = w%*%x
    error = expected - H(result)
    w <- w + error*x
    }
}
print("END")

test <- as.matrix(read.table("iris_data_norm_test.txt",sep=","))
no_err <- 0
test_rows <- nrow(test)
in_size <- ncol(test)-1
for(i in 1:test_rows){
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

acc <- ((test_rows-no_err)/test_rows)*100
print(no_err)
print(acc)
