# By Gaurav Yadav
# reading file and storing that in matrix form
train = as.matrix(read.table("Iris_data_norm_train.txt",sep=","))
test <- as.matrix(read.table("iris_data_norm_test.txt",sep=","))
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
in_size <- ncol(train)-1 # last column will be the class of data
w <- runif(in_size,0,1)
#for plotting the error in each learning step make an array to store them
errorin <- vector(mode="numeric",length=0)
errorout <- vector(mode="numeric",length=0)

rows <- nrow(train)
test_rows <- nrow(test)

for(f in 1:100){
  no_errin=0
  no_errout=0

  for (i in 1:rows ){
    x <- train[i,1:in_size]
    x <- as.numeric(x)
    expected <- as.numeric(train[i,in_size+1])
    result = w%*%x
    error = expected - H(result)
    w <- w + 0.5*error*x
  }

  for(i in 1:rows){
    #i <- as.integer(runif(1,1,rows))
    x <- train[i,1:in_size]
    x <- as.numeric(x)
    expected <- as.numeric(train[i,in_size+1])
    result = w%*%x
    if(result<0 && expected == 1){
      no_errin <- no_errin+1
    }
    if(result>0 && expected == -1){
      no_errin <- no_errin+1
    }
  }

  for(i in 1:test_rows){
    y <- test[i,1:in_size]
    ex_out <- as.numeric(test[i,in_size+1])
    y <- as.numeric(y)
    result_out = y%*%w
    if(result_out<0 && ex_out == 1){
      no_errout <- no_errout+1
    }
    if(result>0 && ex_out == -1){
      no_errout <- no_errout+1
    }
  }
  errorin <- c(errorin,no_errin)
  errorout <- c(errorout,no_errout)
}
print("END")

errorin
print("errorout")
errorout


#old.par <- par(mfrow=c(1, 2))
jpeg('30-70.jpg')
plot(errorin,ylab="Error per iteration",xlab="iteration",main=" Error plot for 30% of training data and 70% of test",pch='.',col='blue')
lines(errorin,col="blue")
par(new = T)
plot(errorout,xlab=NA,ylab=NA,main=NA,pch='.', col='blue',axes=FALSE)
lines(errorout,col="red")
legend("topright",c("error_in","error_out"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red") )
#dev.copy(png,'60-40.png')
dev.off()
#points(iris$Petal.Length, iris$Petal.Width,iris$Sepal.Lenght , pch=19, col=iris$Species)
