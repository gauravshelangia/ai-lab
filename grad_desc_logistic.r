#library(scatterplot3d)
#library(plot3D)
X = as.matrix(read.table("ex2data1.txt",sep=","))
#X = as.numeric(file)
n_row = nrow(X)
n_row

n_col = ncol(X)
n_col
#initialize weight to random
w = runif(n_col-1,0,1)*0.1
EIN = vector(mode="numeric",length=0)
miscount = vector(mode="numeric",length=0)
# nt for neeta
nt = 1
line = vector(mode="numeric",length = 0)

for(i in 1:100){
  #calculate delta E_in
  result = vector(mode="numeric",length = n_col-1)
  result1 = vector(mode="numeric",length =n_col-1)
  err =0
  #calculate the gradient of Ein
  for(j in 1:n_row){
    y= as.numeric(X[j,n_col])
    x = as.numeric(X[j,1:n_col-1])
    result = result+(y*x)/(1+exp(y*w%*%x))
  }
  # compute weight vector
  dE_in = -(result)/n_row
  w = w - nt*dE_in

  #calculate E_in for plotting further
  for(j in 1:n_row){
    y = as.numeric(X[j,n_col])
    x = as.numeric(X[j,1:n_col-1])
    result1 = result1 + log(1+exp(-y*w%*%x), base=exp(1))
    result1 = result1/n_row

    h = 1/(1+exp(w%*%x))
    if(h>0 && y ==0){
      err = err+1
    }
    if(h<0 && y==1){
      err= err+1
    }

  }
  #print(result1)
  #store the result1 in EIN
  EIN = c(EIN,result1)
  #acc = (118-err)*100/118
  #print(acc)
  miscount = c(miscount,err)
  line = c(line,w[2]/w[1]*X[i,3])
}
#miscount
jpeg('E_in1.jpg')
plot(EIN,ylab="Error ",xlab="iteration",main=" Plot of Ein ",pch=19,col='blue')
lines(EIN,col="blue")

#f = length(EIN)

jpeg('miscount.jpg')
plot(miscount,ylab="misclassifications count",xlab="iteration",main=" Plot of Ein ",pch=19,col='blue')
lines(miscount,col="blue")
acc = (100-err)
w
acc
