require("ISLR")

################# MULTIVARIATE LINEAR REGRESSION ####################

#Getting the data
autodata=Auto

#response-mpg 

########### PRE-PROCESSING #################
y=as.vector(autodata$mpg)
xmul=autodata[,2:8]
xmul_scale=scale(xmul)
m=392
n=8
xmul1=cbind(1,scale(xmul))

xmul1=t(xmul1)


########## CREATING THE MODEL  ############

#Initializing the parameters
theta=numeric(n)
temp_cost=numeric(n)
iterations=0
h=numeric(m)
jvec=numeric(10000)
cost=0

#Training the model
repeat{
  iterations=iterations+1
  h=theta%*%as.matrix(xmul1)
  cost=(1/(2*m))*sum((h-y)^2)
  jvec[iterations]=cost
  print(cost)
  temp_cost=(0.01/(m)*(h-y)%*%t(xmul1))
  theta=theta-temp_cost
  #print(theta)
  if(iterations==10000){
    break
  }
}

#Using the model for prediction
y1=theta%*%(xmul1)
theta

##########  VALIDATION USING THE ANALYTICAL METHOD(NORMAL EQUATION) #########
x_mul=autodata[,2:8]
y_mul=autodata$mpg
n=ncol(x_mul)
m=392
xmul1=cbind(1,scale(x_mul))
xmul1=as.matrix(xmul1)
normaleq_mul=ginv(t(xmul1)%*%xmul1)%*%t(xmul1)%*%y_mul

