require("ISLR")
require("caTools")

########### LOGISTIC REGRESSION  ############

#response-mpg


#Getting the Auto dataset
autodata=Auto
y=as.vector(autodata$mpg)
xmul=autodata[2:8]
m=392
n=ncol(xmul)

########## PRE-PROCESSING  #############

#Scaling the predictors
xmul_scale=scale(xmul)

#Converting the response to binary

mpg01=numeric(length(y))
for(i in 1:m){
  if(y[i]>median(y))
  {
    mpg01[i]=1
  }else{
    mpg01[i]=0
  }
}

#Creating the new data frame after applying the changes
new.Auto=cbind(scale(xmul),mpg01)
new.Auto=cbind(1,new.Auto)
new.Auto=t(new.Auto)

############ CREATING THE MODEL ##############

#Initializing the parameters
theta=numeric(n+1)
temp_cost=numeric(n+1)
iterations=0
h=numeric(m)
cost=0

#Training  the model 
repeat{
  iterations=iterations+1
  h=1/(1+exp(-(theta%*%as.matrix(new.Auto[1:8,]))))
  cost=(-1/m)*sum(mpg01*log(h)+((1-mpg01)*log(1-h)))
  print(cost)
  #jvec1[iterations]=cost
   temp_cost=(0.1/(m)*(h-mpg01)%*%t(new.Auto[1:8,]))
  theta=theta-temp_cost
  #print(theta)
  if(iterations==10000){
    break
  }
}

new_mpg01=numeric(length(y))
y1=1/(1+exp(-(theta%*%(new.Auto[1:8,]))))
for(i in 1:length(y)){
  if(y1[i]>median(y1)){
    new_mpg01[i]=1
  }else{
    new_mpg01[i]=0
  }
}

accuracy=sum(mpg01==new_mpg01)/length(y)
accuracy

########## VALIDATION USING glm() FUNCTION ########

new.Auto = t(new.Auto)
new.Auto = as.data.frame(new.Auto)
new.Auto$V1 = NULL
log_mod = glm(mpg01~.,family = binomial(link = 'logit'),data=new.Auto)

#Compare the coefficients of log_mod to theta
log_mod$coefficients
theta


