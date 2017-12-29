require("ISLR")
library("plotly")

############# SIMPLE LINEAR REGRESSION #########

#Getting the data
autodata=Auto

#response-mpg and predictor-horsepower
 

######## PRE-PROCESSING #########
y=as.vector(autodata$mpg)
x=autodata$horsepower
m=length(x)
n=2
x1=cbind(1,scale(x))

########### CREATING THE MODEL ##########

#Initializing the parameters
theta=numeric(n)
temp_cost=numeric(n)
iterations=0
jvecs=numeric(10000)
temp_theta0=numeric(100)
temp_theta1=numeric(100)
x1=t(x1)

#Training the Model
repeat{
  iterations=iterations+1
  h=theta%*%x1
  cost=(1/(2*m))*sum((h-y)^2)
  jvecs[iterations]=cost
  temp_cost=(0.1/(m)*(h-y)%*%t(x1))
  theta=theta-temp_cost
  temp_theta0[iterations]=theta[1]
  temp_theta1[iterations]=theta[2]
  if(iterations==10000){
    break
  }
}

#Prediction using the model
y1=theta%*%(x1)


##########  VALIDATION USING THE ANALYTICAL METHOD(NORMAL EQUATION) ###########
y=autodata$mpg
x=autodata$horsepower
m=length(x)
n=2
x1=cbind(1,scale(x))
normaleq=ginv(t(x1)%*%x1)%*%t(x1)%*%y

#Observe the identical values
theta
normaleq


######## OPTIONAL SECTION #######

#Plot for variation of cost with theta
# plot_ly(
#   x = temp_theta0[1:100], 
#   y = temp_theta1[1:100], 
#   z = jvecs, 
#   type = "contour" 
# )
