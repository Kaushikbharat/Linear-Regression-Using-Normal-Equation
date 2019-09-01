#~~~~~~~Let us Perform Linear Regression ~~~~~~#

#Let's Start with very basics

# What is Linear Regression ?

# Linear Regression gives us linear relationship between Dependent and Independent Variable.

# If there is only one variable we called it as Simple Linear Regression
# Y = Bo + B1.X ; where Y is a Dependent variable and Bo is intercept and B1 is coefficient

# If more than one variable it is called as Multiple Linear Regression
# Y = Bo + B1.X1 + B2.X2 + B3.X3 + ....... + BnXn

#Assumptions of Linear Regression:-
# 1) Linearity 
# 2) Homoscedasticity
# 3) No Multicollinearity
# 4) Normality
# 5) No Autocorrelation

# The aim to have a linear regression model is to find the possible values for the coefficient (B) 
# that minimize the error in the prediction of the output variable y.

# To minimize the errror we have two ways:-
# Normal Equation
# Gradient Descent

# Linear Regression using Normal Equation

# As the function is h(B) = Bo.X0 + B1.X1 + B2.X2 + B3.X3 + ....... + Bn.Xn
# Being dot product above (equation can also be written as h(B) = B(T) X,where X is any m * n matrix
# to minimize th error we have a cost function which is:- J(B) = 1/2m[sum(Predicted - Expected)^2]
# By doing some calculation we can easily derive B = (X(T)X)^-1 . X(T)Y ,so this is 
# Normal Equation with B giving the minimumvalue for the cost function, where 

#Let's Perform Simple Linear Regression first:

#Let's have a look have at this data this consists of Sales and Spends so we have to find out how Spends are drivimg my Sales.

# Dependent(Y) is Sales and Indpendent(X) is Spends.
Y <- mtcars$mpg
X <- mtcars$cyl

# Let's convert them into matrix form
X <- as.matrix(x = cbind(Bias = rep(1,length(X)),Spends = X))
Y <- matrix(data = Y,nrow = length(Y),ncol = 1,dimnames = list(c(rep("S",length(Y))),"Sales"))

# Let's try to solve using Normal Equation
Xhat <- t(X) %*% X  #X(T).X
Zhat <- inv(Xhat)   #(X(T).X)^-1
Yhat <- t(X) %*% Y  #X(T).Y
HBeta <- Zhat %*% Yhat #((X(T).X)^-1) . (X(T).Y)

# Results extracted from HBeta

# Coefficients 
#Intercept 37.884592
#Estimate -2.875788

# Predicted for Simple Linear Regression
Predicted <- (HBeta[2,] * mtcars$cyl) + HBeta[1,]

# Mean Squared Error
MSE <- sum((Predicted - Y)^2)/length(Y)

# Now Let's run regression using lm() function to compare the results.
Result <- lm(mpg~cyl,mtcars)
#Call:
#  lm(formula = mpg ~ cyl, data = mtcars)

#Coefficients:
#(Intercept)    cyl  
#37.885       -2.876 

# Now Let's Perform Multiple Linear Regression :
Ym <- mtcars$mpg
# Let's convert it into matrix form
Xm <- as.matrix(x = cbind(Bias = rep(1,nrow(mtcars)),mtcars[,-1]))
Ym <- matrix(data = Y,nrow = length(Y),ncol = 1,dimnames = list(c(rep("S",length(Ym))),"mpg"))
# Let's try to solve using Normal Equation
Xhatm <- t(Xm) %*% Xm  #X(T).X
Zhatm <- inv(Xhatm)   #(X(T).X)^-1
Yhatm <- t(Xm) %*% Ym  #X(T).Y
HBetam <- Zhatm %*% Yhatm #((X(T).X)^-1) . (X(T).Y)
row.names(HBetam) <- c("Intercept",names(mtcars)[-1])

#  12.30339 -0.11144048 0.01333524 -0.02148212 0.78711097 -3.71530393 0.82104075 0.31776281 2.52022689 0.65541302 -0.19941925

#Calculating Predicted

#Coefficients * Data
CDm <- data.frame(matrix(data = NA,nrow = nrow(mtcars),ncol = length(HBetam)-1))
names(CDm) <- row.names(HBetam)[-1]
for(name in names(mtcars[-1])){
  #name <- names(mtcars)[2]
  CDm[,name] <- HBetam[name,]*mtcars[,name]
}
CDm$Intercept <- HBetam[1]
Predictedm <- rowSums(CDm)
names(Predictedm) <- row.names(mtcars)
Predictedm
# Now Let's run regression usig lm() function to compare the results for multiple regression

Resultm <- lm(mpg~.,mtcars)

#Intercept    cyl      disp       hp      drat     wt       qsec      vs      am        gear   carb 
#12.30337  -0.11144  0.01334  -0.02148  0.78711 -3.71530  0.82104  0.31776   2.52023  0.65541 -0.19942
 
  