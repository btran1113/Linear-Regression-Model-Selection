```r
library(ISLR)
library(lars)
library(leaps)
```

## PRESS calculator
```r
regpluspress <- function(x,y)
{
str<-lsfit(x,y)
#Calculate PRESS statistic with hat matrix and studentized residuals
press<-sum(str$resid/1-hat(x)^2)
str$press<-press
str
}
```
## 2nd order function
```r
matrix.2ndorder.make<-function(x, only.quad=F){
  x0<-x
  dimn<-dimnames(x)[[2]] #extract the names of the variables
  num.col<-length(x[1,]) # how many columns
  for(i in 1:num.col){
    # if we are doing all 2nd order
    if(!only.quad){
      for(j in i:num.col){
        x0<-cbind(x0,x[,i]*x[,j])
        dimn<-c(dimn,paste(dimn[i],dimn[j],sep=""))
        #create interaction dimnames
      }
    }
    else{
      #in here only if doing only squared terms
      x0<-cbind(x0,x[,i]*x[,i])
      dimn<-c(dimn,paste(dimn[i],"2",sep="")) # squared dimmension names
    }
  }
  dimnames(x0)[[2]]<-dimn
  x0
}
```
## A Leaps automatic model selector using Cp, and PRESS
```r
ncheck = 10
auto.mat<-as.matrix(Auto[,-9])
#Makes second order set of variables for auto data
 x.auto2<-matrix.2ndorder.make(auto.mat[,-1])
#Response vector mpg
y.auto<-auto.mat[,1]
#X matrix of predictors
x.auto<-auto.mat[,-1]
```
```r
#Removes 4 columns since leaps does not allow more than 31 variables
x.auto2<-x.auto2[,-c(29,32,34,35)]
#Fits x matrix of predictors and response vector mpg to find best model
dumleaps.auto<-leaps(x.auto2,y.auto)
```
```r
z1<-dumleaps.auto$Cp
o1<-order(z1)
#Identifies 10 best regression models based on the lowest Cp values
matwhich<-(dumleaps.auto$which[o1,])[1:ncheck,]
z2<-z1[o1][1:ncheck]
for(i in 1:ncheck){
  #Set dumleaps.auto0 as least squares fit structure with regpluspress function
  dumleaps.auto0<-regpluspress(x.auto2[,matwhich[i,]],y.auto)
  print(i)
  print(paste("Press=",dumleaps.auto0$press))
  parvec<-matwhich[i,]
  #Calculate number of predictors included in model
  npar<-sum(parvec)
  #Outputs mean squared prediction error, the expected squared distance between what the predictors predict for mpg and what the true value is
  print(paste("MSPE=",dumleaps.auto0$press/(length(y.auto)-(npar+1))))
  print(paste("Cp=",z2[i]))
}
#Calculate the actual predicted value
yprediction<-cbind(1,x.auto2[,matwhich[i,]])%*%dumleaps.auto0$coefficients
#Plots the actual y value vs prediction
plot(yprediction,y.auto, main = "Leaps Auto")
```
```r
sumabs<-function(x)
  {sum(abs(x))}

betanorm.lars<-function(str){
  v1<-apply(str$beta,1,sumabs)
  v1/max(v1)
}
```
### Lars automatic model selector using both Cp and cross validation MSE
```r
int=F
#Makes second order set of variables for auto data
x.auto2<-matrix.2ndorder.make(auto.mat[,-1])
y.auto<-auto.mat[,1]
#Set dumlars.auto as sequence of coefficients and fits for least angle regression with intercept not included in model
dumlars.auto<-lars(x.auto2,y.auto,intercept=int)
print(x.auto2)
#Set cv.str as K-fold cross-validated mean squared prediction error for least angle regression without intercept and plot
cv.str<-cv.lars(x.auto2,y.auto,plot.it=F,intercept=int)
o1<-order(cv.str$cv)
#Set mindex as index value of lowest cv value
mindex<-cv.str$index[o1][1]
beta<-coef(dumlars.auto)
#Set index0 as a list of the absolute value sum of coefficients for each row using sumabs function
index0<-apply(beta,1,sumabs)
#Calculates the proportion of each absolute sum of coefficients to the highest absolute sum of coefficients
index0<-index0/max(index0)
o1<-order(abs(index0-mindex))
I1<-(abs(index0-mindex)==min(abs(index0-mindex)))
n1<-length(beta[,1])
beta.out<-beta[I1,]
if(sum(abs(beta.out))==0){
  v1<-dumlars.auto$Cp-dumlars.auto$p
  o2<-order(v1)
  beta.out<-beta[o1,][1,]
}
Ind.out<-beta.out!=0
outlist<-list(beta.out=beta.out,ind.out=Ind.out)
if(int){
  #Calculate intercept
  Int.out1<-mean(y.auto)-mean(x.auto2%*%beta.out[i])
  #Creates list of these beta coefficients, logical vector, and intercept
  outlist<-list(beta.out=beta.out,ind.out=Ind.out,int.out=Int.out1)
}
outlist
#Calculate the actual predicted value
yprediction2<-x.auto2%*%beta.out
#Plots the actual y value vs prediction
plot(yprediction2,y.auto,main = "Lars Auto")
```

```r
#Get country of origin separately 
Auto.mat.japan<-Auto.mat[Auto.mat[,8]==3,]
Auto.mat.germany<-Auto.mat[Auto.mat[,8]==2,]
Auto.mat.usa<-Auto.mat[Auto.mat[,8]==1,]
#Makes 2nd order set of variables for Auto data on each country separately
Auto.mat.japan2<-matrix.2ndorder.make(Auto.mat.japan[,-1])
Auto.mat.germany2<-matrix.2ndorder.make(Auto.mat.germany[,-1])
Auto.mat.usa2<-matrix.2ndorder.make(Auto.mat.usa[,-1])
row.names(Auto.mat.japan2)<-NULL
row.names(Auto.mat.germany2)<-NULL
row.names(Auto.mat.usa2)<-NULL
#Use these as y vector and x matrix in the above code to get predictions on each country of origin separately
y.auto<-Auto.mat.japan[,1]
x.auto2<-Auto.mat.japan2
y.auto<-Auto.mat.germany[,1]
x.auto2<-Auto.mat.germany2
y.auto<-Auto.mat.usa[,1]
x.auto2<-Auto.mat.usa2
```

```r
#Calculates average mpg for each country of origin (1 = American, 2 = German, 3 = Japanese)
sum(Auto$mpg[Auto$origin == 1])/sum(Auto$origin == 1)
sum(Auto$mpg[Auto$origin == 2])/sum(Auto$origin == 2)
sum(Auto$mpg[Auto$origin == 3])/sum(Auto$origin == 3)
```
