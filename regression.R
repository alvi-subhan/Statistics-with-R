library (MASS )
library (ISLR )
lm.fit =lm(medv~., data= Boston )
attach (Boston )
lm.fit =lm(medv~.)
summary(lm.fit)
names(lm.fit)
confint(lm.fit)
#confidence interval
predict (lm.fit , data.frame ( lstat =(c(5 ,10 ,15) )),interval =" confidence ")
#prediction interval
predict (lm.fit , data.frame ( lstat =(c(5 ,10 ,15) )),interval ="prediction")
plot(lstat ,medv)
abline (lm.fit )

abline (lm.fit ,lwd =3)
abline (lm.fit ,lwd =3, col =" red ")
plot(lstat ,medv ,col =" red ")
plot(lstat ,medv ,pch =20)
plot(lstat ,medv ,pch ="+")
plot (1:20 ,1:20 , pch =1:20)
#diagonistic plot
par ( mfrow =c(3,3) )
plot(lm.fit )
#
plot( hatvalues (lm.fit ))
#handling non linearity in medv~lstat
lm.fit2=lm(medv~lstat +I( lstat ^2) )
y=predict(lm.fit2)
#prediction using nonlinear
Y=predict (lm.fit , data.frame ( lstat))
plot(medv,Y)

##question 8.1 ISLR
lm.fit=lm(mpg~horsepower,data=Auto)
