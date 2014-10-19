---
title: "RMQuiz2"
author: "Varun Boodram"
date: "October 18, 2014"
output: html_document
---
Question 1

Consider the following data with x as the predictor and y as as the outcome.

```r
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
```
Give a P-value for the two sided hypothesis test of whether $\beta1$ from a linear regression model is 0 or not.


```r
data<-data.frame(cbind(x, y))
# lm(outcome, predictor)
regr<-lm(formula = y~x, data)
coef(summary(regr))
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   0.1885     0.2061  0.9143  0.39098
## x             0.7224     0.3107  2.3255  0.05296
```
In the coefficient summary, under Pr(>|t|), we can find the desired value. 

Question 2

Consider the previous problem, give the estimate of the residual standard deviation.

The desired value can be read off the third-to-last line of 

```r
summary(regr)
```

```
## 
## Call:
## lm(formula = y ~ x, data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.2764 -0.1881  0.0136  0.1660  0.2714 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    0.188      0.206    0.91    0.391  
## x              0.722      0.311    2.33    0.053 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.223 on 7 degrees of freedom
## Multiple R-squared:  0.436,	Adjusted R-squared:  0.355 
## F-statistic: 5.41 on 1 and 7 DF,  p-value: 0.053
```
or it can be called directly with 

```r
summary(regr)$sigma
```

```
## [1] 0.223
```

Question 3

In the ```mtcars``` data set, fit a linear regression model of weight (predictor) on mpg (outcome). Get a 95% confidence interval for the expected mpg at the average weight. What is the lower endpoint?


```r
# fit regression line
fit<-lm(formula = mpg~wt, data = mtcars)
averageWeight<-mean(mtcars$wt)
```
Consider developing a probabilistic model for linear regression
$$Y_i=\beta_0+\beta_1X_i+\epsilon$$
where the$\epsilon$ are assumed iid $N\sim(0,1)$.Because of this, the conditional expectations of the Ys given the Xs, which we denote $\mu_i$ is
$$E[Y_i\vert X_i=x_i]=\mu_i=\beta_0+\beta_1x_i$$ This is easy to see: take the expected values  
$$
\begin{aligned}
E[Y_i\vert X_i=x_i]&=E[\beta_0+\beta_1X_i+\epsilon]\\
&=\beta_0+\beta_1X_i+E[\epsilon]\\
&=\beta_0+\beta_1X_i+0
\end{aligned}
$$
The point estimate for the outcome (mpg) at the average value of the predictor (wt) is $$\beta_0+\beta_1\bar{X}$$

```r
b0<-fit$coef[1]; b0
```

```
## (Intercept) 
##       37.29
```

```r
b1<-fit$coef[2]; b1
```

```
##     wt 
## -5.344
```

```r
point<-b0+b1*averageWeight; point
```

```
## (Intercept) 
##       20.09
```

Use the formula
$$
\begin{aligned}
95\%\text{CI}&=\text{point est}\pm ME\\
&=\text{point} \pm t_{tf}^*SE_{b_1}
\end{aligned}
$$

```r
point+c(1,-1)*qt(p = 0.975, df = fit$df)*0.5591
```

```
## [1] 21.23 18.95
```
The lower endpoint is larger than the higher. This is because mpg and wt are inversely corelated

```r
plot( mpg~wt, data=mtcars)
abline(fit, lwd=3, col="red")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Question 4

Refer to the previous question. Read the help file for mtcars. What is the weight coefficient interpreted as?

According to ```?mtcars```, the variable wt is Weight (lb/1000). Thus the weight coefficient is The estimated expected change in mpg per 1,000 lb increase in weight.

Question 5

Consider again the ```mtcars``` data set and a linear regression model with mpg as predicted by weight (1,000 lbs). A new car is coming weighing 3000 pounds. Construct a 95% prediction interval for its mpg. What is the upper endpoint?

The weight variable is is in units of (lb/1000). Define

```r
newWeight<-3000/1000
```
and predict the mpg with 

```r
# including the interval argument gives the 95% CI (default) upper and lower bounds for a predicted value
predMpg<-predict(object = fit, newdata = data.frame(wt = newWeight), interval = "predict"); predMpg
```

```
##     fit   lwr   upr
## 1 21.25 14.93 27.57
```
Question 6 (wrong ans)

Consider again the ```mtcars``` data set and a linear regression model with mpg as predicted by weight (in 1,000 lbs). A “short” ton is defined as 2,000 lbs. Construct a 95% confidence interval for the expected change in mpg per 1 short ton increase in weight. Give the lower endpoint.

Consider the impact of changing the units of X. Therefore, multiplication of $X$ by a factor $\alpha$ results in dividing the coefficient by a factor of $\alpha$.
$$
\begin{aligned}
Y_i&=\beta_0+\beta_1X_i+\epsilon_i\\
&=\beta_0+\frac{\beta_1}{\alpha}(\alpha X_i)+\epsilon_i\\
\end{aligned}
$$

Conversion to short tons is equivalent to multiplication of $X$ by $1/2$, so $\beta_1$ is scaled by a factor of 2. 

```r
b1<-2*b1
```
To calculate the Confidence interval for the slope, use the same reasoning as in question 3
$$
\begin{aligned}
95\%\text{CI}&=\text{point est}\pm ME\\
&=\text{b_1} \pm t_{tf}^*SE_{b_1}
\end{aligned}
$$

```r
b1+c(1,-1)*qt(p = 0.975, df = fit$df)*0.5591
```

```
## [1]  -9.547 -11.831
```
Question 7
If my X from a linear regression is measured in centimeters and I convert it to meters what would happen to the slope coefficient?
It would get multiplied by 100.

Question 9  
Refer back to the mtcars data set with mpg as an outcome and weight (wt) as the predictor. About what is the ratio of the the sum of the squared errors, $\sum_{i=1}^n(Yi−\hat{Y_i})^2$ when comparing a model with just an intercept (denominator) to the model with the intercept and slope (numerator)?

We need to fit two models: 

* numerator: regression with slope
* denominator: mean only regresion

The mean-only regression line is $\bar{Y}$, the mean of $Y$. This model is fitted with ```lm(outcome~1, data)```

```r
meanOnly<-lm(mpg~1, data = mtcars); meanOnly
```

```
## 
## Call:
## lm(formula = mpg ~ 1, data = mtcars)
## 
## Coefficients:
## (Intercept)  
##        20.1
```
anova gives the sum quared errors

```r
anova(meanOnly, fit)
```

```
## Analysis of Variance Table
## 
## Model 1: mpg ~ 1
## Model 2: mpg ~ wt
##   Res.Df  RSS Df Sum of Sq    F  Pr(>F)    
## 1     31 1126                              
## 2     30  278  1       848 91.4 1.3e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
278/1126
```

```
## [1] 0.2469
```
