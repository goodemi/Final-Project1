Final-Project1
==============

Final Final Project

A. > setwd("C:/Documents and Settings/Russ Baker/Desktop/Legal Analytics/Final Project")

> traffic <- read.csv("traffic.csv")

> View(traffic)

> summary(traffic)
       X              state            year     
 Min.   :  1.00   Min.   : 1.00   Min.   :1982  
 1st Qu.: 84.75   1st Qu.:18.75   1st Qu.:1983  
 Median :168.50   Median :30.50   Median :1985  
 Mean   :168.50   Mean   :30.19   Mean   :1985  
 3rd Qu.:252.25   3rd Qu.:42.50   3rd Qu.:1987  
 Max.   :336.00   Max.   :56.00   Max.   :1988  
     mrall           beertax             mlda      
 Min.   :0.8212   Min.   :0.04331   Min.   :18.00  
 1st Qu.:1.6237   1st Qu.:0.20885   1st Qu.:20.00  
 Median :1.9560   Median :0.35259   Median :21.00  
 Mean   :2.0404   Mean   :0.51326   Mean   :20.46  
 3rd Qu.:2.4179   3rd Qu.:0.65157   3rd Qu.:21.00  
 Max.   :4.2178   Max.   :2.72076   Max.   :21.00  
     jaild           comserd           vmiles      
 Min.   :0.0000   Min.   :0.0000   Min.   : 4.576  
 1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.: 7.183  
 Median :1.0000   Median :1.0000   Median : 7.796  
 Mean   :0.7202   Mean   :0.8155   Mean   : 7.891  
 3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.: 8.504  
 Max.   :1.0000   Max.   :1.0000   Max.   :26.148  
     unrate           perinc     
 Min.   : 2.400   Min.   : 9514  
 1st Qu.: 5.475   1st Qu.:12086  
 Median : 7.000   Median :13763  
 Mean   : 7.347   Mean   :13880  
 3rd Qu.: 8.900   3rd Qu.:15175  
 Max.   :18.000   Max.   :22193



B. > ggplot(traffic) + geom_point(aes(x=vmiles, y= mrall)) + scale_x_continuous("Average Miles Driven Per Driver") + scale_y_continuous("Traffic Fatality Rate (Deaths per 10,000 People)") + ggtitle("Average Miles Driven Per Driver vs. Traffic Fatality Rate") + geom_smooth(method=lm, aes(x=vmiles, y=mrall))




C. > m1 <- lm(mrall ~ beertax + jaild + comserd + vmiles + unrate + perinc, data= traffic)
> summary(m1)

Call:
lm(formula = mrall ~ beertax + jaild + comserd + vmiles + unrate + 
    perinc, data = traffic)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.7035 -0.2553 -0.0681  0.2360  1.8998 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.536e+00  3.521e-01   7.203 4.05e-12 ***
beertax      9.686e-02  5.718e-02   1.694  0.09119 .  
jaild       -1.502e-01  6.582e-02  -2.282  0.02312 *  
comserd     -1.961e-01  7.525e-02  -2.605  0.00959 ** 
vmiles       1.381e-01  1.772e-02   7.796 8.52e-14 ***
unrate       8.118e-03  1.250e-02   0.650  0.51636    
perinc      -1.028e-04  1.473e-05  -6.980 1.63e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4337 on 329 degrees of freedom
Multiple R-squared:  0.4318,	Adjusted R-squared:  0.4215 
F-statistic: 41.68 on 6 and 329 DF,  p-value: < 2.2e-16


> m2 <- lm(mrall ~ beertax + jaild + comserd + vmiles + perinc, data= traffic)
> summary(m2)

Call:
lm(formula = mrall ~ beertax + jaild + comserd + vmiles + perinc, 
    data = traffic)

Residuals:
     Min       1Q   Median       3Q      Max 
-2.64832 -0.25841 -0.06616  0.22798  1.90341 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.7110510  0.2273182  11.926  < 2e-16 ***
beertax      0.0914406  0.0565141   1.618   0.1066    
jaild       -0.1554600  0.0652650  -2.382   0.0178 *  
comserd     -0.1915707  0.0748656  -2.559   0.0109 *  
vmiles       0.1336926  0.0163304   8.187 5.92e-15 ***
perinc      -0.0001084  0.0000120  -9.034  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4333 on 330 degrees of freedom
Multiple R-squared:  0.4311,	Adjusted R-squared:  0.4225 
F-statistic: 50.02 on 5 and 330 DF,  p-value: < 2.2e-16






D. > m3 <- lm(mrall ~ jaild + comserd + vmiles + perinc, data= traffic)
> summary(m3)

Call:
lm(formula = mrall ~ jaild + comserd + vmiles + perinc, data = traffic)

Residuals:
     Min       1Q   Median       3Q      Max 
-2.73862 -0.25585 -0.06221  0.25184  1.85266 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.852e+00  2.104e-01  13.555  < 2e-16 ***
jaild       -1.308e-01  6.362e-02  -2.056  0.04053 *  
comserd     -2.215e-01  7.272e-02  -3.046  0.00251 ** 
vmiles       1.376e-01  1.619e-02   8.495 6.79e-16 ***
perinc      -1.169e-04  1.081e-05 -10.815  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4344 on 331 degrees of freedom
Multiple R-squared:  0.4266,	Adjusted R-squared:  0.4197 
F-statistic: 61.57 on 4 and 331 DF,  p-value: < 2.2e-16



E. > cor.test(traffic$vmiles, traffic$mrall)

	Pearson's product-moment correlation

data:  traffic$vmiles and traffic$mrall
t = 7.9011, df = 334, p-value = 4.041e-14
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.3026872 0.4833037
sample estimates:
      cor 
0.3968298 

