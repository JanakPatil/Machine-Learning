#Name - Janak Patil
#Net ID - jsp170630 
#Assignment 2

############################################################

library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(broom)
library(data.table)
library(DBI)
library(RSQLite)
library(sandwich)
library(lmtest)
library(tseries)
################# Problem 2.1 #####################

connection <- SQLite() %>% dbConnect("wooldridge.db")
vote1 <- connection %>% dbReadTable("VOTE1") %>% data.table
connection %>% dbReadTable("VOTE1_labels")
connection %>% dbDisconnect
#index variable.name  type format                  variable.label
#1      0         state  str2    %9s               state postal code
#2      1      district  byte  %3.0f          congressional district
#3      2        democA  byte  %3.2f             =1 if A is democrat
#4      3         voteA  byte  %5.2f              percent vote for A
#5      4       expendA float  %8.2f     camp. expends. by A, $1000s
#6      5       expendB float  %8.2f     camp. expends. by B, $1000s
#7      6      prtystrA  byte  %5.2f            % vote for president
#8      7      lexpendA float  %9.0g                    log(expendA)
#9      8      lexpendB float  %9.0g                    log(expendB)
#10     9        shareA float  %5.2f 100*(expendA/(expendA+expendB))

model1 <- lm((voteA~lexpendA+lexpendB+prtystrA), data=vote1)
summary(model1)
  #(i) What is the interpretation of b1?
#Answer- Every 1% increase in expendA there is a 0.0608% increase in Vote for candidate A

##(ii) In terms of the parameters, state the null hypothesis that a 1% increase in A's expenditures is offset by a 1% increase in B's expenditures.

# h1 --> mu == 0 -- there is no offset!
# h2 --> mu != 0 -- there is offset.

# (iii) Estimate the given model using the data in VOTE1.RAW and report the results in usual form. Do A's expenditures affect the outcome? What about B's expenditures? Can you use these results to test the hypothesis in part (ii)?
model1
# voteA = 45.079 + 6.083lexpendA - 6.615lexpendB + 0.152prtystrA

# Yes, A's expenditue affects the outcome.{With 1% increase in expenditure of A there is 0.060% increase in votes for candidate A.}
# Also, B's expenditure will affect the outcome as With 1% increase in expenditure of B there is 0.066% decrease in votes for candidate A.

#(iv) Estimate a model that directly gives the t statistic for testing the hypothesis in part(ii). What do you conclude? (Use a two-sided alternative.)
coeftest(model1) 
#h1 --> mu == 0 -- there is no offset!
#h2 --> mu != 0 -- ther is offset?
# As the p value is 0 we reject the null hypothesis in favor of alternate hypothesis i.e there is an offset between A's expenditure and B's expenditure.


###################### Problem 2.2 ########################

connection <- SQLite() %>% dbConnect("wooldridge.db")
lawsch85 <- connection %>% dbReadTable("LAWSCH85") %>% data.table
connection %>% dbReadTable("LAWSCH85_labels")
connection %>% dbDisconnect

  
# index variable.name  type format             variable.label
# 1      0          rank   int  %9.0g         law school ranking
# 2      1        salary float  %9.0g     median starting salary
# 3      2          cost   int  %9.0g            law school cost
# 4      3          LSAT   int  %9.0g          median LSAT score
# 5      4           GPA float  %9.0g         median college GPA
# 6      5        libvol   int  %9.0g no. volumes in lib., 1000s
# 7      6       faculty   int  %9.0g             no. of faculty
# 8      7           age   int  %9.0g     age of law sch., years
# 9      8        clsize   int  %9.0g     size of entering class
# 10     9         north  byte  %9.0g     =1 if law sch in north
# 11    10         south  byte  %9.0g     =1 if law sch in south
# 12    11          east  byte  %9.0g      =1 if law sch in east
# 13    12          west  byte  %9.0g      =1 if law sch in west
# 14    13       lsalary float  %9.0g                log(salary)
# 15    14       studfac float  %9.0g      student-faculty ratio
# 16    15         top10  byte  %9.0g     =1 if ranked in top 10
# 17    16        r11_25  byte  %9.0g         =1 if ranked 11-25
# 18    17        r26_40  byte  %9.0g         =1 if ranked 26-40
# 19    18        r41_60  byte  %9.0g         =1 if ranked 41-60
# 20    19       llibvol float  %9.0g                log(libvol)
# 21    20         lcost float  %9.0g                  log(cost)

#(i) Using the same model as in Problem 4 in Chapter 3, state and test the null hypothesis that the rank of law schools has no ceteris paribus effect on median starting salary.

# h0 --> mu == 0 : The rank of law school has no ceteris paribus effect on median starting salary.
# h0 --> mu != 0 : The rank of law school has ceteris paribus effect on median starting salary.

model2 <- lm(salary~rank, data = lawsch85)
coeftest(model2) 

# Here the t stat value is 0 so we reject the null hypothesis in favor of alternate hypothesis. i.e. the rank of law school has cetris paribus effect on median starting salary.

# (ii) Are features of the incoming class of students-namely, LSAT and GPA-individually or jointly significant for explaining salary? (Be sure to account for missing data on LSAT and GPA.)


### Replacing LSAT NA values by mean of LSAT.
temp<-lawsch85
lsat_avg <-mean(temp$LSAT,na.rm = T)
temp<-setDT(temp)
temp[,LSAT:=ifelse(is.na(LSAT)==T,lsat_avg,LSAT)]
temp
summary(lawsch85$GPA)
### Replacing GPA NA values by mean of GPA.
gpa_avg <- mean(temp$GPA,na.rm = T)
temp[,GPA:=ifelse(is.na(GPA)==T,gpa_avg,GPA)]
summary(temp$GPA)

model3 <- lm(salary~LSAT, data = temp)
model4 <- lm(salary~GPA, data = temp)
anova(model3,model4)
#Res.Df        RSS Df  Sum of Sq      F    Pr(>F)    
#1    146 1.0088e+10                                   
#2    143 7.3403e+09  3 2748165292 17.846 6.796e-10 ***
 # ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# YES, LSAT and GPA-individually and jointly significant for explaining salary.

# (iii) Test whether the size of the entering class (clsize) or the size of the faculty (faculty) needs to be added to this equation; carry out a single test. (Be careful to account for missing data on clsize and faculty.)

# Replacing clsize NA values by median of clsize.

median_clsize <- median(temp$clsize, na.rm = T)
temp[,clsize:=ifelse(is.na(clsize)==T,median_clsize,clsize)]
summary(temp$clsize)

median_faculty <- median(temp$faculty, na.rm = T)
temp[,faculty:=ifelse(is.na(faculty)==T,median_faculty,faculty)]
summary(temp$faculty)

model4 <- lm(salary~LSAT+GPA+clsize+faculty, data = temp)
summary(model4)

# Yes, the size of entering class(clsize) needs to be added to this equation to increase the R-squared value. Faculty is not significant.


#(iv) What factors might influence the rank of the law school that are not included in the salary regression?

model5 <-lm(salary~LSAT+GPA+clsize+faculty+age, data = temp)
summary(model5)

# Age factor influences the rank of the law school heavily.

###################### Problem 2.3 #####################
connection <- SQLite() %>% dbConnect("wooldridge.db")
hprice1 <- connection %>% dbReadTable("hprice1") %>% data.table
connection %>% dbReadTable("hprice1_labels")
connection %>% dbDisconnect

#index variable.name  type format               variable.label
#1      0         price float  %9.0g          house price, $1000s
#2      1        assess float  %9.0g       assessed value, $1000s
#3      2         bdrms  byte  %9.0g              number of bdrms
#4      3       lotsize float  %9.0g   size of lot in square feet
#5      4         sqrft   int  %9.0g size of house in square feet
#6      5      colonial  byte  %9.0g =1 if home is colonial style
#7      6        lprice float  %9.0g                   log(price)
#8      7       lassess float  %9.0g                   log(assess
#9      8      llotsize float  %9.0g                 log(lotsize)
#10     9        lsqrft float  %9.0g                   log(sqrft)
#(i)You are interested in estimating and obtaining a confidence interval for the percentage change in price when a 150-square-foot bedroom is added to a house. In decimal form, this is u1 5 150b1 1 b2. Use the data in HPRICE1.RAW to estimate u1.
model3 <- lm(log(price)~sqrft+bdrms, data = hprice1)
model3
summary(model3)
#Estimated model equation is : log(price) = 4.766 + 0.0003794 sqrft + 0.02888 bdrms
#Sample size = 88, R-squared = 0.5883. To estimate  , (^??1) = 150 * 0.0003794 + 0.02888 = 0.08579, that is an 
#additional 150 sqft bedroom increases the predicted price by about 8.6%

#(ii)
#??2 = ??1 - 150??1,here
#log(price) = ??1 + ??1sqrft + (??1 - 150??1)bdrms + u
#log(price) = ??0 + ??1(sqrft - 150bdrms) + ??1bdrms + u

#(iii)
temp <- (hprice1$sqrft-(150 * hprice1$bdrms))
model3a <- lm(log(price)~temp+bdrms, data = hprice1)
model3a
summary(model3a)
mean(hprice1$lprice)
conint <- data.frame(lotsize=0,sqrft=0,bdrms=0)
predict(model3a, newdata= unknownprice, interval = 'confidence')
#From part (ii), we run the regression: log(price) on (sqrft- 150bdrms) and bdrms, and
#obtain the coefficient of bdrms. We already know that ^??1 = 0.0858; now we also get
#se(^??1) = 0.0268. The 95% confidence interval is 0.0858 ± 1.987(0.0268), where 1.987 is the
#critical value with df = 90 - 88 2 ??? 1

###################### Problem 2.4 ########################
wage2 <- connection %>% dbReadTable("WAGE2") %>% data.frame
connection %>% dbReadTable("WAGE2_labels")


model6 <- lm(lwage~educ+exper+tenure, data = wage2)
summary(model6)

# log(wage) = 5.49 + 0.0748educ + 0.0153exper + 0.0133tenure
# h0 = b0 - b1 == 0 and 
# h1 = b0 - b1 != 0
# P-values are 0 and so we reject the null in favor of alternate hypothesis i.e. another year of general workforce experience does not have same effect on log(wage) as another year of tenure with the current employer.

model7 <- lm(log(wage)~exper+tenure, data = wage2)
model7
summary(model7)

###################### Problem 2.5 ########################

connection <- SQLite() %>% dbConnect('wooldridge.db')
fourooneksubs <- connection %>% dbReadTable("401KSUBS") %>% data.frame
connection %>% dbReadTable("401KSUBS_labels")
connection %>% dbDisconnect

#index variable.name  type format               variable.label
#1      0         e401k  byte  %9.0g     =1 if eligble for 401(k)
#2      1           inc float  %9.0g        annual income, $1000s
#3      2          marr  byte  %9.0g                =1 if married
#4      3          male  byte  %9.0g        =1 if male respondent
#5      4           age  byte  %9.0g                     in years
#6      5         fsize  byte  %9.0g                  family size
#7      6        nettfa float  %9.0g net total fin. assets, $1000
#8      7         p401k  byte  %9.0g  =1 if participate in 401(k)
#9      8          pira  byte  %9.0g               =1 if have IRA
#10     9         incsq float  %9.0g                        inc^2
#11    10         agesq   int  %9.0g                        age^2

#(i) How many single-person households are there in the data set?

NROW(fourooneksubs$fsize[fourooneksubs$fsize == 1])
# 2017 single-person households are there in data set.

#(ii) Use OLS to estimate the model nettfa 5 b0 1 b1inc 1 b2age 1 u, and report the results using the usual format. be sure to use only the single-person households in the sample. Interpret the slope coefficients. Are there any surprises in the slope estimates?
filtered <- fourooneksubs[fsize==1]
model8 <- lm(nettfa~inc+age, data = filtered)
model8
summary(model8)
#OLS Estimated model equation is nettfa = -43.03981 + 0.79932 inc + 0.84266 age. The coefficient on inc states that a dollar increase in the annual income increases the net total fin. assets by about 0.80 dollars approximately
#and the coefficient on age indicates that if age increases by one, the nettfa increases by 842.66 dollars. These slope estimates are not surprising

#(iii) Does the intercept from the regression in part (ii) have an interesting meaning? Explain.

#The intercept obtained from part (ii) is not interesting as it gives the nettfa when inc = 0 and age = 0, we can say that there are no
#values in the dataset satisfying these conditions.

#(iv) Find the p-value for the test H0: b2 5 1 against H1: b2 , 1. Do you reject H0 at the 1% significance level?
#H0 : ??2 = 1 
#H1 : ??2 < 1
#the p-value is 2e-16 and the alpha value is 0.01(1%).As the p-value is less than alpha, we can reject H0 at the 1% significance level.

#(v) If you do a simple regression of nettfa on inc, is the estimated coefficient on inc much different from the estimate in part (ii)? Why or why not?
model9 <- lm(nettfa ~ inc, data = filtered)
model9
cor(tempdata$age,tempdata$inc)
# In simple regression of nettfa on inc, the independent variable inc is the approx. same as the in model7 because, there the correlation between inc and age is 3%.

###################### Problem 2.6 ########################
kielmc <- connection %>% dbReadTable("KIELMC") %>% data.table
connection %>% dbReadTable("KIELMC_labels")
# 
# index variable.name  type format                  variable.label
# 1      0          year   int  %9.0g                    1978 or 1981
# 2      1           age   int  %9.0g                    age of house
# 3      2         agesq float  %9.0g                           age^2
# 4      3           nbh  byte  %9.0g               neighborhood, 1-6
# 5      4           cbd float  %9.0g dist. to cent. bus. dstrct, ft.
# 6      5         intst float  %9.0g        dist. to interstate, ft.
# 7      6        lintst float  %9.0g                      log(intst)
# 8      7         price float  %9.0g                   selling price
# 9      8         rooms  byte  %9.0g                # rooms in house
# 10     9          area   int  %9.0g         square footage of house
# 11    10          land float  %9.0g              square footage lot
# 12    11         baths  byte  %9.0g                     # bathrooms
# 13    12          dist float  %9.0g dist. from house to incin., ft.
# 14    13         ldist float  %9.0g                       log(dist)
# 15    14          wind  byte  %9.0g  prc. time wind incin. to house
# 16    15        lprice float  %9.0g                      log(price)
# 17    16           y81  byte  %9.0g              =1 if year == 1981
# 18    17         larea float  %9.0g                       log(area)
# 19    18         lland float  %9.0g                       log(land)
# 20    19      y81ldist float  %9.0g                       y81*ldist
# 21    20      lintstsq float  %9.0g                        lintst^2
# 22    21       nearinc  byte  %9.0g             =1 if dist <= 15840
# 23    22      y81nrinc  byte  %9.0g                     y81*nearinc
# 24    23        rprice float  %9.0g             price, 1978 dollars
# 25    24       lrprice float  %9.0g                     log(rprice)

#(i) To study the effects of the incinerator location on housing price, consider the simple regression model log(price) 5 b0 1 b1log(dist) 1 u, where price is housing price in dollars and dist is distance from the house to the incinerator measured in feet. Interpreting this equation causally, what sign do you expect for b1 if the presence of the incinerator depresses housing prices? Estimate this equation and interpret the results. model10 <- lm(log(price)~log(dist), data = kielmc) summary(model10)
model10 <- lm(log(price)~log(dist), data = kielmc)
summary(model10)
# The sign should be negative to show that price decrease.
# For 1% increase in the distance from house to incinerator, the price increases by 0.3% dollors.

# 
# (ii) To the simple regression model in part (i), add the variables log(intst), log(area),
# log(land), rooms, baths, and age, where intst is distance from the home to the interstate,
# area is square footage of the house, land is the lot size in square feet, rooms is
# total number of rooms, baths is number of bathrooms, and age is age of the house in
# years. Now, what do you conclude about the effects of the incinerator? Explain why
# (i) and (ii) give conflicting results.


model10 <- lm(log(price)~log(dist)+log(area)+log(land)+rooms + baths + age, data = context6)
summary(model10)

#table <- data.frame(cor(context6))
#View(table)
# For 1% increase in the distance from house to incinerator, the price increases by 0.2% dollors. There is conflicting results i.e log(dist) becomes insignificant due endogenity because of intst variable. 

#(iii) Add [log(intst)]2 to the model from part (ii). Now what happens? What do you conclude about the importance of functional form?

model11 <- lm(log(price)~log(dist)+log(intst)+I(log(intst)^2)+log(area)+log(land)+rooms+baths+age, data = context6)
summary(model11)

# The functional 

# (iv) Is the square of log(dist) significant when you add it to the model from part (iii)?

model12 <- lm(log(price)~log(dist)+I(log(dist)^2)+log(intst)+I(log(intst)^2)+log(area)+log(land)+rooms+baths+age, data = context6)
summary(model12)

# No, The  square of log(dist) is not significant when we add to the above equation.

###################### Problem 2.7 ########################

wage1 <- connection %>% dbReadTable("WAGE1") %>% data.table
connection  %>% dbReadTable("WAGE1_labels")

# index variable.name  type format                  variable.label
# 1      0          wage float  %8.2g         average hourly earnings
# 2      1          educ  byte  %8.0g              years of education
# 3      2         exper  byte  %8.0g      years potential experience
# 4      3        tenure  byte  %8.0g     years with current employer
# 5      4      nonwhite  byte  %8.0g                  =1 if nonwhite
# 6      5        female  byte  %8.0g                    =1 if female
# 7      6       married  byte  %8.0g                   =1 if married
# 8      7        numdep  byte  %8.0g            number of dependents
# 9      8          smsa  byte  %8.0g              =1 if live in SMSA
# 10     9      northcen  byte  %8.0g =1 if live in north central U.S
# 11    10         south  byte  %8.0g   =1 if live in southern region
# 12    11          west  byte  %8.0g    =1 if live in western region
# 13    12      construc  byte  %8.0g  =1 if work in construc. indus.
# 14    13       ndurman  byte  %8.0g  =1 if in nondur. manuf. indus.
# 15    14      trcommpu  byte  %8.0g  =1 if in trans, commun, pub ut
# 16    15         trade  byte  %8.0g    =1 if in wholesale or retail
# 17    16      services  byte  %8.0g        =1 if in services indus.
# 18    17      profserv  byte  %8.0g     =1 if in prof. serv. indus.
# 19    18       profocc  byte  %8.0g    =1 if in profess. occupation
# 20    19       clerocc  byte  %8.0g    =1 if in clerical occupation
# 21    20       servocc  byte  %8.0g     =1 if in service occupation
# 22    21         lwage float  %9.0g                       log(wage)
# 23    22       expersq   int  %9.0g                         exper^2
# 24    23       tenursq   int  %9.0g                        tenure^2


# (i) Use OLS to estimate the equation log(wage) 5 b0 1 b1educ 1 b2exper 1 b3exper 2 1 u and report the results using the usual format.


model10 <- lm(log(wage)~educ+exper+(I(exper*exper)), data = wage1)
summary(model10)  

#Is exper2 statistically significant at the 1% level?
# Yess, exper2 is statistically significant at the 1% level

#(ii)
#As the p-value of exper^2 is 1.63e-09, which is equivalent to 0, this is less than the alpha value of 0.01,thus
#exper^2 is statiscally significant at 1%.

#(iii)
#To estimate the return of the fifth year of experience, take exper = 4 and ???exper = 1,
#^(%???wage) =  100 * (0.0409731 - 2(0.0007121)4) * 1 = 3.52763 %
#To estimate the return of the twentieth year of experience, take exper = 19 and ???exper = 1,
#^(%???wage) =  100 * (0.0409731 - 2(0.0007121)19) * 1 = 1.39133 %

#(iv)
#The value of exper at which the predicted wage decreases with additional experience is 
#exper = 0.0409731 / (2 * 0.0007121) = 28.77 years
number <- nrow (wage1[exper >= 28.77])
number
#there are 121 people with atleast 28.77 years of experience in data set


#########################Problem 2.8 ##########################

connection <- SQLite() %>% dbConnect('wooldridge.db')
wage2 <- connection %>% dbReadTable('WAGE2') %>% data.table
connection %>% dbReadTable('WAGE2_labels')
connection %>% dbDisconnect

#index variable.name  type format                variable.label
#1      0          wage   int  %9.0g              monthly earnings
#2      1         hours  byte  %9.0g          average weekly hours
#3      2            IQ   int  %9.0g                      IQ score
#4      3           KWW  byte  %9.0g knowledge of world work score
#6      5         exper  byte  %9.0g      years of work experience
#7      6        tenure  byte  %9.0g   years with current employer
#8      7           age  byte  %9.0g                  age in years
#9      8       married  byte  %9.0g                 =1 if married
#11    10         south  byte  %9.0g           =1 if live in south
#12    11         urban  byte  %9.0g            =1 if live in SMSA
#13    12          sibs  byte  %9.0g            number of siblings
#14    13       brthord  byte  %9.0g                   birth order
#15    14         meduc  byte  %9.0g            mother's education
#16    15         feduc  byte  %9.0g            father's education
#17    16         lwage float  %9.0g           natural log of wage

model11 <- lm(log(wage)~ educ + exper + (educ*exper), data = wage2)
summary(model11)
model11 <- lm(log(wage)~ educ + exper + (educ*exper), data = wage2)
summary(model11)

#(i) Show that the return to another year of education (in decimal form), holding exper fixed, is b1 1 b3exper.

#log(wage) = ??0 + ??1educ +??2exper 
#???log(wage) = ??1educ + ??3exper???educ = (??1+??3exper)???educ
#???log(wage)/???educ = (??1 + ??3exper), the estimated change in wage variable given a change in the years of education

#(ii) State the null hypothesis that the return to education does not depend on the level of exper. What do you think is the appropriate alternative?
# h0 --> m0: b3 != 0
# h1 --> m1: b3 == 0

# (iii) Use the data in WAGE2.RAW to test the null hypothesis in (ii) against your stated alternative.
# As the p-value is 0.6 we reject the null hypo in favor of alternate hypothesis.

#(iv) Let ???1 denote the return to education (in decimal form), when exper 5 10: ???1 5 b1 1 10b3. Obtain ???^ 1 and a 95% confidence interval for ???1. (Hint: Write b1 5 ???1 2 10b3 and plug this into the equation; then rearrange. This gives the regression for obtainin the confidence interval for ???1.)
#log(wage) = ??0 +  (??1 -10 ??3)educ + ??2exper + ??3educ*exper
#log(wage) = ??0 +  ??1 educ -10 ??3educ + ??2exper + ??3educ*exper
#log(wage) = ??0 +  ??1 educ + ??2exper + ??3educ*exper-??3educ *10
#log(wage) = ??0 +  ??1 educ + ??2exper + ??3educ(exper-10)
m = context8$educ*(context8$exper - 10) #where m = educ(exper -10)
model13 <- lm(log(wage)~educ+exper+m, data=context8)
model13
summary(model13)
#Upon running the regression, we get the ^(??1)=0.076080 and se(^(??1))  = 0.006615, thus the 95% C.I
#for is from about (0.076080-0.006615 = 0.069465, 0.076080+0.006615 = 0.082695), ie 0.069465 to 0.082695

###################### Problem 2.9 #################

connection <- SQLite() %>% dbConnect('wooldridge.db')
GPA2 <- connection %>% dbReadTable("GPA2") %>% data.table
connection %>% dbReadTable("GPA2_labels")
connection %>% dbDisconnect
# 
# index variable.name   type format                   variable.label
# 1      0           sat    int %10.0g               combined SAT score
# 2      1        tothrs    int %10.0g  total hours through fall semest
# 3      2        colgpa  float  %9.0g          GPA after fall semester
# 4      3       athlete   byte  %8.0g                    =1 if athlete
# 5      4      verbmath  float  %9.0g            verbal/math SAT score
# 6      5         hsize double %10.0g           size grad. class, 100s
# 7      6        hsrank    int %10.0g              rank in grad. class
# 8      7        hsperc  float  %9.0g high school percentile, from top
# 9      8        female   byte  %9.0g                     =1 if female
# 10     9         white   byte  %9.0g                      =1 if white
# 11    10         black   byte  %9.0g                      =1 if black
# 12    11       hsizesq  float  %9.0g                          hsize^2

#(i) Use OLS to estimate the equation log(wage) 5 b0 1 b1educ 1 b2exper 1 b3exper 2 1 u and report the results using the usual format.

model13 <- lm(sat~ hsize + I(hsize*hsize), data = GPA2)
summary(model13)
# sat = 997.98 + 19.814hsize -2.131 hsize*hsize
# Yes, the qaudratic term is statistically significant.

# (ii) Using the estimated equation from part (i), what is the "optimal" high school size? Justify your answer.

table(GPA2$hsize)
summary(GPA2$sat)

#-19.814/2/-2.131 = 4.64 hundred is the optimal high school size.

# (iii) Is this analysis representative of the academic performance of all high school seniors?Explain.
# No, it is not representative of high school seniors. And not all students take SAT's.The data is old

#(iv) Find the estimated optimal high school size, using log(sat) as the dependent variable. Is it much different from what you obtained in part (ii)?

model14 <- lm(log(sat)~ hsize + I(hsize*hsize), data = GPA2)
summary(model14)

#-0.0196/2/-0.02087
#4.695 is the optimal high school score. It's not that different than the previous score.

###################### Problem 2.10 ####################

connection <- SQLite() %>% dbConnect("wooldridge.db")
hprice1 <- connection %>% dbReadTable("hprice1") %>% data.table
connection %>% dbReadTable("hprice1_labels")
connection %>% dbDisconnect

#index variable.name  type format               variable.label
#1      0         price float  %9.0g          house price, $1000s
#2      1        assess float  %9.0g       assessed value, $1000s
#3      2         bdrms  byte  %9.0g              number of bdrms
#4      3       lotsize float  %9.0g   size of lot in square feet
#5      4         sqrft   int  %9.0g size of house in square feet
#6      5      colonial  byte  %9.0g =1 if home is colonial style
#7      6        lprice float  %9.0g                   log(price)
#8      7       lassess float  %9.0g                   log(assess
#9      8      llotsize float  %9.0g                 log(lotsize)
#10     9        lsqrft float  %9.0g                   log(sqrft)

#(i)
model16 <- lm(log(price) ~ log(lotsize)+log(sqrft)+bdrms, data=hprice1)
model16
summary(model16)

#Estimated equation of the model is log(price) = -1.29704 + 0.16797 log(lotsize) + 0.70023 log(sqrft) + 0.03696 bdrms
#Sample size = 88, R-squared= 0.643

#(ii)
#Substituting the following values of lotsize=20,000, sqrft=2,500, and bdrms=4.
#the predicted value of lprice = -1.29704 + 0.16797 log (20,000) + 0.70023 log(2,500) + 0.03696 * 4=5.993
#the predicted value of price is as follows, ^(price)=exp(5.992)=$400.214

#(iii)
model17 <- lm(price ~ lotsize+sqrft+bdrms, data=hprice1)
model17
summary(model17) #Equation of the model is price = -21.770309 + 0.002068lotsize + 0.122778sqrft + 13.852522bdrms
plot(model16$residuals)
plot(model17$residuals)
#From the above two plots, it can be seen that in the model16,the residual values are scattered more around the mean compared to the 
#the plot from model17, therefore the log(model) better explains the variation in price. 

###################### Problem 2.11 ####################

connection <- SQLite() %>% dbConnect("wooldridge.db")
hprice1 <- connection %>% dbReadTable("hprice1") %>% data.table
connection %>% dbReadTable("hprice1_labels")
connection %>% dbDisconnect
summary(hprice1)

index variable.name  type format               variable.label
#1      0         price float  %9.0g          house price, $1000s
#2      1        assess float  %9.0g       assessed value, $1000s
#3      2         bdrms  byte  %9.0g              number of bdrms
#4      3       lotsize float  %9.0g   size of lot in square feet
#5      4         sqrft   int  %9.0g size of house in square feet
#6      5      colonial  byte  %9.0g =1 if home is colonial style
#7      6        lprice float  %9.0g                   log(price)
#8      7       lassess float  %9.0g                   log(assess
#9      8      llotsize float  %9.0g                 log(lotsize)
#10     9        lsqrft float  %9.0g                   log(sqrft)

#(i) Estimate the model price 5 b0 1 b1lotsize 1 b2sqrft 1 b3bdrms 1 u and report the results in the usual form, including the standard error of the regression. Obtain predicted price, when we plug in lotsize 5 10,000, sqrft 5 2,300, and bdrms 5 4; round this price to the nearest dollar.

lm(price~lotsize+sqrft+bdrms,data=hprice1) %>% summary
lm(price~lotsize+sqrft+bdrms,data=hprice1) %>% predict(data.frame(lotsize=10000,sqrft=2300,bdrms=4))

#(ii) Run a regression that allows you to put a 95% confidence interval around the predicted value in part (i). Note that your prediction will differ somewhat due to rounding error.

hprice1$lotsize_star <- hprice1$lotsize-10000
hprice1$sqrft_star <- hprice1$sqrft-2300
hprice1$bdrms_star <- hprice1$bdrms-4

lm(price~lotsize_star+sqrft_star+bdrms_star,data=hprice1) %>% summary
(lm(price~lotsize_star+sqrft_star+bdrms_star,data=hprice1) %>% summary %>% coef)[1]
lm(price~lotsize_star+sqrft_star+bdrms_star,data=hprice1) %>% confint
c(322.041735710, 3.513716e+02)

#(iii) Let price0 be the unknown future selling price of the house with the characteristics used in parts (i) and (ii). Find a 95% CI for price0 and comment on the width of this confidence interval.
322.041735710-3.513716e+02
# 29.32986 is a very small confidence interval

###################### Problem 2.12 ########################

connection <- SQLite() %>% dbConnect("wooldridge.db")
benefits <- connection %>% dbReadTable("benefits") %>% data.table
connection %>% dbReadTable("benefits_labels")
connection %>% dbDisconnect

#index variable.name   type format                         variable.label
#1      0        distid  float  %9.0g                    district identifier
#2      1         schid    int  %9.0g                      school identifier
#3      2         lunch  float  %9.0g           percent eligible, free lunch
#4      3        enroll    int  %9.0g                      school enrollment
#5      4         staff  float  %9.0g                staff per 1000 students
#6      5         exppp    int  %9.0g                 expenditures per pupil
#7      6        avgsal  float  %9.0g              average teacher salary, $
#8      7        avgben    int  %9.0g average teacher non-salary benefits, $
#9      8         math4  float  %9.0g    percent passing 4th grade math test
#10     9        story4  float  %9.0g percent passing 4th grade reading test
#11    10            bs  float  %9.0g                          avgben/avgsal
#12    11       lavgsal  float  %9.0g                            log(avgsal)
#13    12       lenroll  float  %9.0g                            log(enroll)
#14    13        lstaff  float  %9.0g                             log(staff)
#15    14         bsbar double %10.0g              within-district avg of bs
#16    15      lunchbar double %10.0g           within-district avg of lunch
#17    16    lenrollbar double %10.0g         within-district avg of lenroll
#18    17     lstaffbar double %10.0g          within-district avg of lstaff

#(i)
model19 <- lm(lavgsal ~ bs, data=benefits)
model19
summary(model19)

#As the p-value of bs is (0.00248*2= 0.00496 - for a two-sided alternative) which is less than the significance level value of 0.05, we reject the null hypothesis that H0: ??(bs) = 0, In the same way, for testing the null hypothesis of H0: ??(bs) = -1, Similarly for the null hypothesis,
#H0: ??(bs) = -1, the p-value could be around 0.0015, and hence we can reject the null hypothesis.


#(ii)
lbs <- log(benefits$bs)
#logval(bs)
minval <- min(lbs)
minval
maxval <- max(lbs)
maxval
stdev <- sd(lbs)
stdev
#The range of values for log(bs) is -2.330832(low) to -0.4162912(high), ie (-2.330832,-0.4162912) with the standard deviation as 0.09682376


#(iii)
model20 <- lm(lavgsal ~ log(bs), data=benefits)
model20
summary(model20)
#The R-squared value obtained is 0.003907 in part(iii), which is less compared to that of 0.004949 obtained from part(i)
#Hence, we can say that both the models do not fit well as the R-squared value is close to 0 and the variation in the dependent 
#variable is hardly explained by the independent variables


#(iv)
model21 <- lm(lavgsal ~ bs+lenroll+lstaff+lunch, data=benefits)
model21
summary(model21)
#Estimated equation of the model is lavgsal = 13.7236146 -0.1774391bs -0.0292405lenroll -0.6907026lstaff -0.0008471lunch
#Sample size is 1848 and R-squared is 0.4826. The coefficient of bs is -0.1774391, which falls within the magnitude by
#a huge amount. As the p-value of bs(0.14) is larger than the significance value of 0.05, we say that bs is not statistically significant

#(v)
#The coefficient of variable lstaff is -0.6907026. With the other variables fixed, we can say that a 1% increase in lstaff
#causes a 69% drop in the variable avgsal. We can state that teachers are paid less on the note that the students to be taught are less

#(vi)
model22 <- lm(lavgsal ~ bs+lenroll+lstaff+lunch+I(lunch^2), data=benefits)
model22
summary(model22)
#As the p-value of lunch^2 (3.14e-08) is less than the significance level of 0.05, we can say that lunch^2 
#is statistically significant. The turning point is calculated as follows,
#-3.600e-03 /(2*-3.178e-05) = 4.1851167
min(benefits$lunch)
#0
max(benefits$lunch)
#100
numb <- nrow(benefits[lunch > 4.1851167])
numb
#number of values of lunch greater than the turning point of 4.1851167 is 1701

#(vii)
#At the turning point of 4.1851167, we get the least predicted value of lavgsal, keeping all other variables constant/fixed,
#we can  write it as -3.600e-03 (lunch) - 3.178e-05 (lunch^2).
#For lunch = 0, we get 0.
#For lunch = 50, (-3.600e-03*50)-3.178e-05(50*50) = -62.495
#For lunch = 100, (-3.600e-03*100)-3.178e-05(100*100)) = -232.0552
#Thus for teaching at a school with lunch = 50 gives a predicted average salary lesser than the school having lunch = 0

###################### Problem 2.13 ########################

connection <- SQLite() %>% dbConnect('wooldridge.db')
wage2 <- connection %>% dbReadTable("WAGE2") %>% data.table
connection %>% dbReadTable("WAGE2_labels")
connection %>% dbDisconnect

#index variable.name  type format                variable.label
#1      0          wage   int  %9.0g              monthly earnings
#2      1         hours  byte  %9.0g          average weekly hours
#3      2            IQ   int  %9.0g                      IQ score
#4      3           KWW  byte  %9.0g knowledge of world work score
#5      4          educ  byte  %9.0g            years of education
#6      5         exper  byte  %9.0g      years of work experience
#7      6        tenure  byte  %9.0g   years with current employer
#8      7           age  byte  %9.0g                  age in years
#9      8       married  byte  %9.0g                 =1 if married
#10     9         black  byte  %9.0g                   =1 if black
#11    10         south  byte  %9.0g           =1 if live in south
#12    11         urban  byte  %9.0g            =1 if live in SMSA
#13    12          sibs  byte  %9.0g            number of siblings
#14    13       brthord  byte  %9.0g                   birth order
#15    14         meduc  byte  %9.0g            mother's education
#16    15         feduc  byte  %9.0g            father's education
#17    16         lwage float  %9.0g           natural log of wage
model14 <- lm(log(wage)~ educ+exper+tenure+married+black+south+urban,data = wage2)
summary(model14)

#(i)Estimate the model log(wage) 5 b0 1 b1educ 1 b2exper 1 b3tenure 1 b4married 1 b5black 1 b6south 1 b7urban 1 u and report the results in the usual form. Holding other factors fixed, what is the approximate difference in monthly salary between blacks and nonblacks? Is this difference statistically significant?
#Approximately salary difference is 18.8% and Yes, it is statistically significant.

model15 <- lm(log(wage)~ educ+exper+tenure+married+black+south+urban+I(exper*exper)+I(tenure*tenure),data = wage2)
summary(model15)
anova(model14,model15)

#(ii) Add the variables exper 2 and tenure2 to the equation and show that they are jointly insignificant at even the 20% level.
#Not rejecting the null as p value 0.226, so they are not significant.

model16 <- lm(log(wage)~ educ+exper+tenure+married+black+south+urban+educ:black,data = wage2)
summary(model16)

#(iii) Extend the original model to allow the return to education to depend on race and test whether the return to education does depend on race.
#If you're black, return to education is only 4.5% whereas if you're white it is 6.7%. The difference in insignificant. 


#### Brute force method:

wage2$sing_black <- (1-wage2$married)*wage2$black
wage2$sing_non_black <- (1-wage2$married)*(1-wage2$black)
wage2$marr_black <- wage2$married*wage2$black
wage2$marr_non_black <- wage2$married*(1-wage2$black)

#(iv) Again, start with the original model, but now allow wages to differ across four groups of people: married and black, married and nonblack, single and black, and single and nonblack. What is the estimated wage differential between married blacks and married nonblacks? model17 <- lm(log(wage)~ educ+exper+tenure+south+urban+sing_black+sing_non_black+marr_black,data = wage2) summary(model17)

#There's a 17.9% decrease in the wage for people who are married and black vs who are married and non black.


## How get rid of intercept
model18 <- lm(log(wage)~ educ+exper+tenure+south+urban+sing_black+sing_non_black+marr_black+marr_non_black-1,data = wage2)
summary(model18)

#  5.413242-5.592708 [1] -0.179466 %


## Approach 2
model19 <- lm(log(wage)~ educ+exper+tenure+married*black+south+urban,data = wage2)
summary(model19)

-0.24+0.0613
# Same as above!!!! 17% difference

###################### Problem 2.14 ########################

connection <- SQLite() %>% dbConnect('wooldridge.db')
mlbone <- connection %>% dbReadTable("MLB1") %>% data.table
connection %>% dbReadTable("MLB1_labels")
connection %>% dbDisconnect

# (i) State the null hypothesis that, controlling for other factors, catchers and outfielders
# earn, on average, the same amount. Test this hypothesis using the data in MLB1.RAW
# and comment on the size of the estimated salary differential.

model30 <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar+frstbase+scndbase+thrdbase+shrtstop+catcher, data = mlbone)
summary(model30)

# ho = B13 = 0; h1 = b13 != 0 Not Significant, there is no enough evidence to reject the null hyothesis.

# (ii) State and test the null hypothesis that there is no difference in average salary
# across positions, once other factors have been controlled for.

# ho = b9,b10,b11,b12,b13 == 0; h1; b9,b10,b11,b12,b13 ! = 0
#there is no enough evidence to reject the null hyothesis.

###################### Problem 2.15 ########################

connection <- SQLite() %>% dbConnect('wooldridge.db')
GPA2 <-connection %>% dbReadTable("GPA2") %>% data.table
connection %>% dbReadTable("GPA2_labels")
connection %>% dbDisconnect


model31 <- lm(colgpa~hsize+I(hsize^hsize)+hsperc+sat+female+athlete, data = GPA2)
summary(model31)

# We are unsure about coeffecients of hsize and female.
# Coefficients like hsperc,sat should be positive and higher while athelete can be negative.

# (ii) Estimate the equation in part (i) and report the results in the usual form. What is
# the estimated GPA differential between athletes and nonathletes? Is it statistically
# significant?

1.197e+00 + 1.710e-01 - 1.197e+00
# 0.171 is the estimated difference, Yes it is statistically significant.

# (iii) Drop sat from the model and reestimate the equation. Now, what is the estimated
# effect of being an athlete? Discuss why the estimate is different than that obtained
# in part (ii).


model32 <- lm(colgpa~hsize+I(hsize^hsize)+hsperc+female+athlete, data = GPA2)
summary(model32)
# The estimate is .00791 because the sat is not controlled in this model.

# (iv) In the model from part (i), allow the effect of being an athlete to differ by gender
# and test the null hypothesis that there is no ceteris paribus difference between
# women athletes and women nonathletes.

GPA2$femaleathletes <- GPA2$female * GPA2$athlete
GPA2$femalenonathletes <- GPA2$female * (!GPA2$athlete)
GPA2$maleathletes <- GPA2$athlete * (!GPA2$female)
GPA2$malenonathletes <- (!GPA2$athlete) * (!GPA2$female)

model33 <- lm(colgpa~hsize+I(hsize^hsize)+hsperc+sat+(femaleathletes+femalenonathletes), data = GPA2)
summary(model33)
anova(model32,model33)

# As p-value of F-statistics is 2.2e-16 which is appr. 0, we can say there is a significant difference between female athelets and non-athletes.

# (v) Does the effect of sat on colgpa differ by gender? Justify your answer.

model34 <- lm(colgpa~hsize+I(hsize^hsize)+hsperc+sat+femaleathletes+malenonathletes+maleathletes+I(female*sat), data = GPA2)
summary(model34)

#As the p-value of the coefficient of female*sat (0.6837) is greater than the p-critical value of 0.05 at 5% significance level
#showing that there is no statistically significant difference of effect of sat on colgpa by gender



############################ Problem 2.16 #########################

connection <- SQLite() %>% dbConnect('wooldridge.db')
loanapp <- connection %>% dbReadTable("LOANAPP") %>% data.table
connection %>% dbReadTable("LOANAPP_labels")

# (i) If there is discrimination against minorities, and the appropriate factors have been
# controlled for, what is the sign of b1?

model26 <- lm(approve~black+hispan, data = loanapp)
summary(model26)

# Yes is there is discrimination against minorities as the the sign for b1 is negative.


# (ii) Regress approve on white and report the results in the usual form. Interpret the
# coefficient on white. Is it statistically significant? Is it practically large?

model25 <- lm(approve~white, data = loanapp)
summary(model25)
# For every white person, the chances of approving the loan is 0.2 and it is statistically significant. Yes, it is large.


# (iii) As controls, add the variables hrat, obrat, loanprc, unem, male, married, dep, sch,
# cosign, chist, pubrec, mortlat1, mortlat2, and vr. What happens to the coefficient
# on white? Is there still evidence of discrimination against nonwhites?

model27 <- lm(approve~black+hispan+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data = loanapp)
summary(model27)

model28 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data = loanapp)
summary(model28)

# The coefficient of white has decreased for .2 to .12 but there is still discrimination as the coefficients of black and hispan are still negative.

# (iv) Now, allow the effect of race to interact with the variable measuring other obligations
#as a percentage of income (obrat). Is the interaction term significant?

model29 <- lm(approve~I(white*obrat), data = loanapp)
summary(model29)


# Yes, the interaction term is significant.

# (v) Using the model from part (iv), what is the effect of being white on the probability
# of approval when obrat = 32, which is roughly the mean value in the sample?
#   Obtain a 95% confidence interval for this effect.


predict(model29,data.frame(white = 1,obrat = 32),interval = "confidence")

#   fit       lwr       upr
# 0.8920034 0.8767971 0.9072097

###################### Problem 2.17 ########################

connection <- SQLite() %>% dbConnect('wooldridge.db')
fourooneksubs <- connection %>% dbReadTable("401KSUBS") %>% data.table
connection %>% dbReadTable("401KSUBS_labels")
connection %>% dbDisconnect

#(i) Compute the average, standard deviation, minimum, and maximum values of nettfa in the sample.
summary(fourooneksubs$nettfa)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-502.30   -0.50    2.00   19.07   18.45 1536.80 
sd(fourooneksubs$nettfa)
# 63.96384

#(ii) Test the hypothesis that average nettfa does not differ by 401(k) eligibility status; use a two-sided alternative. What is the dollar amount of the estimated difference?
model21 <- lm(nettfa ~ e401k, data = fourooneksubs)
summary(model21)
#  Hypothesis is  B_1 == 0; 18.96 thousand dollors is the estimated difference.

#(iii) From part (ii) of Computer Exercise C9, it is clear that e401k is not exogenous in a simple regression model; at a minimum, it changes by income and age. Estimate a multiple linear regression model for nettfa that includes income, age, and e401k as explanatory variables. The income and age variables should appear as quadratics. Now, what is the estimated dollar effect of 401(k) eligibility?

model22 <- lm(nettfa ~inc+age+I(inc*inc)+I(age*age)+e401k, data = fourooneksubs)
summary(model22)
# 9.7046879 thousand dollors is the estimated difference in net financial asets controlling income and age.



#(iv) To the model estimated in part (iii), add the interactions e401k ? (age 2 41) and e401k · (age 2 41)2. Note that the average age in the sample is about 41, so that in the new model, the coefficient on e401k is the estimated effect of 401(k) eligibility at the average age. Which interaction term is significant?
model23 <- lm(nettfa ~inc+age+I(inc*inc)+I(age*age)+I(e401k*(age-41))+I(e401k*(age-41)^2)+e401k, data = fourooneksubs)
summary(model23)

## The linear i.e. I(e401k * (age - 41)) is significant.

#(v) Comparing the estimates from parts (iii) and (iv), do the estimated effects of 401(k) eligibility at age 41 differ much? Explain.
9.9598436 - 9.70
## 260$ is the difference, which is quite small.

#(vi) Now, drop the interaction terms from the model, but define five family size dummy variables: fsize1, fsize2, fsize3, fsize4, and fsize5. The variable fsize5 is unity for families with five or more members. Include the family size dummies in the model estimated from part (iii); be sure to choose a base group. Are the family dummies significant at the 1% level?

fourooneksubs$fsize1 <- as.numeric(fourooneksubs$fsize == 1)
fourooneksubs$fsize2 <- as.numeric(fourooneksubs$fsize == 2)
fourooneksubs$fsize3 <- as.numeric(fourooneksubs$fsize == 3)
fourooneksubs$fsize4 <- as.numeric(fourooneksubs$fsize == 4)
fourooneksubs$fsize5 <- as.numeric(fourooneksubs$fsize >= 5)

model24 <- lm(nettfa ~inc+age+I(inc*inc)+I(age*age)+e401k+fsize2+fsize3+fsize4+fsize5, data = fourooneksubs)
summary(model24)
# Som dummies are significant, some are not.
anova(model24,model22)
# Jointly they are all significant.

########################## Problem 2.18 ######################

connection <- SQLite() %>% dbConnect('wooldridge.db')
hprice1 <- connection %>% dbReadTable("HPRICE1") %>% data.table
connection %>% dbReadTable("HPRICE1_labels")

model43 <- lm(price~lotsize+sqrft+bdrms, data = hprice1)
model43
summary(model43)
coeftest(model43,vcov.=vcovHC)
#there are changes in std. error for lotsize, bdrms and sqrft.

#(ii)
model44 <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms, data = hprice1)
model44
summary(model44)
coeftest(model44,vcov.=vcovHC)
#there are no much changes in std. error for lotsize, bdrms and sqrft..

#(iii)
#use of algorithm transformation reduces the heteroscedasticity

########################### Problem 2.19 ########################

loanapp <- connection %>% dbReadTable("LOANAPP") %>% data.table
connection %>% dbReadTable("LOANAPP_labels")

#(i) 
model45<-lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data = loanapp)
model45
coeftest(model45,vcov.=vcovHC)
#95% confidence interval on bwhite 
#upper limit : 0.1288196+(0.0260882* 1.96) = 0.1795
#lower limit : 0.1288196-(0.0260882* 1.96) = 0.07811
#95% confidence level of non robust model
#upper limit : 0.1288+(1.96*0.0197) = 0.16749
#lower limit : 0.1288-(1.96*0.0197) = 0.09014
#the confidence limit from non robust model to robust model change by by 0.16749 to  0.1795 and from 0.09014 to0.07811.

#(ii)
model46 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data = loanapp)
h1 <- model46$fitted.values
h1

min(model46$fitted.values)
max(model46$fitted.values)
#there is no value less than 0
#there are total 102 values which are greater than 1
#for all 102 values which are greater than 1, the hi values will be negative.Therefore we can not apply WLS.




########################### Problem 2.20 ######################
# Model1 setup
gpa1 <- connection %>% dbReadTable("GPA1") %>% data.table
model71 <- lm(colGPA~hsGPA+ACT+skipped+PC,data=gpa1)
model71 %>% summary
model71 %>% coeftest(vcov.=vcovHC)

# Get se difference:
se_white <- (model1 %>% coeftest(vcov.=vcovHC))[,2]
se <- (model71 %>% coeftest())[,2]
sum(((se - se_white)/se)^2)

# Residuals and squares --> weights
gpa1$res <- model71 %>% residuals
gpa1$ressq <- gpa1$res^2

gpa1$h <- lm(ressq~colGPA+I(colGPA^2),data=gpa1) %>% predict  # Change from the question. Use the actual college GPA don't use the predicted colGPA. This reduces the heteroskedasticity.
summary(gpa1$h)
gpa1$weight <- 1/gpa1$h

model72 <- lm(colGPA~hsGPA+ACT+skipped+PC,weights=weight,data=gpa1)
model72 %>% summary
model72 %>% coeftest(vcov.=vcovHC)

# Get se difference:
se_white <- (model72 %>% coeftest(vcov.=vcovHC))[,2]
se <- (model72 %>% coeftest())[,2]
sum(((se - se_white)/se)^2) # se Difference has gone down
