#Janak Patil
#Assignment No - 5
#jsp170630



library(data.table)
library(dplyr)
library(partykit)

n <- 500
set.seed(75080)

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 50
y   <- -100*z+ 1100 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt1 <- data.table('id'=1:500,'sat'=y,'income'=x,'group'=rep(1,n))
View(dt1)
z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 80
y   <- -80*z+ 1200 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt2 <- data.table('id'=501:1000,'sat'=y,'income'=x,'group'=rep(2,n))
View(dt2)
z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 30
y   <- -120*z+ 1000 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt3 <- data.table('id'=1001:1500,'sat'=y,'income'=x,'group'=rep(3,n))

dtable <- merge(dt1    ,dt2, all=TRUE)
dtable <- merge(dtable ,dt3, all=TRUE)
dtable$group <- as.factor(dtable$group)

pooled <- lm(sat~income,data=dtable)
within <- lm(sat~income+group-1,data=dtable)
model1 <- lm(sat~income,data=dtable[group==1])
model2 <- lm(sat~income,data=dtable[group==2])
model3 <- lm(sat~income,data=dtable[group==3])
prop.table((table(dtable$group)))

summary(pooled)
summary(within)
summary(model1)
summary(model2)
summary(model3)

#a) The data generation is using rnorm function(a pseudo random number 
#  generation function), which generates random value numbers from normal
#  distribution. We have set.seed function to get the same value of random
#  number for all the variables. In this case we have set the seed to 75080

#b) The signs for within and pooled are different as pooled data takes into 
# cosideration all the data on the other hand the within data runs on individual 
# groups

#c) 
tree1 <- ctree(sat~income, data = dtable)
tree1 %>% plot
# From the tree we can see that the SAT score of people with income between 
# 72.834 and 75.53 is the highest ranging from 1300 - 1500

tree2 <- ctree(sat~group, data = dtable)
tree2 %>% plot
# From the tree we can deduce that the people in group 1 have median score of 1100
# From the tree we can deduce that the people in group 2 have median score of 1200
# From the tree we can deduce that the people in group 3 have median score of 1000


tree3 <- ctree(sat~income + group, data = dtable)
tree3 %>% plot
nodeprune(tree3,c(5,12,16,19,26,29,33,36,43,47,57) )%>% plot
# The tree is very wide to decipher anything sensible

#d)
model1 <- glmtree(sat~income|group, data = dtable)
summary(model1)
#e)
auto.kmeans <- function(data,maxclu=10,seed=1,nstart=10) {
  wss <- rep(NA,maxclu)
  for (i in 1:maxclu) { 
    set.seed(seed)
    model <- kmeans(data,centers=i,nstart=nstart)
    wss[i] <- model$tot.withinss
  }
  plot(1:maxclu,	wss, type="b", 
       xlab="Number of Clusters",
       ylab="Aggregate Within Group SS")
}
auto.kmeans(dtable)
set.seed(1)
modelk <- kmeans(dtable,centers=3,nstart=10)
modelk$centers
groups_k <- modelk$cluster
groups_k

dtable$kmeans_groups <- groups_k
dtable$kmeans_groups <- as.factor(dtable$kmeans_groups)

prop.table(table(dtable$kmeans_groups,dtable$group),1)

#f)
prop.table(table(dtable$kmeans_groups,dtable$group),1)
# It shows perfect grouping by kmeans as that of generated data.

auto.hclust <- function(data,model=hclust(dist(data)),maxclu=10) {
  wss <- rep(NA,maxclu)
  r <- ncol(data)+1
  for(i in 1:maxclu) {
    groups <- cutree(model,i)
    means <- data[order(groups),lapply(.SD,mean),by=groups]
    means <- means[,2:r]
    demeaned <- data - means[groups]
    wss[i] <- sum(rowSums(demeaned^2))
  }
  plot(1:maxclu,	wss, type="b", 
       xlab="Number of Clusters",
       ylab="Aggregate Within Group SS")
}

data <- dtable %>% select(-kmeans_groups)
data$group <- as.numeric(dtable$group)
auto.hclust(data)

auto.hclust(data,model=hclust(dist(data),method="complete"))
auto.hclust(data,model=hclust(dist(data),method="average"))
auto.hclust(data,model=hclust(dist(data),method="median"))
auto.hclust(data,model=hclust(dist(data),method="ward.D"))

# We are getting the same number of results as in kmeans i.e 3.

#g)
lm(sat~income+kmeans_groups-1, data = dtable) %>% summary

#h)
lm(sat~income,data = dtable[kmeans_groups == 1]) %>% summary
lm(sat~income,data = dtable[kmeans_groups == 2]) %>% summary
lm(sat~income,data = dtable[kmeans_groups == 3]) %>% summary

#i)
dtable$sat <- as.numeric(dtable$sat)
dtable$income <- as.numeric(dtable$income)
dtable1 <- dtable %>% select(-id,-group,-kmeans_groups)
str(dtable1)

auto.kmeans <- function(data,maxclu=10,seed=1,nstart=10) {
  wss <- rep(NA,maxclu)
  for (i in 1:maxclu) { 
    set.seed(seed)
    model <- kmeans(data,centers=i,nstart=nstart)
    wss[i] <- model$tot.withinss
  }
  plot(1:maxclu,	wss, type="b", 
       xlab="Number of Clusters",
       ylab="Aggregate Within Group SS")
}
auto.kmeans(scale(dtable1))
set.seed(1)
modelk1 <- kmeans(dtable,centers=3,nstart=10)
modelk1$centers
groups_k1 <- modelk$cluster
groups_k1
dtable1$kmeans_groupk1 <- groups_k1
dtable1$kmeans_groupk1 <- as.factor(dtable1$kmeans_groupk1)

lm(scale(sat)~scale(income)+kmeans_groupk1-1, data = dtable1) %>% summary
lm(scale(sat)~scale(income),data = dtable1[kmeans_groupk1 == 1]) %>% summary
lm(scale(sat)~scale(income),data = dtable1[kmeans_groupk1 == 2]) %>% summary
lm(scale(sat)~scale(income),data = dtable1[kmeans_groupk1 == 3]) %>% summary

