## the signed test------

# One sample H0: theta= theta0
# yi = xi-theta0
# T = number of +'s in the yi data

# Paired sample H0: theta1= theta2
# di = xi-yi
# T = number of +'s in the di data

#step 1: remove 相减之差等于0的项
#step 2: sample size n = 所有data-相减之差等于0的项

# greater 
pbinom(T, n, 0.5, lower.tail = FALSE)
# less
pbinom(T, n, 0.5)
# two-sided
# T1 = number of +'s in the yi data
# 找到对应的T2
p_value<-pbinom(T1, n, 0.5)+pbinom(T2, n, 0.5, lower.tail = FALSE)
p_value
#或者用下面这个test
#x= T = number of +'s in the di data
binom.test(x=T, n=n, p=0.5, alternative="two.sided")


#############################################################################################

#signed rank test

#计算|yi-xi| 按小到大排序
T stat= T+ = yi>xi的所有rank相加！即yi-xi为正
#将rank比如1 2 3 4的所有正负可能性list一遍 
#如下一个就是-1 2 3 4 一直到-1 -2 -3 -4
#找与T+相同或更极端的数（根据题意 ）
p-value= # of 极端的数/# of 所有regrouping的方式

#############################################################################################

##Wilcoxon rank sum test
female_bill_length <- subset(penguins, sex == 'female')$bill_length_mm
male_bill_length <- subset(penguins, sex == 'male')$bill_length_mm

wilcox.test(female_bill_length, male_bill_length)
wilcox.test(female_bill_length, male_bill_length, alternative = "greater")
wilcox.test(female_bill_length, male_bill_length, alternative = "less")
#总结：wilcox.test(A,B,alternative=?)

#############################################################################################

##   KW test---
a <- c(1,2,3)
b <- c(3,5,9)
c <- c(7,6,9,8)
data <- c(a,b,c)
#把每一组data用C()组合到一起
factor <- c(rep(1,3), rep(2,3), rep(3,4))
#建立对应的factor
kruskal.t est(data~ factor) 
#H0: all groups have same distribution

#手算版本

#H is the test statistic 但是我没找到能直接算的code，如果直接问H的话其实可以直接用上面的code算，找个最相近的答案就可以了，千层面说可以有容错范围，
#或者带入公式反推一下（Hc=H*1.0005956）

pchiq(H,df,lower.tail=FALSE)

#############################################################################################

## permutation test

x = c(rep(0,17), rep(1,2), 2)
y =c(rep(1,3),3)


library(exactRankTests)
perm.test(x, y, paired=FALSE,
          alternative="less", mu=0, exact=TRUE, conf.int=FALSE)

perm.test(x, y,paired = F,alternative = "less")


#############################################################################################

#Power of test
#Use the function: 
dbinom(x, size, prob)
#x: a vector of quantile values (eg. vector of number of successes)
#size: the number of trials
#prob: the probability of success on each trial 

#example 1
#To find Pr(T=8), given thatT∼B(10,0.5)
dbinom(8, 10, 0.5)

#example 2
#To find Pr(T>=8), given thatT∼B(10,0.5)
dbinom(8, 10, 0.5) + dbinom(9, 10, 0.5) + dbinom(10, 10, 0.5)]
#or
sum(dbinom(c(8,9,10), 10, 0.3))
#or
pbinom(7, 10, 0.5, lower.tail=FALSE)

#plotting power function curve
#To create a power function curve
curve(pbinom(7, 10, x, lower.tail=FALSE), 0, 1) #0, 1 indicates plot x from 0 to 1

#additional info
#Compute the power of the one- or two- sample t test, 
#or determine parameters to obtain a target power
?power.t.test
power.t.test(n = NULL, delta = NULL, sd = 1, sig.level = 0.05,
             power = NULL,
             type = c("two.sample", "one.sample", "paired"),
             alternative = c("two.sided", "one.sided"))

#############################################################################################

#fisher exact test
#e11是2x2左上角的值
z score=e11
#probability of observed value, P(Z=x):
choose(C1,x)*choose(C2,R1-x)/choose(n,R1)
#例子,P(Z=2) 替换x为2 C1,C2,R1永远不变
choose(C1,2)*choose(C2,R1-2)/choose(n,R1)

#计算和对比两者的p-value

#nrow记得替换 就是看有几行
fisher.test(matrix(c(matrix),nrow=x,byrow=TRUE))
#例子
fisher.test(matrix(c(3,6,9,2),nrow=2,byrow=TRUE))
chisq.test(matrix(c(matrix),nrow=x,byrow=TRUE),correct=FALSE)
#例子
chisq.test(matrix(c(3,6,9,2),nrow=2,byrow=TRUE),correct=FALSE)

#############################################################################################

## One Way anova
# Or, if .csv file, use this(请提前自行建立,已发群)
my_data <- read.delim(file.choose())
my_data

library(dplyr)
group_by(my_data, Type) %>%
  summarise(
    count = n(),
    mean = mean(my_data$data, na.rm = TRUE),
    sd = sd(my_data$data, na.rm = TRUE)
  )

# 输出anova 表格
res.aov <- aov(data ~ Type, data = my_data)
# Summary of the analysis
summary(res.aov)

#############################################################################################

#QQplots
#Nomally Distributed QQ plot
par(mfrow = c(1, 2))

# normal_density are the y-values for the normal curve
# zs are the x-values for the normal curve
n <- 1000
normal_density <- dnorm(seq(-4, 4, 0.01))
zs <- seq(-4, 4, 0.01)

# Add some spice to the default histogram function
hist_ <- function(x, ...){
  hist(x, breaks = 30, xlab = "Z", ylab = "",  yaxt='n', freq = FALSE, ...)
  lines(zs, normal_density, type = "l", col = "red", lwd = 2)
}


# rnorm() generates random numbers from a normal distribution
# gaussian_rv is the dataset that will be compared to the Gaussian distribution
gaussian_rv <- rnorm(n)

# Draw the histogram
hist_(gaussian_rv, main = "Gaussian Distribution")
# Draw the Q-Q plot
qqnorm(gaussian_rv)
qqline(gaussian_rv, col = "blue", lwd = 2)

# Skewed Right
# skew_right is the dataset that will be compared to the Gaussian distribution
skew_right <- c(gaussian_rv[gaussian_rv > 0] * 2.5, gaussian_rv)

hist_(skew_right, main = "Skewed Right", ylim = c(0, max(normal_density)))

qqnorm(skew_right)
qqline(skew_right, col = "blue", lwd = 2)

# Skewed Left
# skew_left is the dataset that will be compared to the Gaussian distribution
skew_left <- c(gaussian_rv[gaussian_rv < 0]*2.5, gaussian_rv)

hist_(skew_left, main = "Skewed Left", ylim = c(0, max(normal_density)))

qqnorm(skew_left)
qqline(skew_left, col = "blue", lwd = 2)

# Fat Tails
fat_tails <- c(gaussian_rv*2.5, gaussian_rv)

hist_(fat_tails, main = "Fat Tails", ylim = c(0, max(normal_density)), xlim = c(-10, 10))

qqnorm(fat_tails)
qqline(fat_tails, col = "blue", lwd = 2)

# Thin Tails
thin_tails <- rnorm(n, sd = .7)

hist_(thin_tails, main = "Thin Tails")

qqnorm(thin_tails)
qqline(thin_tails, col = "blue", lwd = 2)

##如果QQplot是一条直线的话，呢么我们可以说他的data是Normal Distributed的
##xi=s*zi+bar(x) 
##xi是given data, zi是normal scores,bar(x)是sample mean, s（slope）是sample sd

#############################################################################################

# 2way ANOVA
# 列出四组data, 每组个数为x
data1<-c(....)
data2<-c(....)
data3<-c(....)
data4<-c(....)
# Combine these data in a data frame with columns into a table
table<-data.frame(column1=c(data1,data2,data3,data4), 
                  column2=rep(c("",""), each=x),
                  column3= rep(c("",""), each=2*x)) #引号内写column包含的种类, each后面的数字需要结合题意
table
#summary
summary(aov(column1~column2*column3, data=table))
#############################################################################################

# Power
# 用dbinom or pbinom.
# x: number of successes
# size: the number of trials
# prob: the probability of success on each trial.
dbinom(x, size, prob)

# 默认计算the probability for the lower tail (P[X≤x])
# 用lower.tail = FALSE) 计算P[X > x].
# 比如
pbinom(7, 10, 0.3, lower.tail = FALSE)

