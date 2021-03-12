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


##   KW test---
a <- c(1,2,3)
b <- c(3,5,9)
c <- c(7,6,9,8)
data <- c(a,b,c)
#把每一组data用C()组合到一起
factor <- c(rep(1,3), rep(2,3), rep(3,4))
#建立对应的factor
kruskal.test(data~ factor) 
#H0: all groups have same distribution

#手算版本

#H is the test statistic 但是我没找到能直接算的code，如果直接问H的话其实可以直接用上面的code算，找个最相近的答案就可以了，千层面说可以有容错范围，
#或者带入公式反推一下（Hc=H*1.0005956）

pchiq(H,df,lower.tail=FALSE)

## permutation test

x = c(rep(0,17), rep(1,2), 2)
y =c(rep(1,3),3)


library(exactRankTests)
perm.test(x, y, paired=FALSE,
          alternative="less", mu=0, exact=TRUE, conf.int=FALSE)

perm.test(x, y,paired = F,alternative = "less")



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


##Wilcoxon rank sum test
female_bill_length <- subset(penguins, sex == 'female')$bill_length_mm
male_bill_length <- subset(penguins, sex == 'male')$bill_length_mm

wilcox.test(female_bill_length, male_bill_length)
wilcox.test(female_bill_length, male_bill_length, alternative = "greater")
wilcox.test(female_bill_length, male_bill_length, alternative = "less")
#总结：wilcox.test(A,B,alternative=?)
