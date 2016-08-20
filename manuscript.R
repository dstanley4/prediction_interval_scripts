# Sampling Error: Correlation Example ---------------------------------------------

#install.packages("MASS")
library(MASS)

##Define population level correlation
sigma <- diag(2)
sigma[2,1] <- .20
sigma[1,2] <- .20
mu <- c(0,0)

##Obtain sampling distribution
number_samples <- 50000
sample_size <- 100
set.seed(9)
sampled_r_values <- sort(replicate(number_samples,cor(mvrnorm(n=sample_size,mu=mu,Sigma=sigma))[2]))

#Obtain values that bound middle 95% of values
lower_limit <- sampled_r_values[round(number_samples*.005)+1]
upper_limit <- sampled_r_values[round(number_samples*.995)]
# If you consider middle 90% of 1 to 10 then you use the values 2 to 9 (Not 1 to 9)
# Hence you add 1 to lower_limit from above when calculating middle 99% of 50,0000 values
print("Bounds of the sampling distribution")
print(lower_limit)
print(upper_limit)


print("Distribution Mean")
distribution_mean <- round(mean(sampled_r_values),2)
print(distribution_mean)

#install.packages("ggplot2")
library(ggplot2)
figure1 <- qplot(sampled_r_values,fill=I("gray50"), colour=I("gray50")) 
figure1 <- figure1 + geom_vline(xintercept = .20,size=1)
figure1 <- figure1 + labs(x="Replication Correlations",y="Frequency")
figure1 <- figure1 + coord_cartesian(xlim=c(-.20,.60))
figure1 <- figure1 + scale_y_continuous(expand=c(0,0))
figure1 <- figure1 + theme_classic()
figure1 <- figure1 + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
figure1 <- figure1 + theme(text=element_text(family="Times"))
print(figure1)
ggsave("Figure1.tiff",width=7,height=7,dpi=300)







# Prediction Interval: Means ---------------------------------------------
SD1 <- 14.76
N1 <- 50
N2 <- 100
df <- N1-1 #df is N1-1 not N1+N2 -2 because SD1 was used twice in the denominator (SD2 was not used)
tv <- qt(.975,df)

## Mean Prediction Interval
M1 <- 98.59
tv*sqrt( ((SD1^2)/N1)  +  ((SD1^2)/N2)  )
M1 - tv*sqrt( ((SD1^2)/N1)  +  ((SD1^2)/N2)  )
M1 + tv*sqrt( ((SD1^2)/N1)  +  ((SD1^2)/N2)  )

#Test Example
library(predictionInterval)
set.seed(9)
m.demo <- pi.m.demo(mu=100,sigma=15,n=50,rep.n=100,number.trials = 50000)
print(m.demo)







# Prediction Interval: Correlations ---------------------------------------------
## Original Correlation CI
r1 <- .35
N1 <- 100
z.r <- atanh(r1)
z.se1 <- 1 / sqrt(N1-3)
l1.z <- z.r - qnorm(.975) * z.se1
u1.z <- z.r + qnorm(.975) * z.se1
l1 <- tanh(l1.z)
u1 <- tanh(u1.z)

## Replication Correlation (unknown) CI
r1 <- .35
z.r <- atanh(r1)
N2 <- 200
z.se2 <- 1 / sqrt(N2-3)
l2.z <- z.r - qnorm(.975) * z.se2
u2.z <- z.r + qnorm(.975) * z.se2
l2 <- tanh(l2.z)
u2 <- tanh(u2.z)

## Correlation Prediction Interval
LL <- r1 - sqrt((r1-l1)^2 + (u2-r1)^2)
UL <- r1 + sqrt((u1-r1)^2 + (r1-l2)^2)


#Test Example
library(predictionInterval)
set.seed(10)
r.demo <- pi.r.demo(rho=.50,n=100,rep.n=200,number.trials=50000)
print(r.demo)







# Prediction Interval: d-values ---------------------------------------------

#install.packages("MBESS",dep=TRUE)

## Original d-value CI
d1 <- .65
n1 <- 50
n2 <- 50
library(MBESS)
original_d_ci <- ci.smd(smd=d1, n.1 = n1,n.2=n2)
l1 <- original_d_ci$Lower.Conf.Limit.smd
u1 <- original_d_ci$Upper.Conf.Limit.smd


## Replication d-value CI
d1 <- .65
rep.n1 <- 50
rep.n2 <- 50
library(MBESS)
replication_d_ci <- ci.smd(smd=d1, n.1 =rep.n1,n.2=rep.n2)
l2 <- replication_d_ci$Lower.Conf.Limit.smd
u2 <- replication_d_ci$Upper.Conf.Limit.smd


## d-value Prediction Interval
LL <- d1 - sqrt((d1-l1)^2 + (u2-d1)^2)
UL <- d1 + sqrt((u1-d1)^2 + (d1-l2)^2)


#Simulation for d (without bias correction)
library(predictionInterval)
set.seed(11)
d.demo <- pi.d.demo(pop.d=.80,n1=50,n2=50,rep.n1 =50,rep.n2=50,number.trials=50000,bias.correction = FALSE)
print(d.demo)


#Simulation for d-unbiased (i.e., with correlation)
library(predictionInterval)
set.seed(11)
d.demo <- pi.d.demo(pop.d=.80,n1=50,n2=50,rep.n1 =50,rep.n2=50,number.trials=50000,bias.correction = TRUE)
print(d.demo)



#Example Prediction Interval
library(predictionInterval)
#Mean
pi.m(M=98.59,SD=14.76,n=50,rep.n=100)

#Correlation
pi.r(r=.35,n=100,rep.n=200)

#d-value
pi.d(d=.65,n1=50,n2=50,rep.n1=100,rep.n2=100)

