#Example
library(predictionInterval)
set.seed(9)
m.demo <- pi.m.demo(mu=100,sigma=15,n=50,rep.n=100,number.trials = 50000)
print(m.demo)



# Create Mean Simulation Table --------------------------------------------
library(predictionInterval)
K <- 50000

# Mean Examples
number_sizes <- 25
sample_sizes  <-list()
sample_sizes[[1]] <- c(25, 25)
sample_sizes[[2]] <- c(50, 50)
sample_sizes[[3]] <- c(100, 100)
sample_sizes[[4]] <- c(250, 250)
sample_sizes[[5]] <- c(500, 500)

sample_sizes[[6]] <- c(25, 50)
sample_sizes[[7]] <- c(25, 100)
sample_sizes[[8]] <- c(25, 250)
sample_sizes[[9]] <- c(25, 500)

sample_sizes[[10]] <- c(50, 25)
sample_sizes[[11]] <- c(50, 100)
sample_sizes[[12]] <- c(50, 250)
sample_sizes[[13]] <- c(50, 500)

sample_sizes[[14]] <- c(100, 25)
sample_sizes[[15]] <- c(100, 50)
sample_sizes[[16]] <- c(100, 250)
sample_sizes[[17]] <- c(100, 500)

sample_sizes[[18]] <- c(250, 25)
sample_sizes[[19]] <- c(250, 50)
sample_sizes[[20]] <- c(250, 100)
sample_sizes[[21]] <- c(250, 500)

sample_sizes[[22]] <- c(500, 25)
sample_sizes[[23]] <- c(500, 50)
sample_sizes[[24]] <- c(500, 100)
sample_sizes[[25]] <- c(500, 250)


results <- data.frame(Example=1:number_sizes)
set.seed(9)
for (i in 1:number_sizes) {
     ns <- sample_sizes[[i]]
     n <- ns[1]
     rep.n <- ns[2]
     print(sprintf("%d %d",n,rep.n))
     demo_ouput <- pi.m.demo(n=n,rep.n=rep.n,number.trials = K,mu=0,sigma=1)
     ri_capture_percent <- round(demo_ouput$percent_in_ri,1)
     ci_capture_percent <- round(demo_ouput$percent_in_ci,1)
     rm(demo_ouput)
     results$n[i] <- n
     results$rep.n[i] <- rep.n
     results$ci_capture_percent[i] <- ci_capture_percent
     results$ri_capture_percent[i] <- ri_capture_percent
}
print(results)
write.csv(results,"resultsTable1_MeanDemo.csv")




