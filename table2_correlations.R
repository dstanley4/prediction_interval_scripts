#Example
library(predictionInterval)
set.seed(10)
r.demo <- pi.r.demo(rho=.50,n=100,rep.n=200,number.trials=50000)
print(r.demo)

# Create Correlation Simulation Table --------------------------------------------

library(predictionInterval)
K <- 50000
number_sizes <- 16
sample_sizes  <-list()
sample_sizes[[1]] <- c(100, 100)
sample_sizes[[2]] <- c(250, 250)
sample_sizes[[3]] <- c(500, 500)
sample_sizes[[4]] <- c(1000, 1000)
sample_sizes[[5]] <- c(100, 250)
sample_sizes[[6]] <- c(100, 500)
sample_sizes[[7]] <- c(100, 1000)
sample_sizes[[8]] <- c(250, 100)
sample_sizes[[9]] <- c(250, 500)
sample_sizes[[10]] <- c(250, 1000)
sample_sizes[[11]] <- c(500, 100)
sample_sizes[[12]] <- c(500, 250)
sample_sizes[[13]] <- c(500, 1000)
sample_sizes[[14]] <- c(1000, 100)
sample_sizes[[15]] <- c(1000, 250)
sample_sizes[[16]] <- c(1000, 500)
cor_values <- c(.10, .30, .50)
number_cors <- length(cor_values)
results <- data.frame(Example=1:(number_sizes*number_cors))
count <- 0
set.seed(9)
for (i in 1:number_cors) {
     for (j in 1:number_sizes) {
          count <- count + 1
          rho <- cor_values[i]
          ns <- sample_sizes[[j]]
          n <- ns[1]
          rep.n <- ns[2]
          print(sprintf("rho=%1.2f, n=%d, rep.n=%d",rho,n,rep.n))
          demo_ouput <- pi.r.demo(n=n,rep.n=rep.n,number.trials = K,rho=rho)
          print(demo_ouput)
          ri_capture_percent <- round(demo_ouput$percent_in_ri,1)
          ci_capture_percent <- round(demo_ouput$percent_in_ci,1)
          rm(demo_ouput)

          results$rho[count] <- rho
          results$n[count] <- n
          results$rep.n[count] <- rep.n
          results$ci_capture_percent[count] <- ci_capture_percent
          results$ri_capture_percent[count] <- ri_capture_percent
     }
}
print(results)
write.csv(results,"resultsTable2_CorrelationDemo.csv")
