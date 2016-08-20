#Examples
## Simulation for d (without bias correction)
library(predictionInterval)
set.seed(11)
d.demo <- pi.d.demo(pop.d=.80,n1=50,n2=50,rep.n1 =50,rep.n2=50,number.trials=50000,bias.correction = FALSE)
print(d.demo)


## Simulation for d-unbiased (i.e., with correlation)
library(predictionInterval)
set.seed(11)
d.demo <- pi.d.demo(pop.d=.80,n1=50,n2=50,rep.n1 =50,rep.n2=50,number.trials=50000,bias.correction = TRUE)
print(d.demo)


# Create d-value Simulation Table --------------------------------------------
## The d-value simulations are very time consuming so a parallel computing approach was used
## A consequence of this is that the code is less transparent

# Functions for later use
d_sim <- function(x){
     K <- 50000
     d <- x[1]
     n1 <- x[2]
     n2 <- x[2]
     rep.n1 <- x[3]
     rep.n2 <- x[3]
     print(sprintf("pop.d=%1.2f, n1=%d, n2=%d, rep.n1=%d, rep.n2=%d",d,n1,n2,rep.n1,rep.n2))
     demo_ouput <- predictionInterval::pi.d.demo(n1=n1,n2=n2,rep.n1=rep.n1,rep.n2=rep.n2,number.trials = K,pop.d = d,bias.correction = FALSE)
     pi_capture_percent <- round(demo_ouput$percent_in_pi,1)
     ci_capture_percent <- round(demo_ouput$percent_in_ci,1)
     rm(demo_ouput)

     results <- list()
     results$d <- d
     results$n1 <- n1
     results$n2 <- n2
     results$rep.n1 <- rep.n1
     results$rep.n2 <- rep.n2
     results$ci_capture_percent <- ci_capture_percent
     results$ri_capture_percent <- pi_capture_percent
     return(results)
}

d_sim_unbiased <- function(x){
     K <- 50000
     d <- x[1]
     n1 <- x[2]
     n2 <- x[2]
     rep.n1 <- x[3]
     rep.n2 <- x[3]
     print(sprintf("pop.d=%1.2f, n1=%d, n2=%d, rep.n1=%d, rep.n2=%d",d,n1,n2,rep.n1,rep.n2))
     demo_ouput <- predictionInterval::pi.d.demo(n1=n1,n2=n2,rep.n1=rep.n1,rep.n2=rep.n2,number.trials = K,pop.d = d,bias.correction = TRUE)
     pi_capture_percent <- round(demo_ouput$percent_in_pi,1)
     ci_capture_percent <- round(demo_ouput$percent_in_ci,1)
     rm(demo_ouput)

     results <- list()
     results$d <- d
     results$n1 <- n1
     results$n2 <- n2
     results$rep.n1 <- rep.n1
     results$rep.n2 <- rep.n2
     results$ci_capture_percent <- ci_capture_percent
     results$ri_capture_percent <- pi_capture_percent
     return(results)
}

#Set up simulation conditions
library(predictionInterval)
library(parallel)
RNGkind("L'Ecuyer-CMRG")
effect_sizes <- c(.2,.5,.8)
n_orig <- c(25,50,100,250,500,  25,  25,  25,  25,  50,  50,  50,  50, 100, 100, 100, 100, 250, 250, 250, 250, 500, 500, 500, 500)
n_rep  <- c(25,50,100,250,500,  50, 100, 250, 500,  25, 100, 250, 500,  25,  50, 250, 500,  25, 50,  100, 500,  25,  50, 100, 250)
n_orig_df <- expand.grid(n_orig, effect_sizes)
names(n_orig_df) <- c("n_orig","d")
n_rep_df  <- expand.grid(n_rep,effect_sizes)
names(n_rep_df) <- c("n_rep","d2")

df <- cbind(n_rep_df,n_orig_df)
df <- df[,c("d","n_orig","n_rep")]
dfm <- as.matrix(df)
simulation_conditions <- as.list(as.data.frame(t(dfm)))

#Run the biased d-value simulation
set.seed(9)
dvalue_results <- mclapply(simulation_conditions,d_sim,mc.cores = 2)   # Most machines have 2 cores
#dvalue_results <- mclapply(simulation_conditions,d_sim,mc.cores = 14) # We used 14 cores
print(s)
results<- do.call(rbind, lapply(dvalue_results, data.frame, stringsAsFactors=FALSE))
filename_str <- "resultsTable3_dDemo_biased.csv"
print(filename_str)
print(results)
write.csv(results,filename_str)


#Run the unbiased d-value simulation
set.seed(9)
dvalue_results_unbiased <- mclapply(simulation_conditions,d_sim_unbiased,mc.cores = 2)  # Most machines have 2 cores
#dvalue_results_unbiased <- mclapply(simulation_conditions,d_sim_unbiased,mc.cores = 14) # We used 14 cores 
results_unbiased<- do.call(rbind, lapply(dvalue_results_unbiased, data.frame, stringsAsFactors=FALSE))
filename_str <- "resultsTable3_dDemo_unbiased.csv"
print(filename_str)
print(results_unbiased)
write.csv(results_unbiased,filename_str)

