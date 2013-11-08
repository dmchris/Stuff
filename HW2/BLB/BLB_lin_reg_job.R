######################
# STA250 Homework 2
# Hao Ji 998362038
######################
#######################################################################################################
## Handle batch job arguments:
#============================== Setup for running on Gauss... ==============================#

args <- commandArgs(TRUE)

cat("Command-line arguments:\n")
print(args)

####
# sim_start ==> Lowest possible dataset number
###

###################
sim_start <- 0
###################

if (length(args)==0){
  sim_num <- sim_start + 1
  set.seed(121231)
} else {
  # SLURM can use either 0- or 1-indexing...
  # Lets use 1-indexing here...
  sim_num <- sim_start + as.numeric(args[1])
  sim_seed <- (762*(sim_num-1) + 121231)
}

cat(paste("\nAnalyzing dataset number ",sim_num,"...\n\n",sep=""))

# Find r and s indices:

#============================== Run the simulation study ==============================#

library(bigmemory)

# Attach the big data set in order to read in data
data <- attach.big.matrix("/home/pdbaines/data/blb_lin_reg_data.desc")
n <- nrow(data)
p <- ncol(data)
gamma <- 0.7
b <- as.integer(n^gamma)
s <- 5
r <- 50

### Work out the s_index and r_index from the job number
s_index <- (sim_num%%s)+1
r_index <- as.integer(sim_num/s)+1

### Sub-Sample Specification
# Based on command line argument,  specify the corresponding subsample
# from which the bootstrap sample are generated.
# The subsample indices are randomly drawn and saved beforehand, load it:
load("~/STA250/Stuff/HW2/BLB/SubSampleIndex.RData")
subsampleindex <- SubSampleIndex[[s_index]]

### Read in Data from subsample and Perform Bootstrap
Dat <- data[subsampleindex,]
bs.freq <- rmultinom(1,size=n, rep(1,b)/b)
### Fit the linear regression model and get estimated coefficients betahat
fit <- lm(Dat[,p]~-1+Dat[,1:(p-1)],weights=bs.freq/n)
betahat <- fit$coefficients

### Save the coefficients in the specified file
outfile <- paste0("output/","coef_", sprintf("%02d",s_index),"_",sprintf("%02d",r_index),".txt")
write.table(betahat,file=paste("~/STA250/Stuff/HW2/BLB/",outfile,sep=""),
            row.names=FALSE, col.names=FALSE)