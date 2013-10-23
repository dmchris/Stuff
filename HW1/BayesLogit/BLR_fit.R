##########################################
### STA250 Advanced Statistical Computing
### Homework 1
##########################################

#######################################################################################################
#######################################################################################################
## Handle batch job arguments:

# 1-indexed version is used now.
args <- commandArgs(TRUE)

cat(paste0("Command-line arguments:\n"))
print(args)

####
# sim_start ==> Lowest simulation number to be analyzed by this particular batch job
###

#######################
sim_start <- 1000
length.datasets <- 200
#######################

if (length(args)==0){
  sinkit <- FALSE
  sim_num <- sim_start + 1
  set.seed(1330931)
} else {
  # Sink output to file?
  sinkit <- TRUE
  # Decide on the job number, usually start at 1000:
  sim_num <- sim_start + as.numeric(args[1])
  # Set a different random seed for every job number!!!
  set.seed(762*sim_num + 1330931)
}

# Simulation datasets numbered 1001-1200
#######################################################################################################
#######################################################################################################

library(MASS)
### Take the Command Line Argument integer fileno
fileno <- sim_num

source("/home/jihao/STA250/Stuff/HW1/BayesLogit/function.R")
#data<-read.table(sprintf("C:/HaoJi/UC DAVIS PHD in STAT/2013-2014/Fall13/STA250/Homework1/blr_data_%d.csv",fileno),
#	header=TRUE, sep=",")
data <- read.table(file=paste("/home/jihao/STA250/Stuff/HW1/BayesLogit/data/blr_data_",fileno,".csv",sep=""), header=TRUE, sep=",")

y <- data[,1]
m <- data[,2]
X <- as.matrix(data[,3:4])
beta.0 <- c(0,0)
p <- length(beta.0)
Sigma.0.inv <- diag(c(1,1))
beta.initial <- c(1,1) # initial values of sampling algorithm
result <- matrix(rep(0,11000*p),nrow=(11000),ncol=p)

result <- bayes.logreg(m,y,X,beta.0,Sigma.0.inv,niter=10000,burnin=1000,print.every=1000,retune=100,verbose=FALSE)
### result is a niter by p matrix containing the resulting posterior sample
BetaPercentile <- matrix(rep(0,99*p),nrow=99)
for(j in 1:p){
	Rj <- sort(result[,j])
	for(i in 1:99){BetaPercentile[i,j] <- Rj[100*i]}
}

write.table(BetaPercentile, file=paste("/home/jihao/STA250/Stuff/HW1/BayesLogit/results/blr_res_",fileno,".csv",sep=""),
	row.names=FALSE,col.names=FALSE,sep=",")