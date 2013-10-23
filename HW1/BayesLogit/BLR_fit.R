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

#data<-read.table("C:/HaoJi/UC DAVIS PHD in STAT/2013-2014/Fall13/STA250/Homework1/blr_data_1001.csv",
#	header=TRUE, sep=",")
data <- read.table(sprintf("~/data/blr_data_%d.csv",
	fileno+1000), header=TRUE, sep=",")
head(data)
para <- read.table("C:/HaoJi/UC DAVIS PHD in STAT/2013-2014/Fall13/STA250/Homework1/blr_pars_1001.csv",header=TRUE)
para

### Data Input and Prior Distn Specification
y <- data[,1]
m <- data[,2]
X <- as.matrix(data[,3:4])
beta.0 <- c(0,0)
p <- length(beta.0)
Sigma.0.inv <- diag(c(1,1))
beta.initial <- c(1,1) # initial values of sampling algorithm
result <- matrix(rep(0,11000*p),nrow=(11000),ncol=p)

### Function for calculating acceptance rate
logalpha <- function(bt,bstar){
	logpistar <- -0.5*t(bstar)%*%Sigma.0.inv%*%bstar+t(beta.0)%*%Sigma.0.inv%*%bstar+
		t(y)%*%(X%*%bstar)-t(m)%*%log(1+exp(X%*%bstar))
	logpit <- -0.5*t(bt)%*%Sigma.0.inv%*%bt+t(beta.0)%*%Sigma.0.inv%*%bt+
		t(y)%*%(X%*%bt)-t(m)%*%log(1+exp(X%*%bt))
	a <- min(0, logpistar-logpit)
	a
}

### Fit the Logistic Model using the following function
bayes.logreg <- function(m,y,X,beta.0,Sigma.0.inv,way=1,
niter=10000,burnin=1000, print.every=1000,retune=100,verbose=TRUE){
	result.sample <- matrix(nrow=(niter+burnin),ncol=p)
	betat <- beta.initial

	### METHOD1: MH within GIBBS Sampler:
	if(way==1){
		v <- c(0.5,p)
		count <- rep(0,p)
		countiter <- rep(0,p)
		for (i in 1:(burnin+niter)){
			beta.star <- rep(0,p)
			for(j in 1:p){
				beta.star[j] <- rnorm(1,mean=betat[j],sd=v[j])
				bnew <- betat
				bnew[j] <- beta.star[j]
				lalpha <- logalpha(betat,bnew)
				U <- runif(1,min=0,max=1)
				if(log(U) < lalpha){
					result.sample[i,j] <- beta.star[j]
					betat[j] <- beta.star[j]
					count[j] <- count[j]+1
					if(i>burnin) countiter[j] <- countiter[j]+1
				}
				if(log(U) >= lalpha){result.sample[i,j] <- betat[j]}
			}	
		
			if(i%%retune==0 && i<=burnin){
				accrate <- count/retune
				for(jj in 1:p){
					if(accrate[jj]<0.3) v[jj] <- v[jj]/sqrt(5)
					if(accrate[jj]>0.6) v[jj] <- v[jj]*sqrt(5)
				}
				cat("Acceptance Rate after", i, "iterations during burnin", accrate, "\n")
				cat("Tuning Parameters after", i, "iterations during burnin: ", v, "\n")
				count <- rep(0,p)
			}
			if(i%%print.every==0){cat(i," iterations of MH in Gibbs have been completed. \n")}

		}#Gibbs For loop Ends
		cat("Acceptance Rate after Burnin is ", countiter/(niter*p), "\n") ### FOR METHOD1
		if(verbose){
			par(mfrow=c(2,2))
			for(k in 1:p){
				plot(result.sample[-(1:burnin),k],type='l')
				hist(result.sample[-(1:burnin),k])
			}
		}
		if(way==1)cat("Tuning Parameter After Burnin is: ", v, "\n") ### FOR METHOD1
	}

	### METHOD2: MH with smart proposal:
	if(way==2){
		delta <- 1; 
		#Sigma.p <- matrix(c(0.0463,0.0057,0.0057,0.0271),byrow=TRUE,nrow=2)
		Sigma.p <- diag(rep(1,p))
		count <- 0; countiter <- 0
		for (i in 1:(burnin+niter)){
			beta.star <- rep(0,p)
			beta.star <- mvrnorm(n=1,mu=betat,Sigma=delta*Sigma.p)
			bnew <- beta.star
			lalpha <- logalpha(betat,bnew)
			U <- runif(1,min=0,max=1)
			if(log(U) < lalpha){
				result.sample[i,] <- beta.star
				betat <- beta.star
				count <- count+1
				if(i>burnin) countiter <- countiter+1
			}
			if(log(U) >= lalpha){result.sample[i,] <- betat}
			
			if(i%%retune==0 && i<=burnin){
				accrate <- count/retune
				if(accrate<0.3) delta <- delta/sqrt(5)
				if(accrate>0.6) delta <- delta*sqrt(5)
				cat("Acceptance Rate after", i, "iterations during burnin", accrate, "\n")
				cat("Tuning Parameters after", i, "iterations during burnin: ", delta, "\n")
				count <- 0
			}
			if(i%%print.every==0){cat(i," iterations of MH in Gibbs have been completed. \n")}
		}#Gibbs For loop Ends
		cat("Acceptance Rate after Burnin is ", countiter/niter, "\n") ### FOR METHOD2
		if(verbose){
			par(mfrow=c(2,2))
			for(k in 1:p){
				plot(result.sample[-(1:burnin),k],type='l')
				hist(result.sample[-(1:burnin),k])
			}
		}
		cat("Tuning Parameter After Burnin is: ", delta, "\n") ### FOR METHOD2
	}
result.sample[-(1:burnin),]
}#Function Ends

### Function Call
result <- bayes.logreg(m,y,X,beta.0,Sigma.0.inv,niter=10000,burnin=1000,print.every=1000,retune=100,verbose=FALSE)
### result is a niter by p matrix containing the resulting posterior sample
BetaPercentile <- matrix(rep(0,99*p),nrow=99)
for(j in 1:p){
	Rj <- sort(result[,j])
	for(i in 1:99){BetaPercentile[i,j] <- Rj[100*i]}
}

### Output the Percentiles of posterior beta to a csv file
#write.table(BetaPercentile, sprintf("C:/HaoJi/UC DAVIS PHD in STAT/2013-2014/Fall13/STA250/Homework1/blr_res_%d.csv",1+1000),
#	row.names=FALSE,col.names=FALSE,sep=",")
write.table(BetaPercentile, sprintf("~/results/blr_res_%d.csv",fileno+1000),
	row.names=FALSE,col.names=FALSE,sep=",")