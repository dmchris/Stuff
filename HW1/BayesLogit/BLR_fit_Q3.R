##########################################
### STA250 Advanced Statistical Computing
### Homework 1
##########################################

library(MASS)

data<-read.table("C:/HaoJi/UC_DAVIS_PHD_in_STAT/2013-2014/Fall13/STA250/Homework1/breast_cancer.txt",
	header=TRUE)
head(data)
nrow(data)

### Data Input and Prior Distn Specification
n <- 569
y <- rep(0,n)
for(i in 1:n){if(data[i,11]=="M") y[i]<-1}
m <- rep(1,569)
X <- cbind(rep(1,569),as.matrix(data[,1:10])) # 569 by 11 data matrix
head(X)
beta.0 <- rep(0,11)
p <- 11
Sigma.0.inv <- diag(rep(1/1000,11))

### GLM Fit for the breast cancer data
glmfit <- glm(y~-1+X,family=binomial())
summary(glmfit)
beta.hat <- glmfit$coefficients
COV.beta.hat <- summary(glmfit)$cov.scaled

#beta.initial <- c(-10,0.06,-3,50,5,-55,0.2,-4,60,10,0.3) # initial values of sampling algorithm
beta.initial <- beta.hat
result <- matrix(rep(0,11000*p),nrow=11000,ncol=p)

### Function for calculating acceptance rate
logalpha <- function(bt,bstar){
	logpistar <- -0.5*t(bstar)%*%Sigma.0.inv%*%bstar+
		t(y)%*%(X%*%bstar)-sum(log(1+exp(X%*%bstar)))
	logpit <- -0.5*t(bt)%*%Sigma.0.inv%*%bt+
		t(y)%*%(X%*%bt)-sum(log(1+exp(X%*%bt)))
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
		v <- rep(0.25,3) #c(1,1,1)
    count <- rep(0,3)
		countiter <- rep(0,3)
		for (i in 1:(burnin+niter)){
			beta.star <- rep(0,p)
			for(j in 1:3){
        indices <- (4*(j-1)+1):min(p,4*j)
        beta.star[indices] <- mvrnorm(1,mu=betat[indices],Sigma=v[j]*COV.beta.hat[indices,indices])
        bnew <- betat
        bnew[indices] <- beta.star[indices]
        lalpha <- logalpha(betat,bnew)
        U <- runif(1,min=0,max=1)
        if(log(U) < lalpha){
          result.sample[i,indices] <- beta.star[indices]
          betat[indices] <- beta.star[indices]
          count[j] <- count[j]+1
          if(i>burnin) countiter[j] <- countiter[j]+1
        }
        if(log(U) >= lalpha) {result.sample[i,indices] <- betat[indices]}
			}
      
			
			if(i%%retune==0 && i<=burnin){
			  accrate <- count/retune
			  for(jj in 1:3){
			    if(accrate[jj]<0.35) v[jj] <- v[jj]/sqrt(5)
			    if(accrate[jj]>0.5) v[jj] <- v[jj]*sqrt(5)
			  }
			  cat("Acceptance Rate after", i, "iterations during burnin", accrate, "\n")
			  cat("Tuning Parameters after", i, "iterations during burnin: ", v, "\n")
			  count <- rep(0,3)
			}
			if(i%%print.every==0){cat(i," iterations of MH in Gibbs have been completed. \n")}
			
		}#Gibbs For loop Ends
		cat("Acceptance Rate after Burnin is ", countiter/(niter), "\n") ### FOR METHOD1
      
		if(verbose){
			par(mfrow=c(3,4))
			for(k in 1:p){
				plot(result.sample[-(1:burnin),k],type='l')
				#hist(result.sample[-(1:burnin),k])
			}
		}
		cat("Tuning Parameter After Burnin is: ", v, "\n") ### FOR METHOD1
	}

	### METHOD2: MH with smart proposal:
	if(way==2){
		delta <- 1; 
		Sigma.p <- COV.beta.hat
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
				if(accrate<0.3) delta <- delta/sqrt(4)
				if(accrate>0.6) delta <- delta*sqrt(4)
				cat("Acceptance Rate after", i, "iterations during burnin", accrate, "\n")
				cat("Tuning Parameters after", i, "iterations during burnin: ", delta, "\n")
				count <- 0
			}
			if(i%%print.every==0){cat(i," iterations of MH have been completed. \n")}
		}#Gibbs For loop Ends
		cat("Acceptance Rate after Burnin is ", countiter/niter, "\n") ### FOR METHOD2
		if(verbose){
			par(mfrow=c(3,4))
			for(k in 1:p){
				plot(result.sample[,k],type='l')
				#hist(result.sample[,k])
			}
		}
		cat("Tuning Parameter After Burnin is: ", delta, "\n") ### FOR METHOD2
	}
result.sample[-(1:burnin),]
}#Function Ends

### Part(a) Fit the Bayesian Logistic Regression Model
### Function Call
result <- bayes.logreg(m,y,X,beta.0,Sigma.0.inv,niter=10000,way=2,burnin=2000,print.every=2000,retune=100,verbose=TRUE)
nrow(result)
### result is a niter by p matrix containing the resulting posterior sample

### Part(b) Lag-1 correlation for each component of beta
ACF.beta <- rep(0,p)
for(j in 1:p){
	ACF.beta[j] <- acf(result[,j],plot=F)$acf[2]
}
ACF.beta

### Part(c)
par(mfrow=c(3,4))
for(k in 1:p){
	#plot(result[,k],type='l') # Traceplot
	hist(result[,k])  # Histogram
	abline(v=sort(result[,k])[250],col=2)
	abline(v=sort(result[,k])[9750],col=2)
	cat("The", p, "th covariate has 95% credible interval as (",
	sort(result[,k])[250] ,",", sort(result[,k])[9750],").\n")
}

### Part(d) Posterior Predictive Check
N <- 1000 # number of samples to sample from posterior distribution
beta.pos <- matrix(nrow=N,ncol=p)
y.mean <- rep(0,N)

for(i in 1:N){
	betaindex <- sample(10000,1)
	beta.pos[i,] <- result[betaindex,]
	y.pos <- rbinom(n,size=1,prob=1-(exp(X%*%(beta.pos[i,]))+1)^(-1))
	y.mean[i] <- mean(y.pos)
}
# MEAN
hist(y.mean,xlim=c(0.3,0.45))
abline(v=mean(y),col=2)

### Part(e) According to the Posterior Predictive Check, the Bayes model 
# fits the model quite well since the mean response of the observed data set is
# around the center of those generated by sampled parameters from its posterior distribution
