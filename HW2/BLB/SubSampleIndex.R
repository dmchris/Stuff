SubSampleIndex <- list()
s <- 5
n <- 1000000
gamma <- 0.7
b <- as.integer(n^gamma)

for(i in 1:s){
  SubSampleIndex[[i]] <- sample(n,b,replace=FALSE)
}
save(SubSampleIndex,file="~/STA250/Stuff/HW2/BLB/SubSampleIndex.RData")
