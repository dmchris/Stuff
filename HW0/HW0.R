#######################
### STA250 Homework 0
### Hao Ji 998362038
#######################

### Q1 Fizz & Buzz
for (i in 1:100){
if(i%%3==0 && i%%5==0) print("FizzBuzz")
if(i%%3==0) print("Fizz")
if(i%%5==0) print("Buzz")
if(i%%3!=0 && i%%5!=0) print(i)
}

### Q2 
X <- runif(10000, min=0, max=2*pi)
Y <- runif(10000, min=0, max=1)
U <- Y*cos(X)
V <- Y*sin(X)
plot(U,V)
R <- sqrt(U^2+V^2)
# R follows uniform(0,1) distribution

### Q3
# Save separate characters to separate files
txt <- "Hello, my name is Bob. I am a statistician. I like statistics very much."
n <- nchar(txt)
for(i in 1:n){
sub <- substr(txt,start=i,stop=i)
fname <- sprintf("out_%02d.txt", i)
write.table(as.character(sub), fname, row.name=FALSE, col.name=FALSE)
}
# Read characters to recover the string
str <- ""
for(j in 1:n){
s <- scan(sprintf("out_%02d.txt", j),what="")
str <- paste(str,as.character(s),sep="")
}
str

### Q4
