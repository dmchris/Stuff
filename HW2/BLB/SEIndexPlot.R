SE <- read.table("C:/HaoJi/UC_DAVIS_PHD_in_STAT/2013-2014/Fall13/STA250/Homework2/blb_lin_reg_data_s5_r50_SE.txt",
                 header=TRUE)
head(SE)
nrow(SE)
plot(1:1000, as.matrix(SE), main="Index Plot of Standard Deviation",
     xlab="Index", ylab="Estimated Standard Error")