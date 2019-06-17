B=10000
png("Q1_binomial.png")
v = c()
par(mfrow=c(3,3))
for(n in c(2,5,10,20,30,50,100,200)){
    for(i in 1:B){
        v[i] = mean( rbinom(n,n,1/n) )
    }
    hist(v,main = paste("n = ",n),breaks=20)
    
}

