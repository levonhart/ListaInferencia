B=10000
png("Q1_norm_mix.png")
v = c()
par(mfrow=c(3,3))
for(n in c(2,5,10,20,30,50,100,200)){
    for(i in 1:B){
        index = rbinom(n,1,0.2)
        v[i] = mean( index*rnorm(n,-1,sqrt(2)) + (1-index)*rnorm(1.5,sqrt(2)) )
    }
    hist(v,main = paste("n = ",n),breaks=20)
    
}

