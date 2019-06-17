B=10000
png("Q1_uniforme.png")
v = c()
par(mfrow=c(3,3))
for(n in c(2,5,10,20,30,50,100,200)){
    for(i in 1:B){
        v[i] = mean( runif(n,0,1) )
    }
    hist(v,main = paste("n = ",n),breaks=20)
    
}

    
