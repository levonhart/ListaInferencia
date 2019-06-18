B=10000
v=c()
dados= data.frame(
                n = c(10,30,50,100,200),
                a = numeric(5),
                b = numeric(5),
                c = numeric(5)
                )
alpha = c(0.01,0.05,0.10)
param = c(0.1,0.5,0.8)
Ma = matrix(0,5,9)
Mb = matrix(0,5,9)
Mc = matrix(0,5,9)
for(k in 1:5){
    n=dados$n[k]
    for(i in 1:B){
        for (l in 1:3) {
            v = rbinom(n,1,param[l])
            emv = mean(v)
            z = (emv - param[l])/sqrt(param[l]*(1-param[l])/n)
            for (j in 1:3) {
                if ( abs(z) > qnorm( 1-alpha[j]/2, 0, 1 ) ){
                   Ma[k,3*(l-1)+j] = Ma[k,3*(l-1)+j] + 1
                }
                if ( z > qnorm( 1-alpha[j], 0, 1 ) ){
                   Mb[k,3*(l-1)+j] = Mb[k,3*(l-1)+j] + 1
                }
                if ( z < -qnorm( 1-alpha[j], 0, 1 ) ){
                   Mc[k,3*(l-1)+j] = Mc[k,3*(l-1)+j] + 1
                }
            }
        }

    } 
    dados$a = Ma/B
    dados$b = Mb/B
    dados$c = Mc/B
}
print(dados[,1:2])
print(dados[,c(1,3)])
print(dados[,c(1,4)])
