B=10000
v=c()
dados= data.frame(
                n = c(10,30,50,100,200),
                a = numeric(5),
                b = numeric(5),
                c1 = numeric(5),
                c2 = numeric(5)
                )
gama = c(0.8,0.9,0.95)
Ma = matrix(0,5,3)
Mb = matrix(0,5,3)
Mc1 = matrix(0,5,3)
Mc2 = matrix(0,5,3)
for(k in 1:5){
    n=dados$n[k]
    print(paste("n = ",n,": "))
    for(i in 1:B){
        v = rbeta(n,6,1)
        soma_log = sum(log(v))
        for(j in 1:3){
            q1 = qchisq( (1-gama[j])/2 ,2*n)/(-2*soma_log)
            q2 = qchisq( (1+gama[j])/2 ,2*n)/(-2*soma_log)            
            if(q1<=6&&6<=q2){
                Ma[k,j] = Ma[k,j] + 1
            }
        }
        
        emv = 1/mean(-log(v))
        for(j in 1:3){
            q1 = emv/(1+qnorm( (1+gama[j])/2, 0, 1)/sqrt(n))
            q2 = emv/(1-qnorm( (1+gama[j])/2, 0, 1)/sqrt(n))
            if(q1<=6&&6<=q2){
                Mc1[k,j] = Mc1[k,j] + 1
            }
        }
        for(j in 1:3){
            q1 = emv - qnorm( (1+gama[j])/2, 0, 1)*emv/sqrt(n)
            q2 = emv + qnorm( (1+gama[j])/2, 0, 1)*emv/sqrt(n)
            if(q1<=6&&6<=q2){
                Mc2[k,j] = Mc2[k,j] + 1
            }
        }

    } 
    dados$a = Ma/B
    dados$b = Mb/B
    dados$c1 = Mc1/B
    dados$c2 = Mc2/B
}
dados
