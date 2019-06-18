B=10000
v=c()
dados= data.frame(
                n = c(10,30,50,100,200),
                a = numeric(5),
                b1 = numeric(5),
                b2 = numeric(5)
                )
gama = c(0.8,0.9,0.95)
Ma = matrix(0,5,3)
Mb1 = matrix(0,5,3)
Mb2 = matrix(0,5,3)
comp = matrix(0,5,9)
for(k in 1:5){
    n=dados$n[k]
    for(i in 1:B){
        v = rbeta(n,6,1)
        soma_log = sum(log(v))
        for(j in 1:3){
            q1 = qchisq( (1-gama[j])/2 ,2*n)/(-2*soma_log)
            q2 = qchisq( (1+gama[j])/2 ,2*n)/(-2*soma_log)            
            comp[k,j] = comp[k,j] + q2-q1
            if(q1<=6&&6<=q2){
                Ma[k,j] = Ma[k,j] + 1
            }
        }
        
        emv = 1/mean(-log(v))
        for(j in 1:3){
            q1 = emv/(1+qnorm( (1+gama[j])/2, 0, 1)/sqrt(n))
            q2 = emv/(1-qnorm( (1+gama[j])/2, 0, 1)/sqrt(n))
            comp[k,j+3] = comp[k,j+3] + q2-q1
            if(q1<=6&&6<=q2){
                Mb1[k,j] = Mb1[k,j] + 1
            }
        }
        for(j in 1:3){
            q1 = emv - qnorm( (1+gama[j])/2, 0, 1)*emv/sqrt(n)
            q2 = emv + qnorm( (1+gama[j])/2, 0, 1)*emv/sqrt(n)
            comp[k,j+6] = comp[k,j+6] + q2-q1
            if(q1<=6&&6<=q2){
                Mb2[k,j] = Mb2[k,j] + 1
            }
        }

    } 
    dados$a = Ma/B
    dados$b1 = Mb1/B
    dados$b2 = Mb2/B
}
comp = comp/B
dados
print("Comprimento médio\n")
comp
