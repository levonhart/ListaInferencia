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
comp = matrix(0,5,12)
for(k in 1:5){
    n=dados$n[k]
    for(i in 1:B){
        v = rexp(n,0.1)
        soma = sum(v)
        for(j in 1:3){
            q1 = 2*soma/qchisq( (1+gama[j])/2 ,2*n)
            q2 = 2*soma/qchisq( (1-gama[j])/2 ,2*n)
            comp[k,j] = comp[k,j] + q2-q1
            if(q1<=10&&10<=q2){
                Ma[k,j] = Ma[k,j] + 1
            }
        }

        y = min(v)
        for(j in 1:3){
            q1 = y*n/qexp( (1+gama[j])/2, 1 )
            q2 = y*n/qexp( (1-gama[j])/2, 1 )
            comp[k,j+3] = comp[k,j+3] + q2-q1
            if(q1<=10&&10<=q2){
                Mb[k,j] = Mb[k,j] + 1
            }
        }
        
        media = soma/n
        for(j in 1:3){
            q1 = media/(1+qnorm( (1+gama[j])/2, 0, 1)/sqrt(n))
            q2 = media/(1-qnorm( (1+gama[j])/2, 0, 1)/sqrt(n))
            comp[k,j+6] = comp[k,j+6] + q2-q1
            if(q1<=10&&10<=q2){
                Mc1[k,j] = Mc1[k,j] + 1
            }
        }
        for(j in 1:3){
            q1 = media - qnorm( (1+gama[j])/2, 0, 1)*media/sqrt(n)
            q2 = media + qnorm( (1+gama[j])/2, 0, 1)*media/sqrt(n)
            comp[k,j+9] = comp[k,j+9] + q2-q1
            if(q1<=10&&10<=q2){
                Mc2[k,j] = Mc2[k,j] + 1
            }
        }

    } 
    dados$a = Ma/B
    dados$b = Mb/B
    dados$c1 = Mc1/B
    dados$c2 = Mc2/B
}
comp = comp/B
dados
print("Comprimento médio\n")
comp
