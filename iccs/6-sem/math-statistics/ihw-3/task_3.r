sig <- 4
b1 <- 5
b2 <- 6
b3<-5

x<-round(rnorm(10,4,1), digits = 3)
y<-round(rnorm(10,5,12),digits = 4)
dat<-data.frame(Y=y, X=x)
y<-b1+b2*x+b3*x^2+rnorm(100,0,sig)
dat[1:10,]
x0<-array(1,n)
Y<-as.matrix(y)
n<-dim(dat)[1]
x0<-array(1,dim=n)
x<-dat$X

Xl<-matrix(c(x0,x),nrow=2,ncol=n,byrow = TRUE)
Y<-as.matrix(y) # Vector-stolbec
Sl<-Xl%*%t(Xl) # X * X^t
Sl1<-solve(Sl) # Obratnaya matrica
bhatl<-Sl1%*%Xl%*%Y
plot(x,y,cex=5) 
x1<-c(min(x),max(x))
y1<-bhatl[1]+bhatl[2]*x1
points(x1,y1,"l",col="red",cex=1)
x2<-x1[1]+c(0:1000)*(x1[2]-x1[1])/1000
y2<-bhatl[1]+bhatl[2]*x2+bhatl[3]*x2^2
# Esli vse X odinakovi => bespolezno tak kak matrica virojdena 
points(x2,y2,"l",col="blue",cex=10)
q1<-lm(Y~X,data=dat)
q1s<-summary(q1)
# Y-X^T beta

#quadratic
X<-matrix(c(x0,x,x^2), nrow=3,ncol=n,byrow=TRUE)
S<-X%*%t(X)
S1<-solve(S)
bhat<-S1%*%X%*%Y
x2<-x1[1]+c(0:1000)*(x1[2]-x1[1])/1000
y2<-bhat[1]+bhat[2]*x2+bhat[3]*x2^2
points(x2,y2,"l",col="blue",cex=1)
legend("topleft", legend=c("linear", "quadratic"), col=c("red","blue"),lwd=1)

#
q2<-lm(Y~I(x) + I(X^2), data=dat) #Y~X+X^2 == Linear model!!!
q2s<-summary(q2)
B<-q2s$"cov.scaled" # 
V<-q2s$sigma^2*B # Matrica kovariacii
sig2<-diag(V) # vector ocenki dispersii
sig21<-as.matrix(1/sqrt(sig2)) # maybe diag() - not as.matrix
CR<-diag(sig21)%*%V%*%diag(sig21) #ocenka kovariacionnoy matrici
q2c<-confint(q2)
q2a<-anova(q2)

#Quadratic
res<-y-bhat[1]-bhat[2]*x-bhat[3]*x^2
res1<-q2$residuals
s2<-sum(res^2)/(n-3)
s21<-q2s$sigma

## 3

res<-Y-t(X)%*%bhat
res1<-q2$residuals 
h<-hist(res,probability=TRUE)
xx<-c(min(res),max(res))
x3<-xx[1]+c(0:1000)*(xx[2]-xx[1])/1000
y3<-dnorm(x3,0,q2s$sigma)
points(x3,y3,"l",col="red",cex=2)
h$counts # obyedinit 
brk<-h$breaks[-c(2,8)]
hh<-hist(res,breaks=brk,plot=FALSE)
nu<-hh$counts
brk2<-hh$breaks
r<-length(brk2)-1
brk2[1]<--Inf
brk2[r+1]<-Inf
lw<-brk2[1:r]
up<-brk2[2:(r+1)]

#
prob.norm<-function(x){
    pnorm(up,0,x)-pnorm(lw,0,x)
}
#
csq.stat<-function(x){
    ex<-n*prob.norm(x)
    return(sum((nu-ex)^2/ex))
}


#
csqq<-nlm(csq.stat, q2s$sigma)
csqs<-csqq$minimum
pv<-pchisq(csqs,(r-2),lower.tail = FALSE)

#4 
solve(V) # Matrica kotoraya opredelayet,
#          koeficienti sootvetctvuyushey kvadratichnoy forme

### Doveritelniy interval: q2int<-confint(q2, level = 0.99)
q2int<-confint(q2, level = 0.99) 
# Stroyim dlya beta 2 i beta 3 => delaem 2 vivoda 

# 5 Dispersioniy analis: 
# Mojno postiot tablicu 
q0<-lm(Y~1)

# int.a<-anova(r2.a,r)
# smk.a<-anova(r2smk,r2)
# age.a<-anova(r2age,r2)
# no.a<-anova(r2no,r2)

lin.a<-anova(q1,q2)
ind.a<-anova(q0,q2)
#Res
# ТАБЛИЦА !!!!
t.aov<-rbind(lin.a[2,],ind.a[2,])
SEE<-t.aov$residuals
DfR<-t.aov$Res.Df
SSH<-t.aov$Df
Df<-t.aov$Df
SSE.m<-SSE/DfR
SSH.m<-SSH/Df
Fst<-SSH.m/SSE.m
pv<-pf(Fst,Df,DfR,lower.tail = FALSE)
xal<-qf(0.95,Df,DfR)
#AIC
AIC(q2,q1,q0)
BIC(q2,q1,q0)

