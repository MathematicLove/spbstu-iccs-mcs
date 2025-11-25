al <- 0.05 # уровень значимости
a0 <-1 #nolint
sig0 <-1 #nolint
x<-rnorm(100,a0,sig0) #генератор данных 100 - кол-во наблюдений в выборкие, a0, sig0  # nolint
                    # - параметры распределения, при этом sig0 - ско # nolint
xs = sort(x)
v1 <- pnorm(xs, a0,sig0)
n = length(x) #nolint

#y = ecdf(x) # функция строящая выборочную функцию распределения, непрерывная справа при этом. #nolint
# для печати хороша функция pdf
# pdf(что-то)
# код графика
# dev.off() закрыли
plot.ecdf(x, pch = NA, verticals = FALSE, main = "ECDF", ylab ="Distribution finction" , lwd =1, cex = 0.2)
#main - название, ylab - подпись под осью соответсвующей.
points(xs, c(0:(n-1))/n, cex = 0.2, pch =19 );
#рисуем непрерывную слева функцию, поэтому рисеум без точек, у нас в итоге все получается #nolint

# теперь посмотрим насколько выборочная отличается от теоритической
#
m1<-min(x)
m2<-max(x) # nolint: infix_spaces_linter.
#
# сетка
x1 = m1 +c(0:1000)/1000*(m2-m1)
y1 = pnorm (x1,a0,sig0) # функция вычисления распределения с известными нам параметрами. #nolint
# в лабе парамеров не будет
points(x1,y1, "l", col = "blue", lwd = 1)
# гистограмма, h- шаг гистограммы. В случае чего в лабе можно будет поменять
h= 2
mm = m2-m1 
# сами разбираем на шаги, в каких точке сделать разрывы. И тогда он сам обязательно ставить разрывы там, где ты хочешь
brks = m1 + h*(c(0:ceiling(mm/h)))
#breaks - может не перестраивать, если ему скармливать просто число, так как R сам обозначает, что он хочет.
# Есть способ когда сами выбираем границы.
# сейчас его и рассмотрим
hh = hist(x, breaks = brks, probability = TRUE, ylab = "Плотность распределния", xlim = c(-1.5,5), ylim = c(0,0.5))
# сетка
x1 = -2 +c(0:1000)/1000*8
y1 = dnorm (x1,a0,sig0) # функция вычисления распределения с известными нам параметрами. #nolint
points(x1,y1,"l", col="blue", lwd = 2)
#
# в гистограме probability - всегда тру, так как нам нужна в лабе такая оценка ( не успел записать какая)
# дял пуасоновскиого распр в диаграмме может использовать стандартный шаг.
# теперб про полигоны
# полигон - ломанная, соед. середины столбцов гистограмм
# Сказал, что можео построить самим, но если не смогем, можно просто нарисовать будет ручкой в очтете..
# помимо гист и полигона можно посмотреть ядерную оценку плотности
# этоге нет в дз, но сейчас замутим: 
dns = density(x) # функция ядерной оценки тут параметр сглаживания 0,35
dns1 = density(x, bw = 0.5) # ядерная оценка с парметром сглаживания 0,05
# ядерная не оценивает группировку но оценивает параметр сглаживания
points(dns$x,dns$y,"l", col="#26f326", lwd = 3)
points(dns1$x,dns1$y,"l", col="orange", lwd = 3)
# в целом это все, что надо сделать в первом задании идз
# работа в целом - защищается.

#рассмотрим те числовые характеристики, которые надо будет вычислить в совем идз
m = mean(x)# Среднее заначение
# var - функция выч. исправенную выборочную дисперсию для исправления нужно умножить на n-1 и поделить на n
# на это 100% посмотрит, так что обязательно проверять
s2 = (n-1)*var(x)/n # выю дисперсия
# есть альт формула - выборочная квадрата - квадрат мат ожидания - выдает тоже самое.
#Две характерстики матожидание и дисперия, выборочные!

#Вроде функция для выборочной медианы, но не уверен

if(n%%2) {
    med = xs[n/2]
} else {
    med = xs [(trunc(n/2)+1)]
}
asi = mean ((x-m)^3/s2^(3/2))
exc = mean((x - m)^4 / s2^2) - 3  # Эксцесс
#тут проблемы какие-то не могу сообразить
brk <- c(-2, 0, 0.5, 1, 1.5, 2, 5)  # ваши исходные границы
brk[1] <- -Inf   # первая граница = -∞
brk[length(brk)] <- Inf  # последняя граница = +∞

l.brk <- length(brk)
lw <- brk[c(1:(l.brk-1))]  # нижние границы
up <- brk[c(2:l.brk)]      # верхние границы
#prb = sum ((x>=c) & (x<=d))/n #вообще сравнение генерит булевы значения, но в сумме они кастятся в инты нормальные

#строим дов.интервал
qal = qt(1-al/2, n-1)
il = qal * sqrt(s2) / sqrt(n-1)
CIa = c (m-il, m+il)
#
#квантили
x1a = qchisq(qal/2, n-1)
x2a = qchisq(1-qal/2,n-1)
CIs = n*s2/c(x2a,x1a)
#
#Объединим все построив датафрейм:breaks
CI<-data.frame(Par = c("Mean", "Var"), Lw = c(CIa[1], CIs2[1]), Up = c(CIa[2],CIs2[2]))
# тут просто вывод в датафрейм, не успел записать
#5 Проверка гипотез простых нет, это критерий колмагорова
i = c(1:n)
v0 = xs
vq = c (0:(n-1))/n
v2 = v1 +1/n
v3 = pnorm (xs, a0, sig0)
v4<-abs(v3-v1)
v5<-abs(v3-v2)
v6<-pmax(v4,v5)
KS.tab<-data.frame(i=i,xi=v0,lecdf = v1, recdf=v2, hcdf=v3, ldif=v4, rdif=v5, maxdif=v6)
j<-which(KS.tab$maxdif==max(KS.tab$maxdif))
KS.tab[j,]
D<-max(v6)*sqrt(n)

distrf<-function(x){
    prorm(x,a0,sig0)
}
ks<-ks.test(x,distrf)
ks
names(ks)


#Вотут норм
brk = c(-2,0,0.5,1,1.5,2,5)
hh = hist(x,breaks=brk, plot = FALSE)
hh1 = hist(x,breaks=brk, plot = FALSE)
l.brk = length(brk)
brk[1] = Inf
brk[l.brk] = Inf
nu = hh1$counts 
lw = brk[c(1:(l.brk-1))]
up = brk[c(2:l.brk)]
# Надо выбрать так чтобы частоты боли больше или равны пяти.


#Практика за 18.04
pr <- pnorm(up,a0,sig0) - pnorm(lw,a0,sig0)
npr<-pr*n
res<-(nu-npr)/sqrt(npr)
res2<-res^2
X2<-sum(res2)
ngr<-length(nu)
csq.t<-
data.frame(Group=c(1:ngr), lower=lw,upper=up,count=nu, prob=pr,exp=npr,resid=res,resid2=res2)
csq.t

xal<-qchisq(1-al,ngr-1)
X2>xal
pv.csq<-pchisq(X2,ngr-1,lower.tail = FALSE) 
q<-chisq.test(nu, p=pr)

prob.norm<-function(x){
    pnorm(up,x[1],x[2])-pnorm(lw,x[1],x[2])
}

csq.stat<-function(x){
    ex<-n*prob.norm(x)
    return(sum((nu-ex)^2/ex))
}

qq<-nlm(csq.stat, c(m,sqrt(s2)))
X2C<-qq$minimum
xal<-qchisq(1-al.ngr-3)
X2C >xal
pv.csq.c<-pchisq(X2C, ngr-3,lower.tail = FALSE) # 1-phisq(X2,ngr-1)


