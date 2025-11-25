if (is.null(getOption("repos")[["CRAN"]]) ||
    getOption("repos")[["CRAN"]] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

pkgs <- c("moments", "ggplot2")
to.install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to.install)) install.packages(to.install)

suppressPackageStartupMessages({
  library(moments)
  library(ggplot2)
})

if (!dir.exists("fig")) dir.create("fig")

theme_set(theme_bw())

x1 <- c(0,0,2,1,0,0,0,0,0,1,0,1,0,0,0,2,0,2,1,0,
        1,1,0,1,1,3,0,0,0,0,0,1,0,0,0,0,4,1,5,2,
        0,0,2,0,0,1,1,0,0,1)
n1          <- length(x1)
a1 <- 0;  b1 <- 1.79
lambda0.1   <- 0.60;  lambda1.1 <- 1.40
alpha1      <- 0.002

freq1 <- table(factor(x1, levels = 0:max(x1)))
df.freq1 <- data.frame(k = as.integer(names(freq1)),
                       n.k = as.integer(freq1),
                       rel = as.numeric(freq1) / n1)

# ECDF
F.n1 <- ecdf(x1)

png("fig/emp_dist_1.png", 800, 600)
plot(F.n1, main = "ECDF • задача 1", verticals = TRUE,
     do.points = FALSE, lwd = 2)
dev.off()

png("fig/hist_1.png", 800, 600)
hist(x1, breaks = seq(-0.5, max(x1) + 0.5, by = 1),
     col = "#FDBE85", border = "grey20",
     main = "Гистограмма • задача 1")
dev.off()

## 1.2  выборочные характеристики --------------------------
m1      <- mean(x1)
s2.1    <- var(x1)
skew1 <- (sqrt(n1*(n1-1))/(n1-2)) * skewness(x1)
kurt1 <- ((n1-1)/((n1-2)*(n1-3))) * ((n1+1)*kurtosis(x1) + 6)
p.ab.1  <- mean(x1 >= a1 & x1 <= b1)

## 1.3  оценки λ -------------------------------------------
lambda.hat.1 <- m1              # MLE = MOM
bias.lambda.1 <- 0

## 1.4  Доверительный интервал -----------------------------
z.a <- qnorm(1 - alpha1 / 2)
ci.low.1  <- lambda.hat.1 - z.a * sqrt(lambda.hat.1 / n1)
ci.high.1 <- lambda.hat.1 + z.a * sqrt(lambda.hat.1 / n1)

## 1.5  χ² (простая) ---------------------------------------
obs.simple <- c(sum(x1 == 0), sum(x1 == 1), sum(x1 >= 2))
exp.simple <- c(dpois(0, lambda0.1),
                dpois(1, lambda0.1),
                1 - dpois(0, lambda0.1) - dpois(1, lambda0.1)) * n1
chi2.simple <- sum((obs.simple - exp.simple)^2 / exp.simple)
p.simple.1  <- pchisq(chi2.simple, df = 2, lower.tail = FALSE)

chi2.table.1 <- data.frame(
  i   = 1:3,
  lw  = c("k=0", "k=1", "k≥2"),
  np  = round(exp.simple / n1, 4),          # теор. вероятности
  nu  = obs.simple,                         # n_i
  p   = round(exp.simple / n1, 4),          # (=np для простоты)
  np_r= exp.simple,                         # ожидания
  res = round((obs.simple - exp.simple) / sqrt(exp.simple), 3),
  res2= round((obs.simple - exp.simple)^2 / exp.simple, 3)
)

## 1.6  χ² (сложная) ---------------------------------------
exp.comp <- dpois(0:5, lambda.hat.1) * n1
obs.comp <- c(freq1["0"], freq1["1"], freq1["2"],
              sum(freq1[c("3","4","5")]))           # 4 класса
exp.comp <- c(exp.comp[1:3], sum(exp.comp[4:6]))

chi2.comp <- sum((obs.comp - exp.comp)^2 / exp.comp)
p.comp.1  <- pchisq(chi2.comp, df = length(obs.comp) - 2,
                    lower.tail = FALSE)              # −1 параметр

##  динамически подбираем подписи
labels.comp <- c("k=0", "k=1", "k=2", "k≥3")[1:length(obs.comp)]

chi2.table.1c <- data.frame(
  i    = 1:length(obs.comp),
  lw   = labels.comp,
  np   = round(exp.comp / n1, 4),      # теоретические вероятности
  ν    = obs.comp,                     # наблюдённые частоты
  p    = round(exp.comp / n1, 4),      # дублируем, чтобы был столбец «p»
  np_r = exp.comp,                     # np — ожидания
  res  = round((obs.comp - exp.comp) / sqrt(exp.comp), 3),
  res2 = round((obs.comp - exp.comp)^2 / exp.comp, 3)
)


## 1.7  Неймана–Пирсона 
S.1   <- sum(x1)
c.np.1 <- qpois(1 - alpha1, lambda0.1 * n1)
decision.np.1 <- ifelse(S.1 >= c.np.1,
                        "Отвергнуть H₀", "Сохранить H₀")

#2
x2 <- c(10.34, 2.18, 8.80, 2.28, 1.95, 0.85, 3.73, 10.26, 5.01, 0.70,
        2.38, 0.25, 0.45, 0.31, 1.73, 2.67, 1.00, 1.59, 14.28, 2.14,
        1.85, 0.67, 2.70, 2.07, 5.31, 6.37, 3.24, 3.27, 1.31, 2.75,
        6.06, 1.05, 0.86, 2.43, 0.03, 3.70, 0.11, 1.06, 6.28, 0.55,
        9.07, 6.52, 0.94, 2.61, 0.89, 1.67, 0.24, 1.68, 3.34, 1.38)
n2 <- length(x2)
h   <- 1.20;  c2 <- 2.40; d2 <- 6.00
lambda0.2 <- 0.20;  lambda1.2 <- 0.33
alpha2    <- 0.001

bins2 <- seq(0, ceiling(max(x2)/h)*h, by = h)  
hist2 <- hist(x2, breaks = bins2, plot = FALSE)

png("fig/emp_dist_2.png", 800, 600)
plot(ecdf(x2), main = "ECDF • задача 2", verticals = TRUE,
     do.points = FALSE, lwd = 2)
dev.off()

png("fig/hist_2.png", 800, 600)
h2 <- hist(x2, breaks = bins2, col = "#B2DF8A",
           border = "grey20", main = "Гистограмма • задача 2")
lines((bins2[-1] + bins2[-length(bins2)])/2, h2$counts,
      type = "b", pch = 19, lwd = 2)
dev.off()

## 2.1  выборочные характеристики  
## 2.1  vyborochnye kharakteristiki 
m2     <- mean(x2)
s2.2   <- var(x2)

skew2  <- (sqrt(n2*(n2-1))/(n2-2)) * skewness(x2)         # b1
kurt2  <- ((n2-1)/((n2-2)*(n2-3))) * ((n2+1)*kurtosis(x2) + 6)  # b2

p.cd.2 <- mean(x2 >= c2 & x2 <= d2)


## 2.2  оценки λ  
lambda.hat.2  <- 1 / m2
bias.lambda.2 <- lambda.hat.2 / (n2 - 1)

## 2.3  доверительный интервал 
z.b <- qnorm(1 - alpha2 / 2)
ci.low.2  <- lambda.hat.2 - z.b * lambda.hat.2 / sqrt(n2)
ci.high.2 <- lambda.hat.2 + z.b * lambda.hat.2 / sqrt(n2)

## 2.4  Колмогоров (простая) 
ks2 <- ks.test(x2, "pexp", rate = lambda0.2)

# ручная таблица
xs2      <- sort(x2)
F.emp.u  <- (1:n2) / n2
F.emp.l  <- (0:(n2-1)) / n2
F.theor  <- pexp(xs2, rate = lambda0.2)
delta.l  <- abs(F.theor - F.emp.l)
delta.r  <- abs(F.theor - F.emp.u)
delta.m  <- pmax(delta.l, delta.r)

tab.KS <- data.frame(
  i    = 1:n2,
  lw   = F.emp.l,
  np   = F.theor,
  nu   = F.emp.u,
  p    = delta.l,
  np_r = delta.r,
  res  = delta.m,
  res2 = delta.m^2
)

KS.head <- head(tab.KS, 3)

## 2.5  χ² (простая) 
prob.exp0 <- diff(pexp(bins2, rate = lambda0.2))
exp2      <- prob.exp0 * n2
obs2      <- hist2$counts

# объединение интервалов с E<5
combine <- function(obs, exp) {
  o <- obs; e <- exp; i <- 1
  while (i <= length(e)) {
    if (e[i] < 5) {
      if (i == 1) {
        e[2] <- e[2] + e[1];  o[2] <- o[2] + o[1]
        e <- e[-1];  o <- o[-1]
      } else {
        e[i-1] <- e[i-1] + e[i];  o[i-1] <- o[i-1] + o[i]
        e <- e[-i];  o <- o[-i];  i <- i - 1
      }
    }
    i <- i + 1
  }
  list(obs = o, exp = e)
}
tmp <- combine(obs2, exp2)
obs2.c <- tmp$obs;  exp2.c <- tmp$exp

chi2.simple.2 <- sum((obs2.c - exp2.c)^2 / exp2.c)
p.simple.2    <- pchisq(chi2.simple.2, df = length(obs2.c)-1,
                        lower.tail = FALSE)

chi2.table.2 <- data.frame(
  i    = 1:length(obs2.c),
  lw   = head(bins2, -1),
  up   = tail(bins2, -1),
  ν    = obs2.c,                      # ν (nu) – частоты
  p    = round(exp2.c / n2, 4),
  np   = exp2.c,
  res  = round((obs2.c - exp2.c) / sqrt(exp2.c), 3),
  res2 = round((obs2.c - exp2.c)^2 / exp2.c, 3)
)
chi2.head.2 <- head(chi2.table.2, 3)

## 2.6  χ² (сложная) 
prob.exphat <- diff(pexp(bins2, rate = lambda.hat.2))
tmp  <- combine(obs2, prob.exphat * n2)
obs2.comp <- tmp$obs;  exp2.comp <- tmp$exp
chi2.comp.2 <- sum((obs2.comp - exp2.comp)^2 / exp2.comp)
p.comp.2    <- pchisq(chi2.comp.2, df = length(obs2.comp)-2,
                      lower.tail = FALSE)

## 2.7  Неймана–Пирсона 
S.2   <- sum(x2)
c.np.2 <- qgamma(alpha2, shape = n2, scale = 1/lambda0.2)
decision.np.2 <- ifelse(S.2 >= c.np.2,
                        "Отвергнуть H₀", "Сохранить H₀")


cat("\n================  ЗАДАЧА 1  ============================\n")
cat(sprintf(
  "n = %d,  Σx = %d\nx̄ = %.4f,  s² = %.4f,  med = 0\n",
  n1, sum(x1), m1, s2.1))
cat(sprintf(
  "Skew = %.4f,  Kurt(ex) = %.4f,  P[%g,%g] = %.2f\n",
  skew1, kurt1, a1, b1, p.ab.1))
cat(sprintf(
  "λ̂ = %.4f (MLE=MOM),  CI₉₉.₈%% = [%.4f; %.4f]\n",
  lambda.hat.1, ci.low.1, ci.high.1))

cat("\n--- χ²-таблица (простая), первые 3 строки --------------\n")
print(chi2.table.1, row.names = FALSE)

cat(sprintf("χ² = %.4f  (df = 2)  p = %.4f\n", chi2.simple, p.simple.1))

cat("\n--- χ²-таблица (сложная), первые 3 строки -------------\n")
print(chi2.table.1c, row.names = FALSE)

cat(sprintf("χ² = %.4f  (df = 2)  p = %.4f\n", chi2.comp, p.comp.1))

cat(sprintf(
  "\nNP-тест:   S = %d,  c = %d  ⇒  %s\n",
  S.1, c.np.1, decision.np.1))

cat("\n================  ЗАДАЧА 2  ============================\n")
cat(sprintf(
  "n = %d,  Σx = %.2f\nx̄ = %.4f,  s² = %.4f,  med = %.3f\n",
  n2, sum(x2), m2, s2.2, median(x2)))
cat(sprintf(
  "Skew = %.4f,  Kurt(ex) = %.4f,  P[%g,%g] = %.2f\n",
  skew2, kurt2, c2, d2, p.cd.2))
cat(sprintf(
  "λ̂ = %.4f,  bias ≈ %.5f,  CI₉₉.₉%% = [%.4f; %.4f]\n",
  lambda.hat.2, bias.lambda.2, ci.low.2, ci.high.2))

cat("\n---  таблица Колмогорова (первые 3 строки) -------------\n")
print(KS.head, row.names = FALSE)
cat(sprintf("D = %.4f,  p-value = %.5f\n", ks2$statistic, ks2$p.value))

cat("\n--- χ²-таблица (простая), первые 3 строки --------------\n")
print(chi2.head.2, row.names = FALSE)
cat(sprintf("χ² = %.4f  (df = %d)  p = %.4f\n",
            chi2.simple.2, length(obs2.c)-1, p.simple.2))

cat(sprintf(
  "\nχ² (сложная) = %.4f  (df = %d)  p = %.4f\n",
  chi2.comp.2, length(obs2.comp)-2, p.comp.2))

cat(sprintf(
  "\nNP-тест:   S = %.2f,  c = %.2f  ⇒  %s\n",
  S.2, c.np.2, decision.np.2))

write.csv(chi2.table.1,  "chi2_task1_simple_head3.csv", row.names = FALSE)
write.csv(chi2.table.1c, "chi2_task1_comp_head3.csv",   row.names = FALSE)
write.csv(KS.head,       "ks_task2_head3.csv",          row.names = FALSE)
write.csv(chi2.head.2,   "chi2_task2_simple_head3.csv", row.names = FALSE)
