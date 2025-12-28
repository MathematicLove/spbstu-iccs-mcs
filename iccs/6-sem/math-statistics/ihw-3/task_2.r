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


#2
x2 <- c(10.34, 2.18, 8.80, 2.28, 1.95, 0.85, 3.73, 10.26, 5.01, 0.70,
        2.38, 0.25, 0.45, 0.31, 1.73, 2.67, 1.00, 1.59, 14.28, 2.14,
        1.85, 0.67, 2.70, 2.07, 5.31, 6.37, 3.24, 3.27, 1.31, 2.75,
        6.06, 1.05, 0.86, 2.43, 0.03, 3.70, 0.11, 1.06, 6.28, 0.55,
        9.07, 6.52, 0.94, 2.61, 0.89, 1.67, 0.24, 1.68, 3.34, 1.38)

n2 <- length(x2)
h      <- 1.20
c2     <- 2.40
d2     <- 6.00
lambda0.2 <- 0.20
lambda1.2 <- 0.33
alpha2    <- 0.001

bins2 <- seq(0, ceiling(max(x2)/h)*h, by = h)

##  ECDF
png("fig/emp_dist_2.png", 800, 600)
plot(ecdf(x2), main = "ECDF • zadacha 2", verticals = TRUE,
     do.points = FALSE, lwd = 2)
dev.off()

##  Gistogramma s poligonom
hist2 <- hist(x2, breaks = bins2, plot = FALSE)
png("fig/hist_2.png", 800, 600)
h2 <- hist(x2, breaks = bins2, col = "#B2DF8A",
           border = "grey20", main = "Gistogramma • zadacha 2")
lines((bins2[-1] + bins2[-length(bins2)])/2, h2$counts,
      type = "b", pch = 19, lwd = 2)
dev.off()

## 2.1  vyborochnye kharakteristiki 
m2     <- mean(x2)
s2.2   <- sum((x2 - m2)^2) / (n2 - 1)
skew2  <- skewness(x2)
kurt2  <- kurtosis(x2) - 3
p.cd.2 <- mean(x2 >= c2 & x2 <= d2)

## 2.2  otsenki lambda 
lambda.hat.2  <- 1 / m2
bias.lambda.2 <- lambda.hat.2 / (n2 - 1)

## 2.3  doveritelnyi interval 
z.b <- qnorm(1 - alpha2 / 2)
ci.low.2  <- lambda.hat.2 - z.b * lambda.hat.2 / sqrt(n2)
ci.high.2 <- lambda.hat.2 + z.b * lambda.hat.2 / sqrt(n2)

## 2.4  Kolmogorov (prostaya) 
ks2 <- ks.test(x2, "pexp", rate = lambda0.2)

##  tablica (pervye 3 stroki)
xs2      <- sort(x2)
F.emp.u  <- (1:n2) / n2
F.emp.l  <- (0:(n2-1)) / n2
F.theor  <- pexp(xs2, rate = lambda0.2)
delta.l  <- abs(F.theor - F.emp.l)
delta.r  <- abs(F.theor - F.emp.u)
delta.m  <- pmax(delta.l, delta.r)
KS.head <- head(data.frame(
  i    = 1:n2,
  lw   = F.emp.l,
  np   = F.theor,
  nu   = F.emp.u,
  p    = delta.l,
  np_r = delta.r,
  res  = delta.m,
  res2 = delta.m^2
), 3)

# Находим и выводим максимальную строку
max_row_index <- which.max(delta.m)
max_row <- data.frame(
  i    = max_row_index,
  x_i  = xs2[max_row_index],
  F_minus = F.emp.l[max_row_index],
  F_plus = F.emp.u[max_row_index],
  F_0 = F.theor[max_row_index],
  Delta_minus = delta.l[max_row_index],
  Delta_plus = delta.r[max_row_index],
  max = delta.m[max_row_index]
)

cat("\n--- Maximalnaya stroka v kriterii Kolmogorova ------------\n")
cat("i | x_i | F- | F+ | F_0(x(i)) | Delta- | Delta+ | max\n")
cat("--------------------------------------------------------\n")
cat(sprintf("%d | %.2f | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f\n",
            max_row$i, max_row$x_i, max_row$F_minus, max_row$F_plus,
            max_row$F_0, max_row$Delta_minus, max_row$Delta_plus, max_row$max))

## 2.5  khi^2 (prostaya) 
prob.exp0 <- diff(pexp(bins2, rate = lambda0.2))
exp2      <- prob.exp0 * n2
obs2      <- hist2$counts

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
  nu   = obs2.c,
  p    = round(exp2.c / n2, 4),
  np   = exp2.c,
  res  = round((obs2.c - exp2.c) / sqrt(exp2.c), 3),
  res2 = round((obs2.c - exp2.c)^2 / exp2.c, 3)
)
chi2.head.2 <- head(chi2.table.2, 3)

## 2.6  khi^2 (slozhnaya) 
# H₀: X ~ Exp(λ), где λ - неизвестный параметр
# H₁: X не следует экспоненциальному распределению
#
# Математическая формулировка:
# 1. E[X] = 1/λ
# 2. Оценка параметра: λ̂ = 1/E[X] = n/∑X_i
# 3. Теоретические вероятности: p_i = F(b_i) - F(a_i), где F(x) = 1 - e^(-λ̂x)
# 4. Статистика критерия: χ² = ∑(n_i - np_i)²/(np_i)
# 5. Критическая область: χ² > χ²_{1-α}(k-2), где k - число интервалов
# 6. Максимальный уровень значимости: α_max = P(χ²_{k-2} > χ²_набл)
prob.exphat <- diff(pexp(bins2, rate = lambda.hat.2))  # Используем λ̂
tmp  <- combine(obs2, prob.exphat * n2)
obs2.comp <- tmp$obs
exp2.comp <- tmp$exp

# Проверочный вывод
cat("\n--- Proverka khi^2 (slozhnaya) ---\n")
cat("Nablyudaemye chastoty (n_i):\n")
print(obs2.comp)
cat("\nTeoreticheskie chastoty (np_i):\n")
print(round(exp2.comp, 4))
cat("\nVklad v khi^2:\n")
print(round((obs2.comp - exp2.comp)^2 / exp2.comp, 4))

chi2.comp.2 <- sum((obs2.comp - exp2.comp)^2 / exp2.comp)
p.comp.2    <- pchisq(chi2.comp.2, df = length(obs2.comp)-2,  # df = k-2, так как оцениваем параметр
                      lower.tail = FALSE)

# Находим максимальный уровень значимости
alpha_max <- p.comp.2
cat(sprintf("\nMaksimalnyi uroven znachimosti: α_max = %.5f\n", alpha_max))

chi2.table.2c <- data.frame(
  i    = 1:length(obs2.comp),
  lw   = head(bins2, -1),
  up   = tail(bins2, -1),
  nu   = obs2.comp,
  p    = round(exp2.comp / n2, 4),
  np   = exp2.comp,
  res  = round((obs2.comp - exp2.comp) / sqrt(exp2.comp), 3),
  res2 = round((obs2.comp - exp2.comp)^2 / exp2.comp, 3)
)

## 2.7  Neimana–Pirsona 
## 2.7  Критерий Неймана–Пирсона (исправленный)
alpha      <- 0.001
lambda0    <- 0.20
lambda1    <- 0.33
n          <- length(x2)

S.obs      <- sum(x2)
c.alpha    <- qgamma(alpha, shape = n, rate = lambda0)
phi        <- as.numeric(S.obs <= c.alpha)   # 1 – отвергнуть H0, 0 – нет
p.value    <- pgamma(S.obs, shape = n, rate = lambda0)

cat(sprintf("NP-φ(S) = %d   (S = %.2f,  c = %.5f)\n", phi, S.obs, c.alpha))
cat(sprintf("p-value = %.6f  →  %s H0\n",
            p.value, ifelse(phi, "отвергнуть", "сохранить")))


cat(sprintf("n = %d,  Sum x = %.2f\nx̄ = %.4f,  s^2 = %.6f,  med = %.3f\n",
            n2, sum(x2), m2, s2.2, median(x2)))
cat(sprintf("Skew = %.4f,  Kurt(ex) = %.4f,  P[%g,%g] = %.2f\n",
            skew2, kurt2, c2, d2, p.cd.2))
cat(sprintf("lambda^ = %.4f,  bias ~= %.5f,  CI_99.9%% = [%.4f; %.4f]\n",
            lambda.hat.2, bias.lambda.2, ci.low.2, ci.high.2))

cat("\n--- tablica Kolmogorova (pervye 3 stroki) ------------\n")
print(KS.head, row.names = FALSE)
cat(sprintf("D = %.4f,  p-value = %.5f\n",
            ks2$statistic, ks2$p.value))

# Добавляем сводную таблицу результатов критерия Колмогорова
cat("\n--- Svodnaya tablica kriteriya Kolmogorova -------------\n")
ks_summary <- data.frame(
  Parameter = c("D_n", "p-value", "λ₀", "α", "Решение"),
  Value = c(
    sprintf("%.4f", ks2$statistic),
    sprintf("%.5f", ks2$p.value),
    sprintf("%.2f", lambda0.2),
    sprintf("%.3f", alpha2),
    ifelse(ks2$p.value < alpha2, "Отвергнуть H₀", "Сохранить H₀")
  )
)
print(ks_summary, row.names = FALSE)

cat("\n--- khi^2 tablica (prostaya), pervye 3 stroki --------\n")
print(chi2.head.2, row.names = FALSE)
cat(sprintf("khi^2 = %.4f  (df = %d)  p = %.4f\n",
            chi2.simple.2, length(obs2.c)-1, p.simple.2))

cat(sprintf("\nkhi^2 (slozhnaya) = %.4f  (df = %d)  p = %.4f\n",
            chi2.comp.2, length(obs2.comp)-2, p.comp.2))
