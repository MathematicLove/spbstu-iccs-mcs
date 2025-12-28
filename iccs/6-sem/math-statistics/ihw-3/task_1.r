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


## 1
x1 <- c(0,0,2,1,0,0,0,0,0,1,0,1,0,0,0,2,0,2,1,0,1,1,0,1,1,3,0,0,0,0,0,1,0,0,0,0,4,1,5,2,0,0,2,0,0,1,1,0,0,1)

n1        <- length(x1)
a1 <- 0;  b1 <- 1.79
lambda0.1 <- 0.60;  lambda1.1 <- 1.40
alpha1    <- 0.002

freq1 <- table(factor(x1, levels = 0:max(x1)))
df.freq1 <- data.frame(k = as.integer(names(freq1)),
                       n.k = as.integer(freq1),
                       rel = as.numeric(freq1) / n1)

##  ECDF
F.n1 <- ecdf(x1)
png("fig/emp_dist_1.png", 800, 600)
plot(F.n1, main = "ECDF • zadacha 1", verticals = TRUE,
     do.points = FALSE, lwd = 2)
dev.off()

##  Gistogramma
png("fig/hist_1.png", 800, 600)
hist(x1, breaks = seq(-0.5, max(x1) + 0.5, by = 1),
     col = "#FDBE85", border = "grey20",
     main = "Gistogramma • zadacha 1")
dev.off()

## 1.2  vyborochnye kharakteristiki 
m1      <- mean(x1)
s2.1    <- sum((x1 - m1)^2) / (n1 - 1)
skew1   <- skewness(x1)
kurt1   <- kurtosis(x1) - 3
p.ab.1  <- mean(x1 >= a1 & x1 <= b1)

## 1.3  otsenki lambda 
lambda.hat.1 <- m1              # MLE = MOM
bias.lambda.1 <- 0

## 1.4  doveritelnyi interval 
z.a <- qnorm(1 - alpha1 / 2)
ci.low.1  <- lambda.hat.1 - z.a * sqrt(lambda.hat.1 / n1)
ci.high.1 <- lambda.hat.1 + z.a * sqrt(lambda.hat.1 / n1)

## 1.5  khi^2 (prostaya) 
obs.simple <- c(sum(x1 == 0), sum(x1 == 1), sum(x1 >= 2))
exp.simple <- c(dpois(0, lambda0.1),
                dpois(1, lambda0.1),
                1 - dpois(0, lambda0.1) - dpois(1, lambda0.1)) * n1
chi2.simple <- sum((obs.simple - exp.simple)^2 / exp.simple)
p.simple.1  <- pchisq(chi2.simple, df = 2, lower.tail = FALSE)

chi2.table.1 <- data.frame(
  i   = 1:3,
  lw  = c("k=0", "k=1", "k>=2"),
  np  = round(exp.simple / n1, 4),
  nu  = obs.simple,
  p   = round(exp.simple / n1, 4),
  np_r= exp.simple,
  res = round((obs.simple - exp.simple) / sqrt(exp.simple), 3),
  res2= round((obs.simple - exp.simple)^2 / exp.simple, 3)
)

## 1.6  khi^2 (slozhnaya) 
# H₀: X ~ Pois(λ), где λ - неизвестный параметр
# H₁: X не следует распределению Пуассона
#
# Математическая формулировка:
# 1. E[X] = λ
# 2. Оценка параметра: λ̂ = E[X] = (1/n)∑X_i
# 3. Теоретические вероятности: p_i = (λ̂^k e^(-λ̂))/k!
# 4. Статистика критерия: χ² = ∑(n_i - np_i)²/(np_i)
# 5. Критическая область: χ² > χ²_{1-α}(k-2), где k - число интервалов
# 6. Максимальный уровень значимости: α_max = P(χ²_{k-2} > χ²_набл)

exp.comp <- dpois(0:5, lambda.hat.1) * n1
obs.comp <- c(freq1["0"], freq1["1"], freq1["2"],
              sum(freq1[c("3","4","5")]))
exp.comp <- c(exp.comp[1:3], sum(exp.comp[4:6]))

chi2.comp <- sum((obs.comp - exp.comp)^2 / exp.comp)
p.comp.1  <- pchisq(chi2.comp, df = length(obs.comp) - 2,  # df = k-2, так как оцениваем параметр
                    lower.tail = FALSE)

# Находим максимальный уровень значимости
alpha_max <- p.comp.1
cat(sprintf("\nMaksimalnyi uroven znachimosti: α_max = %.5f\n", alpha_max))

labels.comp <- c("k=0", "k=1", "k=2", "k>=3")[1:length(obs.comp)]

chi2.table.1c <- data.frame(
  i    = 1:length(obs.comp),
  lw   = labels.comp,
  np   = round(exp.comp / n1, 4),
  nu   = obs.comp,
  p    = round(exp.comp / n1, 4),
  np_r = exp.comp,
  res  = round((obs.comp - exp.comp) / sqrt(exp.comp), 3),
  res2 = round((obs.comp - exp.comp)^2 / exp.comp, 3)
)

## 1.7  Neimana–Pirsona 
S.1   <- sum(x1)
c.np.1 <- qpois(1 - alpha1, lambda0.1 * n1)
decision.np.1 <- ifelse(S.1 >= c.np.1,
                        "Otvergnyt H0", "Sokhranit H0")

cat(sprintf("n = %d,  Sum x = %d\nx̄ = %.4f,  s^2 = %.4f,  med = 0\n",
            n1, sum(x1), m1, s2.1))
cat(sprintf("Skew = %.4f,  Kurt(ex) = %.4f,  P[%g,%g] = %.2f\n",
            skew1, kurt1, a1, b1, p.ab.1))
cat(sprintf("lambda^ = %.4f (MLE=MOM),  CI_99.8%% = [%.4f; %.4f]\n",
            lambda.hat.1, ci.low.1, ci.high.1))

cat("\n--- khi^2 tablica (prostaya), pervye 3 stroki --------\n")
print(chi2.table.1, row.names = FALSE)
cat(sprintf("khi^2 = %.4f  (df = 2)  p = %.4f\n",
            chi2.simple, p.simple.1))

cat("\n--- khi^2 tablica (slozhnaya), pervye 3 stroki -------\n")
print(chi2.table.1c, row.names = FALSE)
cat(sprintf("khi^2 = %.4f  (df = 2)  p = %.4f\n",
            chi2.comp, p.comp.1))

cat(sprintf("\nNP-test:  S = %d,  c = %d  =>  %s\n",
            S.1, c.np.1, decision.np.1))

