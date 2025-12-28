# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

## ваши массивы Y2, A2, B2
y <- c(25.82,27.99,25.94,27.79,29.57,30.36,40.96,42.45,42.17,39.55,38.61,
       38.20,31.18,34.95,35.82,29.70,30.03,30.07,27.26,26.09,26.74,34.12,
       31.40,32.74,15.56,15.68,15.31,25.09,29.03,28.75,39.53,39.64,39.26,
       19.87,22.35,22.59,17.27,23.83,22.55,21.26,24.38,19.25,27.35,23.23,
       25.52,20.31,20.17,22.21)

A <- factor(c(1,1,1,1,1,1,1,1,1, 1,1,1,2,2,2,2,2,
              2,2,2,2,2,2,2,3,3, 3,3,3,3,3,3,3,3,
              3,3,4,4,4,4,4,4,4, 4,4,4,4,4))
B <- factor(c(1,1,1,2,2,2,3,3,3, 4,4,4,1,1,1,2,2,
              2,3,3,3,4,4,4,1,1, 1,2,2,2,3,3,3,4,
              4,4,1,1,1,2,2,2,3, 3,3,4,4,4))

## полная и упрощённые модели
mod_full <- lm(y ~ A*B)
mod_add  <- lm(y ~ A + B)
mod_A    <- lm(y ~ A)
mod_B    <- lm(y ~ B)
mod_null <- lm(y ~ 1)

# подключите пакет 'moments' перед вызовом jarque.test()
if (!requireNamespace("moments", quietly = TRUE))
    install.packages("moments")
library(moments)

jb <- jarque.test(resid(mod_full))
print(jb)

## --- МНК-оценки полной модели ---
coef(mod_full)                # все β, включая Intercept
sigma2 <- summary(mod_full)$sigma^2  # несмещённая σ²
dfE    <- df.residual(mod_full)      # 32

## --- средние Y по ячейкам (таблица) ---
means <- with(data.frame(A,B,y), tapply(y, list(A, B), mean))
print(round(means, 2))

## --- ANOVA-таблица ---
print(anova(mod_full))        # SS, MS, F, p для A, B, A:B

## --- AIC / BIC ---
aic <- c(AxB=AIC(mod_full), AplusB=AIC(mod_add),
         A=AIC(mod_A), B=AIC(mod_B), Null=AIC(mod_null))
bic <- c(AxB=BIC(mod_full), AplusB=BIC(mod_add),
         A=BIC(mod_A), B=BIC(mod_B), Null=BIC(mod_null))
knitr::kable(cbind(AIC=aic, BIC=bic), digits=2)

## --- диагн. нормальности остатков ---
shapiro.test(resid(mod_full))
jarque.test(resid(mod_full))          # из пакета «moments»
