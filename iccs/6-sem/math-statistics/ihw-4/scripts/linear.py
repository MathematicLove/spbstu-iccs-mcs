# ------------------------------------------------------------
#  Д В У Х Ф А К Т О Р Н Ы Й   A N O V A   (ВАШИ ДАННЫЕ)
#  — полная, аддитивная, отдельные факторы, нулевая модели —
# ------------------------------------------------------------
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf
from scipy import stats

# ----------  1.  ваши данные  --------------------------------
Y = [25.82,27.99,25.94,27.79,29.57,30.36,40.96,42.45,42.17,39.55,38.61,
     38.20,31.18,34.95,35.82,29.70,30.03,30.07,27.26,26.09,26.74,34.12,
     31.40,32.74,15.56,15.68,15.31,25.09,29.03,28.75,39.53,39.64,39.26,
     19.87,22.35,22.59,17.27,23.83,22.55,21.26,24.38,19.25,27.35,23.23,
     25.52,20.31,20.17,22.21]

A = [1,1,1,1,1,1,1,1,1, 1,1,1,2,2,2,2,2,
     2,2,2,2,2,2,2,3,3, 3,3,3,3,3,3,3,3,
     3,3,4,4,4,4,4,4,4, 4,4,4,4,4]

B = [1,1,1,2,2,2,3,3,3, 4,4,4,1,1,1,2,2,
     2,3,3,3,4,4,4,1,1, 1,2,2,2,3,3,3,4,
     4,4,1,1,1,2,2,2,3, 3,3,4,4,4]

df = pd.DataFrame({'Y':Y, 'A':pd.Categorical(A), 'B':pd.Categorical(B)})

# ----------  2.  модели  -------------------------------------
null   = smf.ols('Y ~ 1',          df).fit()
onlyA  = smf.ols('Y ~ C(A)',       df).fit()
onlyB  = smf.ols('Y ~ C(B)',       df).fit()
add    = smf.ols('Y ~ C(A)+C(B)',  df).fit()
full   = smf.ols('Y ~ C(A)*C(B)',  df).fit()       # A + B + A:B

# ----------  3.  резюме коэффициентов полной модели ----------
print('\n=== OLS-коэффициенты:  Y ~ C(A)*C(B) ===')
print(full.params.round(3).to_string())

# ----------  4.  несмещённая дисперсия ошибок ----------------
print(f'\nMSE (σ̂²) полной модели: {full.mse_resid:.3f}   df_E = {int(full.df_resid)}')

# ----------  5.  сводные средние ------------------------------
means = df.pivot_table(values='Y', index='A', columns='B', aggfunc='mean')
print('\nТаблица средних Y (строки-A, столбцы-B):')
print(means.round(2).to_string())

# ----------  6.  ANOVA-таблица (тип II) -----------------------
anova = sm.stats.anova_lm(full, typ=2)
print('\nANOVA (тип II):')
print(anova.round(2).to_string())

# ----------  7.  AIC / BIC всех пяти моделей -----------------
table = pd.DataFrame({
    'AIC':[null.aic, onlyA.aic, onlyB.aic, add.aic, full.aic],
    'BIC':[null.bic, onlyA.bic, onlyB.bic, add.bic, full.bic],
    'RSS':[null.ssr, onlyA.ssr, onlyB.ssr, add.ssr, full.ssr],
    'k'  :[null.df_model+1, onlyA.df_model+1, onlyB.df_model+1,
           add.df_model+1 , full.df_model+1]},
    index=['µ', 'A', 'B', 'A+B', 'A*B'])
print('\nAIC / BIC / RSS:')
print(table.round(2).sort_values('AIC'))

# ----------  8.  проверка нормальности остатков ---------------
w,p_sw = stats.shapiro(full.resid)
jb,p_jb = stats.jarque_bera(full.resid)
print(f'\nShapiro–Wilk: W={w:.3f},  p={p_sw:.3f}')
print(f'Jarque–Bera:  JB={jb:.2f}, p={p_jb:.3f}')

# ----------  9.  interaction-plot -----------------------------
plt.figure(figsize=(6,4))
for b_lbl, g in df.groupby('B'):
    plt.plot(g.groupby('A')['Y'].mean().index.astype(int),
             g.groupby('A')['Y'].mean().values,
             marker='o', label=f'B={b_lbl}')
plt.xlabel('Уровень A'); plt.ylabel('Средний Y')
plt.title('Профильные графики взаимодействия A × B')
plt.legend(); plt.tight_layout()
plt.show()
