import os, numpy as np, pandas as pd, matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf
from   scipy import stats

Y1 = [ 9.61,9.22,4.76,4.37,14.20,11.21,13.98,10.68,7.97,8.20,
       11.22,6.38,8.57,7.99,10.97,7.72,8.83,8.34,9.61,10.58,
       6.83,10.92,7.90,8.89,7.85,8.19,8.23,6.92,8.46,8.00,
       11.93,8.68,8.41,8.02,7.30,17.78,9.43,7.17,5.79,8.27,
       9.42,8.58,11.67,6.86,7.44,8.56,10.03,16.13,5.89,12.99]

X1 = [ 2,4,2,2,1,1,2,2,3,1,1,2,2,2,1,3,1,1,2,2,
       3,2,1,2,2,1,3,2,1,0,2,3,4,2,3,0,3,0,2,3,
       2,2,2,2,3,2,3,2,3,2]

Y2 = [25.82,27.99,25.94,27.79,29.57,30.36,40.96,42.45,42.17,39.55,38.61,
       38.20,31.18,34.95,35.82,29.70,30.03,30.07,27.26,26.09,26.74,34.12,
       31.40,32.74,15.56,15.68,15.31,25.09,29.03,28.75,39.53,39.64,39.26,
       19.87,22.35,22.59,17.27,23.83,22.55,21.26,24.38,19.25,27.35,23.23,
       25.52,20.31,20.17,22.21]

A2 = [1,1,1,1,1,1,1,1,1, 1,1,1,2,2,2,2,2,
      2,2,2,2,2,2,2,3,3, 3,3,3,3,3,3,3,3,
      3,3,4,4,4,4,4,4,4, 4,4,4,4,4]           

B2 = [1,1,1,2,2,2,3,3,3, 4,4,4,1,1,1,2,2,
      2,3,3,3,4,4,4,1,1, 1,2,2,2,3,3,3,4,
      4,4,1,1,1,2,2,2,3, 3,3,4,4,4]          

df1          = pd.DataFrame({'Y':Y1,'X':X1})
df1['X2']    = df1['X']**2
lin          = sm.OLS(df1.Y, sm.add_constant(df1[['X']])).fit()
poly         = sm.OLS(df1.Y, sm.add_constant(df1[['X','X2']])).fit()

Sxx = ((df1.X-df1.X.mean())**2).sum()
Sxy = ((df1.X-df1.X.mean())*(df1.Y-df1.Y.mean())).sum()

rss_lin, rss_poly = lin.ssr, poly.ssr
df_lin , df_poly  = lin.df_resid, poly.df_resid
F_lin = ((rss_lin-rss_poly)/(df_lin-df_poly))/(rss_poly/df_poly)
p_lin = 1-stats.f.cdf(F_lin, df_lin-df_poly, df_poly)
t_indep, p_indep = lin.tvalues['X'], lin.pvalues['X']

# Calculate global hypothesis for linearity and independence
F_global = ((rss_lin - rss_poly)/(df_lin - df_poly)) / (rss_poly/df_poly)
p_global = 1-stats.f.cdf(F_global, df_lin-df_poly, df_poly)

alpha = .01
tcrit = stats.t.ppf(1-alpha/2, df_poly)
ci_b2 = poly.params['X']  + np.array([-1,1])*tcrit*poly.bse['X']
ci_b3 = poly.params['X2'] + np.array([-1,1])*tcrit*poly.bse['X2']
Fcrit = stats.f.ppf(1-alpha, 2, df_poly)
ell99 = 2*poly.mse_resid*Fcrit

# 3.  З А Д А Ч А 2  —  двухфакторный ANOVA
df2   = pd.DataFrame({'Y':Y2,'A':A2,'B':B2})
null  = smf.ols('Y ~ 1', df2).fit()  # Нулевая модель
full  = smf.ols('Y ~ C(A)*C(B)', df2).fit()
add   = smf.ols('Y ~ C(A)+C(B)', df2).fit()
onlyA = smf.ols('Y ~ C(A)',       df2).fit()
onlyB = smf.ols('Y ~ C(B)',       df2).fit()

F_int = ((add.ssr-full.ssr)/(add.df_resid-full.df_resid)) / (full.ssr/full.df_resid)
p_int = 1-stats.f.cdf(F_int, add.df_resid-full.df_resid, full.df_resid)

print("\n==========  ЗАДАЧА 2 ==========")
print("\nСравнение моделей по AIC и BIC:")
print(f"Null (Y~1):")
print(f"  AIC = {null.aic:.2f}")
print(f"  BIC = {null.bic:.2f}")

print(f"\nFull (Y~A*B):")
print(f"  AIC = {full.aic:.2f}")
print(f"  BIC = {full.bic:.2f}")

print(f"\nAdditive (Y~A+B):")
print(f"  AIC = {add.aic:.2f}")
print(f"  BIC = {add.bic:.2f}")

print(f"\nOnly A (Y~A):")
print(f"  AIC = {onlyA.aic:.2f}")
print(f"  BIC = {onlyA.bic:.2f}")

print(f"\nOnly B (Y~B):")
print(f"  AIC = {onlyB.aic:.2f}")
print(f"  BIC = {onlyB.bic:.2f}")

# Calculate differences from null model
print("\nРазница с нулевой моделью:")
print(f"Full (Y~A*B):")
print(f"  ΔAIC = {null.aic - full.aic:.2f}")
print(f"  ΔBIC = {null.bic - full.bic:.2f}")

print(f"\nAdditive (Y~A+B):")
print(f"  ΔAIC = {null.aic - add.aic:.2f}")
print(f"  ΔBIC = {null.bic - add.bic:.2f}")

print(f"\nOnly A (Y~A):")
print(f"  ΔAIC = {null.aic - onlyA.aic:.2f}")
print(f"  ΔBIC = {null.bic - onlyA.bic:.2f}")

print(f"\nOnly B (Y~B):")
print(f"  ΔAIC = {null.aic - onlyB.aic:.2f}")
print(f"  ΔBIC = {null.bic - onlyB.bic:.2f}")

# Calculate F-statistics for model comparisons
F_full_vs_null = ((null.ssr - full.ssr)/(null.df_resid - full.df_resid)) / (full.ssr/full.df_resid)
F_add_vs_null = ((null.ssr - add.ssr)/(null.df_resid - add.df_resid)) / (add.ssr/add.df_resid)
F_A_vs_null = ((null.ssr - onlyA.ssr)/(null.df_resid - onlyA.df_resid)) / (onlyA.ssr/onlyA.df_resid)

print("\nF-статистики для сравнения с нулевой моделью:")
print(f"Full vs Null: F = {F_full_vs_null:.2f}")
print(f"Additive vs Null: F = {F_add_vs_null:.2f}")
print(f"Only A vs Null: F = {F_A_vs_null:.2f}")

print(f"F(interaction) = {F_int:.2f},  p = {p_int:.3g}")

def p(name): return os.path.join('figs', name)

os.makedirs('figs', exist_ok=True)

def resid_hist(model, fname, title):
    r = model.resid
    mu, sig = r.mean(), r.std(ddof=model.df_model+1)
    x = np.linspace(r.min()-1, r.max()+1, 300)
    plt.figure()
    plt.hist(r, bins='auto', density=True,
             edgecolor='k', alpha=.75, label='гистограмма')
    plt.plot(x, stats.norm.pdf(x, mu, sig),
             lw=2, label='N(μ̂,σ̂²)')
    plt.xlabel('Residual'); plt.ylabel('Density')
    plt.title(title); plt.legend(); plt.tight_layout()
    plt.savefig(p(fname), dpi=300); plt.close()


xx = np.linspace(df1.X.min(), df1.X.max(), 200)

plt.scatter(df1.X, df1.Y, s=36, alpha=.8)
plt.xlabel('X'); plt.ylabel('Y'); plt.title('Только точки')
plt.tight_layout(); plt.savefig(p('task1_scatter.png'), dpi=300); plt.close()

plt.scatter(df1.X, df1.Y, s=36, alpha=.8)
plt.plot(xx, lin.predict(sm.add_constant(xx)), lw=2, label='Linear')
plt.xlabel('X'); plt.ylabel('Y'); plt.legend()
plt.title('Точки + линейная регрессия')
plt.tight_layout(); plt.savefig(p('task1_linear.png'), dpi=300); plt.close()

plt.scatter(df1.X, df1.Y, s=36, alpha=.8)
plt.plot(xx, lin.predict(sm.add_constant(xx)), lw=2, label='Linear')
plt.plot(xx, poly.predict(sm.add_constant(np.c_[xx,xx**2])),
         lw=2, ls='--', label='Quadratic')
plt.xlabel('X'); plt.ylabel('Y'); plt.legend()
plt.title('Точки + линейная + квадратичная')
plt.tight_layout(); plt.savefig(p('task1_lin_quad.png'), dpi=300); plt.close()

resid_hist(lin , 'task1_resid_hist_lin.png',  'Линейная модель: остатки')
resid_hist(poly, 'task1_resid_hist_poly.png', 'Квадратичная модель: остатки')


resid_hist(full, 'task2_resid_hist_full.png', 'FULL  A*B: остатки')
resid_hist(add , 'task2_resid_hist_add.png',  'Additive A+B: остатки')
# -------------------------------------------------------------
# 6-бис.  Interaction-plot  A × B   (средние при фикс. B)
# -------------------------------------------------------------
plt.figure()
for b, g in df2.groupby('B'):
    means = g.groupby('A')['Y'].mean().sort_index()
    plt.plot(means.index, means.values, marker='o', label=f'B={b}')
plt.xlabel('Уровень A'); plt.ylabel('Средний Y')
plt.title('Профильный график взаимодействия  A×B')
plt.legend(); plt.tight_layout()
plt.savefig(p('task2_interaction.png'), dpi=300); plt.close()



#  Рисунок: 99-% доверительный эллипсоид для (β₂,β₃)
# ---------------------------------------------------------------
from matplotlib.patches import Ellipse

# ковариационная матрица оценок (2×2)
Sigma = poly.cov_params().loc[['X','X2'],['X','X2']]
sigma2 = poly.mse_resid
Sigma = Sigma.values * sigma2               # σ̂²·(X'X)⁻¹

c  = 2 * stats.f.ppf(1-alpha, 2, df_poly)   # эллипсоидное «F»-масштабирование
vals, vecs = np.linalg.eigh(Sigma)          # собственные значения/векторы
ang = np.degrees(np.arctan2(*vecs[:,1][::-1]))
width, height = 2*np.sqrt(c*vals)           # диаметры по осям

fig, ax = plt.subplots()
ax.scatter(poly.params['X'], poly.params['X2'], color='red', label=r'$\hat\beta$')
ell = Ellipse(xy=(poly.params['X'], poly.params['X2']),
              width=width, height=height, angle=ang,
              edgecolor='blue', facecolor='none', lw=2)
ax.add_patch(ell)
ax.set_xlabel(r'$\beta_2$'); ax.set_ylabel(r'$\beta_3$')
ax.set_title('99\\% доверительный эллипсоид для $(\\beta_2,\\beta_3)$')
ax.legend(); ax.grid(True); plt.tight_layout()
plt.savefig(p('task1_conf_ellipse.png'), dpi=300); plt.close()

# -------------------------------------------------------------
# 7.  К О Н С О Л Ь  — вывод ключевых расчётов
# -------------------------------------------------------------
print("\n==========  ЗАДАЧА 1 ==========")
print(f"Sxx = {Sxx:.2f},  Sxy = {Sxy:.2f}")
print(f"β̂_lin  = {lin.params.values.round(3)}")
print(f"β̂_poly = {poly.params.values.round(3)}")
print(f"R²_lin={lin.rsquared:.3f},  R²_poly={poly.rsquared:.3f}")
print(f"AIC lin / poly = {lin.aic:.2f} / {poly.aic:.2f}")
print(f"BIC lin / poly = {lin.bic:.2f} / {poly.bic:.2f}")
print(f"F(β₃=0) = {F_lin:.3f},  p = {p_lin:.3f}")
print(f"t(β₂=0) = {t_indep:.3f}, p = {p_indep:.3f}")
print(f"F(глобальная гипотеза) = {F_global:.3f}, p = {p_global:.3f}")
print(f"CI99%(β₂)=({ci_b2[0]:.2f}; {ci_b2[1]:.2f})  "
      f"CI99%(β₃)=({ci_b3[0]:.2f}; {ci_b3[1]:.2f})")
print(f"Эллипсоид 99 %: (β-β̂)'X'X(β-β̂) ≤ {ell99:.2f}")

print("\n==========  ЗАДАЧА 2 ==========")
print(f"F(interaction) = {F_int:.2f},  p = {p_int:.3g}")
print(f"AIC full / add / A = {full.aic:.1f} / {add.aic:.1f} / {onlyA.aic:.1f}")
print(f"BIC full / add / A = {full.bic:.1f} / {add.bic:.1f} / {onlyA.bic:.1f}")

# Print factor levels
print("\nУровни факторов:")
print("Фактор A:", sorted(df2['A'].unique()))
print("Фактор B:", sorted(df2['B'].unique()))

# Print all possible combinations
print("\nВсе возможные комбинации факторов:")
combinations = df2[['A', 'B']].drop_duplicates().sort_values(['A', 'B'])
print(combinations.to_string(index=False))

# Print number of observations for each combination
print("\nКоличество наблюдений для каждой комбинации:")
obs_counts = df2.groupby(['A', 'B']).size().unstack(fill_value=0)
print(obs_counts)

# Calculate sigma squared
sigma2 = full.mse_resid
print(f"\nσ̂² = {sigma2:.4f}")

# Calculate mean values table
print("\nТаблица средних значений Y:")
means = df2.groupby(['A', 'B'])['Y'].mean().unstack()
print(means.round(2))

# Calculate ANOVA table
print("\nТаблица ANOVA:")
# Total sum of squares
TSS = ((df2['Y'] - df2['Y'].mean())**2).sum()
# Sum of squares for A
SSA = sum(len(g) * (g['Y'].mean() - df2['Y'].mean())**2 for _, g in df2.groupby('A'))
# Sum of squares for B
SSB = sum(len(g) * (g['Y'].mean() - df2['Y'].mean())**2 for _, g in df2.groupby('B'))
# Sum of squares for interaction
SSAB = full.ssr - add.ssr
# Residual sum of squares
SSE = full.ssr

# Degrees of freedom
dfA = len(df2['A'].unique()) - 1
dfB = len(df2['B'].unique()) - 1
dfAB = dfA * dfB
dfE = full.df_resid

# Mean squares
MSA = SSA / dfA
MSB = SSB / dfB
MSAB = SSAB / dfAB
MSE = SSE / dfE

# F-statistics
FA = MSA / MSE
FB = MSB / MSE
FAB = MSAB / MSE

# p-values
pA = 1 - stats.f.cdf(FA, dfA, dfE)
pB = 1 - stats.f.cdf(FB, dfB, dfE)
pAB = 1 - stats.f.cdf(FAB, dfAB, dfE)

print("\nИсточник вариации | Степени свободы | Сумма квадратов | Средний квадрат | F-статистика | p-value")
print("-" * 100)
print(f"A                | {dfA:14d} | {SSA:14.2f} | {MSA:14.2f} | {FA:12.2f} | {pA:.4f}")
print(f"B                | {dfB:14d} | {SSB:14.2f} | {MSB:14.2f} | {FB:12.2f} | {pB:.4f}")
print(f"A*B              | {dfAB:14d} | {SSAB:14.2f} | {MSAB:14.2f} | {FAB:12.2f} | {pAB:.4f}")
print(f"Ошибка           | {dfE:14d} | {SSE:14.2f} | {MSE:14.2f} |")
print(f"Всего            | {dfA+dfB+dfAB+dfE:14d} | {TSS:14.2f} |")

# Calculate influence percentages
print("\nВлияние факторов:")
print(f"A:    {SSA/TSS*100:.2f}%")
print(f"B:    {SSB/TSS*100:.2f}%")
print(f"A+B:  {(SSA+SSB)/TSS*100:.2f}%")
print(f"A*B:  {SSAB/TSS*100:.2f}%")