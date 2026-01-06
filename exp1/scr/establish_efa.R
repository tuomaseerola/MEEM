par <- fa.parallel(MEAN_cm, fa="fa",cor="cor",fm="minres",n.obs = 100) # 3

KMO(MEAN_cm) # 0.96

fac <- principal(MEAN_cm,nfactors = 3,n.obs = 100)
print(fac$Structure,digits = 3,cut = 0.3,short = TRUE)

print(fac)
