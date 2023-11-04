bde = read.csv("big_dick_energy/erect_size_by_country.csv")

lm_bde = lm(erect_length~extraversion_excess, data = bde)
summary(lm_bde)
res_lm_bde = lm_bde$residuals

lm_bde_adj = lm(erect_length~extraversion+body_size, data = bde)
summary(lm_bde_adj)

