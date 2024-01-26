
workforce = read.csv("dentist_oral_health/iran_dentists_workforce.csv")

dmft = openxlsx::read.xlsx("dentist_oral_health/iran_oral_health.xlsx")

poverty = read.csv("dentist_oral_health/iran_poverty_index.csv")

dmft = dplyr::filter(dmft, year == 2017 & age_cat == "agestd")

combined = merge(workforce, dmft, by.x = "province", by.y = "location")
combined = merge(combined, poverty, by = "province")

both_dmft = dplyr::filter(combined, sex == "Both" & index == "DMFT")

both_d = dplyr::filter(combined, sex == "Both" & index == "D")

both_f = dplyr::filter(combined, sex == "Both" & index == "F")

both_m = dplyr::filter(combined, sex == "Both" & index == "M")

# Regression

# unadjusted

lm_dmft = lm(value~Ratio.of.general.dentist.to.100000.population.of.province, data = both_dmft)
summary(lm_dmft)

lm_d = lm(value~Ratio.of.general.dentist.to.100000.population.of.province, data = both_d)
summary(lm_d)

lm_m = lm(value~Ratio.of.general.dentist.to.100000.population.of.province, data = both_m)
summary(lm_m)

lm_f = lm(value~Ratio.of.general.dentist.to.100000.population.of.province, data = both_f)
summary(lm_f)


# adjusted
lm_dmft_adj = lm(value~Ratio.of.general.dentist.to.100000.population.of.province+poverty_index_2020, data = both_dmft)
summary(lm_dmft_adj)

lm_d_adj = lm(value~Ratio.of.general.dentist.to.100000.population.of.province+poverty_index_2020, data = both_d)
summary(lm_d_adj)

lm_m_adj = lm(value~Ratio.of.general.dentist.to.100000.population.of.province+poverty_index_2020, data = both_m)
summary(lm_m_adj)

lm_f_adj = lm(value~Ratio.of.general.dentist.to.100000.population.of.province+poverty_index_2020, data = both_f)
summary(lm_f_adj)


