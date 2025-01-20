

model.int.ec.alt <- lmer(sqrt(F_avg) ~ BA*Plot*Year + soil_ec_avg + (1|ID:Year),
                     data = tsb_2)
emmeans(model.int.ec.alt, specs = pairwise ~ BA*Plot|Year)
