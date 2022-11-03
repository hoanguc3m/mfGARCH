model = fit_mfgarch(data = df_mfgarch, y = "return", x = "RVm", low.freq = "year_month", K = i,
                    multi.start = FALSE,
                    control = list(par.start = c(mu = 0, alpha = 0.02, beta = 0.85,
                                                 gamma = 0.04, m = 0, theta = 1, w2 = 3) ))
c(model$K,model$broom.mgarch$estimate[6],model$broom.mgarch$p.value[6],
  model$broom.mgarch$estimate[7],model$broom.mgarch$p.value[7],
  model$llh/tmp,tmp)
model = fit_mfgarch(data = df_mfgarch, y = "return", x = "RVm", low.freq = "year_month", K = i,
                    multi.start = TRUE)
model$broom.mgarch$term
model$broom.mgarch$estimate

model = fit_mfgarch(data = df_mfgarch, y = "return", x = "RVm", low.freq = "year_month", K = 12,
                    multi.start = FALSE,
                    control = list(par.start = c(mu = 0, alpha = 0.02, beta = 0.85,
                                                 gamma = 0.04, m = 0, theta = 1, w2 = 3) ))
model$broom.mgarch$term
model$broom.mgarch$estimate

df_short <- df_mfgarch[c(1:11938),]
data = df_short
model = smfgarch(data = df_short, y = "return", x = "RVm", low.freq = "year_month", K = 6,
                     multi.start = FALSE,
                     control = list(par.start = c(mu = 0, alpha = 0.02, beta = 0.85,
                                                  gamma = 0.04, m = 0, theta = 1, w2 = 3) ))


par.start = c(mu = 0, alpha = 0.02, beta = 0.85,
              gamma = 0.04, m = 0, theta = -1, w2 = 3)

model = smfgarch(data = df_short, y = "return", x = "RVm",
                 low.freq = "year_month", K = 6, positive =  TRUE)
