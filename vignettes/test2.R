par.start <- c(mu = 0, alpha = 0.02, beta = 0.85, gamma = 0.04,
               m = 0, theta = 1, w2 = 3)
par.start <- c(mu = 0, alpha = 0.02, beta = 0.85, gamma = 0.04,
               m = 0, theta = 0.1, w2 = 3)
par.start <- c(mu = 0, alpha = 0.02, beta = 0.85, gamma = 0.04,
               m = 0, theta = -1, w2 = 3)
p.e.nlminb <- constrOptim(theta = par.start, f = function(theta) { sum(lf(theta)) },
                          grad = NULL, ui = ui.opt, ci = ci.opt, hessian = FALSE,
                          mu = 1e-04, outer.iterations = 100, outer.eps = 1e-05,
                          method = "Nelder-Mead")

p.e.nlminb$par

fi = function(theta) { sum(lf(theta)) }
fi(par.start)
fi(p.e.nlminb$par)
fi(c(mu = 0.03009722,  alpha = 0.02204714,  beta = 0.91651324,  gamma = 0.10230564,
     m = 0.48092412,
     theta = -0.49957962,  w2 = 7.75213122))
fi(c(mu = 3.431724e-02,  alpha = 1.132619e-06,  beta = 8.356302e-01,  gamma = 1.924261e-01,
     m = -1.095590e+00,
     theta = 1.028189e+00,  w2 = 1.046099e+00))



ui.opt <- rbind(c(0, -1, -1, -1/2, 0, 0, 0),
                c(0,  0,  0,    0, 0, 0, 1),
                c(0,  1,  0,    0, 0, 0, 0),
                c(0,  0,  1,    0, 0, 0, 0),
                c(0,  0,  0,    0, 0, 1, 0))
ci.opt <- c(-0.99999999, 1, 0, 0,0)

ui.opt <- rbind(c(0, -1, -1, -1/2, 0, 0, 0),
                c(0,  0,  0,    0, 0, 0, 1),
                c(0,  1,  0,    0, 0, 0, 0),
                c(0,  0,  1,    0, 0, 0, 0))
ci.opt <- c(-0.99999999, 1, 0, 0)
