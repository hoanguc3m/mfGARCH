rm(list=ls())
library(mfGARCH)
library(lubridate)
library(xtable)

head(df_mfgarch)


## Add monthly RV measure

date  = as.Date(df_mfgarch$date)
datem = format(date,"%Y-%m")
datemind = unique(datem)

# calculate RV based on current month's log-returns
# no need to lag! the model lags it
RVm       = RVroll = RVavrg = rep(NA,length(df_mfgarch$return))
for(i in 1:length(datemind)){
    RVm[which(datem==datemind[i])] =
        sd(df_mfgarch$return[which(datem==datemind[i])],na.rm = TRUE)
    RVavrg[which(datem==datemind[i])]= mean(df_mfgarch$rv[which(datem==datemind[i])],na.rm = TRUE)
}
# calculate RVroll based on last 22 log-returns
for(i in 1:length(df_mfgarch$return)){
    RVroll[i] = sd(df_mfgarch$return[max(c(1,i-21)):(i)],na.rm = TRUE)
}

plot(date,df_mfgarch$rv,type='l',lwd=2)
lines(date,RVm^2,col=2)
lines(date,RVroll^2,col=4)
lines(date,RVavrg,col=3)

df_mfgarch$RVm=RVm
df_mfgarch$RVm2=RVm^2
df_mfgarch$RVroll=RVroll
df_mfgarch$RVavrg=RVavrg


cor(cbind(df_mfgarch$RVm^2,df_mfgarch$RVroll^2,df_mfgarch$RVavrg,df_mfgarch$rv),use="pairwise.complete.obs")
lags = c(3,6,12,24,36)

info = NULL
for(i in lags){
    model = fit_mfgarch(data = df_mfgarch, y = "return", x = "RVm", low.freq = "year_month", K = i)
    tmp = sum(!is.na(model$df.fitted$residuals))

    info = rbind(info,c(model$K,model$broom.mgarch$estimate[6],model$broom.mgarch$p.value[6],
                        model$broom.mgarch$estimate[7],model$broom.mgarch$p.value[7],
                        model$llh/tmp,tmp))
    }

for(i in lags){
    model = fit_mfgarch(data = df_mfgarch, y = "return", x = "RVm2", low.freq = "year_month", K = i)
    tmp = sum(!is.na(model$df.fitted$residuals))

    info = rbind(info,c(model$K,model$broom.mgarch$estimate[6],model$broom.mgarch$p.value[6],
                        model$broom.mgarch$estimate[7],model$broom.mgarch$p.value[7],
                        model$llh/tmp,tmp))
}


colnames(info) = c('K','theta','pvalue','w2','pvalue','llh/n','n')

print(xtable(info,digits =4 ,caption = 'sp500 data: RVmonthly and RVmonthly$^2$ as explanatory variables.',label='table:sp500'),
      file='Exploratory/sp500.tex',scalebox=0.8,include.colnames = TRUE,
      include.rownames = FALSE,  hline.after =c(-1,0,5,10))


load('Exploratory/mydf.Rdata')
mydf$RV2 = (mydf$RV)^2
mydfs = na.omit(mydf[,c("date_m","date","lret","RV","RV2")])

info = NULL
lags = c(3,6,12,24,36)

for(i in lags){
    model = fit_mfgarch(data = mydfs, y = "lret", x = "RV", low.freq = "date_m", K = i)
    tmp = sum(!is.na(model$df.fitted$residuals))

    info = rbind(info,c(model$K,model$broom.mgarch$estimate[6],model$broom.mgarch$p.value[6],
                        model$broom.mgarch$estimate[7],model$broom.mgarch$p.value[7],
                        model$llh/tmp,tmp))
    }

for(i in lags){
    model = fit_mfgarch(data = mydfs, y = "lret", x = "RV2", low.freq = "date_m", K = i)
    tmp = sum(!is.na(model$df.fitted$residuals))

    info = rbind(info,c(model$K,model$broom.mgarch$estimate[6],model$broom.mgarch$p.value[6],
                        model$broom.mgarch$estimate[7],model$broom.mgarch$p.value[7],
                        model$llh/tmp,tmp))
    }


colnames(info) = c('K','theta','pvalue','w2','pvalue','llh/n','n')

print(xtable(info,digits =4 ,caption = 'wti data: RVmonthly and RVmonthly$^2$ as explanatory variables.',label='table:sp500'),
      file='Exploratory/wti_expl.tex',scalebox=0.8,include.colnames = TRUE,
      include.rownames = FALSE,  hline.after =c(-1,0,5,10))


