# 1 - Libraries ----
library(insee)
library(tseries)
library(ggplot2)
library(broom)

# 2 - Import of data ----
data <- as.data.frame(insee::get_insee_idbank("010767815"))
data[,"DATE"] <- as.Date(paste(data[,"TIME_PERIOD"], "-01", sep=""))
data <- data[, c("DATE", "OBS_VALUE")]

data <- data[order(data[, "DATE"]),]

# 3 - Stationnarity tests for basic series -----
tseries::adf.test(data[, "OBS_VALUE"]) # p-value = 0.7, stationnaire
tseries::kpss.test(data[, "OBS_VALUE"]) # p-value = 0.01, pas stationnaire
tseries::pp.test(data[, "OBS_VALUE"]) # p-value = 0.01, stationnaire

data <- data[data$DATE < as.Date("2020-01-01"),]
# 4 - Stationnarity for the differentiated series ----
data[, "diff1"] <- c(NA, diff(data[, "OBS_VALUE"]))

tseries::adf.test(na.omit(data[, "diff1"])) # p-value = 0.01, stationnaire
tseries::kpss.test(na.omit(data[, "diff1"])) # p-value = 0.1, limite stationnaire
tseries::pp.test(na.omit(data[, "diff1"])) # p-value = 0.01, stationnaire


# 5 - Visualization -----

#Before treating the series
ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
  geom_line(color = 'turquoise') + 
  
  labs(title = "Before transformation - index of manufacture of perfumes and toiletries, France",
       x = "Date",
       y = "Index Manufacture of perfumes and toiletries") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  theme_minimal() + 
  theme(plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size=7, angle=90), 
        axis.title.y = element_text(size = 8))

# After 1-st order differentiation 

ggplot(data, aes(x = DATE, y = diff1)) +
  geom_line(color = 'orange') + 
  
  labs(title = "After transformation - index of manufacture of perfumes and toiletries, France",
       x = "Date",
       y = "Index of manufacture of perfumes and toiletries") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  theme_minimal() + 
  theme(plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size=7, angle=90), 
        axis.title.y = element_text(size = 8))


#6 - Picking ARMA 
parfum = na.omit(data[, "diff1"])
parfum <- parfum - mean(parfum)

acf(parfum) #q = 3
pacf(parfum) # p = 3

arima303 = arima(parfum, c(3,0,3))
arima303
plot(arima303$residuals, col='darkblue', xlab = 'Period', ylab='Residuals', main = 'ARMA(3,3) residuals plot')


Box.test(arima303$residuals, lag=7, type = "Ljung-Box")
Qtests(arima303$residuals, 24)

#### fonction
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}

signif <- function(estim){ #fonction de test des significations individuelles des coefficients
  coef <- estim$coef
  se <- sqrt(diag(estim$var.coef))
  t <- coef/se
  pval <- (1-pnorm(abs(t)))*2
  return(rbind(coef,se,pval))
}


##
arimafit <- function(estim){
  adjust <- round(signif(estim),3)
  pvals <- Qtests(estim$residuals,24,length(estim$coef)-1)
  pvals <- matrix(apply(matrix(1:24,nrow=6),2,function(c) round(pvals[c,],3)),nrow=6)
  colnames(pvals) <- rep(c("lag", "pval"),4)
  cat("tests de nullite des coefficients :\n")
  print(adjust)
  cat("\n tests d'absence d'autocorrelation des residus : \n")
  print(pvals)
}

modelchoice <- function(p,q,data1=parfum, k=24){
  estim <- try(arima(data1, c(p,0,q),optim.control=list(maxit=20000)))
  if (class(estim)=="try-error") return(c("p"=p,"q"=q,"arsignif"=NA,"masignif"=NA,"resnocorr"=NA, "ok"=NA))
  arsignif <- if (p==0) NA else signif(estim)[3,p]<=0.05
  masignif <- if (q==0) NA else signif(estim)[3,p+q]<=0.05
  resnocorr <- sum(Qtests(estim$residuals,24,length(estim$coef)-1)[,2]<=0.05,na.rm=T)==0
  checks <- c(arsignif,masignif,resnocorr)
  ok <- as.numeric(sum(checks,na.rm=T)==(3-sum(is.na(checks))))
  return(c("p"=p,"q"=q,"arsignif"=arsignif,"masignif"=masignif,"resnocorr"=resnocorr,"ok"=ok))
}

armamodelchoice <- function(pmax,qmax){
  pqs <- expand.grid(0:pmax,0:qmax)
  t(apply(matrix(1:dim(pqs)[1]),1,function(row) {
    p <- pqs[row,1]; q <- pqs[row,2]
    cat(paste0("Computing ARMA(",p,",",q,") \n"))
    modelchoice(p,q)
  }))
}

armamodels <- armamodelchoice(3,3) #estimation de tous les arima possibles 

arima300 = arima(parfum, c(3,0,0))#3, 0 ; 2 1 ; 0 3
arima201 = arima(parfum, c(2,0,1))
arima003 = arima(parfum, c(0,0,3))

arima003

AIC(arima300)
AIC(arima201)
AIC(arima003)
BIC(arima300)
BIC(arima201)
BIC(arima003)

#parfum_nondiff <- data[, "OBS_VALUE"]
#arima013 <- arima(parfum_nondiff, c(0,1,3))
#arima310 = arima(parfum_nondiff, c(3,1,0))#3, 0 ; 2 1 ; 0 3
#arima211 = arima(parfum_nondiff, c(2,1,1))

#AIC(arima310)
#AIC(arima211)
#AIC(arima013)
#BIC(arima310)
#BIC(arima211)
#BIC(arima013)
