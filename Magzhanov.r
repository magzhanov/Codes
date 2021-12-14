####################
#    Magzhanov     #
#    12/12/2021    #
#       HW2        #
####################

##### PACKAGES #####

{
install.packages("splm")
install.packages("plm")
install.packages("texreg")
install.packages("vtable")

}

##### LIBRARIES #####

{
library(plm)
library(splm)
library(car)
library(ggplot2)
library(stargazer)
library(texreg)
library(vtable)
library(stats)
}


##### DATA MANIPULATION #####

# Summary statistics

data('Produc', package = 'plm')
Produc <- pdata.frame(Produc, index=c('state', 'year'))
st(Produc,out='latex')

# Plots

# log(gdp) vs log(pcap) - by states

ggplot(Produc, aes(x=log(pcap), y=log(gsp), color=state)) + geom_point() +
  ggtitle("log(gsp) vs log(pcap)") +
  labs(x = "log(pcap)", y = "log(gsp)")

# log(gsp) vs log(pcap) - totals

gsp_sum <- aggregate(gsp ~ year, Produc, sum)
pcap_sum <- aggregate(pcap ~ year, Produc, sum)

Produc_sum <- cbind(as.data.frame(gsp_sum$year), as.data.frame(gsp_sum$gsp), as.data.frame(pcap_sum$pcap))
colnames(Produc_sum) <- c("year", "gsp", "pcap")

ggplot(Produc_sum, aes(x=log(pcap), y=log(gsp))) + geom_point()+
  ggtitle("log(gsp) vs log(pcap) - totals") +
  labs(x = "log(pcap)", y = "log(gsp)")

# log(gsp) by years

ggplot(Produc, aes(x=year, y=log(gsp), color=state)) + geom_point()

# log(pcap) by years

ggplot(Produc, aes(x=year, y=log(pcap), color=state)) + geom_point()


# log(gsp) by years - quantiles:

Produc$Date <- as.Date(as.character(paste(as.character(Produc$year), "-01-01", sep = "")), "%Y-%m-%d")
Produc$Date <- as.numeric(Produc$year)

convertiblesToNumeric <- function(x){
  x2 <- cbind.data.frame(lapply(seq_along(x), function(i) {
    if (!all(is.na(as.numeric(x[, i])))){
      as.numeric(x[, i])
    } else {
      x[, i]
    }
  }), stringsAsFactors=FALSE)
  names(x2) <- names(x)
  return(x2)
}

Produc1 <- as.data.frame(Produc)
Produc1 <- convertiblesToNumeric(Produc1)
Produc1 <- as.data.frame(Produc1)

Produc2 <- Produc1$state
Produc2 <- as.data.frame(Produc2)
Produc2$year <- Produc1$year
Produc2$gsp <- Produc1$gsp

r01 <- aggregate(gsp ~ year, Produc2, FUN = quantile, probs = c(0.15))
r02 <- aggregate(gsp ~ year, Produc2, FUN = quantile, probs = c(0.25))
r03 <- aggregate(gsp ~ year, Produc2, FUN = quantile, probs = c(0.50))
r04 <- aggregate(gsp ~ year, Produc2, FUN = quantile, probs = c(0.75))
r05 <- aggregate(gsp ~ year, Produc2, FUN = quantile, probs = c(0.85))

r <- as.data.frame(r01)
colnames(r) <- c("year", "r01")
r$r02 <- r02$gsp
r$r03 <- r03$gsp
r$r04 <- r04$gsp
r$r05 <- r05$gsp
r <- log(r)

r$year <- c(1970:1986)

ggplot(r, aes(x = year)) +
  geom_line(aes(y = r01, col="15%")) +
  geom_line(aes(y = r02, col="25%")) +
  geom_line(aes(y = r03, col="50%")) +
  geom_line(aes(y = r04, col="75%")) +
  geom_line(aes(y = r05, col="85%")) +
  labs(x = "Year", y = "log(gsp)") +
  ggtitle("log(gsp) percentiles by years")


# log(pcap) by years - quantiles:

Produc2 <- Produc1$state
Produc2 <- as.data.frame(Produc2)
Produc2$year <- Produc1$year
Produc2$pcap <- Produc1$pcap

r01 <- aggregate(pcap ~ year, Produc2, FUN = quantile, probs = c(0.15))
r02 <- aggregate(pcap ~ year, Produc2, FUN = quantile, probs = c(0.25))
r03 <- aggregate(pcap ~ year, Produc2, FUN = quantile, probs = c(0.50))
r04 <- aggregate(pcap ~ year, Produc2, FUN = quantile, probs = c(0.75))
r05 <- aggregate(pcap ~ year, Produc2, FUN = quantile, probs = c(0.85))

r <- as.data.frame(r01)
colnames(r) <- c("year", "r01")
r$r02 <- r02$pcap
r$r03 <- r03$pcap
r$r04 <- r04$pcap
r$r05 <- r05$pcap
r <- log(r)

r$year <- c(1970:1986)

ggplot(r, aes(x = year)) +
  geom_line(aes(y = r01, col="15%")) +
  geom_line(aes(y = r02, col="25%")) +
  geom_line(aes(y = r03, col="50%")) +
  geom_line(aes(y = r04, col="75%")) +
  geom_line(aes(y = r05, col="85%")) +
  labs(x = "Year", y = "log(pcap)") +
  ggtitle("log(pcap) percentiles by years")


##### REGRESSIONS #####

formula <- 'log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp'

# individual effects: 

cross_section <- plm(formula, data=Produc, effect='individual', index = c("year","state"))
summary(cross_section)

# time effects:

time_series <- plm(formula, data=Produc, effect='time', index = c("year","state"))

# pooled:

pooled <- plm(formula, data=Produc, model='pooling', index = c("year","state"))
summary(pooled)

# two-ways effects:

two_ways <- plm(formula, data=Produc, effect='twoways', model ='within', index = c("year","state"))
summary(two_ways)

# constant return of scale test (constant return to scale rejected):

linearHypothesis(two_ways, "log(pcap) + log(pc) + log(emp) = 1")

# individual vs two-ways effects (two-ways preferred):

pFtest(two_ways, cross_section)

# pooled vs two-ways effects (two-ways preferred):

pFtest(two_ways, pooled)

# Random effects (two-ways):
random_effects <- plm(formula, data=Produc, effect='twoways', model ='random')
summary(random_effects)

# Summary: 

stargazer(pooled, cross_section, time_series, two_ways, random_effects)

# BP-test (random effects are better than pooled):
plmtest(pooled, type=c("bp"))

# Hausman test (two-ways preferred)
phtest(two_ways, random_effects)


##### SPATIAL DEPENDENCE #####

# Data (weights matrix)

data(usaww, package = "splm")

usalw <- mat2listw(usaww)

# Ñross-sectional dependence test (CD-test)

pcdtest(Produc$gsp)

pcdtest(two_ways)

# Moran's I for different years: 

moran_test <- function(variable, data=Produc, 
                       listw=usalw) {
  moran_test_result <- c()
  moran_pvalues <- c()
  for (y in years) {
    test_result <- moran.test(data[data$year == y, variable], listw)
    moran_test_result <- c(moran_test_result, test_result$estimate[1])
    moran_pvalues <- c(moran_pvalues, test_result$p.value[1])
  }
  return(data.frame(moran_test_result, moran_pvalues))
}

years <- 1970:1986

# gsp: 

moran.summary <- data.frame(moran_test('gsp'), row.names = 1970:1986)
colnames(moran.summary) <- c('Moran I-stat.', 'pvalue')
stargazer(t(moran.summary), type='text', summary=F)

# pc: 

moran.summary <- data.frame(moran_test('pc'), row.names = 1970:1986)
colnames(moran.summary) <- c('Moran I-stat.', 'pvalue')
stargazer(t(moran.summary), type='text', summary=F)

# pcap: 

moran.summary <- data.frame(moran_test('pcap'), row.names = 1970:1986)
colnames(moran.summary) <- c('Moran I-stat.', 'pvalue')
stargazer(t(moran.summary), type='text', summary=F)

# unemp: 

moran.summary <- data.frame(moran_test('unemp'), row.names = 1970:1986)
colnames(moran.summary) <- c('Moran I-stat.', 'pvalue')
stargazer(t(moran.summary), type='text', summary=F)


##### SPATIAL REGRESSIONS #####

extract.splm <- function(model) {
  s <- summary(model)
  
  # extract information from model and summary object here
  full.coefs <- rbind(s[["CoefTable"]], s[["ARCoefTable"]], s[["ErrCompTable"]])
  
  coef.names <- rownames(full.coefs)
  coef <- full.coefs[, 'Estimate']
  se <- full.coefs[, 'Std. Error']
  pvalues <- full.coefs[, 'Pr(>|t|)']
  gof.names <- c('logLik')
  #gof.names <- c('R2*100')
  gof <- c(s[["logLik"]])
  #gof <- (1 - sum(s$residuals^2)/s$tss)*100
  gof.decimal <- c(T)
  
  # then create and return a texreg object (replace NULL with actual values):
  tr <- createTexreg(
    coef.names = coef.names,    # character vector of coefficient labels
    coef = coef,                # numeric vector with coefficients
    se = se,                    # numeric vector with standard error values
    pvalues = pvalues,          # numeric vector with p-values
    gof.names = gof.names,      # character vector with goodness-of-fit labels
    gof = gof,                  # numeric vector of goodness-of-fit statistics
    gof.decimal = gof.decimal   # logical vector: GOF statistic has decimal points?
  )       
  return(tr)
}

# register the method
setMethod("extract", signature = className("splm", "splm"), 
          definition = extract.splm)

fm <- log(gsp)~log(pcap)+log(pc)+log(emp)+unemp

# Pooling SAR

sarpool <- spml(formula = fm, data = Produc,
                listw = usalw, model = "pooling",
                spatial.error = "none", lag = TRUE)

# Pooling SEM

sempool <- spml(formula = fm, data = Produc, listw = usalw, model = "pooling",
                spatial.error = "b", lag = FALSE)

# Pooling SARAR

sararpool <- spml(formula = fm, data = Produc, listw = usalw, model = "pooling",
                spatial.error = "b", lag = TRUE)

# Pooling results

screenreg(list(sarpool, sempool, sararpool), digits = 3)
texreg(list(sarpool, sempool, sararpool), digits = 3)

# RE SAR

sarre <- spml(formula = fm, data = Produc,
                listw = usalw, model = "random",
                spatial.error = "none", lag = TRUE)

# RE SEM B

semBre <- spml(formula = fm, data = Produc, listw = usalw, model = "random",
                spatial.error = "b", lag = FALSE)


# RE SEM KKP

semKKPre <- spml(formula = fm, data = Produc, listw = usalw, model = "random",
               spatial.error = "kkp", lag = FALSE)

# RE SARAR B

sararBre <- spml(formula = fm, data = Produc, listw = usalw, model = "random",
               spatial.error = "b", lag = TRUE)

# RE SARAR KKP

sararKKPre <- spml(formula = fm, data = Produc, listw = usalw, model = "random",
                 spatial.error = "kkp", lag = TRUE)

# RE SAR SR

sarreSR <- spreml(formula = fm, data = Produc, w = usalw,
                        errors = "srre", lag = TRUE)

# RE SEM SR

semreSR <- spreml(formula = fm, data = Produc, w = usalw,
                  errors = "sem2sr", lag = FALSE)

# RE SARAR SR

sararreSR <- spreml(formula = fm, data = Produc, w = usalw,
                  errors = "sem2sr", lag = TRUE)


# RE results

screenreg(list(sarre, semBre, semKKPre, sararBre, sararKKPre, sarreSR, semreSR, sararreSR), digits = 3)
texreg(list(sarre, semBre, semKKPre, sararBre, sararKKPre, sarreSR, semreSR, sararreSR), digits = 3)


# FE INDIVIDUAL SAR 

sarfeind <- spml(formula = fm, data = Produc,
                listw = usalw, model = "within", effect = "individual",
                spatial.error = "none", lag = TRUE)

# FE TIME SAR 

sarfetime <- spml(formula = fm, data = Produc,
                 listw = usalw, model = "within", effect = "time",
                 spatial.error = "none", lag = TRUE)

# FE 2-WAYS SAR 

sarfe2ways <- spml(formula = fm, data = Produc,
                  listw = usalw, model = "within", effect = "twoways",
                  spatial.error = "none", lag = TRUE)

# FE INDIVIDUAL SEM 

semfeind <- spml(formula = fm, data = Produc,
                 listw = usalw, model = "within", effect = "individual",
                 spatial.error = "b", lag = FALSE)

# FE TIME SEM 

semfetime <- spml(formula = fm, data = Produc,
                 listw = usalw, model = "within", effect = "time",
                 spatial.error = "b", lag = FALSE)

# FE 2-WAYS SEM 

semfe2ways <- spml(formula = fm, data = Produc,
                  listw = usalw, model = "within", effect = "twoways",
                  spatial.error = "b", lag = FALSE)

# FE INDIVIDUAL SARAR

sararfeind <- spml(formula = fm, data = Produc,
                 listw = usalw, model = "within", effect = "individual",
                 spatial.error = "b", lag = TRUE)

# FE TIME SARAR

sararfetime <- spml(formula = fm, data = Produc,
                  listw = usalw, model = "within", effect = "time",
                  spatial.error = "b", lag = TRUE)

# FE 2-WAYS SARAR

sararfe2ways <- spml(formula = fm, data = Produc,
                   listw = usalw, model = "within", effect = "twoways",
                   spatial.error = "b", lag = TRUE)

# FE results

screenreg(list(sarfeind, sarfetime, sarfe2ways))
texreg(list(sarfeind, sarfetime, sarfe2ways))


extract.splm <- function(model) {
  s <- summary(model)
  
  # extract information from model and summary object here
  full.coefs <- rbind(s[["CoefTable"]], s[["ARCoefTable"]], s[["ErrCompTable"]])
  
  coef.names <- rownames(full.coefs)
  coef <- full.coefs[, 'Estimate']
  se <- full.coefs[, 'Std. Error']
  pvalues <- full.coefs[, 'Pr(>|t|)']
  #gof.names <- c('logLik')
  gof.names <- c('R2*100')
  #gof <- c(s[["logLik"]])
  gof <- (1 - sum(s$residuals^2)/s$tss)*100
  gof.decimal <- c(T)
  
  # then create and return a texreg object (replace NULL with actual values):
  tr <- createTexreg(
    coef.names = coef.names,    # character vector of coefficient labels
    coef = coef,                # numeric vector with coefficients
    se = se,                    # numeric vector with standard error values
    pvalues = pvalues,          # numeric vector with p-values
    gof.names = gof.names,      # character vector with goodness-of-fit labels
    gof = gof,                  # numeric vector of goodness-of-fit statistics
    gof.decimal = gof.decimal   # logical vector: GOF statistic has decimal points?
  )       
  return(tr)
}

# register the method
setMethod("extract", signature = className("splm", "splm"), 
          definition = extract.splm)

screenreg(list(sarfeind, sarfetime, sarfe2ways, semfeind, semfetime, semfe2ways, sararfeind, sararfetime, sararfe2ways), digits = 3)
texreg(list(sarfeind, sarfetime, sarfe2ways, semfeind, semfetime, semfe2ways, sararfeind, sararfetime, sararfe2ways), digits = 3)


##### FINAL TABLE #####

semSR <-  spreml(formula = fm, data = Produc, w = usalw,
                           errors = "semsr", lag = FALSE)

sem2w <- spml(formula = fm, data = Produc,
              listw = usalw, model = "within", effect = "twoways",
              spatial.error = "b", lag = FALSE)

sarari <- spml(formula = fm, data = Produc,
               listw = usalw, model = "within", effect = "individual",
               spatial.error = "b", lag = TRUE)

screenreg(list(semSR, sem2w, sarari), digits = 3)
texreg(list(semSR, sem2w, sarari), digits = 3)

##### TESTS #####

# Hausman test (plm)
print(hausman_panel<-phtest(fm, data = Produc))

# Hausman test robust to spatial autocorrelation (splm)
print(spat_hausman_ML_SEM<-sphtest(fm, data=Produc, listw=usalw, spatial.model = "error", method="ML"))

# Joint test
bsjktest(fm, data=Produc, listw=usalw, test="J")

# Conditional test
bsjktest(fm, data=Produc, listw=usalw, test="C.2")



Produc$unemp_lag <- slag(Produc$unemp, listw=usalw)

sp_fm <- log(gsp)~log(pcap)+log(pc)+log(emp)+unemp+unemp_lag

semSR_mod <-  spreml(formula = sp_fm, data = Produc, w = usalw,
                 errors = "semsr", lag = FALSE)

sem2w_mod <- spml(formula = sp_fm, data = Produc,
              listw = usalw, model = "within", effect = "twoways",
              spatial.error = "b", lag = FALSE)

sarari_mod <- spml(formula = sp_fm, data = Produc,
               listw = usalw, model = "within", effect = "individual",
               spatial.error = "b", lag = TRUE)

texreg(list(semSR, sem2w, sarari, semSR_mod, sem2w_mod, sarari_mod), digits = 3)


summary(sarari_mod)

