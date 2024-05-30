library(Matrix)
library(metafor)
library(tidyverse)
library(haven)
library(here)
library(psych)

<<<<<<< HEAD
my_data <- read_sav("C://Users//Salome Wagner//OneDrive - UT Cloud//Metaanalyse//cbf-writing//02_data//03_SPSS files//Metaanalysis_control_N14.sav")
my_data
## Schlaufe ########################################################################################
datT <- data.frame(
  id = c(1,1,1,2,3,3,4,4,5,6,7,8,8,8),
  m_pre = my_data$M_Feedback,
  m_post = my_data$M_Feedback2,
  sd_pre = my_data$SD_Feedback,
  sd_post = my_data$SD_Feedback2,
  ni = my_data$N_Feedback, # .50 < ri < .75 (original: .72)
  slab = my_data$author)
datT
=======
dataN14 <- read_sav(here("02_data/03_SPSS files/Metaanalysis_control_N14.sav"))
>>>>>>> 3ec52fbe387b0a5cfcb344516c1e3db2d0a8794b

#### LOOP BEGIN ################################################################
datT <- data.frame(
  sampleNr = 1:14,
  id = c(1,1,1,2,3,3,4,4,5,6,7,8,8,8),
  m_pre = dataN14$M_Feedback,
  m_post = dataN14$M_Feedback2,
  sd_pre = dataN14$SD_Feedback,
  sd_post = dataN14$SD_Feedback2,
  ni = dataN14$N_Feedback, # .50 < ri < .75 (original: .72)
  slab = dataN14$author)

datC <- data.frame(
  sampleNr = 1:14,
  id = c(1,1,1,2,3,3,4,4,5,6,7,8,8,8),
  m_pre = dataN14$M_NoFeedback,
  m_post = dataN14$M_NoFeedback2,
  sd_pre = dataN14$SD_NoFeedback,
  sd_post = dataN14$SD_NoFeedback2,
  ni = dataN14$N_noFeedback, # .64 < ri < .89 (original: .86)
  slab = dataN14$author)

sensitivity <- data.frame(ri_t = as.numeric(),  # assumed pre-post correlation treatment group
                          ri_c = as.numeric(),  # assumed pre-post correlation control group
                          pvalue = as.numeric(), # p value of ES
                          beta = as.numeric(),   # meta-analytic ES
                          se = as.numeric(),     # SE of meta-analytic ES
                          yi.f01 = as.numeric(), # ES of individual studies
                          yi.f02 = as.numeric(),
                          yi.f03 = as.numeric(),
                          yi.f04 = as.numeric(),
                          yi.f05 = as.numeric(),
                          yi.f06 = as.numeric(),
                          yi.f07 = as.numeric(),
                          yi.f08 = as.numeric(),
                          yi.f09 = as.numeric(),
                          yi.f10 = as.numeric(),
                          yi.f11 = as.numeric(),
                          yi.f12 = as.numeric(),
                          yi.f13 = as.numeric(),
                          yi.f14 = as.numeric(),
                          sei.f01 = as.numeric(),  # SEs of ES of individual studies
                          sei.f02 = as.numeric(),
                          sei.f03 = as.numeric(),
                          sei.f04 = as.numeric(),
                          sei.f05 = as.numeric(),
                          sei.f06 = as.numeric(),
                          sei.f07 = as.numeric(),
                          sei.f08 = as.numeric(),
                          sei.f09 = as.numeric(),
                          sei.f10 = as.numeric(),
                          sei.f11 = as.numeric(),
                          sei.f12 = as.numeric(),
                          sei.f13 = as.numeric(),
                          sei.f14 = as.numeric()
                          )

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from between-subjects data
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(ri_t, 14),
                   slab = datT$slab)
  
  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((ri_t+.14), 14),
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,8,8,8)
  rma_overall_clustered <- rma.mv(yi, vi, data=dat, random = ~ 1 | id)
  
  sensitivity <- sensitivity %>%
    add_row(ri_t = ri_t, 
            ri_c = (ri_t+.14),
            pvalue = rma_overall_clustered$pval,
            beta = rma_overall_clustered$beta[,1],
            se = rma_overall_clustered$se[1],
            yi.f01 = rma_overall_clustered$yi.f[1],
            yi.f02 = rma_overall_clustered$yi.f[2],
            yi.f03 = rma_overall_clustered$yi.f[3],
            yi.f04 = rma_overall_clustered$yi.f[4],
            yi.f05 = rma_overall_clustered$yi.f[5],
            yi.f06 = rma_overall_clustered$yi.f[6],
            yi.f07 = rma_overall_clustered$yi.f[7],
            yi.f08 = rma_overall_clustered$yi.f[8],
            yi.f09 = rma_overall_clustered$yi.f[9],
            yi.f10 = rma_overall_clustered$yi.f[10],
            yi.f11 = rma_overall_clustered$yi.f[11],
            yi.f12 = rma_overall_clustered$yi.f[12],
            yi.f13 = rma_overall_clustered$yi.f[13],
            yi.f14 = rma_overall_clustered$yi.f[14],
            sei.f01 = sqrt(dat$vi[1]),
            sei.f02 = sqrt(dat$vi[2]),
            sei.f03 = sqrt(dat$vi[3]),
            sei.f04 = sqrt(dat$vi[4]),
            sei.f05 = sqrt(dat$vi[5]),
            sei.f06 = sqrt(dat$vi[6]),
            sei.f07 = sqrt(dat$vi[7]),
            sei.f08 = sqrt(dat$vi[8]),
            sei.f09 = sqrt(dat$vi[9]),
            sei.f10 = sqrt(dat$vi[10]),
            sei.f11 = sqrt(dat$vi[11]),
            sei.f12 = sqrt(dat$vi[12]),
            sei.f13 = sqrt(dat$vi[13]),
            sei.f14 = sqrt(dat$vi[14])
    )
}

## LOOP END ################################################################## #


<<<<<<< HEAD
rma_overall_clustered
##result: g = .2500, p = .0536, CI 95% [-0.0037, 0.4715] nicht sign.
#Heterogeneity: Q(df = 13) = 75.0329, p < .0001, I^2 = 75%

summary(sensitivity) #mittlere Effektst?rke bei beta ablesen: g = .25 (ranging from .23 to .26), p = .041 (ranging from .033 to .054)

## forest plot
forest(rma_overall_clustered) #zeigt in diesem Fall die Werte f?r die letzte Korrelation an
=======
sensitivity %>%
  dplyr::select(ri_t, beta, se, pvalue) %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


# compute mean, min and max of ES, CI and pvalue from meta-analysis
sensitivity %>%
  dplyr::summarise(ES_mean         = mean(beta),
                   ES_min  = min(beta),
                   ES_max  = max(beta),
                   CI_lower_mean        = mean(beta-(1.96*se)),
                   CI_lower_min = min(beta-(1.96*se)),
                   CI_lower_max = max(beta-(1.96*se)),
                   CI_upper_mean        = mean(beta+(1.96*se)),
                   CI_upper_min = min(beta+(1.96*se)),
                   CI_upper_max = max(beta+(1.96*se)),
                   pvalue_mean        = mean(pvalue),
                   pvalue_min = min(pvalue),
                   pvalue_max = max(pvalue)) %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, background = "#A66A7A") %>%
  column_spec(2, background = "#FFA3BC") %>%
  column_spec(3, background = "#DB8CA1") %>%
  column_spec(4, background = "#99BBBF") %>%
  column_spec(5, background = "#C8F5FA") %>%
  column_spec(6, background = "#BAE3E8") %>%
  column_spec(7, background = "#A0A603") %>%
  column_spec(8, background = "#EAF205") %>%
  column_spec(9, background = "#D2D904") %>%
  column_spec(10, background = "#F0CBA0") %>%
  column_spec(11, background = "#FFF0E3") %>%
  column_spec(12, background = "#FFE4CE")
>>>>>>> 3ec52fbe387b0a5cfcb344516c1e3db2d0a8794b

# compute mean, min and max of ES and CI from each study
sensitivity %>%
  dplyr::select(-c(beta, pvalue, se)) %>%
  pivot_longer(c(3:30),                       # reshape data data from
               names_to = "variable",         # sensitivity analysis
               values_to = "values") %>%
  mutate(sampleNr = as.numeric(str_sub(variable, -2, -1)),
         variable = str_sub(variable, 1, -5)) %>%
  pivot_wider(id_cols = c(sampleNr, ri_t, ri_c), 
              names_from = "variable", 
              values_from = "values") %>%
  group_by(sampleNr) %>%
  dplyr::summarise(ES_mean = mean(yi),    # compute mean, min and max
                   ES_min  = min(yi),     # of ES and CI
                   ES_max  = max(yi),     # from each study
                   CI_lower_mean = mean(yi-(1.96*sei)),
                   CI_lower_min  = min(yi-(1.96*sei)),
                   CI_lower_max  = max(yi-(1.96*sei)),
                   CI_upper_mean = mean(yi+(1.96*sei)),
                   CI_upper_min  = min(yi+(1.96*sei)),
                   CI_upper_max  = max(yi+(1.96*sei)))  %>%
  right_join(data_pp[c("sampleNr", "slab")],., by = "sampleNr") %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(3, background = "#A66A7A") %>%
  column_spec(4, background = "#FFA3BC") %>%
  column_spec(5, background = "#DB8CA1") %>%
  column_spec(6, background = "#99BBBF") %>%
  column_spec(7, background = "#C8F5FA") %>%
  column_spec(8, background = "#BAE3E8") %>%
  column_spec(9, background = "#A0A603") %>%
  column_spec(10, background = "#EAF205") %>%
  column_spec(11, background = "#D2D904")


## FOREST PLOT with mean correlation
###compute effect sizes for writing quality
# effect sizes from within-subjects data
datT_es<- escalc(measure = "SMCR",
                 m1i=datT$m_post, m2i=datT$m_pre,
                 sd1i=datT$sd_pre, sd2i=datT$sd_post,
                 ni=datT$ni, ri=rep(.625, 14),
                 slab = datT$slab)

datT_es$id <- datT$id

datC_es <- escalc(measure="SMCR",
                  m1i=datC$m_post, m2i=datC$m_pre,
                  sd1i=datC$sd_pre, sd2i=datC$sd_post,
                  ni=datC$ni, ri=rep((.625+.14), 14),
                  slab = datC$slab)
datC_es$id<-datC$id

dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)

dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,8,8,8)

REM <- rma.mv(yi, vi, data=dat, random = ~ 1 | id)

###FOREST PLOT
forest(REM)




############################################################################## #
#### PUBLICATION BIAS ##########################################################
############################################################################## #
##Funnel plot
##Funnel plot
funnel(REM, legend = T)

##Trim & fill
rma_trimfill <- rma(yi, vi, data=dat) # compute random-effects model without
                                      # clustering, as trimfill() can't handle
                                      # these objects
taf_overall <- trimfill(rma_trimfill)
taf_overall 

funnel(taf_overall, legend = T)

##Egger's test
regtest(rma_trimfill) 

###HETEROGENEITY
W<-diag(1/REM$vi)
X<-model.matrix(REM)
P<- W-W%*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100*sum(REM$sigma2)/(sum(REM$sigma2)+ (REM$k-REM$p)/sum(diag(P)))

<<<<<<< HEAD
100*rma_overall_clustered$sigma2/(sum(rma_overall_clustered$sigma2)+(rma_overall_clustered$k - rma_overall_clustered$p)/sum(diag(P)))
#I? = 75.23% of the detected variation could be related to  true variation among studies
=======
100*REM$sigma2/(sum(REM$sigma2)+(REM$k - REM$p)/sum(diag(P)))
>>>>>>> 3ec52fbe387b0a5cfcb344516c1e3db2d0a8794b



############################################################################## #
####MODERATION ANALYSES ########################################################
############################################################################## #


## REPRESENTATION #######################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(ri_t, 14),
                   slab = datT$slab)
  
  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((ri_t+.14), 14),
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,8,8,8)
  dat$rep <- as.factor(dataN14$FB_representation)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = rep)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

skim(sensitivity_moderator)


## ORDER ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(ri_t, 14),
                   slab = datT$slab)
  
  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((ri_t+.14), 14),
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,8,8,8)
  dat$order<- as.factor(dataN14$FB_order)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

skim(sensitivity_moderator)


## ORDER LOW ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(ri_t, 14),
                   slab = datT$slab)
  
  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((ri_t+.14), 14),
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,8,8,8)
  dat$order_low <- as.factor(ifelse(dataN14$FB_order == 1, 1, 0))
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_low)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

skim(sensitivity_moderator)


## ORDER HIGH ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(ri_t, 14),
                   slab = datT$slab)
  
  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((ri_t+.14), 14),
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,8,8,8)
  dat$order_hi <- as.factor(ifelse(dataN14$FB_order == 2, 1, 0))
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_hi)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

skim(sensitivity_moderator)


## ORDER HIGH-LOW ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(ri_t, 14),
                   slab = datT$slab)
  
  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((ri_t+.14), 14),
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,8,8,8)
  dat$order_hilow <- ifelse(dataN14$FB_order == 3, 1, 0)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_hilow)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

skim(sensitivity_moderator)


## SPECIFITY ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(ri_t, 14),
                   slab = datT$slab)
  
  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((ri_t+.14), 14),
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,8,8,8)
  dat$spec <- as.factor(dataN14$FB_specificity)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = spec)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

skim(sensitivity_moderator)



## TOOL NUMBERS ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(ri_t, 14),
                   slab = datT$slab)
  
  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((ri_t+.14), 14),
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,8,8,8)
  dat$tool <- as.factor(dataN14$FB_tool_numbers)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = tool)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

skim(sensitivity_moderator)



## PRIOR KNOWLEDGE ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(ri_t, 14),
                   slab = datT$slab)
  
  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((ri_t+.14), 14),
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,8,8,8)
  dat$WQP_pre <- as.factor(dataN14$WQP_pre)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = WQP_pre)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

skim(sensitivity_moderator)



## EXPERT  ######################################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(ri_t, 14),
                   slab = datT$slab)
  
  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((ri_t+.14), 14),
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,8,8,8)
  dat$Expert <- as.factor(c(0, 0, 0, 1, 1, 1, 0, # WQP_FB_pre: 0 = under 60% 
                            1, 0, 0, 0, 0, 1, 1))# (low prior knowledge)
                                                 # 1 = over 60% (high prior knowledge)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = Expert)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

skim(sensitivity_moderator)