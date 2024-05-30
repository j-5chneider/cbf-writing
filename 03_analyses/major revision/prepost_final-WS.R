library(tidyverse)
library(here)
library(haven)
library(metafor)
library(kableExtra)

data <- read_sav("C://Users//Salome Wagner//OneDrive - UT Cloud//Metaanalyse//submission EPR//major revision//Nacherhebung//Metaanalysis_prepost_neu_N16_final-WS.sav")

#### LOOP BEGIN ################################################################
data_pp <- data.frame(
  sampleNr = 1:33,
  id = c(1,1,1,2,2,3,3,4,4,4,4,4,4,5,5,6,7,7,8,8,9,10,11,12,12,13,14,14,14,15,15, 16, 17),
  m_pre = data$M_pre,
  m_post = data$M_post,
  sd_pre = data$SD_pre,
  sd_post = data$SD_post,
  ni = data$N, # .50 < ri < .75 (from open data (Burkhart et al., 2020): ri=.72)
  slab = data$author)

# Establish empty data frame to be filled with results
sensitivity <- data.frame(ri_t = as.numeric(),   # assumed pre-post correlation
                          beta = as.numeric(),   # meta-analytic ES
                          pvalue = as.numeric(), # p value of ES
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
                          yi.f15 = as.numeric(),
                          yi.f16 = as.numeric(),
                          yi.f17 = as.numeric(),
                          yi.f18 = as.numeric(),
                          yi.f19 = as.numeric(),
                          yi.f20 = as.numeric(),
                          yi.f21 = as.numeric(),
                          yi.f22 = as.numeric(),
                          yi.f23 = as.numeric(),
                          yi.f24 = as.numeric(),
                          yi.f25 = as.numeric(),
                          yi.f26 = as.numeric(),
                          yi.f27 = as.numeric(),
                          yi.f28 = as.numeric(),
                          yi.f29 = as.numeric(),
                          yi.f30 = as.numeric(),
                          yi.f31 = as.numeric(),
                          yi.f32 = as.numeric(),
                          yi.f33 = as.numeric(),
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
                          sei.f14 = as.numeric(),
                          sei.f15 = as.numeric(),
                          sei.f16 = as.numeric(),
                          sei.f17 = as.numeric(),
                          sei.f18 = as.numeric(),
                          sei.f19 = as.numeric(),
                          sei.f20 = as.numeric(),
                          sei.f21 = as.numeric(),
                          sei.f22 = as.numeric(),
                          sei.f23 = as.numeric(),
                          sei.f24 = as.numeric(),
                          sei.f25 = as.numeric(),
                          sei.f26 = as.numeric(),
                          sei.f27 = as.numeric(),
                          sei.f28 = as.numeric(),
                          sei.f29 = as.numeric(),
                          sei.f30 = as.numeric(),
                          sei.f31 = as.numeric(),
                          sei.f32 = as.numeric(),
                          sei.f33 = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  
  # compute the meta-analysis
  rma_overall_clustered <- rma.mv(yi, vi, 
                                  data=data_pp_es, 
                                  random = ~ 1 | id # take clustered data into account
  )
  
  # save estimates for sensitivity analysis
  sensitivity <- sensitivity %>%
    add_row(ri_t = ri_t, 
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
            yi.f15 = rma_overall_clustered$yi.f[15],
            yi.f16 = rma_overall_clustered$yi.f[16],
            yi.f17 = rma_overall_clustered$yi.f[17],
            yi.f18 = rma_overall_clustered$yi.f[18],
            yi.f19 = rma_overall_clustered$yi.f[19],
            yi.f20 = rma_overall_clustered$yi.f[20],
            yi.f21 = rma_overall_clustered$yi.f[21],
            yi.f22 = rma_overall_clustered$yi.f[22],
            yi.f23 = rma_overall_clustered$yi.f[23],
            yi.f24 = rma_overall_clustered$yi.f[24],
            yi.f25 = rma_overall_clustered$yi.f[25],
            yi.f26 = rma_overall_clustered$yi.f[26],
            yi.f27 = rma_overall_clustered$yi.f[27],
            yi.f28 = rma_overall_clustered$yi.f[28],
            yi.f29 = rma_overall_clustered$yi.f[29],
            yi.f30 = rma_overall_clustered$yi.f[30],
            yi.f31 = rma_overall_clustered$yi.f[31],
            yi.f32 = rma_overall_clustered$yi.f[32],
            yi.f33 = rma_overall_clustered$yi.f[33],
            sei.f01 = sqrt(data_pp_es$vi[1]),
            sei.f02 = sqrt(data_pp_es$vi[2]),
            sei.f03 = sqrt(data_pp_es$vi[3]),
            sei.f04 = sqrt(data_pp_es$vi[4]),
            sei.f05 = sqrt(data_pp_es$vi[5]),
            sei.f06 = sqrt(data_pp_es$vi[6]),
            sei.f07 = sqrt(data_pp_es$vi[7]),
            sei.f08 = sqrt(data_pp_es$vi[8]),
            sei.f09 = sqrt(data_pp_es$vi[9]),
            sei.f10 = sqrt(data_pp_es$vi[10]),
            sei.f11 = sqrt(data_pp_es$vi[11]),
            sei.f12 = sqrt(data_pp_es$vi[12]),
            sei.f13 = sqrt(data_pp_es$vi[13]),
            sei.f14 = sqrt(data_pp_es$vi[14]),
            sei.f15 = sqrt(data_pp_es$vi[15]),
            sei.f16 = sqrt(data_pp_es$vi[16]),
            sei.f17 = sqrt(data_pp_es$vi[17]),
            sei.f18 = sqrt(data_pp_es$vi[18]),
            sei.f19 = sqrt(data_pp_es$vi[19]),
            sei.f20 = sqrt(data_pp_es$vi[20]),
            sei.f21 = sqrt(data_pp_es$vi[21]),
            sei.f22 = sqrt(data_pp_es$vi[22]),
            sei.f23 = sqrt(data_pp_es$vi[23]),
            sei.f24 = sqrt(data_pp_es$vi[24]),
            sei.f25 = sqrt(data_pp_es$vi[25]),
            sei.f26 = sqrt(data_pp_es$vi[26]),
            sei.f27 = sqrt(data_pp_es$vi[27]),
            sei.f28 = sqrt(data_pp_es$vi[28]),
            sei.f29 = sqrt(data_pp_es$vi[29]),
            sei.f30 = sqrt(data_pp_es$vi[30]),
            sei.f31 = sqrt(data_pp_es$vi[31]),
            sei.f32 = sqrt(data_pp_es$vi[32]),
            sei.f33 = sqrt(data_pp_es$vi[33])
    )
}
## LOOP END ################################################################## #

sensitivity %>%
  dplyr::select(ri_t, beta, se, pvalue) %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


# compute mean, min and max of ES, CI and pvalue from meta-analysis
sensitivity %>%
  dplyr::summarise(ES_mean = mean(beta),
                   ES_min  = min(beta),
                   ES_max  = max(beta),
                   se_mean = mean(se),
                   se_min = min(se),
                   se_max = max(se),
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

# compute mean, min and max of ES and CI from each study
sensitivity %>%
  dplyr::select(-c(beta, pvalue, se)) %>%
  pivot_longer(c(2:64),                       # reshape data from
               names_to = "variable",         # sensitivity analysis
               values_to = "values") %>%
  mutate(sampleNr = as.numeric(str_sub(variable, -2, -1)),
         variable = str_sub(variable, 1, -5)) %>%
  pivot_wider(id_cols = c(sampleNr, ri_t), 
              names_from = "variable", 
              values_from = "values") %>%
  group_by(sampleNr) %>%
  dplyr::summarise(ES_mean = mean(yi),    # compute mean, min and max
                   ES_min  = min(yi),     # of ES and CI
                   ES_max  = max(yi),     # from each study
                   se_mean = mean(sei),
                   se_min = min(sei),
                   se_max = max(sei),
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
data_pp_es <- escalc(measure = "SMCR",
                     m1i=data_pp$m_post, m2i=data_pp$m_pre,
                     sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                     ni=data_pp$ni, ri=rep(.625, 33),
                     slab = data_pp$slab)
# append the id variable
data_pp_es$id <- data_pp$id

REM <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id)
REM
###FOREST PLOT
forest(REM)


############################################################################## #
#### PUBLICATION BIAS ##########################################################
############################################################################## #

####PUBLICATION BIAS
##Funnel plot
funnel(REM, legend = T)

##Trim & fill
rma_trimfill <- rma(yi, vi, data=data_pp_es) # compute random-effects model without
# clustering, as trimfill() can't handle
# these objects
taf_overall <- trimfill(rma_trimfill)
taf_overall 

funnel(taf_overall, legend = T)
# no missing studies on the left side, change of effect, now the effect of feedback on writing is sign. g = .4364, se = .1008, p < .0001, CI 95% [0.2389, 0.6340]
# Heterogeneity: Q(df = 32) = 602.8700, p-val < .0001; tau² = .3068 (se = .0833); I² = 97.05%; H² = 33.86

##Egger's test
regtest(rma_trimfill) #asymmetry z = 6.2669, p < .0001, b = -0.2622 (CI: -0.5056, -0.0188)

###HETEROGENEITY

# Establish empty data frame to be filled with results
heterogeneity_sen <- data.frame(ri_t = as.numeric(),   # assumed pre-post correlation
                                I2 = as.numeric(), # I²
                                tau2 = as.numeric()) #tau²

# starting loop over 26 possible pre-post-correlations
for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  
  # compute the meta-analysis
  REM <- rma.mv(yi, vi, 
                data=data_pp_es, 
                random = ~ 1 | id # take clustered data into account
  )
  
  # Formula
  W <- diag(1/REM$vi)
  X <- model.matrix(REM)
  P <- W-W%*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  I2 <- 100*REM$sigma2/(sum(REM$sigma2)+(REM$k - REM$p)/sum(diag(P)))
  tau2 <-REM$tau2
  
  # save estimates for sensitivity analysis
  heterogeneity_sen <- heterogeneity_sen %>%
    add_row(ri_t = ri_t, 
            I2 = I2,
            tau2 = tau2)
}

heterogeneity_sen %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))



W<-diag(1/REM$vi)
X<-model.matrix(REM)
P<- W-W%*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100*sum(REM$sigma2)/(sum(REM$sigma2)+ (REM$k-REM$p)/sum(diag(P)))

100*REM$sigma2/(sum(REM$sigma2)+(REM$k - REM$p)/sum(diag(P)))
#I? = 99.10 (97%) of the detected variation could be related to  true variation among studies


##calculating Q for the mean correlation
data_pp_es <- escalc(measure = "SMCR",
                     m1i=data_pp$m_post, m2i=data_pp$m_pre,
                     sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                     ni=data_pp$ni, ri=rep(.625, 33),
                     slab = data_pp$slab)
data_pp_es$id <- data_pp$id
REM <- rma.mv(yi, vi, 
              data=data_pp_es, 
              random = ~ 1 | id) # take clustered data into account

REM
############################################################################## #
####MODERATION ANALYSES ########################################################
############################################################################## #


## REPRESENTATION #######################
#graphical representation
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$rep_g <- as.factor(data$rep_graphical)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = rep_g)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = .037, p = .639

#numeric representation
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$rep_n <- as.factor(data$rep_numeric)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = rep_n)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.037, p = .631

#highlighting representation
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$rep_h <- as.factor(data$rep_highlighting)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = rep_h)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator)  #mean = .100, p = .863

#text-based representation
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$rep_t <- as.factor(data$rep_text_based)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = rep_t)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = .625, p = .494

#mono vs. multiple representation
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$rep_nr <- as.factor(data$rep_nr)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = rep_nr)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = .278, p = .162

## ORDER ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$order<- as.factor(data$FB_order)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = order)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = .267, p = .413

## ORDER HIGH ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$order_hi <- as.factor(ifelse(data$FB_order == 2, 1, 0))
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = order_hi)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.269, p = .413

## ORDER HIGH-LOW ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$order_hilow <- ifelse(data$FB_order == 3, 1, 0)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = order_hilow)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = .267, p = .414

## SPECIFICITY ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$spec <- as.factor(data$FB_specificity)
  data_pp_es$holistic <- ifelse(data$FB_specificity == 1, 1, 0)
  data_pp_es$analytic <- ifelse(data$FB_specificity == 2, 1, 0)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = spec)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = .396, p = .424

#sub categories:
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 31),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$holistic <- ifelse(data$FB_specificity == 1, 1, 0)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = holistic)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

skim(sensitivity_moderator)
summary(sensitivity_moderator)


sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 31),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$analytic <- ifelse(data$FB_specificity == 2, 1, 0)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = analytic)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

skim(sensitivity_moderator)
summary(sensitivity_moderator)

## TOOL NUMBERS ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$tool <- as.factor(data$FB_tool_numbers)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = tool)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.124, p = .002

table(data$FB_tool_numbers) #CohViz mit 12 überrepräsentiert

#post-hoc
CohViz <- rma(yi, vi, data=data_pp_es, subset = tool =="1") #estimate = .52, se = .14, p = .003
CohViz

criterion <- rma(yi, vi, data=data_pp_es, subset = tool =="2") #n.s.
criterion

writingPal <- rma(yi, vi, data=data_pp_es, subset = tool =="3") #estimate = .204, se = .040, p < .0001
writingPal

NCWrite <- rma(yi, vi, data=data_pp_es, subset = tool =="4") #estimate = .706, se = .265, p = .008
NCWrite

eRevise <- rma(yi, vi, data=data_pp_es, subset = tool =="5") #estimate = .283, se = .134, p = .035
eRevise

cRaterML <- rma(yi, vi, data=data_pp_es, subset = tool =="6") #estimate = .352, se = .063, p < .001
cRaterML

MIWrite <- rma(yi, vi, data=data_pp_es, subset = tool =="7") #estimate = .336, se = .168, p = .046
MIWrite

ChatGPT <- rma(yi, vi, data=data_pp_es, subset = tool =="8") #estimate = -.665, se = .049, p <.001
ChatGPT

dat.comp <- data.frame(estimate = c(coef(CohViz), coef(criterion), coef(writingPal), coef(NCWrite),coef(eRevise), coef(cRaterML), coef(MIWrite), coef(ChatGPT)), stderror = c(CohViz$se, criterion$se, writingPal$se, NCWrite$se, eRevise$se, cRaterML$se, MIWrite$se, ChatGPT$se),
                       meta = c("1","2", "3", "4", "5", "6", "7", "8"), tau2 = round(c(CohViz$tau2, criterion$tau2, writingPal$tau2, NCWrite$tau2, eRevise$tau2, cRaterML$tau2, MIWrite$tau2, ChatGPT$tau2),3))
dat.comp 


rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=dat.comp, digits=3) #Writing Pal: -.031, p = .034; ChatGPT: -1.18, p < .001; Intercept (CohViz?): .052, p < .001


data_pp_es$tool <- data$FB_tool
res <- rma(yi, vi, mods = ~ factor(tool), data=data_pp_es)
res

## PRIOR KNOWLEDGE ############################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$WQP_pre <- as.factor(data$WQP_pre)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = WQP_pre)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.016, p < .001

#Expertise ####noch nicht angepasst
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 24),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$Expert <- as.factor(c(0, 0, 0, 1, 1, 1, 0,0,0,0, # WQP_FB_pre: 0 = under 60% 
                                   1, 0, 0, 0, 0, 0,0, 1, 0,1,1,0,0,0))# (low prior knowledge)
  # 1 = over 60% (high prior knowledge)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = Expert)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

skim(sensitivity_moderator)
summary(sensitivity_moderator)

##SETTING######################
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$setting <- as.factor(data$setting)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = setting)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.214, p = .421

##EDUCATIONAL LEVEL#####################
#dummy coded
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$edu <- as.factor(data$education_dummy)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = edu)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = .483, p = .137


##POST MEASURE (second vs. final draft)
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$post_measure <- as.factor(data$post_measure)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = post_measure)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.252, p = .434

##AMOUNT
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$amount <- as.factor(data$amount)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = amount)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.075, p = .803

###CODING
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$coding <- as.factor(data$coding)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = coding)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.266, p = .314

##OUTCOME ORDER
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$order_outcome <- as.factor(data$order_outcome)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = order_outcome)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.040, p = .848

##EXPERIMENT
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$experiment <- as.factor(data$experiment)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = experiment)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.695, p = .085

##SYSTEM
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$system <- as.factor(data$system_type)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = system)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.107, p = .341

##teacher_effects
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$teacher <- as.factor(data$teacher_effects)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = teacher)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.190, p = .491

##validated_tool
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$validated_tool <- as.factor(data$validated_tool)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = validated_tool)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = 1.19, p = .041


##reliability_measurement
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$reliability <- as.factor(data$reliability_measurement)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = reliability)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = .087, p = .775


##treatment_fidelity
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$fidelity <- as.factor(data$treatment_fidelity)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = fidelity)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean < .01, p = .996

##writing_tests
sensitivity_moderator <- data.frame(
  ri_t = as.numeric(),
  beta = as.numeric(),
  se = as.numeric(),
  ci.lb = as.numeric(),
  ci.ub = as.numeric(),
  pvalue = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 33),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$tests <- as.factor(data$writing_tests)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = tests)
  
  # save estimates for sensitivity analysis
  sensitivity_moderator <- sensitivity_moderator %>%
    add_row(ri_t = ri_t, 
            beta = RMA$beta[,1][2],
            se = RMA$se[2],
            ci.lb = RMA$ci.lb[2],
            ci.ub = RMA$ci.ub[2],
            pvalue = RMA$pval[2]
    )
}

sensitivity_moderator %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

summary(sensitivity_moderator) #mean = -.188, p = .577
