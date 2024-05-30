library(tidyverse)
library(here)
library(haven)
library(metafor)
library(kableExtra)

dataN22 <- read_sav("C://Users//Salome Wagner//OneDrive - UT Cloud//Metaanalyse//cbf-writing//02_data//03_SPSS files//Metaanalysis_prepost_N22.sav")

#### LOOP BEGIN ################################################################


data_pp <- data.frame(
  sampleNr = 1:22,
  id = c(1,1,1,2,3,3,4,4,5,5,6,6,6,7,8,9,10,11,11,11, 12,13),
  m_pre = dataN22$M_pre,
  m_post = dataN22$M_post,
  sd_pre = dataN22$SD_pre,
  sd_post = dataN22$SD_post,
  ni = dataN22$N, # .50 < ri < .75 (from open data (Burkhart et al., 2020): ri=.72)
  slab = dataN22$author)

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
                          sei.f22 = as.numeric()
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es <- escalc(measure = "SMCR",
                       m1i=data_pp$m_post, m2i=data_pp$m_pre,
                       sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                       ni=data_pp$ni, ri=rep(ri_t, 22),
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
            sei.f22 = sqrt(data_pp_es$vi[22])
    )
}
## LOOP END ################################################################## #

sensitivity %>%
  dplyr::select(ri_t, beta, se, pvalue) %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


# compute mean, min and max of ES, CI and pvalue from meta-analysis
sensitivity %>%
  dplyr::summarise(ES_mean         = mean(beta),
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
  pivot_longer(c(2:45),                       # reshape data from
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
                     ni=data_pp$ni, ri=rep(.625, 22),
                     slab = data_pp$slab)
# append the id variable
data_pp_es$id <- data_pp$id

REM <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id)

###FOREST PLOT
forest(REM)


############################################################################## #
#### PUBLICATION BIAS ##########################################################
############################################################################## #

####PUBLICATION BIAS
##Funnel plot
funnel(REM, legend = T)

##Trim & fill
rma_trimfill <- rma(yi, vi, data=dat_es) # compute random-effects model without
# clustering, as trimfill() can't handle
# these objects
taf_overall <- trimfill(rma_trimfill)
taf_overall 

funnel(taf_overall, legend = T)
# no missing studies on the left side, change of effect, now the effect of feedback on writing is sign. g = .5833, p = .0001, CI 95% [0.2893, 0.8772]
# Heterogeneity: Q(df = 21) = 281.2100, p-val < .0001

##Egger's test
regtest(rma_trimfill) #asymmetry z = 5.5140, p < .0001, b = -0.3283 (CI: -0.6772, 0.0206)

###HETEROGENEITY
W<-diag(1/REM$vi)
X<-model.matrix(REM)
P<- W-W%*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100*sum(REM$sigma2)/(sum(REM$sigma2)+ (REM$k-REM$p)/sum(diag(P)))

100*REM$sigma2/(sum(REM$sigma2)+(REM$k - REM$p)/sum(diag(P)))
#I? = 97.49 (97%) of the detected variation could be related to  true variation among studies




############################################################################## #
####MODERATION ANALYSES ########################################################
############################################################################## #


## REPRESENTATION #######################
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
                       ni=data_pp$ni, ri=rep(ri_t, 22),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$rep <- as.factor(dataN22$FB_representation)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = rep)
  
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
                       ni=data_pp$ni, ri=rep(ri_t, 22),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$order<- as.factor(dataN22$FB_order)
  
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

skim(sensitivity_moderator)


## ORDER LOW ############################
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
                       ni=data_pp$ni, ri=rep(ri_t, 22),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$order_low <- as.factor(ifelse(dataN22$FB_order == 1, 1, 0))
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = order_low)
  
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
                       ni=data_pp$ni, ri=rep(ri_t, 22),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$order_hi <- as.factor(ifelse(dataN22$FB_order == 2, 1, 0))
  
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

skim(sensitivity_moderator)


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
                       ni=data_pp$ni, ri=rep(ri_t, 22),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$order_hilow <- ifelse(dataN22$FB_order == 3, 1, 0)
  
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

skim(sensitivity_moderator)


## SPECIFITY ############################
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
                       ni=data_pp$ni, ri=rep(ri_t, 22),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$spec <- as.factor(dataN22$FB_specificity)
  
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
                       ni=data_pp$ni, ri=rep(ri_t, 22),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$tool <- as.factor(dataN22$FB_tool_numbers)
  
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

skim(sensitivity_moderator)



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
                       ni=data_pp$ni, ri=rep(ri_t, 22),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$WQP_pre <- as.factor(dataN22$WQP_pre)
  
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
                       ni=data_pp$ni, ri=rep(ri_t, 22),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$setting <- as.factor(dataN22$setting)
  
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

skim(sensitivity_moderator)
summary(sensitivity_moderator)


##EDUCATIONAL LEVEL#####################
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
                       ni=data_pp$ni, ri=rep(ri_t, 22),
                       slab = data_pp$slab)
  # append the id variable
  data_pp_es$id <- data_pp$id
  data_pp_es$education <- as.factor(dataN22$education)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = education)
  
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
