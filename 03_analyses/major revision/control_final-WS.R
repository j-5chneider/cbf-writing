data <- read_sav("C://Users//Salome Wagner//OneDrive - UT Cloud//Metaanalyse//submission EPR//major revision//Nacherhebung//Metaanalysis_control_neu_final-WS.sav")
#### LOOP BEGIN ################################################################
datT <- data.frame(
  sampleNr = 1:14,
  id = c(1,1,1,2,2,3,3,4,4,5,6,6,7,8),
  m_pre = data$M_Feedback,
  m_post = data$M_Feedback2,
  sd_pre = data$SD_Feedback,
  sd_post = data$SD_Feedback2,
  ni = data$N_Feedback, # .50 < ri < .75 (original: .72)
  slab = data$author)

datC <- data.frame(
  sampleNr = 1:14,
  id = c(1,1,1,2,2,3,3,4,4,5,6,6,7,8),
  m_pre = data$M_NoFeedback,
  m_post = data$M_NoFeedback2,
  sd_pre = data$SD_NoFeedback,
  sd_post = data$SD_NoFeedback2,
  ni = data$N_noFeedback, # .64 < ri < .89 (original: .86)
  slab = data$author)

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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
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
            sei.f14 = sqrt(dat$vi[14]),
    )
}

## LOOP END ################################################################## #


sensitivity %>%
  dplyr::select(ri_t, ri_c, beta, se, pvalue) %>%
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
                   CI_lower_mean = mean(beta-(1.96*se)),
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
                   se_mean = mean(sei),
                   se_min = min(sei),
                   se_max = max(sei),
                   CI_lower_mean = mean(yi-(1.96*sei)),
                   CI_lower_min  = min(yi-(1.96*sei)),
                   CI_lower_max  = max(yi-(1.96*sei)),
                   CI_upper_mean = mean(yi+(1.96*sei)),
                   CI_upper_min  = min(yi+(1.96*sei)),
                   CI_upper_max  = max(yi+(1.96*sei)))  %>%
  right_join(datT[c("sampleNr", "slab")],., by = "sampleNr") %>%
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

dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)

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

funnel(taf_overall, legend = T) #1 missing study on the right side (se = 2.5068) --> warum steht hier k = 15 im output?
#tau² = 0.3226 (se = .1585); I² = 92.54%; H²=13.4; Q(df) = 418.6696, p < .001

##Egger's test
regtest(rma_trimfill)  #keine assymetrie: z = -1.2397, p = .215, b = .5502, CI 95% [-0.0663, 1.1667]

###HETEROGENEITY
# Establish empty data frame to be filled with results
heterogeneity_sen <- data.frame(ri_t = as.numeric(),   # assumed pre-post correlation (treatment group)
                                ri_c = as.numeric(),   # assumed pre-post correlation (control group)
                                I2 = as.numeric())     # I²

# starting loop over 26 possible pre-post-correlations
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  REM <- rma.mv(yi, vi, 
                data=dat, 
                random = ~ 1 | id # take clustered data into account
  )
  
  # Formula
  W <- diag(1/REM$vi)
  X <- model.matrix(REM)
  P <- W-W%*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  I2 <- 100*REM$sigma2/(sum(REM$sigma2)+(REM$k - REM$p)/sum(diag(P)))
  
  # save estimates for sensitivity analysis
  heterogeneity_sen <- heterogeneity_sen %>%
    add_row(ri_t = ri_t, 
            ri_c = ri_t+.14,
            I2 = I2)
}

heterogeneity_sen %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


W<-diag(1/REM$vi)
X<-model.matrix(REM)
P<- W-W%*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100*sum(REM$sigma2)/(sum(REM$sigma2)+ (REM$k-REM$p)/sum(diag(P)))

100*REM$sigma2/(sum(REM$sigma2)+(REM$k - REM$p)/sum(diag(P)))

#calculating Q for the mean correlation
datT_es<- escalc(measure = "SMCR",
                 m1i=datT$m_post, m2i=datT$m_pre,
                 sd1i=datT$sd_pre, sd2i=datT$sd_post,
                 ni=datT$ni, ri=rep(.625, 14),
                 slab = datT$slab)

datT_es$id <- datT$id

datC_es <- escalc(measure="SMCR",
                  m1i=datC$m_post, m2i=datC$m_pre,
                  sd1i=datC$sd_pre, sd2i=datC$sd_post,
                  ni=datC$ni, ri=rep((.765), 14),
                  slab = datC$slab)
datC_es$id<-datC$id

dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)

dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
REM <- rma.mv(yi, vi, 
              data=dat, 
              random = ~ 1 | id # take clustered data into account
)
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$rep_g <- as.factor(data$rep_graphical)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = rep_g)
  
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

summary(sensitivity_moderator) #mean = .45, p = .099

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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$rep_n <- as.factor(data$rep_numeric)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = rep_n)
  
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

summary(sensitivity_moderator) #mean = .134, p = .774

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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$rep_h <- as.factor(data$rep_highlighting)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = rep_h)
  
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

summary(sensitivity_moderator) #mean = .22, p = .458

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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$rep_t <- as.factor(data$rep_text_based)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = rep_t)
  
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

summary(sensitivity_moderator) #mean = -.16, p = .733

#number of representations
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$rep_nr <- as.factor(data$rep_nr)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = rep_nr)
  
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

summary(sensitivity_moderator) #mean = .34, p = .209

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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$FB_order <- as.factor(data$FB_order)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = FB_order)
  
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

summary(sensitivity_moderator) #mean = -.52, p < .001

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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$order_hi <- as.factor(ifelse(data$FB_order == 2, 1, 0))
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_hi)
  
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

summary(sensitivity_moderator) #mean = -.50, p < .001

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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$order_hilow <- ifelse(data$FB_order == 3, 1, 0)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_hilow)
  
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

summary(sensitivity_moderator) #mean = -.33, p = .500

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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$spec <- as.factor(data$FB_specificity)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = spec)
  
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

summary(sensitivity_moderator) #mean = .33, p = .500


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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$tool_nr <- as.factor(data$FB_tool_numbers)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = tool_nr)
  
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

summary(sensitivity_moderator) #mean = -.134, p = .380

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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$WQP_pre <- as.factor(data$WQP_FB_pre)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = WQP_pre)
  
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

summary(sensitivity_moderator) #mean = -.155, p < .001

## EXPERT  ###################################### hier nicht angewandt
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
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(ri_t, 15),
                   slab = datT$slab)
  
  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((ri_t+.14), 15),
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,6,7,8,8,8)
  dat$Expert <- as.factor(c(0, 0, 0, 1, 1, 1, 0, # WQP_FB_pre: 0 = under 60% 
                            1, 0, 0, 0, 0, 0, 1, 1))# (low prior knowledge)
  # 1 = over 60% (high prior knowledge)
  
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = Expert)
  
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$setting <- as.factor(data$setting)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = setting)
  
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

summary(sensitivity_moderator) #mean = .49, p = .271

##EDUCATIONAL LEVEL dummy-coded#####################
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$edu <- as.factor(data$education_dummy)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = edu)
  
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

summary(sensitivity_moderator) #mean = -.486, p = .271

##POST MEASURE##############
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$post_measure <- as.factor(data$post_measure)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = post_measure)
  
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

summary(sensitivity_moderator) #mean = -.40, p = .391

##AMOUNT######
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$amount <- as.factor(data$amount)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = amount)
  
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

summary(sensitivity_moderator) #mean = -.157, p = .726

##CODING#######
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$coding <- as.factor(data$coding)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = coding)
  
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

summary(sensitivity_moderator) #mean = -.48, p = .095

##ORDER OUTCOME
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$order_outcome <- as.factor(data$order_outcome)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_outcome)
  
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

summary(sensitivity_moderator) #mean = .72, p = .105

##EXPERIMENT#########
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$experiment <- as.factor(data$experiment)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = experiment)
  
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

summary(sensitivity_moderator) #mean = -.24, p = .618


##SYSTEM############
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$system <- as.factor(data$system_type)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = system)
  
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

summary(sensitivity_moderator) #mean = -.196, p = .298

##TEACHER EFFECTS########
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$teacher <- as.factor(data$teacher_effects)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = teacher)
  
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

summary(sensitivity_moderator) #mean = -.368, p = .119

###VALIDATED TOOL###############
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$validated_tool <- as.factor(data$validated_tool)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = validated_tool)
  
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

summary(sensitivity_moderator) #mean = 1.12, p = .010


##RELIABILITY MEASURE###########
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$reliability <- as.factor(data$reliability_measurement)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = reliability)
  
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

summary(sensitivity_moderator) #mean = -.64, p = .005

#post-hoc:
#low
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$reliability_low <- ifelse(data$reliability_measurement == 0, 1, 0)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = reliability_low)
  
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

summary(sensitivity_moderator) #mean = .88, p = .116

#high
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$reliability_high <- ifelse(data$reliability_measurement == 1, 1, 0)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = reliability_high)
  
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

summary(sensitivity_moderator) #mean = .31, p = .508

#not specified
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$reliability_not <- ifelse(data$reliability_measurement == 2, 1, 0)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = reliability_not)
  
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

summary(sensitivity_moderator) #mean = -.807, p = .037

##TREATMENT FIDELITY###########
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$fidelity <- as.factor(data$treatment_fidelity)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = fidelity)
  
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

summary(sensitivity_moderator) #mean = .477, p = .007

#post-hoc:
#not specified
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$fidelity_not <- ifelse(data$treatment_fidelity == 0, 1, 0)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = fidelity_not)
  
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

summary(sensitivity_moderator) #mean = -.94, p = .005

#few details/not so good
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$fidelity_low <- ifelse(data$treatment_fidelity == 1, 1, 0)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = fidelity_low)
  
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

summary(sensitivity_moderator) #mean = .244, p = .730

#lot of information
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$fidelity_high <- ifelse(data$treatment_fidelity == 2, 1, 0)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = fidelity_high)
  
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

summary(sensitivity_moderator) #mean = .76, p = .039

##WRITING TESTS###########
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
  
  dat$id <- c(1,1,1,2,2,3,3,4,4,5,6,6,7,8)
  dat$test <- as.factor(data$writing_tests)
  
  # compute the meta-analysis
  RMA <- rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = test)
  
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

summary(sensitivity_moderator) #mean = .46, p = .077