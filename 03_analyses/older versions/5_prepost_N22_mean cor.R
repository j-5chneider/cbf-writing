data <- read_sav(here("02_data/03_SPSS files/Metaanalysis_prepost_N22.sav"))

dat <- data.frame(                            
  id = c(1,1,1,2,3,3,4,4,5,5,6,6,6,7,8,9,10,11,11,11, 12,13),
  m_pre = data$M_pre,
  m_post = data$M_post,
  sd_pre = data$SD_pre,
  sd_post = data$SD_post,
  ni = data$N,
  slab = data$author)

  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  dat_es<- escalc(measure = "SMCR",                                       
                      m1i=dat$m_post, m2i=dat$m_pre,
                      sd1i=dat$sd_pre, sd2i=dat$sd_post,
                      ni=dat$ni, ri=c(rep(.625,22)),  # using mean correlation 
                      slab = dat$slab)                # from sensitivity analysis
  dat_es$id <- dat$id

  REM <- rma.mv(yi, vi, data=dat_es, random = ~ 1 | id)
  summary(REM)

###FOREST PLOT
forest(REM)

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


####MODERATION ANALYSES
dat_es$rep <- as.factor(data$FB_representation)
dat_es$order<- as.factor(data$FB_order)
dat_es$order_low <- as.factor(ifelse(data$FB_order == 1, 1, 0))
dat_es$order_hi <- as.factor(ifelse(data$FB_order == 2, 1, 0))
dat_es$order_hilow <- ifelse(data$FB_order == 3, 1, 0)
dat_es$spec <- as.factor(data$FB_specificity)
dat_es$tool <- as.factor(data$FB_tool_numbers)
dat_es$WQP_pre <- as.factor(data$WQP_pre)


rep<-rma.mv(yi, vi, data=dat_es, random = ~ 1 | id, mods = rep)
rep #not sign. g = .0080, p = .7105
order<-rma.mv(yi, vi, data=dat_es, random = ~ 1 | id, mods = order)
order #not sign. g = .0393, p = .8958
low_order<-rma.mv(yi, vi, data=dat_es, random = ~ 1 | id, mods = order_low)
low_order #not sign. g = -.3001, p = .5964
hi_order<-rma.mv(yi, vi, data=dat_es, random = ~ 1 | id, mods = order_hi)
hi_order #not sign. g = .2478, p = .5451
hilow_order<-rma.mv(yi, vi, data=dat_es, random = ~ 1 | id, mods = order_hilow)
hilow_order #not sign. g = -.0902, p = .8340
spec<-rma.mv(yi, vi, data=dat_es, random = ~ 1 | id, mods = spec)
spec # not sign. g = .2149, p = .6021                                  #############was bisher sign.
tool<-rma.mv(yi, vi, data=dat_es, random = ~ 1 | id, mods = tool)
tool #not sign. g = -.0862, p = .4513
vorwissen<-rma.mv(yi, vi, data=dat_es, random = ~ 1 | id, mods = WQP_pre)
vorwissen #not sign. g = .0259, p = .1707
