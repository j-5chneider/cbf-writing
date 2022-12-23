data <- read_sav("C://Users//Salome Wagner//OneDrive - UT Cloud//Metaanalyse//cbf-writing//02_data//03_SPSS files//Metaanalysis_prepost_N22.sav")

dat <- data.frame(                            
  id = c(1,1,1,2,3,3,4,4,5,5,6,6,6,7,8,9,10,11,11,11, 12,13),
  m_pre = data$M_pre,
  m_post = data$M_post,
  sd_pre = data$SD_pre,
  sd_post = data$SD_post,
  ni = data$N,
  slab = data$author)
dat

sensitivity <- data.frame(ri = as.numeric(),
                          intrcpt = as.numeric(),
                          pvalue = as.numeric(),
                          yi.f01 = as.numeric(),  # ES of the single studies
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
                          vi.f01 = as.numeric(),  # SEs of ES of the single studies
                          vi.f02 = as.numeric(),
                          vi.f03 = as.numeric(),
                          vi.f04 = as.numeric(),
                          vi.f05 = as.numeric(),
                          vi.f06 = as.numeric(),
                          vi.f07 = as.numeric(),
                          vi.f08 = as.numeric(),
                          vi.f09 = as.numeric(),
                          vi.f10 = as.numeric(),
                          vi.f11 = as.numeric(),
                          vi.f12 = as.numeric(),
                          vi.f13 = as.numeric(),
                          vi.f14 = as.numeric(),
                          vi.f15 = as.numeric(),
                          vi.f16 = as.numeric(),
                          vi.f17 = as.numeric(),
                          vi.f18 = as.numeric(),
                          vi.f19 = as.numeric(),
                          vi.f20 = as.numeric(),
                          vi.f21 = as.numeric(),
                          vi.f22 = as.numeric(),
                          beta = as.numeric(),  # meta-analytic ES
                          vb = as.numeric())    # SE of meta-analytic ES

  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  dat_es<- escalc(measure = "SMCR",                                       
                      m1i=dat$m_post, m2i=dat$m_pre,
                      sd1i=dat$sd_pre, sd2i=dat$sd_post,               ###################@Jürgen, stimmt das, dass sd1i SD_PRE ist, obwohl m1i M_POST ist?
                      ni=dat$ni, ri=c(rep(.625,22)),
                      slab = dat$slab)
  dat_es$id <- dat$id
  dat_es
  
  REM <- rma.mv(yi, vi, data=dat_es, random = ~ 1 | id)
  
  sensitivity <- sensitivity %>%
    add_row(ri = .625, 
            intrcpt = REM$beta[1,1],
            pvalue = REM$pval,
            yi.f01 = REM$yi.f[1],
            yi.f02 = REM$yi.f[2],
            yi.f03 = REM$yi.f[3],
            yi.f04 = REM$yi.f[4],
            yi.f05 = REM$yi.f[5],
            yi.f06 = REM$yi.f[6],
            yi.f07 = REM$yi.f[7],
            yi.f08 = REM$yi.f[8],
            yi.f09 = REM$yi.f[9],
            yi.f10 = REM$yi.f[10],
            yi.f11 = REM$yi.f[11],
            yi.f12 = REM$yi.f[12],
            yi.f13 = REM$yi.f[13],
            yi.f14 = REM$yi.f[14],
            yi.f15 = REM$yi.f[15],
            yi.f16 = REM$yi.f[16],
            yi.f17 = REM$yi.f[17],
            yi.f18 = REM$yi.f[18],
            yi.f19 = REM$yi.f[19],
            yi.f20 = REM$yi.f[20],
            yi.f21 = REM$yi.f[21],
            yi.f22 = REM$yi.f[22],
            vi.f01 = REM$vi.f[1],
            vi.f02 = REM$vi.f[2],
            vi.f03 = REM$vi.f[3],
            vi.f04 = REM$vi.f[4],
            vi.f05 = REM$vi.f[5],
            vi.f06 = REM$vi.f[6],
            vi.f07 = REM$vi.f[7],
            vi.f08 = REM$vi.f[8],
            vi.f09 = REM$vi.f[9],
            vi.f10 = REM$vi.f[10],
            vi.f11 = REM$vi.f[11],
            vi.f12 = REM$vi.f[12],
            vi.f13 = REM$vi.f[13],
            vi.f14 = REM$vi.f[14],
            vi.f15 = REM$vi.f[15],
            vi.f16 = REM$vi.f[16],
            vi.f17 = REM$vi.f[17],
            vi.f18 = REM$vi.f[18],
            vi.f19 = REM$vi.f[19],
            vi.f20 = REM$vi.f[20],
            vi.f21 = REM$vi.f[21],
            vi.f22 = REM$vi.f[22],
            beta = REM$beta[,1],
            vb = REM$vb[1,1])


## FOREST PLOT with means of all meta-analyses from sensitivity analysis
# summarize results from all meta-analyses
sensitivity_forest <- sensitivity %>%
  dplyr::summarize(across("yi.f01":"vb", mean))
sensitivity$intrcpt
sensitivity_forest                           
############################@Jürgen, habe ich das richtig verstanden, dass yi jetzt die Effektstärken sind? also yi.f01 der Effekt für die 1. Studie, yi.f02 für die 2. usw.?

# take rma object from last loop and fill in mean these values
REM$yi.f[1:22] <- as.numeric(sensitivity_forest[1, 1:22])
REM$vi.f       <- as.numeric(sensitivity_forest[1, 23:44])
REM$beta[,1]   <- as.numeric(sensitivity_forest[1, 45])
REM$vb[1,1]    <- as.numeric(sensitivity_forest[1, 46])

REM
##result: g = .5445, p = .0061, CI 95% [0.1553, 0.9336] sign., medium effect
#Heterogeneity: Q(df = 21) = 215.3650, p < .0001

###FOREST PLOT
forest(REM)

####PUBLICATION BIAS
##Funnel plot
funnel(REM, legend = T)
rma_funnel <- rma(yi, vi, data=dat_es)
rma_funnel

##Trim & fill
taf_overall <- trimfill(rma_funnel)
taf_overall 

funnel(taf_overall, legend = T)
#no missing studies on the left side, change of effect, now the effect of feedback on writing is sign. g = .5833, p = .0001, CI 95% [0.2893, 0.8772]
#Heterogeneity: Q(df = 21) = 281.2100, p-val < .0001

##Egger's test
regtest(rma_funnel) #asymmetry z = 5.5140, p < .0001, b = -0.3283 (CI: -0.6772, 0.0206)

###HETEROGENEITY
W<-diag(1/REM$vi)
X<-model.matrix(REM)
P<- W-W%*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100*sum(REM$sigma2)/(sum(REM$sigma2)+ (REM$k-rREM$p)/sum(diag(P)))

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
