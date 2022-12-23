my_data <- read_sav("C://Users//Salome Wagner//OneDrive - UT Cloud//Metaanalyse//cbf-writing//02_data//03_SPSS files//Metaanalysis_control_N14.sav")
my_data


datT <- data.frame(
  id = c(1,1,1,2,3,3,4,4,5,6,7,8,8,8),
  m_pre = my_data$M_Feedback,
  m_post = my_data$M_Feedback2,
  sd_pre = my_data$SD_Feedback,
  sd_post = my_data$SD_Feedback2,
  ni = my_data$N_Feedback, 
  slab = my_data$author)
datT


datC <- data.frame(
  id = c(1,1,1,2,3,3,4,4,5,6,7,8,8,8),
  m_pre = my_data$M_NoFeedback,
  m_post = my_data$M_NoFeedback2,
  sd_pre = my_data$SD_NoFeedback,
  sd_post = my_data$SD_NoFeedback2,
  ni = my_data$N_noFeedback, 
  slab = my_data$author)
datC

sensitivity <- data.frame(ri_t = as.numeric(),
                          ri_c = as.numeric(),
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
                          beta = as.numeric(),  # meta-analytic ES 
                          vb = as.numeric()) #SE of meta-analytic ES

sensitivity

  ###compute effect sizes for writing quality
  # effect sizes from between-subjects data
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(.625, 14),            #########mittlere Korrelation (angelesen bei sensitivity)
                   slab = datT$slab)
  
  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((.765), 14),        #########mittlere Korrelation (angelesen bei sensitivity)
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,8,8,8)
  rma_overall_clustered <- rma.mv(yi, vi, data=dat, random = ~ 1 | id)
  
  sensitivity <- sensitivity %>%
    add_row(ri_t = .625, 
            ri_c = .765,
            intrcpt = rma_overall_clustered$beta[1,1],
            pvalue = rma_overall_clustered$pval,
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
            vi.f01 = rma_overall_clustered$vi.f[1],
            vi.f02 = rma_overall_clustered$vi.f[2],
            vi.f03 = rma_overall_clustered$vi.f[3],
            vi.f04 = rma_overall_clustered$vi.f[4],
            vi.f05 = rma_overall_clustered$vi.f[5],
            vi.f06 = rma_overall_clustered$vi.f[6],
            vi.f07 = rma_overall_clustered$vi.f[7],
            vi.f08 = rma_overall_clustered$vi.f[8],
            vi.f09 = rma_overall_clustered$vi.f[9],
            vi.f10 = rma_overall_clustered$vi.f[10],
            vi.f11 = rma_overall_clustered$vi.f[11],
            vi.f12 = rma_overall_clustered$vi.f[12],
            vi.f13 = rma_overall_clustered$vi.f[13],
            vi.f14 = rma_overall_clustered$vi.f[14],
            beta = rma_overall_clustered$beta[,1],
            vb = rma_overall_clustered$vb[1,1])

##summarize results from all meta-analyses 
sensitivity_forest <- sensitivity %>%
  dplyr::summarize(across("yi.f01":"vb", mean))
sensitivity$intrcpt
sensitivity_forest

##actual metaanalysis
rma_overall_clustered$yi.f[1:14] <- as.numeric(sensitivity_forest[1, 1:14])
rma_overall_clustered$vi.f       <- as.numeric(sensitivity_forest[1, 15:28])
rma_overall_clustered$beta[,1]   <- as.numeric(sensitivity_forest[1, 29])
rma_overall_clustered$vb[1,1]    <- as.numeric(sensitivity_forest[1, 30])

rma_overall_clustered
##result: g = .25, p = .0404, CI 95% 0.0109  0.4904]
#Heterogeneity: Q(df = 13) = 75.0329, p < .0001, I? = 75%


## forest plot
forest(rma_overall_clustered) #zeigt in diesem Fall die Werte f?r die letzte Korr


####PUBLICATION BIAS
##Funnel plot
funnel(rma_overall_clustered, legend = T)
rma_funnel <- rma(yi, vi, data=dat)
rma_funnel

##Trim & fill
taf_overall <- trimfill(rma_funnel)
taf_overall 

funnel(taf_overall, legend = T)
#one study was missing on the right side; after including: slightly change of effect, now the effect of feedback on writing is a little bi larger (sign.) g = .27, p = .015, CI 95% [0.0532, 0.4894]
#Heterogeneity: Q(df = 14) = 77.4414, p-val < .0001

##Egger's test
regtest(rma_funnel) #no asymmetry: Test for Funnel Plot Asymmetry: z = -1.6926, p = 0.091, b = 0.5862 (CI: 0.1426, 1.0298)

###HETEROGENEITY
W<-diag(1/rma_overall_clustered$vi)
X<-model.matrix(rma_overall_clustered)
P<- W-W%*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100*sum(rma_overall_clustered$sigma2)/(sum(rma_overall_clustered$sigma2)+ (rma_overall_clustered$k-rma_overall_clustered$p)/sum(diag(P)))

100*rma_overall_clustered$sigma2/(sum(rma_overall_clustered$sigma2)+(rma_overall_clustered$k - rma_overall_clustered$p)/sum(diag(P)))
#I^2 = 63.96 (64%) of the detected variation could be related to  true variation among studies                          ###################jetzt nur noch moderate statt große Heterogenität


####MODERATION ANALYSES
dat$rep <- as.factor(my_data$FB_representation)
dat$order<- as.factor(my_data$FB_order)
dat$order_low <- as.factor(ifelse(my_data$FB_order == 1, 1, 0))
dat$order_hi <- as.factor(ifelse(my_data$FB_order == 2, 1, 0))
dat$order_hilow <- ifelse(my_data$FB_order == 3, 1, 0)
dat$spec <- as.factor(my_data$FB_specificity)
dat$tool <- as.factor(my_data$FB_tool_numbers)
dat$WQP_pre <- as.factor(my_data$WQP_FB_pre)


rep<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = rep)
rep # not sign. g = .1285, p = .0846
order<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order)
order #sign. g = -.2881, p = .0114
low_order<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_low)
low_order #sign. g = .4593, p = .0136
hi_order<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_hi)
hi_order #nicht sign. g = .1882, p = .5089
hilow_order<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_hilow)
hilow_order #sign. g = -.4244, p = .0528
#rma.mv(yi, vi, data=dat, digits=2, random = ~ 1 | id, mods = ~ order_hi + order_hilow) # mit allen order. ABER: in lower order ist ja nur eine Studie
#rma.mv(yi, vi, data=dat%>%dplyr::filter(order != 1), digits=2, random = ~ 1 | id, mods = ~ order_hilow) # deshalb: ohne lower order
spec<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = spec)
spec #sign. g = .4244, p = .0528                                                        ##########nicht mehr sign.
tool<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = tool)
tool #nicht sign. g = .0536, p = .6369
vorwissen<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = WQP_pre)
vorwissen # nicht sign. g = .0102, p = .6929
