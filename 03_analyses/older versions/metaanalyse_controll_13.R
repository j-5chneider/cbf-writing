library(Matrix)
library(metafor)
library(haven)
library(tidyverse)
my_data <- read_sav("C:/Users/Salome Wagner/OneDrive - UT Cloud/Metaanalyse/Analyse und results/Metaanalysis_control_N13.sav")
my_data
## Schlaufe ########################################################################################


datT <- data.frame(
  id = c(1,1,1,2,3,3,4,4,5,6,7,7,7),
  m_pre = my_data$M_Feedback,
  m_post = my_data$M_Feedback2,
  sd_pre = my_data$SD_Feedback,
  sd_post = my_data$SD_Feedback2,
  ni = my_data$N_Feedback, # .50 < ri < .75 (original: .72)
  slab = my_data$author)
datT


datC <- data.frame(
  id = c(1,1,1,2,3,3,4,4,5,6,7,7,7),
  m_pre = my_data$M_NoFeedback,
  m_post = my_data$M_NoFeedback2,
  sd_pre = my_data$SD_NoFeedback,
  sd_post = my_data$SD_NoFeedback2,
  ni = my_data$N_noFeedback, # .64 < ri < .89 (original: .86)
  slab = my_data$author)
datC

sensitivity <- data.frame(ri_t = as.numeric(),
                                  ri_c = as.numeric(),
                                  intrcpt = as.numeric(),
                                  pvalue = as.numeric(),
                                  yi.f01 = as.numeric(),  # ES der einzelnen Studien 
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
                                  vi.f01 = as.numeric(),  # SEs der ES der einzelnen Studien
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
                                  beta = as.numeric(),  # meta-analytische ES 
                                  vb = as.numeric()
                                  )
sensitivity
for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from between-subjects data
  datT_es<- escalc(measure = "SMCR",
                   m1i=datT$m_post, m2i=datT$m_pre,
                   sd1i=datT$sd_pre, sd2i=datT$sd_post,
                   ni=datT$ni, ri=rep(ri_t, 13),
                   slab = datT$slab)

  datT_es$id <- datT$id
  
  datC_es <- escalc(measure="SMCR",
                    m1i=datC$m_post, m2i=datC$m_pre,
                    sd1i=datC$sd_pre, sd2i=datC$sd_post,
                    ni=datC$ni, ri=rep((ri_t+.14), 13),
                    slab = datC$slab)
  datC_es$id<-datC$id
  
  dat <- data.frame(yi = datT_es$yi - datC_es$yi, vi = datT_es$vi + datC_es$vi)
  
  dat$id <- c(1,1,1,2,3,3,4,4,5,6,7,7,7)
  rma_overall_clustered <- rma.mv(yi, vi, data=dat, random = ~ 1 | id)
  
  sensitivity <- sensitivity %>%
    add_row(ri_t = ri_t, 
            ri_c = (ri_t+.14),
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
            beta = rma_overall_clustered$beta[,1],
            vb = rma_overall_clustered$vb[1,1]
    )
}
## Schlaufe ENDE ####################################################################################

####FOREST
#summarize results from all meta-analyses 
sensitivity_forest <- sensitivity %>%
  dplyr::summarize(across("yi.f01":"vb", mean))
sensitivity$intrcpt
sensitivity_forest

###actual metaanalysis
#summary(results_sensitivity)
#rma_overall <- rma(yi, vi, data=dat, digits=2)
#rma_overall_clustered <- rma.mv(yi, vi, data=dat, digits=2, random = ~ 1 | id)
#rma_overall_clustered

#mean(datT$ni)
#mean(datC$ni)

rma_overall_clustered$yi.f[1:13] <- as.numeric(sensitivity_forest[1, 1:13])
rma_overall_clustered$vi.f       <- as.numeric(sensitivity_forest[1, 14:26])
rma_overall_clustered$beta[,1]   <- as.numeric(sensitivity_forest[1, 27])
rma_overall_clustered$vb[1,1]    <- as.numeric(sensitivity_forest[1, 28])

rma_overall_clustered

summary(sensitivity)

## forest plot
forest(rma_overall_clustered)

##FUNNEL PLOT
funnel(rma_overall_clustered, legend = T)
rma_funnel <- rma(yi, vi, data=dat)
rma_funnel

rma_overall_clustered$yi[1:13] <- as.numeric(sensitivity_forest[1, 1:13])
rma_overall_clustered$vi       <- as.numeric(sensitivity_forest[1, 14:26])
taf_overall <- trimfill(rma_funnel)
taf_overall

funnel(taf_overall, legend = T)

regtest(rma_funnel)

## trim and fill method to check for publication bias
taf_overall <- trimfill(rma_funnel)
taf_overall
funnel(taf_overall, legend = T)


######clustering
###is nesting within studies necessary?
#ohne Cluster
full.model <- rma.mv(yi = yi,
                     V = vi,
                     data = dat,
                     test = "t",
                     method = "REML")

##mit cluster
model.removed <- rma.mv(yi = yi,
                        V = vi,
                        slab = id,
                        data = dat,
                        random = ~ 1 | id,
                        test = "t",
                        method = "REML")

#Vergleich der beiden Modelle
anova(full.model, model.removed) #sign. p-Wert. removed-model is preferred over the full model that indicated that there are substantial differences between papers (id).


###Heterogeneity
W<-diag(1/rma_overall_clustered$vi)
X<-model.matrix(rma_overall_clustered)
P<- W-W%*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100*sum(rma_overall_clustered$sigma2)/(sum(rma_overall_clustered$sigma2)+ (rma_overall_clustered$k-rma_overall_clustered$p)/sum(diag(P)))

100*rma_overall_clustered$sigma2/(sum(rma_overall_clustered$sigma2)+(rma_overall_clustered$k - rma_overall_clustered$p)/sum(diag(P)))

############moderators
dat$rep <- as.factor(my_data$FB_representation)
dat$order<- as.factor(my_data$FB_order)
dat$order_low <- as.factor(ifelse(my_data$FB_order == 1, 1, 0))
dat$order_hi <- as.factor(ifelse(my_data$FB_order == 2, 1, 0))
dat$order_hilow <- ifelse(my_data$FB_order == 3, 1, 0)
dat$spec <- as.factor(my_data$FB_specificity)
dat$tool <- as.factor(my_data$FB_tool_numbers)
dat$WQP_pre <- as.factor(my_data$WQP_FB_pre)
dat$WQP_pre

rep<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = rep)
rep
order<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order)
order
low_order<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_low)
low_order
hi_order<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_hi)
hi_order
hilow_order<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_hilow)
hilow_order
#rma.mv(yi, vi, data=dat, digits=2, random = ~ 1 | id, mods = ~ order_hi + order_hilow) # mit allen order. ABER: in lower order ist ja nur eine Studie
#rma.mv(yi, vi, data=dat%>%dplyr::filter(order != 1), digits=2, random = ~ 1 | id, mods = ~ order_hilow) # deshalb: ohne lower order
spec<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = spec)
spec
tool<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = tool)
tool


####subset analysis of tool (use of tool_d for dichotomous)
#based on https://www.metafor-project.org/doku.php/tips:comp_two_independent_estimates


res_CohViz <- rma(yi, vi, data=dat, subset = tool =="1")
res_CohViz

res_criterion <- rma(yi, vi, data=dat, subset = tool =="2")
res_criterion

res_writingPal <- rma(yi, vi, data=dat, subset = tool =="3")
res_writingPal

res_NCWrite <- rma(yi, vi, data=dat, subset = tool =="4")
res_NCWrite


dat.comp <- data.frame(estimate = c(coef(res_CohViz), coef(res_criterion), coef(res_writingPal), coef(res_NCWrite)), stderror = c(res_CohViz$se, res_criterion$se, res_writingPal$se, res_NCWrite$se),
                                    meta = c("1","2", "3", "4"), tau2 = round(c(res_CohViz$tau2, res_criterion$tau2, res_writingPal$tau2, res_NCWrite$tau2),3))
dat.comp



rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=dat.comp, digits=3)


#######Einfluss von Vorwissen
rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = WQP_pre)

datT$WQP_preD <- c(1, 1, 1, 2, 2, 2, 1, 2, 1, 1, 1, 2, 2) #1 = unter 60% (niedriges Vorwissen, 2 = ueber 60% (hohes Vorwissen))
dat$WQP_preTD  <- as.factor(ifelse(datT$WQP_preD==1, 0, 1))
meta_model_Vorwissen <- rma.mv(yi, vi, data = dat, random = ~ 1 | id, mods = WQP_preTD)
meta_model_Vorwissen

#Vorwissen als Faktor
#meta_model_Vorwissen <- rma.mv(yi, vi, data = dat, digits=2, random = ~ 1 | id, mods = ~ factor(WQP_preTD) -1)
#meta_model_Vorwissen

dat$WQP_preF<-as.factor(dat$WQP_preTD)
meta_model_Vorwissen <- rma.mv(yi, vi, data = dat, digits=2, random = ~ 1 | id, mods = WQP_preF)
meta_model_Vorwissen
##separate Analyse für Vorwissen
dat_meta_niedriges_Vorwissen <- subset(dat, (dat$WQP_preTD==0))
# meta_model_niedriges_Vorwissen <- rma(yi, vi, data = dat_meta_niedriges_Vorwissen)
# summary(meta_model_niedriges_Vorwissen)


dat_meta_hohes_Vorwissen <- subset(dat, (dat$WQP_preTD==1))
# meta_model_hohes_Vorwissen <- rma(yi, vi, data = dat_meta_hohes_Vorwissen)
# summary(meta_model_hohes_Vorwissen)


meta_model_niedriges_Vorwissen <- rma.mv(yi, vi, data = dat_meta_niedriges_Vorwissen, random = ~ 1 | id)
meta_model_niedriges_Vorwissen
meta_model_hohes_Vorwissen <- rma.mv(yi, vi, data = dat_meta_hohes_Vorwissen, random = ~ 1 | id)
meta_model_hohes_Vorwissen

forest(meta_model_Vorwissen)
forest(meta_model_niedriges_Vorwissen)
forest(meta_model_hohes_Vorwissen)


#trim and fill method to check for publication bias
rma_overall <- rma.mv(yi, vi, data=dat_within_es, digits=2)
taf_overall <- trimfill(rma_overall)
funnel(taf_overall, legend = T)

##
rma(yi, vi, data = dat, digits=2,  mods = WQP_preTD)
rma(yi, vi, data = dat_meta_niedriges_Vorwissen, digits=2)
rma(yi, vi, data = dat_meta_hohes_Vorwissen, digits=2)


