data <- read_sav("C://Users//Salome Wagner//OneDrive - UT Cloud//Metaanalyse//cbf-writing//02_data//03_SPSS files//Metaanalysis_prepost_N22.sav")

####loop
data_pp <- data.frame(
  id = c(1,1,1,2,3,3,4,4,5,5,6,6,6,7,8,9,10,11,11,11, 12,13),
  m_pre = data$M_pre,
  m_post = data$M_post,
  sd_pre = data$SD_pre,
  sd_post = data$SD_post,
  ni = data$N, # .50 < ri < .75 (original: .72)
  slab = data$author)
data_pp

sensitivity <- data.frame(ri_t = as.numeric(),
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
                          vb = as.numeric()    # SE of meta-analytic ES
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es<- escalc(measure = "SMCR",
                      m1i=data_pp$m_post, m2i=data_pp$m_pre,
                      sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                      ni=data_pp$ni, ri=rep(ri_t, 22),
                      slab = data_pp$slab)
  data_pp_es$id <- data_pp$id
  data_pp_es
  
  rma_overall_clustered <- rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id)
  
  sensitivity <- sensitivity %>%
    add_row(ri_t = ri_t, 
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
            yi.f15 = rma_overall_clustered$yi.f[15],
            yi.f16 = rma_overall_clustered$yi.f[16],
            yi.f17 = rma_overall_clustered$yi.f[17],
            yi.f18 = rma_overall_clustered$yi.f[18],
            yi.f19 = rma_overall_clustered$yi.f[19],
            yi.f20 = rma_overall_clustered$yi.f[20],
            yi.f21 = rma_overall_clustered$yi.f[21],
            yi.f22 = rma_overall_clustered$yi.f[22],
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
            vi.f15 = rma_overall_clustered$vi.f[15],
            vi.f16 = rma_overall_clustered$vi.f[16],
            vi.f17 = rma_overall_clustered$vi.f[17],
            vi.f18 = rma_overall_clustered$vi.f[18],
            vi.f19 = rma_overall_clustered$vi.f[19],
            vi.f20 = rma_overall_clustered$vi.f[20],
            vi.f21 = rma_overall_clustered$vi.f[21],
            vi.f22 = rma_overall_clustered$vi.f[22],
            beta = rma_overall_clustered$beta[,1],
            vb = rma_overall_clustered$vb[1,1]
    )
}
## loop end ####################################################################################

## FOREST PLOT with means of all meta-analyses from sensitivity analysis
# summarize results from all meta-analyses
sensitivity_forest <- sensitivity %>%
  dplyr::summarize(across("yi.f01":"vb", mean))
sensitivity$intrcpt
sensitivity_forest

# take rma object from last loop and fill in mean these values
rma_overall_clustered$yi.f[1:22] <- as.numeric(sensitivity_forest[1, 1:22])
rma_overall_clustered$vi.f       <- as.numeric(sensitivity_forest[1, 23:44])
rma_overall_clustered$beta[,1]   <- as.numeric(sensitivity_forest[1, 45])
rma_overall_clustered$vb[1,1]    <- as.numeric(sensitivity_forest[1, 46])

rma_overall_clustered
##result: g = .5442, p = .0067, CI 95% [0.1501, 0.9308] sign., medium effect
#Heterogeneity: Q(df = 21) = 276.4974, p < .0001

summary(sensitivity) #read mean ES from beta: .5445 (ranging from .54 to .55), p = .006 (ranging from .006 to .007), mean cor: r = .625 (ranging from .5 to .75)

## forest plot
forest(rma_overall_clustered) #shows in this case the values with the last correlation


#########CLUSTERING
###is nesting within studies necessary?
#without cluster
full.model <- rma.mv(yi = yi,
                     V = vi,
                     slab = id,
                     data = data_pp_es,
                     test = "t",
                     method = "REML")

#with cluster
model.removed <- rma.mv(yi = yi,
                        V = vi,
                        slab = id,
                        data = data_pp_es,
                        random = ~ 1 | id,
                        test = "t",
                        method = "REML")

#comparison of both models
anova(full.model, model.removed) #sign. p-value. removed-model is preferred over the full model that indicated that there are substantial differences between papers (id).



####PUBLICATION BIAS
##Funnel plot
funnel(rma_overall_clustered, legend = T)
rma_funnel <- rma(yi, vi, data=data_pp_es)
rma_funnel

##Trim & fill
taf_overall <- trimfill(rma_funnel)
taf_overall 

funnel(taf_overall, legend = T)
#no missing studies on the left side, change of effect, now the effect of feedback on writing is sign. g = .5833, p = .0001, CI 95% [0.2893, 0.8772]
#Heterogeneity: Q(df = 21) = 281.2100, p-val < .0001

##Egger's test
regtest(rma_funnel) #asymmetry z = 5.3689, p < .0001, b = -0.2222 (CI: -0.5538, 0.1094)

###HETEROGENEITY
W<-diag(1/rma_overall_clustered$vi)
X<-model.matrix(rma_overall_clustered)
P<- W-W%*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100*sum(rma_overall_clustered$sigma2)/(sum(rma_overall_clustered$sigma2)+ (rma_overall_clustered$k-rma_overall_clustered$p)/sum(diag(P)))

100*rma_overall_clustered$sigma2/(sum(rma_overall_clustered$sigma2)+(rma_overall_clustered$k - rma_overall_clustered$p)/sum(diag(P)))
#I? = 98.23% of the detected variation could be related to  true variation among studies


####MODERATION ANALYSES
data_pp_es$rep <- as.factor(data$FB_representation)
data_pp_es$order<- as.factor(data$FB_order)
data_pp_es$order_low <- as.factor(ifelse(data$FB_order == 1, 1, 0))
data_pp_es$order_hi <- as.factor(ifelse(data$FB_order == 2, 1, 0))
data_pp_es$order_hilow <- ifelse(data$FB_order == 3, 1, 0)
data_pp_es$spec <- as.factor(data$FB_specificity)
data_pp_es$tool <- as.factor(data$FB_tool_numbers)
data_pp_es$WQP_pre <- as.factor(data$WQP_pre)


rep<-rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = rep)
rep #not sign. g = .0087, p = .6229
order<-rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = order)
order #not sign. g = .0440, p = .8830
low_order<-rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = order_low)
low_order #not sign. g = -.3035, p = .5903
hi_order<-rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = order_hi)
hi_order #not sign. g = .2413, p = .5538
hilow_order<-rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = order_hilow)
hilow_order #not sign. g = -.0822, p = .8479
spec<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = spec)
spec #sign. g = .4258, p = .0428
tool<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = tool)
tool #not sign. g = .0471, p = .6947
vorwissen<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = WQP_pre)
vorwissen #not sign. g = .0430, p = .0948
#prior knowledge: expert level
data_pp$Expert <- c(1, 1, 1, 2, 1, 1, 2, 2, 1,2,1,1,1,1,1,1,2,1,2,2, 1, 1) #WQP_FB_pre: 1 = under 60% (lower prior knowledge, 2 = over 60% (higher prior knowledge))
data_pp_es$Expert <- as.factor(ifelse(data_pp$Expert==1, 0, 1))
expert_level <- rma.mv(yi, vi, data = data_pp_es, random = ~ 1 | id, mods = Expert)
expert_level #not sign. g = .2101, p = .0765
data_pp_es$Novize <- as.factor(ifelse(data_pp$Expert==2, 0, 1))
novize_level <- rma.mv(yi, vi, data = data_pp_es, random = ~ 1 | id, mods = Novize)
novize_level #not sign. g = -.2101, p = .0765

