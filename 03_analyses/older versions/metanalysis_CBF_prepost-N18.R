library(Matrix)
library(metafor)
library(tidyverse)
library(haven)
library(psych)
install.packages("riot")
library(riot)
data <- read_sav("C:/Users/Salome Wagner/OneDrive - UT Cloud/Metaanalyse/Analyse und results/Metaanalysis_prepost_N18.sav")

####Schlaufe
data_pp <- data.frame(
  id = c(1,1,1,2,3,3,4,4,5,5,6,7,8,9,9,9,10,11),
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
                          yi.f14 = as.numeric(),
                          yi.f15 = as.numeric(),
                          yi.f16 = as.numeric(),
                          yi.f17 = as.numeric(),
                          yi.f18 = as.numeric(),
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
                          vi.f14 = as.numeric(),
                          vi.f15 = as.numeric(),
                          vi.f16 = as.numeric(),
                          vi.f17 = as.numeric(),
                          vi.f18 = as.numeric(),
                          beta = as.numeric(),  # meta-analytische ES
                          vb = as.numeric()    # SE meta-alyritsche ES
)

for (ri_t in seq(from = .50, to=.75, by=.01)) {
  ###compute effect sizes for writing quality
  # effect sizes from within-subjects data
  data_pp_es<- escalc(measure = "SMCR",
                   m1i=data_pp$m_post, m2i=data_pp$m_pre,
                   sd1i=data_pp$sd_pre, sd2i=data_pp$sd_post,
                   ni=data_pp$ni, ri=rep(ri_t, 18),
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
            beta = rma_overall_clustered$beta[,1],
            vb = rma_overall_clustered$vb[1,1]
    )
}
## Schlaufe ENDE ####################################################################################

## FOREST PLOT with means of all meta-analyses from sensitivity analysis
# summarize results from all meta-analyses
sensitivity_forest <- sensitivity %>%
  dplyr::summarize(across("yi.f01":"vb", mean))
sensitivity$intrcpt
sensitivity_forest

# take rma object from last loop and fill in mean these values
rma_overall_clustered$yi.f[1:18] <- as.numeric(sensitivity_forest[1, 1:18])
rma_overall_clustered$vi.f       <- as.numeric(sensitivity_forest[1, 19:36])
rma_overall_clustered$beta[,1]   <- as.numeric(sensitivity_forest[1, 37])
rma_overall_clustered$vb[1,1]    <- as.numeric(sensitivity_forest[1, 38])

rma_overall_clustered
funnel(rma_overall_clustered)


##Heterogeneity
W<-diag(1/rma_overall_clustered$vi)
X<-model.matrix(rma_overall_clustered)
P<- W-W%*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100*sum(rma_overall_clustered$sigma2)/(sum(rma_overall_clustered$sigma2)+ (rma_overall_clustered$k-rma_overall_clustered$p)/sum(diag(P)))

100*rma_overall_clustered$sigma2/(sum(rma_overall_clustered$sigma2)+(rma_overall_clustered$k - rma_overall_clustered$p)/sum(diag(P)))
###########ZUSATZ
##Ergebnisansicht
#sensitivity
#summary(sensitivity)
#rma_overall_clustered
#meta_erg<-rma_overall_clustered
#coef(summary(meta_erg))

# Plots der Effektstärken
ggplot(sensitivity, aes(x=ri_t, y=intrcpt)) +
  geom_point() +                            # schwarze Punkte = Effektstärken
  geom_point(aes(y=pvalue), color="red") +  # rote Punkte = p-Werte
  scale_y_continuous(limits = c(0,.5)) +
  theme_light()

forest(rma_overall_clustered)

glimpse(sensitivity)

#### Durchscnittlichen Effekt berechnen
#mean(results_sensitivity$intrcpt)
#mean(results_sensitivity$ri_t)
#mean(results_sensitivity$pvalue)


#FUNNEL PLOT

# trim and fill method to check for publication bias
rma_overall_clustered$yi[1:18] <- as.numeric(sensitivity_forest[1, 1:18])
rma_overall_clustered$vi       <- as.numeric(sensitivity_forest[1, 19:36])


# Funnel plot asymmetry
# Ranktest
ranktest(REM1ig)

#contour-enhanced funnel plot
funnel(rma_funnel, level=c(70,90, 95, 99), shade=c("white", "gray55", "gray75", "gray45"), refline=0)
#Egger's test
regtest(rma_funnel) #Assymetrie ist sign.

rma_funnel <- rma(yi, vi, data=data_pp_es)
taf_overall <- trimfill(rma_funnel)
taf_overall

funnel(taf_overall, legend = T)
rma_funnel

#Fail-safe N and weight function (nicht sinnvoll bei nicht sign. Daten?)
# Fail-safe N (Rosenberg)
#fsn(yi=yi, vi=vi, type="Rosenberg", alpha=.05, data=data_pp_es)

######clustering
###is nesting within studies necessary?
#ohne Cluster
full.model <- rma.mv(yi = yi,
                     V = vi,
                     slab = id,
                     data = data_pp_es,
                     test = "t",
                     method = "REML")

##mit cluster
model.removed <- rma.mv(yi = yi,
                     V = vi,
                     slab = id,
                     data = data_pp_es,
                     random = ~ 1 | id,
                     test = "t",
                     method = "REML")

#Vergleich der beiden Modelle
anova(full.model, model.removed) #sign. p-Wert. removed-model is preferred over the full model that indicated that there are substantial differences between papers (id).



############moderators
data_pp_es$rep <- as.factor(data$FB_representation)
data_pp_es$order <- as.factor(data$FB_order)
data_pp_es$order_low <- ifelse(data$FB_order == 1, 1, 0)
data_pp_es$order_hi <- ifelse(data$FB_order == 2, 1, 0)
data_pp_es$order_hilow <- ifelse(data$FB_order == 3, 1, 0)
data_pp_es$spec <- as.factor(data$FB_specificity)
data_pp_es$tool <- as.factor(data$FB_tool_numbers)
data_pp_es$WQP_pre<-as.factor(data$WQP_pre)
data_pp_es$WQP_preD <- as.factor(c(0,0,0,1,0,0,1,1,0,1,0,0,1,0,1,1,0,0))



representation<-rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = rep)
representation
order<-rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = order)
order
order_low<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_low)
order_low
order_high<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_hi)
order_high
order_hilow<-rma.mv(yi, vi, data=dat, random = ~ 1 | id, mods = order_hilow)
order_hilow
specificity<-rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = spec)
specificity
tool<-rma.mv(yi, vi, data=data_pp_es, random = ~ 1 | id, mods = tool)
tool
Vorwissen <- rma.mv(yi, vi, data = data_pp_es, random = ~ 1 | id, mods = WQP_pre)
Vorwissen
Vorwissen_hilow<-rma.mv(yi, vi, data = data_pp_es, random = ~ 1 | id, mods = WQP_preD)
Vorwissen_hilow

