install.packages("DescTools")
library(DescTools)
data<-read_sav("C://Users//Salome Wagner//Downloads//ICC3_dummy_3.sav")

ratertab <- xtabs (~ data$rater1 + data$rater2)
ratertab

CohenKappa(ratertab, conf.level = 0.95)


rep_graph <- data[data$FBcharacteristics=="rep_graphical",]
view(rep_graph)
https://statistikguru.de/rechner/cohens-kappa-zwei-rater-berechnen.html

ICC_graph12<-subset(rep_graph[,c("rater1","rater2")])#.91
ICC_graph13<-subset(rep_graph[,c("rater1","rater3")])#.81
ICC_graph23<-subset(rep_graph[,c("rater2","rater3")])#.90
irr::icc(ICC_graph12, model = c("twoway"), type = c("agreement"), unit = c("single")) 
irr::icc(ICC_graph13, model = c("twoway"), type = c("agreement"), unit = c("single")) 
irr::icc(ICC_graph23, model = c("twoway"), type = c("agreement"), unit = c("single")) 

rep_numeric<- data[data$FBcharacteristics=="rep_numeric",]
ICC_num12<-subset(rep_numeric[,c("rater1","rater2")])#.40
ICC_num13<-subset(rep_numeric[,c("rater1","rater3")])#1
ICC_num23<-subset(rep_numeric[,c("rater2","rater3")])#.40
irr::icc(ICC_num12, model = c("twoway"), type = c("agreement"), unit = c("single")) 
irr::icc(ICC_num13, model = c("twoway"), type = c("agreement"), unit = c("single")) 
irr::icc(ICC_num23, model = c("twoway"), type = c("agreement"), unit = c("single"))
view(rep_numeric)

rep_highlighting<- data[data$FBcharacteristics=="rep_highlighting",]
ICC_highl12<-subset(rep_highlighting[,c("rater1","rater2")])#-.06
ICC_highl13<-subset(rep_highlighting[,c("rater1","rater3")])#-.06
ICC_highl23<-subset(rep_highlighting[,c("rater2","rater3")])#1
irr::icc(ICC_highl12, model = c("twoway"), type = c("agreement"), unit = c("single")) 
irr::icc(ICC_highl13, model = c("twoway"), type = c("agreement"), unit = c("single")) 
irr::icc(ICC_highl23, model = c("twoway"), type = c("agreement"), unit = c("single"))
view(rep_highlighting)

rep_textBased
rep_MR
low_order
high_order
both_order
specificity
education
setting
country
genre_essay
genre_scientific
genre_explanation
genre_summary
manuscript_journal
manuscript_diss
manuscript_conference


ratertab <- subset(data, select=c(rater1, rater2, rater3))

KappaM(ratertab, method = "Fleiss", conf.level = 0.95) #    kappa    lwr.ci    upr.ci 
                                                        #0.7271979 0.6742066 0.7801893
ratertab