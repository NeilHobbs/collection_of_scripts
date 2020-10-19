##Import datasets
library(lme4)
library(PrevMap)
library(geoR)
library(sf)
library(splancs)
library(tmap)
library(ggplot2)
library(raster)

##Epidemiology Data
Mozamibique_Dicho = read.csv("Mozambique_2011_aggregated_data.csv")
Mozambique_Individual = read.csv("Mozambique_2011_individual_data.csv")

##Map Data
MOZ.adm0 = st_read("./gadm36_MOZ_shp/gadm36_MOZ_0.shp")
st_crs(MOZ.adm0)
MOZ.adm0 = st_transform(MOZ.adm0, crs = 4326) ##32629

MOZ_adm0 = st_read = ("./MOZ_adm/MOZ_adm0.shp")

plot(MOZ.adm0$geometry)

##Analysis of the Individual Data:
##Plot map of Data Points

par(mfrow=c(1,3))
plot(MOZ.adm0$geometry, main = "(a) Mozambique Outline")

point.map(Mozambique_Individual,~haz,coords=~utm_x+utm_y,
          pt.divide="quintiles", main = "(b) Individual")


##Plot map of Data Points from Dichotomous dataset:
point.map(Mozamibique_Dicho, ~(n_stunted/n_childred), coords = ~utm_x+utm_y,
          pt.divide = "quintiles", main = "(c) Dichotomous")



##Intitial Data Vis of Individual Level Data
##Plot scatter of age and haz score, by sex; similar patter between both sexes
ggplot(Mozambique_Individual, aes(x=age, y=haz, color = sex)) +
  geom_point() +
  facet_wrap(~sex)


##boxplot of age and haz scores by sex: No difference between the sexes
ggplot(Mozambique_Individual, aes(x=sex, y=haz)) +
  geom_boxplot()

ggplot(Mozambique_Individual, aes(x=sex, y=age)) +
  geom_boxplot()

##HAZ scores are normally distributed; confirmed by histogram
hist(Mozambique_Individual$haz,
     xlab = "HAZ Score",
     main = " ")



##Create a new column (ID.clusters) that gives a unique ID per set of x/y coords
Mozambique_Individual$ID.clusters <- create.ID.coords(Mozambique_Individual,~utm_x+utm_y)

##function to input "broken stick" ages
max.vec <- function(vec,c){ 
  sapply(vec,function(x) max(x,c))
}


##Initial linear model (excluding any aspect of spatial relationship); broken stick at age 2
lm.fit = lm(haz ~ age + max.vec(age, 2), data = Mozambique_Individual)


##Age Values from min/max of range; with 1000 partitions for predictions
age.values <- seq(min(Mozambique_Individual$age),
                        max(Mozambique_Individual$age),
                        length=1000)

##summary of initial linear model
summary(lm.fit)

##predictions of linear model (+ 95% CIs)
lm.pred.val = predict(lm.fit,  
                       newdata = data.frame(age=age.values), interval = "confidence")

##combine into dataframe for plotting
lm.pred.vals = data.frame(lm.pred.val)
lm.pred.vals = cbind(lm.pred.val, age.values)

##Plot predictions and confidence intervals; distinct change at age 2
ggplot(lm.pred.vals, aes(x=age.values, y = fit)) +
    geom_point(data = Mozambique_Individual, aes(x=age, y=haz), alpha = 0.2, color = "grey")+
  geom_line(color = "green") +
  geom_line(aes(x=age.values, y=lwr), color = "red") +
  geom_line(aes(x=age.values, y=upr), color = "red")+
  ylab("HAZ Score") +
  xlab("Age") +
  theme_bw()


##Accounting for spatial correlation
##Create a column of ID.clusters with unique ID per x-y set.
Mozambique_Individual$ID.clusters <- create.ID.coords(Mozambique_Individual,~utm_x+utm_y)


##Run "broken stick" lmer (age split at 2), with location as random effect
lmer.fit = lmer(haz ~ age + max.vec(age, 2) + (1|ID.clusters),
                data = Mozambique_Individual)

##Summary of lmer
summary(lmer.fit)

##create ID.coords vector [matches ID.clusters]
ID.coords = create.ID.coords(Mozambique_Individual,~utm_x+utm_y)

##Do spatial correlation diagnostic
spat.corr.diagnostic(haz~age + I(max.vec(age,2)), 
                     coords=~utm_x+utm_y, 
                     data = Mozambique_Individual, ID.coords=ID.coords,
                     n.sim = 1000,
                     likelihood = "Gaussian",
                     lse.variogram = TRUE,
                     uvec=seq(0,300,length=15))


##Take Sigma2, phi and tau2 values from spat.corr.diagnostic
sigma2.start = 0.0272145
phi.start = 2.1190285
tau2.start = 0.1923112

#perform linear model MLE on linear model 
geo.fit <- linear.model.MLE(haz ~age+I(max.vec(age, 2)),
                            coords=~utm_x+utm_y, ID.coords=ID.coords,
                            start.cov.pars = c(phi.start,
                                               tau2.start/sigma2.start),
                            kappa = 0.5,
                            data=Mozambique_Individual,
                            fixed.rel.nugget = 0,
                            method= "nlminb")



##summary of linear model MLE
summary(geo.fit)

##coefficients of MLE
beta.hat.geo <- summary(geo.fit)$coefficients[,1:2]

##95% CIs of Estimates from mle
cbind(beta.hat.geo,
      beta.hat.geo[,1]-qnorm(0.975)*beta.hat.geo[,2],
      beta.hat.geo[,1]+qnorm(0.975)*beta.hat.geo[,2])



##unique coords
coords = unique(Mozambique_Individual[,c("utm_x","utm_y")])

plot(coords,pch=20,asp=1)

##make polygon of coords using convex hull approach
poly <- coords[chull(coords),]

lines(rbind(poly,poly[1,]),col=2)


par(mfrow=c(1,3))
plot(MOZ.adm0$geometry, main = "(a) Mozambique Outline")

point.map(Mozambique_Individual,~haz,coords=~utm_x+utm_y,
          pt.divide="quintiles", main = "(b) Individual")
lines(rbind(poly,poly[1,]),col=2)

##Plot map of Data Points from Dichotomous dataset:
point.map(Mozamibique_Dicho, ~(n_stunted/n_childred), coords = ~utm_x+utm_y,
          pt.divide = "quintiles", main = "(c) Dichotomous")
lines(rbind(poly,poly[1,]),col=2)


##convert to matrix
poly = as.matrix(poly)

##prediction grid at 5x5km resolution
grid.pred1 <- gridpts(poly,xs=10,ys=10) # here xs and ys should be your chosen spatial resolution in km

points(grid.pred1,pch=10,cex=0.5,col=3)


##make dataframe of age predictions
age.predict = data.frame(age=rep(2,nrow(grid.pred1)))


##Calculate spatial predictions of prevalence
geo.pred <- spatial.pred.linear.MLE(geo.fit, grid.pred = grid.pred1,
                                    predictors = age.predict, 
                                    scale.predictions = c("prevalence", "logit"),
                                    standard.errors = TRUE,
                                    thresholds = -2,
                                    scale.thresholds = "logit",
                                    n.sim.prev = 1000,
                                    include.nugget = TRUE)

##Calculate probability of HAZ not exceeding -2 [and is stunted]
geo.pred$lower.prob = 1-(geo.pred$exceedance.prob)
geo.pred$exceedance.prob = geo.pred$lower.prob

##Plot prevalence and standard errors
plot(geo.pred,type="prevalence",summary="predictions",main="(a) Prevalence HAZ estimates")
plot(geo.pred,type="prevalence",summary="standard.errors",main="(b) Standard errors of HAZ Prevalence")
plot(geo.pred,type="logit",summary="predictions", main="(b) Logit HAZ Estimates")
plot(geo.pred,type="logit",summary="standard.errors", main="(b) Logit Standard errors")
plot(geo.pred,summary="exceedance.prob",zlim=c(0,1),main="Stunting risk")


##variogram diagnostic to confirm spatial correlation
geo.fit.diag <- variog.diagnostic.lm(geo.fit)

Z.hat <- ranef(lmer.fit)$ID.clusters[,1]
coords <- unique(Mozambique_Individual[,c("utm_x","utm_y")])
data.variogram <- data.frame(Z.hat=Z.hat,
                             utm_x=coords[,1],
                             utm_y=coords[,2])

variogram.Z <- 
  variogram(data.variogram,
            ~Z.hat,~utm_x+utm_y,
            uvec=seq(0,300,length=15))
plot(variogram.Z,typ="b")


#Dichotomous Model:
glm.fit = glm(cbind(n_stunted, n_childred - n_stunted) ~ mean_age,
              family = "binomial", data = Mozamibique_Dicho)

summary(glm.fit)

pred.glm = predict(glm.fit, newdata = data.frame(mean_age = age.values),
                   type = "response", se.fit = TRUE)

##CIs for Predictions
lwr= as.numeric(pred.glm$fit + (1.96 * pred.glm$se.fit))
upr = as.numeric(pred.glm$fit - (1.96 * pred.glm$se.fit))
fit = as.numeric(pred.glm$fit)

pred.glm = data.frame(age.values, upr, lwr, fit)

##Plot predictions
ggplot(pred.glm, aes(x=age.values, y = fit)) +
  geom_point(data = Mozamibique_Dicho, aes(x=mean_age, y=(n_stunted/n_childred)), colour = "grey", alpha = 0.5) +
  geom_line(color = "green") +
  geom_line(aes(x=age.values, y=lwr), color = "red") +
  geom_line(aes(x=age.values, y=upr), color = "red")+
  ylab("Proportion Stunted") +
  xlab("Mean Age") +
  theme_bw()


##Random effects glm, using dichotomous data; and binomial distribution.
Mozamibique_Dicho$ID <- 1:nrow(Mozamibique_Dicho)
glmer.fit <- glmer(cbind(n_stunted,n_childred-n_stunted) ~ log(mean_age) +(1|ID),
                   data=Mozamibique_Dicho, family=binomial)


summary(glmer.fit)

beta.guess <- coef(glm(cbind(n_stunted,n_childred-n_stunted) ~ log(mean_age),
                       family=binomial,data=Mozamibique_Dicho))

spat.corr.diagnostic(n_stunted~log(mean_age),
                     units.m=~n_childred,data=Mozamibique_Dicho,
                     coords=~I(utm_x)+I(utm_y),
                     likelihood = "Binomial",
                     lse.variogram = TRUE)

sigma2.guess <-  0.1433736
phi.guess <- 613.4460530
tau2.guess <-0.1254293

par0.moz <- c(beta.guess,sigma2.guess,phi.guess,tau2.guess)

mcml <- control.mcmc.MCML(n.sim=10000,burnin=2000,thin=8)

fit.mle <-
  binomial.logistic.MCML(n_stunted ~ mean_age,
                         units.m = ~ n_childred,
                         coords=~I(utm_x)+I(utm_y),
                         par0=par0.moz,control.mcmc = mcml,
                         kappa=0.5,
                         start.cov.pars = c(phi.guess,tau2.guess/sigma2.guess),
                         data=Mozamibique_Dicho,method="nlminb")

summary(fit.mle)

##coefficients of MLE
beta.hat.fit <- summary(fit.mle)$coefficients[,1:2]

##95% CIs of Estimates from mle
cbind(beta.hat.fit,
      beta.hat.fit[,1]-qnorm(0.975)*beta.hat.fit[,2],
      beta.hat.fit[,1]+qnorm(0.975)*beta.hat.fit[,2])

##unique coords
coords1 = unique(Mozamibique_Dicho[,c("utm_x","utm_y")])

##make polygon of coords using convex hull approach
poly1 <- coords[chull(coords1),]

##convert to matrix
poly1 = as.matrix(poly1)



##make dataframe of age predictions
age.predict = data.frame(age=rep(2,nrow(grid.pred1)))


grid.pred2 <- gridpts(poly1,xs=10,ys=10) # here xs and ys should be your chosen spatial resolution in km

age.predict1 = data.frame(mean_age=rep(1,nrow(grid.pred2)))
pred.mle <- 
  spatial.pred.binomial.MCML(fit.mle,grid.pred = grid.pred2,
                             predictors = age.predict1,
                             control.mcmc = mcml,
                             scale.predictions = c(
                               "logit","prevalence"),
                             thresholds = 0.4,
                             scale.thresholds = "prevalence",
                             standard.errors = TRUE)

##Calculate exceedance probabilities
fit.mle$lower.prob = 1-(fit.mle$exceedance.prob)
fit.mle$exceedance.prob = fit.mle$lower.prob

plot(pred.mle,"prevalence","predictions", main = "(a) Binomial Stunting Prevalence")
plot(pred.mle,"prevalence","standard.errors", main = "(b) Standard Error of Stunting Prevalence")
plot(pred.mle,summary="exceedance.prob",zlim=c(0,1),main="Stunting risk")



##Binomial Spatial Diagnostics
binomial.diagnostic = variog.diagnostic.glgm(fit.mle,
                       uvec=seq(0,300,length=15),
                       n.sim = 1000)


library(RColorBrewer)


##Plot all maps
par(mfrow=c(2,3))
plot(geo.pred,type="logit",summary="predictions",main="(a) HAZ Estimates: Gaussian")
plot(geo.pred,type="logit",summary="standard.errors",main="(b) Standard Errors: Gaussian")
plot(geo.pred,summary="exceedance.prob",zlim=c(0,1),main="Stunting risk: Gaussian")
plot(pred.mle,"prevalence","predictions", main = "(d) Stunting Prevalence: Binomial")
plot(pred.mle,"prevalence","standard.errors", main = "(e) Standard Errors: Binomial")
plot(pred.mle,summary="exceedance.prob",zlim=c(0,1),main="(f) Stunting risk: Binomial")

##Make raster of predictions to allow use of tmap plotting
Raster_Logit = rasterFromXYZ(cbind(grid.pred1, geo.pred$logit$predictions))
Raster_Binomial = rasterFromXYZ(cbind(grid.pred1, pred.mle$prevalence$predictions))
Raster_Logit_SE = rasterFromXYZ(cbind(grid.pred1, geo.pred$logit$standard.errors))
Raster_Binomial_SE = rasterFromXYZ(cbind(grid.pred1, pred.mle$prevalence$standard.errors))
Raster_Logit_Exceed = rasterFromXYZ(cbind(grid.pred1, geo.pred$exceedance.prob))
Raster_Binomial_Exceed = rasterFromXYZ(cbind(grid.pred1, pred.mle$exceedance.prob))
  
##Make maps using tmap, to improve colour layour to aid visualisation of results
A = tm_shape(Raster_Logit) + 
  tm_raster(palette = brewer.pal(5, "RdBu")) + 
  tm_layout(legend.position =c("right", "bottom"))+
  tm_layout(title = "(a)",
            legend.title.color = "white")

B = tm_shape(Raster_Logit_SE) + 
  tm_raster(palette = brewer.pal(5, "Reds")) + 
  tm_layout(legend.position =c("right", "bottom"))+
  tm_layout(title = "(b)", 
            legend.title.color = "white")

C = tm_shape(Raster_Logit_Exceed) + 
  tm_raster(palette = rev(brewer.pal(7, "PRGn"))) + 
  tm_layout(legend.position =c("right", "bottom"))+
  tm_layout(title = "(c)",
            legend.title.color = "white")

D = tm_shape(Raster_Binomial) + 
  tm_raster(palette = rev(brewer.pal(5,"RdBu"))) + 
  tm_layout(legend.position =c("right", "bottom")) +
  tm_layout(title = "(d)",
            legend.title.color = "white")

E = tm_shape(Raster_Binomial_SE) + 
  tm_raster(palette = brewer.pal(5, "Reds")) + 
  tm_layout(legend.position =c("right", "bottom"))+
  tm_layout(title = "(e)",
            legend.title.color = "white")


F = tm_shape(Raster_Binomial_Exceed) + 
  tm_raster(palette = rev(brewer.pal(7, "PRGn"))) + 
  tm_layout(legend.position =c("right", "bottom"))+
  tm_layout(title = "(f)",
            legend.title.color = "white")

##Arrange map plots
tmap_arrange(A, B, C, D, E, F, nrow =2, ncol = 3)                                      
