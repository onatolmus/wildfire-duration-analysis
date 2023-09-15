library(ggplot2)
library(tidyverse)
library(survival)
library(survminer)
library(MASS)
library(grid)
library(gridExtra)

#read data
cox_data_l <- read_csv('df_cox_l.csv')
cox_data_p <- read_csv('df_cox_p.csv')
cox_data_total <- read_csv('df_cox.csv')

#set reference levels for fire and fuel type
cox_data_l <- within(cox_data_l, {
  fire_type <- relevel(as.factor(fire_type), ref = "Surface") 
  fuel_type <- relevel(as.factor(fuel_type_grouped), ref = "Coniferous")})

cox_data_p <- within(cox_data_p,{ 
  fire_type <- relevel(as.factor(fire_type), ref = "Surface") 
  fuel_type <- relevel(as.factor(fuel_type_grouped), ref = "Coniferous")})

cox_data_total <- within(cox_data_total,{ 
  fire_type <- relevel(as.factor(fire_type), ref = "Surface") 
  fuel_type <- relevel(as.factor(fuel_type_grouped), ref = "Coniferous")})

#### initial analysis with no stratification ###
cox_fit_l_nostr <- coxph(Surv(time_1, time_2, status) ~ size + response_time + ffmc + dmc + dc + bui + isi + fwi + dsr + latitude + longitude + fire_type + fuel_type, data=cox_data_l, id = fire_id)
summary(cox_fit_l_nostr)

cox_fit_p_nostr <- coxph(Surv(time_1, time_2, status) ~ size + response_time + ffmc + dmc + dc + bui + isi + fwi + dsr + latitude + longitude + fire_type + fuel_type, data=cox_data_p, id = fire_id)
summary(cox_fit_p_nostr)

#model selection
step_cox_l_nostr <- stepAIC(cox_fit_l_nostr, trace = TRUE, direction= "both", k=log(dim(cox_data_l)[1]))
summary(step_cox_l_nostr)

step_cox_p_nostr <- stepAIC(cox_fit_p_nostr, trace = TRUE, direction= "both", k=log(dim(cox_data_p)[1]))
summary(step_cox_p_nostr)

#PH assumptions
res_zph_l_nostr <- cox.zph(step_cox_l_nostr)
res_zph_l_nostr
res_zph_p_nostr <- cox.zph(step_cox_p_nostr)
res_zph_p_nostr

### cox models with stratification on size class ###
#full models
cox_fit_l <- coxph(Surv(time_1, time_2, status) ~ size + response_time + ffmc + dmc + dc + bui + isi + fwi + dsr + latitude + longitude + fire_type + fuel_type + strata(size_class), data=cox_data_l, id = fire_id)
summary(cox_fit_l)

cox_fit_p <- coxph(Surv(time_1, time_2, status) ~ size + response_time + ffmc + dmc + dc + bui + isi + fwi + dsr + latitude + longitude + fire_type + fuel_type + strata(size_class), data=cox_data_p, id = fire_id)
summary(cox_fit_p)

#model selection with backward step-wise variable selection based on BIC
step_cox_l <- stepAIC(cox_fit_l, trace = TRUE, direction= "both", k=log(dim(cox_data_l)[1]))
summary(step_cox_l)

step_cox_p <- stepAIC(cox_fit_p, trace = TRUE, direction= "both", k=log(dim(cox_data_p)[1]))
summary(step_cox_p)

round(summary(step_cox_l)$coefficients,5)
round(summary(step_cox_p)$coefficients,5)

#check PH assumptions
res_zph_l <- cox.zph(step_cox_l)
res_zph_l

res_zph_p <- cox.zph(step_cox_p)
res_zph_p

#run total model with the variable cause included in the model as a dummy variable
cox_fit_total <- coxph(Surv(time_1, time_2, status) ~ size + response_time + ffmc + dmc + dc + bui + isi + fwi + dsr + latitude + longitude + fire_type + fuel_type + cause + strata(size_class), data=cox_data_total, id = fire_id)
summary(cox_fit_total)

#PH assumptions of the total model
res_zph_total <- cox.zph(cox_fit_total)
res_zph_total

#check the total model after backward step-wise variable selection with BIC
step_cox_total <- stepAIC(cox_fit_total, trace = TRUE, direction= "both", k=log(dim(cox_data_total)[1]))
summary(step_cox_total)


### survival plots ###
#lighting survival curves
sfit_l <- survfit(step_cox_l)

plot_l_1 <- ggsurvplot(sfit_l, data= cox_data_l, 
                       risk.table = FALSE, title = "Survival Curves for Lightning-caused Fires",xlab = "Time (Hours)",
                       ggtheme = theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) )
plot_l_2 <- ggsurvplot(sfit_l, data= cox_data_l,conf.int = FALSE, ggtheme = theme_light(), 
                       risk.table = FALSE,xlim = c(0,1000),break.x.by = 200)

plot_l_3 <- ggsurvplot(sfit_l, data= cox_data_l,conf.int = FALSE, ggtheme = theme_light(), 
                       risk.table = FALSE,xlim = c(0,200),break.x.by = 20)

plot_l <- plot_l_1$plot + annotation_custom(grob = ggplotGrob(plot_l_2$plot+theme(legend.position = "none") + 
                      theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) ),
                      xmin = 6000,xmax=10000,ymin = .1,ymax = 0.6) +
                    annotation_custom(grob = ggplotGrob(plot_l_3$plot+theme(legend.position = "none") + 
                      theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) ),
                      xmin = 1500,xmax=5500,ymin = .5,ymax = 1)

plot_l

#people survival curves
sfit_p <- survfit(step_cox_p)
plot_p_1 <- ggsurvplot(sfit_p, data= cox_data_p, 
                       risk.table = FALSE, title = "Survival Curves for People-caused Fires",xlab = "Time (Hours)",
                       ggtheme = theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) )
plot_p_2 <- ggsurvplot(sfit_p, data= cox_data_p,conf.int = FALSE, ggtheme = theme_light(), 
                       risk.table = FALSE,xlim = c(0,1000),break.x.by = 200)

plot_p_3 <- ggsurvplot(sfit_p, data= cox_data_p,conf.int = FALSE, ggtheme = theme_light(), 
                       risk.table = FALSE,xlim = c(0,200),break.x.by = 20)

plot_p <- plot_p_1$plot + annotation_custom(grob = ggplotGrob(plot_p_2$plot+theme(legend.position = "none") + 
                              theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) ),
                              xmin = 6000,xmax=10000,ymin = .1,ymax = 0.6) +
                          annotation_custom(grob = ggplotGrob(plot_p_3$plot+theme(legend.position = "none") + 
                              theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) ),
                               xmin = 1500,xmax=5500,ymin = .5,ymax = 1)

plot_p

### Schoenfeld residuals ###

#lightning
ggcoxzph(res_zph_l, var = c("size", "response_time", "ffmc", "dmc"), resid =TRUE,
         ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) ) 
ggcoxzph(res_zph_l, var = c( "dc", "longitude", "fuel_type"), resid =TRUE,
         ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) ) 
#people
ggcoxzph(res_zph_p, var = c("response_time", "dmc","bui", "isi"), resid =TRUE,
         ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) )
ggcoxzph(res_zph_p, var = c("fwi", "latitude", "fire_type", "fuel_type"), resid =TRUE,
         ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) )

### linearity of covariates with Martingale residuals ###

#lightning
ggcoxfunctional(coxph(Surv(time_1, time_2, status) ~ size +log(size) + sqrt(size) + I(size^2),data = cox_data_l,id = fire_id),
                data=cox_data_l,title = "Size in Lightning-caused Fires",
                ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) ) 
ggcoxfunctional(coxph(Surv(time_1, time_2, status) ~ response_time  + sqrt(response_time) + I(response_time^2),data = cox_data_l,id = fire_id),
                data=cox_data_l,title = "Response Time in Lightning-caused Fires",
                ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) )
ggcoxfunctional(coxph(Surv(time_1, time_2, status) ~ ffmc +log(ffmc) + sqrt(ffmc) + I(ffmc^2),data = cox_data_l,id = fire_id),
                data=cox_data_l,title = "FFMC in Lightning-caused Fires",
                ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) ) 
ggcoxfunctional(coxph(Surv(time_1, time_2, status) ~ dmc + sqrt(dmc) + I(dmc^2),data = cox_data_l,id = fire_id),
                data=cox_data_l,title = "DMC in Lightning-caused Fires",
                ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) ) 
ggcoxfunctional(coxph(Surv(time_1, time_2, status) ~ longitude + I(sign(longitude) * log(abs(longitude))) + I(sign(longitude) * sqrt(abs(longitude))) +  I(longitude^2),data = cox_data_l,id = fire_id),
                data=cox_data_l,title = "Longitude in Lightning-caused Fires",
                ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) ) 

#people
ggcoxfunctional(coxph(Surv(time_1, time_2, status) ~ response_time + sqrt(response_time) + I(response_time^2),data = cox_data_p,id = fire_id),
                data=cox_data_p,title = "Response Time in People-caused Fires",
                ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) )
ggcoxfunctional(coxph(Surv(time_1, time_2, status) ~ dmc + sqrt(dmc) + I(dmc^2),data = cox_data_p,id = fire_id),
                data=cox_data_p,title = "DMC in People-caused Fires",
                ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) )
ggcoxfunctional(coxph(Surv(time_1, time_2, status) ~ bui + sqrt(bui) + I(bui^2),data = cox_data_p,id = fire_id),
                data=cox_data_p,title = "BUI in People-caused Fires",
                ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) )
ggcoxfunctional(coxph(Surv(time_1, time_2, status) ~ isi + sqrt(isi) + I(isi^2),data = cox_data_p,id = fire_id),
                data=cox_data_p,title = "ISI in People-caused Fires",
                ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) )
ggcoxfunctional(coxph(Surv(time_1, time_2, status) ~ fwi + sqrt(fwi) + I(fwi^2),data = cox_data_p,id = fire_id),
                data=cox_data_p,title = "FWI in People-caused Fires",
                ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) )
ggcoxfunctional(coxph(Surv(time_1, time_2, status) ~ latitude +log(latitude) + sqrt(latitude) + I(latitude^2),data = cox_data_p,id = fire_id),
                data=cox_data_p,title = "Latitude in People-caused Fires",
                ggtheme =theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) )

### Cox-Snell residuals ###

#lightning
res_l <- residuals(step_cox_l,type="martingale")
cox_data_l$res_cox_snell_l <- cox_data_l$status - res_l
cox_snell_fit_l <- coxph(Surv(res_cox_snell_l, status) ~ 1+ strata(size_class), data = cox_data_l, id = fire_id)
data_base_hazard_l <- basehaz(cox_snell_fit_l, centered = FALSE)


cs_l_a <- ggplot(data = data_base_hazard_l[data_base_hazard_l$strata == "A",], mapping = aes(x = time, y = hazard)) +geom_point(size=0.5) +
  geom_abline(intercept = 0, slope = 1,color="red") +
  labs(x = "Cox-Snell Residuals", y = "Cumulative Hazard Ratio Estimate", title = "Cox-Snell Residuals for Size A Lightning Fires") +  
  theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 12))+ coord_equal() 
cs_l_a <- cs_l_a + expand_limits(x=max(abs(layer_scales(cs_l_a)$y$range$range)),y=max(abs(layer_scales(cs_l_a)$x$range$range)))

cs_l_b <- ggplot(data = data_base_hazard_l[data_base_hazard_l$strata == "B",], mapping = aes(x = time, y = hazard)) +geom_point(size=0.5) +
  geom_abline(intercept = 0, slope = 1,color="red") + 
  labs(x = "Cox-Snell Residuals", y = "Cumulative Hazard Ratio Estimate", title = "Cox-Snell Residuals for Size B Lightning Fires") +  
  theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 12)) + coord_equal()
cs_l_b <- cs_l_b + expand_limits(x=max(abs(layer_scales(cs_l_b)$y$range$range)),y=max(abs(layer_scales(cs_l_b)$x$range$range)))

cs_l_c <- ggplot(data = data_base_hazard_l[data_base_hazard_l$strata == "C",], mapping = aes(x = time, y = hazard)) +geom_point(size=0.5) +
  geom_abline(intercept = 0, slope = 1,color="red") + 
  labs(x = "Cox-Snell Residuals", y = "Cumulative Hazard Ratio Estimate", title = "Cox-Snell Residuals for Size C Lightning Fires") +  
  theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 12))  + coord_equal()
cs_l_c <- cs_l_c + expand_limits(x=max(abs(layer_scales(cs_l_c)$y$range$range)),y=max(abs(layer_scales(cs_l_c)$x$range$range)))

cs_l_d <- ggplot(data = data_base_hazard_l[data_base_hazard_l$strata == "D",], mapping = aes(x = time, y = hazard)) +geom_point(size=0.5) +
  geom_abline(intercept = 0, slope = 1,color="red") + 
  labs(x = "Cox-Snell Residuals", y = "Cumulative Hazard Ratio Estimate", title = "Cox-Snell Residuals for Size D Lightning Fires") +  
  theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 12))  + coord_equal()
cs_l_d <- cs_l_d + expand_limits(x=max(abs(layer_scales(cs_l_d)$y$range$range)),y=max(abs(layer_scales(cs_l_d)$x$range$range)))

cs_l_e <- ggplot(data = data_base_hazard_l[data_base_hazard_l$strata == "E",], mapping = aes(x = time, y = hazard)) +geom_point(size=0.5) + 
  geom_abline(intercept = 0, slope = 1,color="red") + 
  labs(x = "Cox-Snell Residuals", y = "Cumulative Hazard Ratio Estimate", title = "Cox-Snell Residuals for Size E Lightning Fires") +  
  theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 12))  + coord_equal()
cs_l_e <- cs_l_e + expand_limits(x=max(abs(layer_scales(cs_l_e)$y$range$range)),y=max(abs(layer_scales(cs_l_e)$x$range$range)))


#people
res_p <- residuals(step_cox_p,type="martingale")
cox_data_p$res_cox_snell_p <- cox_data_p$status - res_p
cox_snell_fit_p <- coxph(Surv(res_cox_snell_p, status) ~ 1+ strata(size_class), data = cox_data_p, id = fire_id)
data_base_hazard_p <- basehaz(cox_snell_fit_p, centered = FALSE)

cs_p_a <- ggplot(data = data_base_hazard_p[data_base_hazard_p$strata == "A",], mapping = aes(x = time, y = hazard)) +geom_point(size=0.5) +
  geom_abline(intercept = 0, slope = 1,color="red") +  
  labs(x = "Cox-Snell Residuals", y = "Cumulative Hazard Ratio Estimate", title = "Cox-Snell Residuals for Size A People Fires") +  
  theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 12)) + coord_equal()
cs_p_a <- cs_p_a + expand_limits(x=max(abs(layer_scales(cs_p_a)$y$range$range)),y=max(abs(layer_scales(cs_p_a)$x$range$range)))

cs_p_b <- ggplot(data = data_base_hazard_p[data_base_hazard_p$strata == "B",], mapping = aes(x = time, y = hazard)) +geom_point(size=0.5) +
  geom_abline(intercept = 0, slope = 1,color="red") + 
  labs(x = "Cox-Snell Residuals", y = "Cumulative Hazard Ratio Estimate", title = "Cox-Snell Residuals for Size B People Fires") +  
  theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 12)) + coord_equal()
cs_p_b <- cs_p_b + expand_limits(x=max(abs(layer_scales(cs_p_b)$y$range$range)),y=max(abs(layer_scales(cs_p_b)$x$range$range)))

cs_p_c <- ggplot(data = data_base_hazard_p[data_base_hazard_p$strata == "C",], mapping = aes(x = time, y = hazard)) +geom_point(size=0.5) +
  geom_abline(intercept = 0, slope = 1,color="red") +  
  labs(x = "Cox-Snell Residuals", y = "Cumulative Hazard Ratio Estimate", title = "Cox-Snell Residuals for Size C People Fires") +  
  theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 12))  + coord_equal()
cs_p_c <- cs_p_c + expand_limits(x=max(abs(layer_scales(cs_p_c)$y$range$range)),y=max(abs(layer_scales(cs_p_c)$x$range$range)))

cs_p_d <- ggplot(data = data_base_hazard_p[data_base_hazard_p$strata == "D",], mapping = aes(x = time, y = hazard)) +geom_point(size=0.5) +
  geom_abline(intercept = 0, slope = 1,color="red") + 
  labs(x = "Cox-Snell Residuals", y = "Cumulative Hazard Ratio Estimate", title = "Cox-Snell Residuals for Size D People Fires") +  
  theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 12)) + coord_equal()
cs_p_d <- cs_p_d + expand_limits(x=max(abs(layer_scales(cs_p_d)$y$range$range)),y=max(abs(layer_scales(cs_p_d)$x$range$range)))

cs_p_e <- ggplot(data = data_base_hazard_p[data_base_hazard_p$strata == "E",], mapping = aes(x = time, y = hazard)) +geom_point(size=0.5) + 
  geom_abline(intercept = 0, slope = 1,color="red") + 
  labs(x = "Cox-Snell Residuals", y = "Cumulative Hazard Ratio Estimate", title = "Cox-Snell Residuals for Size E People Fires") +  
  theme_light() + theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 12)) + coord_equal()
cs_p_e <- cs_p_e + expand_limits(x=max(abs(layer_scales(cs_p_e)$y$range$range)),y=max(abs(layer_scales(cs_p_e)$x$range$range)))

grid.arrange(cs_l_a,cs_l_b,cs_l_c,cs_p_a,cs_p_b, cs_p_c, ncol=3)
grid.arrange(cs_l_d,cs_l_e,cs_p_d,cs_p_e, ncol=2)


### Adjusted p-values ###
#lightning
p_vals_l <-  summary(step_cox_l)$coefficients[,5]
p_bonf_l <- p.adjust(p_vals_l,method = "bonferroni")
p_holm_l <- p.adjust(p_vals_l,method = "holm")

adj_p_vals_l <- cbind(round(p_vals_l,4),round(p_bonf_l,4),round(p_holm_l,4))
adj_p_vals_l

#people
p_vals_p <-  summary(step_cox_p)$coefficients[,5]
p_bonf_p <- p.adjust(p_vals_p,method = "bonferroni")
p_holm_p <- p.adjust(p_vals_p,method = "holm")

adj_p_vals_p <- cbind(round(p_vals_p,4),round(p_bonf_p,4),round(p_holm_p,4))
adj_p_vals_p

