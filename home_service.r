############################################################
### Packages and settings
###########################################################

setwd("../Home service/R")

library(stargazer)
library("plm")
library(DescTools)
library(robustHD)
library(Hmisc)
library(ggplot2)
library(ggpubr)

theme_set(theme_bw())
############################################################
### Data
############################################################

data_all=read.csv("../data/main_data.csv", encoding = "UTF-8")

political=read.csv("../data/political_majority.csv", encoding = "UTF-8")

data_panel <- read.csv("../data/panel_data_2014_2017.csv", encoding = "UTF-8")

############################################################
### Data treatment and illustration
############################################################

### Create basic variables
data_all$cost_hour = data_all$cost/data_all$hours/12
data_all$density_log = log(data_all$density)
data_all$public_providers=data_all$public_providers/100
data_all$population_log=log(data_all$population)
data_all$populationK = data_all$population/1000

### Density plots before Winsorizing
# Creates data before winsorizing for the plots, they are then recreated below

data=data_all
data <- merge(data, political, by="municipality")
data = data[data$public_providers!=1,]
data = data[!is.na(data$public_providers),]
data = data[!is.na(data$hours),]

data_public=data_all
data_public = data_public[data_public$public_providers==1,]
data_public = data_public[!is.na(data_public$public_providers),]
data_public = data_public[!is.na(data_public$hours),]

jpeg('../images/comparison_cost_hour.jpg', width=1200, height=1200, res=150)
ggplot() + 
  geom_histogram(data=data_all, aes(x=cost_hour, y=..density..), alpha=.3, position="identity", fill="red", bins=30) + 
  geom_histogram(data=data, aes(x=cost_hour, y=..density..), alpha=.2, position="identity", fill="blue", bins=30) +
  xlim(0,1500)
dev.off()

jpeg('../images/comparison_hours.jpg', width=1200, height=1200, res=150)
ggplot() + 
  geom_histogram(data=data_all, aes(x=hours, y=..density..), alpha=.3, position="identity", fill="red", bins=30) + 
  geom_histogram(data=data, aes(x=hours, y=..density..), alpha=.2, position="identity", fill="blue", bins=30) +
  xlim(0,60)
dev.off()

# Windsonrize
data_all$cost_hour=Winsorize(data_all$cost_hour, na.rm = TRUE)
data_all$hours=Winsorize(data_all$hours, na.rm = TRUE)

### Divide data into three data sets
# data_all is all observations
# data is with private>0
# data_public is with only public=1

data=data_all
data <- merge(data, political, by="municipality")
data = data[data$public_providers!=1,]
data = data[!is.na(data$public_providers),]
data = data[!is.na(data$hours),]

data_public=data_all
data_public = data_public[data_public$public_providers==1,]
data_public = data_public[!is.na(data_public$public_providers),]
data_public = data_public[!is.na(data_public$hours),]

############################################################
### Regressions
############################################################

fit <- lm(hours ~ public_providers + cost_hour, data=data)

fit_structure <- lm(hours ~ public_providers + cost_hour + town + countryside + density_log + rightwing + leftwing, data=data)

fit_demographic <- lm(hours ~ public_providers + cost_hour + old + population_log + care_recievers , data=data)

fit_combo <- lm(hours ~ public_providers + cost_hour + town + countryside + old + population_log + density_log + care_recievers + leftwing + rightwing, data=data)

reg_tab2 = stargazer(fit, fit_demographic, fit_structure, fit_combo,
                     label = "reg_tab2",
                     table.placement = "H",
                     type = "latex",
                     font.size = "small",
                     omit.stat = c("f", "ser"),
                     dep.var.labels = c("Hours provided"),
                     covariate.labels = c(
                        "Share of Public Providers", 
                        "Cost per Hour", 
                        "Old", 
                        "Population (log)", 
                        "Share of Care Recievers",
                        "Town",
                        "Countryside",
                        "Population Density (log)",
                        "Right-wing",
                        "Left-wing",
                        "(Constant)"),
                      out = "../tables/regression_table2.tex")

### Plot the data

jpeg('../images/regression_diagsnostics_combo.jpg', width=1200, height=1200, res=150)
layout(matrix(c(1,2,3,4),2,2))
plot(fit_combo)
dev.off()

jpeg('../images/regression_diagsnostics.jpg', width=1200, height=1200, res=150)
layout(matrix(c(1,2,3,4),2,2))
plot(fit)
dev.off()

plot1 = ggplot(data,aes(cost_hour,hours)) +
  geom_smooth(method='lm', se = F, aes(color='red')) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = 'Cost per Hour', y = 'Hours Provided')

plot2 = ggplot(data,aes(public_providers,hours)) +
  geom_smooth(method='lm', se = F, aes(color='red')) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = 'Share of Public Providers', y = 'Hours Provided')

plot3 = ggplot(data,aes(public_providers,cost_hour)) +
  geom_smooth(method='lm', se = F, aes(color='red')) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = 'Share of Public Providers', y = 'Cost per Hour')

jpeg('../images/variable_plots.jpg', width=900, height=350, res = 120)
ggarrange(plot1, plot2, plot3, nrow = 1)
dev.off()

reg_tab1 = stargazer(fit1, fit2, fit,
                     out = "../tables/regression_table1.tex",
                     title = "Regression",
                     label = "reg_tab1",
                     table.placement = "H",
                     # type = "latex",
                     #table.layout ="=ldcm#-t-a-s=n",
                     font.size = "small",
                     #add.lines = list(
                     #   c("Chain Fixed Effects", "No", "Yes", "Yes"),
                     #  c("Year Fixed Effects", "No", "No", "Yes")),
                     #omit = c("chain", "year"),
                     #float.env = "sidewaystable",
                     omit.stat = c("f", "ser"),
                     dep.var.labels = c("Hours Provided"),
                     covariate.labels = c(
                       "Cost per Hour", 
                       "Share of Public Providers", 
                       "(Constant)")
                       )
############################################################
### summary statistics
############################################################

#stargazer(data, summary=TRUE)
#stargazer(data_all, summary=TRUE)
#stargazer(data_public, summary=TRUE)

summary(data$hours[data$leftwing == 1])

summary(data$hours[data$leftwing == 0])
layout(matrix(c(1,2,3,4,5,6),3,2))


jpeg('../images/comparison_public_private_data.jpg', width=1200, height=1800, res=150)
layout(matrix(c(1,2,3,4,5,6),3,2))
hist(data_all$public_providers, breaks=20, xlab = "Share of public providers", main = "All municipalities")
hist(data_all$cost_hour, xlim = c(0,2000), xlab = "Cost per hour", breaks=200, main = "All municipalities")
hist(data_all$hours, xlim = c(0,100), xlab = "Avg. # of hours/month per consumer", breaks=c(5*0:100), main = "All municipalities")

hist(data$public_providers, breaks=20, xlab = "Share of public providers", main = "Municipalities with private providers")
hist(data$cost_hour, xlim = c(0,2000), xlab = "Cost per hour", breaks=20, main = "Municipalities with private providers")
hist(data$hours, xlim = c(0,100), xlab = "Avarage number of hours per month and consumer", breaks=c(5*0:100), main = "Municipalities with private providers")
dev.off()

jpeg('../images/cost_and_hours.jpg', width=1200, height=800, res=150)
layout(matrix(c(1,2,3,4),2,2))
hist(data_all$cost_hour, xlim = c(0,2000), xlab = "Cost per hour", breaks=20, main = "All municipalities")
hist(data_all$hours, xlim = c(0,100), xlab = "Avg. # of hours/month per consumer", breaks=c(5*0:100), main = "All municipalities")
hist(data$cost_hour, xlim = c(0,2000), xlab = "Cost per hour", breaks=10, main = "Municipalities with private providers")
hist(data$hours, xlim = c(0,100), xlab = "Avg. # of hours/month per consumer", breaks=c(5*0:100), main = "Municipalities with private providers")
dev.off()

jpeg('../images/public_providers_hist.jpg', width=1200, height=1000, res=150)
hist(data$public_providers, breaks=10, xlab = "Share of public providers", main = "Distribution of share of public providers")
dev.off()

jpeg('../images/public_providers_hist_raw.jpg', width=1200, height=1000, res=150)
hist(data_all$public_providers, breaks=10, xlab = "Share of public providers", main = "Distribution of share of public providers")
dev.off()

#Test of grouping of histograms (the add=T part)
layout(matrix(c(1),1,1))
hist(data_all$public_providers, breaks=10)
hist(data$public_providers, breaks=10, xlab = "Share of public providers", main = "All municipalities", add=T)
box()

#Test
fit <- lm(hours ~ public_providers + cost_hour, data=data)
fit1 <- lm(hours ~ cost_hour, data=data)
fit2 <- lm(resid(fit1) ~ public_providers, data=data)

############################################################
### Panel regression
############################################################

data_panel$public_providers=data_panel$public_providers/100
data_panel$cost_hour = data_panel$cost/data_panel$hours/12
data_panel$density_log = log(data_panel$density)
data_panel$population_log=log(data_panel$population)

data_panel = data_panel[data_panel$cost_hour < 2000,]
data_panel$competition = 1
data_panel$competition[data_panel$public_providers > 0.9] = 0

data_panel_private = data_panel[data_panel$private ==1,]

panel_private <- pdata.frame(data_panel_private, index=c("municipality","year"), drop.index=TRUE, row.names=TRUE)
lmp <- plm(hours ~ public_providers + cost_hour, data = panel_private, model = "pooling")
lmp_c <- plm(hours ~ public_providers + cost_hour + town + countryside + old + population_log + density_log + care_recievers + leftwing + rightwing, model = "pooling", data = panel_private)
lmp_fe <- plm(hours ~ public_providers + cost_hour + town + countryside + old + population_log + density_log + care_recievers + leftwing + rightwing, data = panel_private, model = "within") 
lmp_fd <- plm(hours ~ public_providers + cost_hour + town + countryside + old + population_log + density_log + care_recievers + leftwing + rightwing, data = panel_private, model = "fd") 



panel <- pdata.frame(data_panel, index=c("municipality","year"), drop.index=TRUE, row.names=TRUE)
rdp <- plm(hours ~ competition + cost_hour, model = "pooling", data = panel)
rdp_c <- plm(hours ~ competition + cost_hour + town + countryside + old + population_log + density_log + care_recievers + leftwing + rightwing, model = "pooling", data = panel)
rdp_fe <- plm(hours ~ cost_hour + competition + town + countryside + old + population_log + density_log + care_recievers + leftwing + rightwing, model = "within", data = panel)
rdp_fd <- plm(hours ~ cost_hour + competition + town + countryside + old + population_log + density_log + care_recievers + leftwing + rightwing, model = "fd", data = panel)

layout(matrix(c(1,2,3),1,3))
plot(log(data_panel$cost_hour),data_panel$hours, xlab="Cost per hour", ylab="Hours provided")
abline(fit1, col="red")
plot(data_panel$public_providers,data_panel$hours, xlab="Share public providers", ylab="Hours provided")
abline(fit2, col="red")
plot(data_panel$public_providers,log(data_panel$cost_hour), xlab="Share public providers", ylab="Cost per hour")

reg_tab3 = stargazer(lmp, lmp_c, rdp, rdp_c, 
                     title="Panel Regression", 
                     label="reg_tab3",
                     table.placement = "H",
                     #type = "text",
                     #table.layout ="=ldcm#-t-a-s=n",
                     font.size = "small",
                     #add.lines = list(
                     #   c("Chain Fixed Effects", "No", "Yes", "Yes"),
                     #  c("Year Fixed Effects", "No", "No", "Yes")),
                     #omit = c("chain", "year"),
                     #float.env = "sidewaystable",
                     omit.stat = c("f", "ser"),
                     dep.var.labels = c("Hours provided"),
                     covariate.labels = c(
                       "Share of Public Providers",
                       "D (competition indicator)",
                       "Cost per Hour",
                       "Old",
                       "Population (log)",
                       "Share of Care Recievers",
                       "Town",
                       "Countryside",
                       "Population Density (log)",
                       "Right-wing",
                       "Left-wing",
                       "(Constant)"),
                     out = "../tables/regression_table3.tex"
)
