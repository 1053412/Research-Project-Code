setwd("C:/Users/Kj170/OneDrive/Desktop/MBiolWork/Toxicity Assays")
getwd()
#importing dataset----
library(dplyr)
library(ggplot2)
dataset<-read.csv("C:\\Users\\Windows\\Documents\\Masters\\key datasets for coding\\data.processing.sheet.final.april.csv")
dataset


colnames(dataset)[colnames(dataset) == "X48hrs"] <- "End"


data_subset<-subset(dataset,End != 'NA')
data_subset<-subset(data_subset, Dose_consumption == 'Y' )

#slf subset----
SLF_subset <- subset(data_subset, Compound == "SLF")
SLF_subset
count_values_SLF <- SLF_subset %>%
  group_by(Dose_ng_per_bee) %>%
  summarise(count = n())

SLF_conc_values<- unique(SLF_subset$Dose_ng_per_bee)
SLF_proportions<-as.numeric(length(SLF_conc_values))


for ( i in 1:length(SLF_conc_values)) {
  SLF_concentration <- SLF_conc_values[i]
  SLF_subset_2<-subset (SLF_subset,Dose_ng_per_bee == SLF_concentration)
  SLF_proportions[i]<- mean(SLF_subset_2$End == '0') #change to 1 for alive 
}



SLF_results<-data.frame(Dose_ng_per_bee= SLF_conc_values, ProportionDeadat48Hours=SLF_proportions)
#wrangling
SLF_results[SLF_results$Dose_ng_per_bee == "0", "ProportionDeadat48Hours"] <- 0

class(SLF_subset$Dose_ng_per_bee)
class(SLF_results$ProportionDeadat48Hours)


SLF_subset$Scaled_dosage<-SLF_subset$Dose_ng_per_bee/8000
library(lme4)
SLF_subset$final <- as.numeric(SLF_subset$final)
SLF_colony_lm <-glmer(final ~ Scaled_dosage+(1|Colony), family = binomial, data = SLF_subset)
summary(SLF_colony_lm)
SLF_lm_no_colony <- glm(final ~ Scaled_dosage, family = binomial, data=SLF_subset)
summary(SLF_lm_no_colony)
#effects seem significant!




library(drc)
SLF_fig <- ggplot(SLF_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = drm, method.args = list(fct = LL.2()), se = FALSE, colour = "black")+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red")+
  geom_vline(xintercept = 127.1, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  theme_bw()
plot(SLF_fig)



SLF_model_drm <- drm(ProportionDeadat48Hours ~ Dose_ng_per_bee, data = SLF_results, fct = LL.2())
summary(SLF_model_drm)
#both b and e (shape and slope) are highly significant

library(drc)
LD50_SLF_drm <- ED(SLF_model_drm, respLev = 50)





SLF_fig2 <- ggplot(SLF_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args=list(family="binomial"), colour = "black")+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  geom_vline(xintercept = 127.1, linetype = "dotted", colour = "red")+
  ggtitle("Sulfoxaflor")+
  theme_bw()+
  annotate("text", x=700, y = 0.52, label= "LD50 = 127.1")
plot(SLF_fig2)

#Although random colony effects are significant, substantially higher AIC
#so preferentially selected simple binomial model
SLFlm <- glm(ProportionDeadat48Hours ~ Dose_ng_per_bee, family = "binomial", data = SLF_results)
summary(SLFlm)
#LD50=127.1

dev_SLF <-deviance(SLFlm)
dev_SLF

library(car)
saturated_SLFlm<-glm(ProportionDeadat48Hours ~ 1, family = "binomial", data = SLF_results)
deviance_test_SLF <- anova(SLFlm, saturated_SLFlm, test ="Chisq")
deviance_test_SLF
chisq <- qchisq(1- (>|Chi|), df)



#very significant difference between saturated and fitted model
#fitted model provides a better fit of data than saturated model
#model seems adequate


# Assuming 'model' is your fitted logistic regression model
install.packages("pROC")
library(pROC)


# Predict probabilities
predicted_probs_SFX <- predict(SLF_lm_no_colony, type = "response")

# Compute ROC curve
roc_curve_SFX <- roc(final ~ Scaled_dosage, data = SLF_subset)

# Plot ROC curve
plot(roc_curve_SFX)

# Calculate AUC
auc_score_SFX <- auc(roc_curve_SFX)
print(auc_score_SFX)
#0.9023
#this model is considered to have excellent discriminatory ability















SLF_results[SLF_results$Dose_ng_per_bee == "0", "Dose_ng_per_bee"] <- 0.1
SLF_results$logdose <- log10(SLF_results$Dose_ng_per_bee)
#SLFlog----
SLF_fig_log<-  ggplot(SLF_results, aes(x = logdose, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args=list(family="binomial"), colour = "black")+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  geom_vline(xintercept = 1.51, linetype = "dotted", colour = "red")+
  theme_bw()
plot(SLF_fig_log)

SLFlm_log <- glm(ProportionDeadat48Hours ~ logdose, family = "binomial", data = SLF_results)
summary(SLFlm_log)
#LD50=1.51
#TMX subset----

TMX_subset <- subset(data_subset, Compound == "TMX")
TMX_subset
count_values <- TMX_subset %>%
  group_by(Dose_ng_per_bee) %>%
  summarise(count = n())

TMX_conc_values<- unique(TMX_subset$Dose_ng_per_bee)
TMX_proportions<-as.numeric(length(TMX_conc_values))


for ( i in 1:length(TMX_conc_values)) {
  TMX_concentration <- TMX_conc_values[i]
  TMX_subset_2<-subset (TMX_subset,Dose_ng_per_bee == TMX_concentration)
  TMX_proportions[i]<- mean(TMX_subset_2$End == '0') #change to 1 for alive 
}



TMX_results<-data.frame(Dose_ng_per_bee= TMX_conc_values, ProportionDeadat48Hours=TMX_proportions)
#wrangling
TMX_results[TMX_results$Dose_ng_per_bee == "0", "ProportionDeadat48Hours"] <- 0
#TMXfigbad----
class(TMX_results$Dose_ng_per_bee)
class(TMX_results$ProportionDeadat48Hours)
TMX_results$Dose_ng_per_bee <- as.numeric(TMX_results$Dose_ng_per_bee)
TMX_results$ProportionDeadat48Hours <- as.numeric(TMX_results$ProportionDeadat48Hours)
library(drc)
TMX_fig_LL.2 <- ggplot(TMX_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = drm, method.args = list(fct = LL.2()), se = FALSE, colour = "black")+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  theme_bw()

plot(TMX_fig_LL.2)



TMX_model_drm <- drm(ProportionDeadat48Hours ~ Dose_ng_per_bee, data = TMX_results, fct = LL.2())
summary(TMX_model_drm)
#both b and e (shape and slope) are  significant

library(drc)
LD50_TMX_drm <- ED(TMX_model_drm, respLev = 50)


#TMXfig----
TMX_fig2 <- ggplot(TMX_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args=list(family="binomial"), colour = "black")+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  geom_vline(xintercept = 6.412, linetype = "dotted", colour = "red")+
  theme_bw()+
  ggtitle("Thiamethoxam")+
  annotate("text", x = 10, y = 0.52, label = "LD50 = 6.412")
plot(TMX_fig2)
TMXlm <- glm(ProportionDeadat48Hours ~ Dose_ng_per_bee, family = "binomial", data = TMX_results)
summary(TMXlm)
#LD50=6.412

dev_TMX <-deviance(TMXlm)
dev_TMX

saturated_TMXlm<-glm(ProportionDeadat48Hours ~ 1, family = "binomial", data = TMX_results)
deviance_test_TMX <- anova(TMXlm, saturated_TMXlm, test ="Chisq")
deviance_test_TMX
#also a significant departure from the saturated model


TMX_results[TMX_results$Dose_ng_per_bee == "0", "Dose_ng_per_bee"] <- 0.1
TMX_results$logdose <- log10(TMX_results$Dose_ng_per_bee)
#TMXlog----
TMX_fig_log<-  ggplot(TMX_results, aes(x = logdose, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args=list(family="binomial"), colour = "black")+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  geom_vline(xintercept = 0.73, linetype = "dotted", colour = "red")+
  theme_bw()
plot(TMX_fig_log)

TMXlm_log <- glm(ProportionDeadat48Hours ~ logdose, family = "binomial", data = TMX_results)
summary(TMXlm_log)
#LD50 = 0.73
10^0.73

#flu subset-----

FLU_subset <- subset(data_subset, Compound == "FLU")
FLU_subset
count_values_FLU<- FLU_subset %>%
  group_by(Dose_ng_per_bee) %>%
  summarise(count = n())

FLU_conc_values<- unique(FLU_subset$Dose_ng_per_bee)
FLU_proportions<-as.numeric(length(FLU_conc_values))


for ( i in 1:length(FLU_conc_values)) {
  FLU_concentration <- FLU_conc_values[i]
  FLU_subset_2<-subset (FLU_subset,Dose_ng_per_bee == FLU_concentration)
  FLU_proportions[i]<- mean(FLU_subset_2$End == '0') #change to 1 for alive 
}



FLU_results<-data.frame(Dose_ng_per_bee= FLU_conc_values, ProportionDeadat48Hours=FLU_proportions)
#wrangling
FLU_results[FLU_results$Dose_ng_per_bee == "0", "ProportionDeadat48Hours"] <- 0

class(FLU_results$Dose_ng_per_bee)
class(FLU_results$ProportionDeadat48Hours)
FLU_results$Dose_ng_per_bee <- as.numeric(FLU_results$Dose_ng_per_bee)
#flu-fig-bad----
library(drc)
FLU_fig <- ggplot(FLU_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = drm, method.args = list(fct = LL.2()), se = FALSE, colour = "black")+
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  theme_bw()

plot(FLU_fig)
FLU_model_drm <- drm(ProportionDeadat48Hours ~ Dose_ng_per_bee, data = FLU_results, fct = LL.2())
summary(FLU_model_drm)
#both b and e (shape and slope) are highly significant

library(drc)
LD50_FLU_drm <- ED(FLU_model_drm, respLev = 50)



#flufig----
#removing values at 5000/10000/20000 - low sample size

FLU_results <- subset(FLU_results, Dose_ng_per_bee != "5000") 
FLU_results <- subset(FLU_results, Dose_ng_per_bee != "10000") 
FLU_results <- subset(FLU_results, Dose_ng_per_bee != "20000") 
FLU_results

FLU_fig2 <- ggplot(FLU_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args=list(family="binomial"), colour = "black")+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  geom_vline(xintercept = 18365.32, linetype = "dotted", colour = "red")+
  theme_bw()+
  ggtitle("Fluparydifurone mortality curve")+
  annotate("text", x = 27000, y = 0.52, label = "LD50 = 18,365.52")+
  scale_x_continuous(
    "Fluparydifurone Dose (ng/bee)",
    sec.axis = sec_axis(~ . /8000, name = "Acetone Dose (%)")
  )
plot(FLU_fig2)

FLUlm <- glm(ProportionDeadat48Hours ~ Dose_ng_per_bee, family = "binomial", data = FLU_results)
summary(FLUlm)
#LD50=18365.52
#summary is borderline significant as is

saturated_FLUlm<-glm(ProportionDeadat48Hours ~ 1, family = "binomial", data = FLU_results)
deviance_test_FLU <- anova(FLUlm, saturated_FLUlm, test ="Chisq")
deviance_test_FLU
#again, a significant departure






#nic----

NIC_subset <- subset(data_subset, Compound == "NIC")
NIC_subset
count_values_NIC<- NIC_subset %>%
  group_by(Dose_ng_per_bee) %>%
  summarise(count = n())

NIC_conc_values<- unique(NIC_subset$Dose_ng_per_bee)
NIC_proportions<-as.numeric(length(NIC_conc_values))


for ( i in 1:length(NIC_conc_values)) {
  NIC_concentration <- NIC_conc_values[i]
  NIC_subset_2<-subset (NIC_subset,Dose_ng_per_bee == NIC_concentration)
  NIC_proportions[i]<- mean(NIC_subset_2$End == '0') #change to 1 for alive 
}



NIC_results<-data.frame(Dose_ng_per_bee= NIC_conc_values, ProportionDeadat48Hours=NIC_proportions)
#wrangling
NIC_results[NIC_results$Dose_ng_per_bee == "0", "ProportionDeadat48Hours"] <- 0

class(NIC_results$Dose_ng_per_bee)
class(NIC_results$ProportionDeadat48Hours)
NIC_results$Dose_ng_per_bee <- as.numeric(NIC_results$Dose_ng_per_bee)

library(drc)
NIC_fig <- ggplot(NIC_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = drm, method.args = list(fct = L.4()), se = FALSE, colour = "black")+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  theme_bw()

plot(NIC_fig)

NIC_fig2 <- ggplot(NIC_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args=list(family="binomial"), colour = "black")+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  geom_vline(xintercept = 150512.68, linetype = "dotted", colour = "red")+
  theme_bw()+
  ggtitle("Nicotine mortality curve")+
  annotate("text", x = 200000, y = 0.52, label = "LD50 = 150,512.68 ")
plot(NIC_fig2)

NIClm <- glm(ProportionDeadat48Hours ~ Dose_ng_per_bee, family = "binomial", data = NIC_results)
summary(NIClm)
#LD50=150512.68

#composite----
ACE_subset <- subset(data_subset, Compound == "ACETONE")
ACE_subset
count_values_ACE<- ACE_subset %>%
  group_by(Dose_ng_per_bee) %>%
  summarise(count = n())

ACE_conc_values<- unique(ACE_subset$Dose_ng_per_bee)
ACE_proportions<-as.numeric(length(ACE_conc_values))


for ( i in 1:length(ACE_conc_values)) {
  ACE_concentration <- ACE_conc_values[i]
  ACE_subset_2<-subset (ACE_subset,Dose_ng_per_bee == ACE_concentration)
  ACE_proportions[i]<- mean(ACE_subset_2$End == '0') #change to 1 for alive 
}



ACE_results<-data.frame(Dose_ng_per_bee= ACE_conc_values, ProportionDeadat48Hours=ACE_proportions)
#wrangling
ACE_results[ACE_results$Dose_ng_per_bee == "0", "ProportionDeadat48Hours"] <- 0

class(ACE_results$Dose_ng_per_bee)
class(ACE_results$ProportionDeadat48Hours)
ACE_results$Dose_ng_per_bee <- as.numeric(ACE_results$Dose_ng_per_bee)
ACE_results$Dose_ng_per_bee <- 8000 * ACE_results$Dose_ng_per_bee

comp_fig <- ggplot(NULL, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(data = FLU_results, colour = "black")+
  geom_smooth(data = FLU_results, method = "glm", 
              se = FALSE,
              method.args=list(family="binomial"), aes(colour = "Fluparydifurone"))+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  geom_vline(xintercept = 18365.32, linetype = "dotted", colour = "red")+
  theme_bw()+
  ggtitle("Fluparydifurone")+
  annotate("text", x = 21500, y = 0.48, label = "LD50 = 18,365.52")+
  geom_point(data = ACE_results, colour = "blue")+
  geom_smooth(data = ACE_results, method = "glm", 
              se = FALSE,
              method.args=list(family="binomial"), aes(colour = "Acetone"))+
  scale_color_manual(name = "Compounds", breaks = c("Fluparydifurone", "Acetone"),
                      values = c("Fluparydifurone"="black", "Acetone" = "blue"))+
  scale_x_continuous(
    "Fluparydifurone Dose (ng/bee)",
    sec.axis = sec_axis(~ . /8000, name = "Acetone Dose (%)")
  )
plot(comp_fig)


library(patchwork)
combined_LD50<- TMX_fig2 + SLF_fig2 + comp_fig + plot_layout(widths= c(0.75, 0.75, 0.9), heights = c(0.6, 0.6, 0.6))
plot(combined_LD50)

comp_fig_1<-comp_fig + theme(axis.title.y = element_text(angle = 0, vjust=0.5)) +
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))
plot(comp_fig_1)
