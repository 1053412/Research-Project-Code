setwd("C:/Users/Windows/Documents/Masters/R LD50 Curves")
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

class(SLF_results$Dose_ng_per_bee)
class(SLF_results$ProportionDeadat48Hours)
SLF_results$Dose_ng_per_bee <- as.numeric(SLF_results$Dose_ng_per_bee)



SLF_fig2 <- ggplot(SLF_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args=list(family="binomial"), colour = "black", size =0.6)+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red", size= 0.6)+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  geom_vline(xintercept = 127.1, linetype = "dotted", colour = "red", size=0.6)+
  ggtitle("Sulfoxaflor")+
  theme_classic()+
  annotate("text", x=1100, y = 0.45, label= "LD50 = 127.1") + 
  theme(
    axis.title = element_text(size = 12),    # Change axis title font size
    axis.text = element_text(size = 11),
    plot.background = element_rect(color = "black", size = 0.5),# Change axis label font size
    plot.title = element_text(size = 16)     # Change plot title font size
  )
plot(SLF_fig2)

SLFlm <- glm(ProportionDeadat48Hours ~ Dose_ng_per_bee, family = "binomial", data = SLF_results)
summary(SLFlm)
#LD50=127.1

SLF_results[SLF_results$Dose_ng_per_bee == "0", "Dose_ng_per_bee"] <- 0.1
SLF_results$logdose <- log10(SLF_results$Dose_ng_per_bee)
 
#semilogs 
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
# TMX_results[TMX_results$Dose_ng_per_bee == "20", "ProportionDeadat48Hours"] <- 1

class(TMX_results$Dose_ng_per_bee)
class(TMX_results$ProportionDeadat48Hours)
TMX_results$Dose_ng_per_bee <- as.numeric(TMX_results$Dose_ng_per_bee)
TMX_results$ProportionDeadat48Hours <- as.numeric(TMX_results$ProportionDeadat48Hours)
library(drc)
TMX_fig <- ggplot(TMX_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = drm, method.args = list(fct = L.4()), se = FALSE, colour = "black")+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  theme_bw()

plot(TMX_fig)

library(ggplot2)
TMX_fig2 <- ggplot(TMX_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args=list(family="binomial"), colour = "black", size =0.6)+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red", size=0.6)+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  geom_vline(xintercept = 6.412, linetype = "dotted", colour = "red", size =0.6)+
  theme_classic()+
  ggtitle("Thiamethoxam")+
  annotate("text", x =11, y = 0.45, label = "LD50 = 6.412") +
  theme(
    axis.title = element_text(size = 12),    # Change axis title font size
    axis.text = element_text(size = 11),     # Change axis label font size
    plot.background = element_rect(color = "black", size = 0.5),
    plot.title = element_text(size = 16)     # Change plot title font size
  )

plot(TMX_fig2)
TMXlm <- glm(ProportionDeadat48Hours ~ Dose_ng_per_bee, family = "binomial", data = TMX_results)
summary(TMXlm)
#LD50=6.412

TMX_results[TMX_results$Dose_ng_per_bee == "0", "Dose_ng_per_bee"] <- 0.1
TMX_results$logdose <- log10(TMX_results$Dose_ng_per_bee)

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
FLU_proportions


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

library(drc)
FLU_fig <- ggplot(FLU_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = drm, method.args = list(fct = L.4()), se = FALSE, colour = "black")+
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red")+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  theme_bw()

plot(FLU_fig)

FLU_fig2 <- ggplot(FLU_results, aes(x = Dose_ng_per_bee, y = ProportionDeadat48Hours)) +
  geom_point(colour = "black")+
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args=list(family="binomial"), colour = "black", size = 0.6)+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red", size =0.6)+
  labs(x= "Dose (ng/bee)", y = "Mortality rate")+
  geom_vline(xintercept = 18365.32, linetype = "dotted", colour = "red", size =0.6)+
  theme_classic()+
  ggtitle("Fluparydifurone mortality curve")+
  annotate("text", x = 27000, y = 0.52, label = "LD50 = 18,365.52")+
  scale_x_continuous(
    "Fluparydifurone Dose (ng/bee)",
    sec.axis = sec_axis(~ . /8000, name = "Acetone Dose (%)")
  )+
  theme(
    axis.title = element_text(size = 10),    # Change axis title font size
    axis.text = element_text(size = 9),     # Change axis label font size
    plot.title = element_text(size = 11)    # Change plot title font size
  )

plot(FLU_fig2)


FLUlm <- glm(ProportionDeadat48Hours ~ Dose_ng_per_bee, family = "binomial", data = FLU_results)
summary(FLUlm)
#LD50=18365.52

#removing values at 5000/10000/20000 - low sample size





FLU_results <- subset(FLU_results, Dose_ng_per_bee != "5000") 
FLU_results <- subset(FLU_results, Dose_ng_per_bee != "10000") 
FLU_results <- subset(FLU_results, Dose_ng_per_bee != "20000") 
FLU_results

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
              method.args=list(family="binomial"), aes(colour = "Flupyradifurone"), size =1.0)+
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red", size =0.6)+
  labs(x= "Dose of flupyradifurone ng/bee  + (% acetone by volume)", y = "Mortality rate")+
  geom_vline(xintercept = 17665.52, linetype = "dotted", colour = "red", size =0.6)+
  theme_classic()+
  ggtitle("Flupyradifurone")+
  annotate("text", x = 34000, y = 0.45, label = "LD50 = 18,365.52", size=5)+
  geom_point(data = ACE_results, colour = "blue")+
  geom_smooth(data = ACE_results, method = "glm", 
              se = FALSE,
              method.args=list(family="binomial"), aes(colour = "Acetone"), size =1.0)+
  scale_color_manual(name = "Compounds", breaks = c("Flupyradifurone", "Acetone"),
                    values = c("Flupyradifurone"="black", "Acetone" = "blue"))+
  scale_x_continuous(breaks = c(0, 20000, 40000, 60000, 80000),
                            labels = c("0(0%)", "20000 (2.5%)", "40000 (5%)", "60000 (7.5%)", "80000 (10%)"))+
  theme(
    axis.title = element_text(size = 18),    # Change axis title font size
    axis.text = element_text(size = 15),     # Change axis label font size
    plot.title = element_text(size = 22), 
    legend.title = element_text(size = 16),
    legend.text= element_text(size= 16), 
    legend.key.size = unit(0.3, "cm"),
    plot.background = element_rect(color = "black", size = 0.5))
  
  

plot(comp_fig)

  plot_layout(ncol = 3) 
library(patchwork)
combined_LD50<- TMX_fig2  +plot_spacer() + SLF_fig2 + plot_spacer()  +comp_fig  + plot_layout(widths= c(1.1, 0.05, 1.1, 0.05, 1.1), heights = c(3,2,2)) +
  plot_annotation(title = "LD50 Mortality Curves")
plot(combined_LD50)

library(gridExtra)

grid.arrange(TMX_fig2, SLF_fig2, comp_fig, nrow = 2)





plot(TMX_fig2)
plot(SLF_fig2)

comp_fig_1<-comp_fig + theme(axis.title.y = element_text(angle = 0, vjust=0.5)) +
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

plot(comp_fig_1)
