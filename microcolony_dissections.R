

library(readxl)
library(bbplot)


mc.data<- read.csv("C:\\Users\\Windows\\Documents\\Masters\\Data sheets\\micrcolony_dissections.csv")

#ideas:
#box plots of mean number of each stage 
#pupae box plot + pictures nice graphic
#plot that shows mean vs no of eggs, pupae, drones (+SE bars)

#CTL
CTL_subset<- subset(mc.data, compound == 'CTL')
CTL_numeric_data <- CTL_subset[, sapply(CTL_subset, is.numeric)]
CTL_means <- colMeans(CTL_numeric_data)
rounded_CTL <- round(CTL_means, 1)
rounded_CTL


#tmx
TMX_subset<-subset(mc.data, compound == 'TMX')
TMX_numeric_data <- TMX_subset[, sapply(TMX_subset, is.numeric)]
TMX_means <- colMeans(TMX_numeric_data)
rounded_TMX<- round (TMX_means, 1)
rounded_TMX

SLF_subset<- subset(mc.data, compound == 'SLF')
SLF_numeric_data <- SLF_subset[, sapply(SLF_subset, is.numeric)]
SLF_means <- colMeans(SLF_numeric_data)
rounded_SLF<- round (SLF_means, 1)
rounded_SLF

FLU_subset<- subset(mc.data, compound == 'FLU')
FLU_numeric_data <- FLU_subset[, sapply(FLU_subset, is.numeric)]
FLU_means <- colMeans(FLU_numeric_data)
rounded_FLU<- round (FLU_means, 1)
rounded_FLU


#combine means
means_combined <- data.frame(  # Assuming categories are represented by indices 1 through 10
  CTL = rounded_CTL,
  TMX = rounded_TMX,
  SLF = rounded_SLF,
  FLU = rounded_FLU
)

length(rounded_SLF)

# Reset row names
means_combined$Life_History_Stage <- row.names(means_combined)
row.names(means_combined) <- NULL
print(means_combined)

data.format<- read.csv("C:\\Users\\Windows\\Documents\\Masters\\data.format.csv")
data.format.pupaemerge<- read.csv("C:\\Users\\Windows\\Documents\\Masters\\data.format.pupaemerge.csv")

data.format$colony_ID <- c("CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10",
                           "CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10",
                           "CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10",
                           "CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10",
                           "CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10",
                           "CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10",
                           "CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10",
                           "CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10"
                      )

data.format.pupaemerge$colony_ID <- c("CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10",
                           "CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10",
                           "CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10",
                           "CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10",
                           "CTL1", "CTL2", "CTL3","CTL4", "CTL5", "CTL6", "CTL7", "CTL8", "CTL9", "CTL10", "CTL11", "CTL12", "CTL13", "CTL14",
                           "TMX1", "TMX2", "TMX3", "TMX4", "TMX5", "TMX6", "TMX7", "TMX8", "TMX9", "TMX10",
                           "SLF1", "SLF2", "SLF3", "SLF4", "SLF5", "SLF6", "SLF7", "SLF8",
                           "FLU1", "FLU2", "FLU3", "FLU4", "FLU5", "FLU6", "FLU7", "FLU8", "FLU9", "FLU10")
                         
                          
 data.format.pupaemerge<-na.omit(data.format.pupaemerge)                          

summary_stats <- aggregate(no ~ compound + lhs, data = data.format, FUN = function(x) {
  c(
    Mean = mean(x),
    SD = sd(x),
    SE = sd(x) / sqrt(length(x))
  )
})

#formatting SD/SE - long format correct values 
#importing summary stats manually as name changing was annoying 

summary<- read.csv(  "C:\\Users\\Windows\\Documents\\Masters\\key datasets for coding\\se.sd.csv")
#this contains means, sd and se 
summary<-na.omit(summary)


# Plot mean vs life history stage
library(ggplot2)
library(tidyverse)
#subset_data<-  subset(summary, lhs!= "s1_pupae", lhs != "s2_pupae",  lhs != "s3_pupae", lhs != "s4_pupae")

included_stages <- c("eggs", "first_inst", "firtd_inst", "fourth_inst")

# Filter the data to include only the specified life history stages
subset_data <- summary %>%
  filter(lhs %in% included_stages)
subset_data<-na.omit(subset_data)

subset_data$compound <- factor(subset_data$compound)

#trying to plot all pupae onto this plot as well rather than just stages


early_dev_dissections_plot<-ggplot(subset_data, aes(x = lhs, y = mean, color = compound, group=compound)) +
  geom_point(aes(group = compound), size= 1.4, alpha= 1.0, position = position_dodge(0.5))+
  #geom_line(size=0.9, alpha=0.7) +
  #geom_point(size=0.8) +
  geom_errorbar(aes(group = compound, ymin = mean - se, ymax = mean + se), width = 0.350, position=position_dodge(0.5), size=0.6)+
  labs( x = "Life History Stage", y = "Average Per Colony (Count)", title="1. Eggs and Larval Instars") +
  theme_classic() +
  scale_color_manual(values = custom_palette, name= 'Treatment',labels= c("Control", "Flupyradifurone", "Sulfoxaflor", "Thiamethoxam"))+
  #theme(axis.text.x = element_text(angle = 360, hjust = 1), axis.title.y = element_text(angle = 360, vjust = 0.5))+
  scale_x_discrete(labels = custom_breaks2) +
  coord_cartesian(ylim= c(0.0, 17.0))+
  scale_y_continuous(breaks = seq(0.0,16.0, by = 2))+
  theme(
    axis.title = element_text(size = 14), # Change size of axis titles
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14),
    legend.title = element_text(size = 12), # Change size of legend title
    legend.text = element_text(size = 12),
    legend.position = "none",
  axis.title.x = element_text(margin = margin(t = 10)), # Adjust margin for x-axis title
  axis.title.y = element_text(margin = margin(r = 10))
  ) 
# Rotate x-axis labels for better readability,

custom_breaks2 <- c( "Eggs", "First/Second\nInstar Larvae", "ThirdInstar\nLarvae", "Fourth Instar\nLarvae")
custom_palette<-c("CTL" = "black", "TMX" = "deeppink", "SLF" = "darkorange", "FLU" = "#00BFC4" )

plot(early_dev_dissections_plot)

#stats?
library(tidyr)

data.format_early<- data.format %>%
  subset(lhs %in% c("eggs", "first_inst", "third_inst", "fourth_inst"))

library(lmerTest)

early_mod<-glmer(no ~ lhs * compound + (1|colony_ID), data= data.format_early, family= poisson, control=p_control)  
p_control<- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

early_mod_2<-glmer(no ~ lhs + compound + (1|colony_ID), data= data.format_early, family= poisson, control=p_control)  



early_mod_3<-glmer(no ~ lhs * compound + (1|colony_ID), data= data.format_early, family= negative.binomial(1), control=p_control)  

early_mod_4<-glmer(no ~ lhs + compound + (1|colony_ID), data= data.format_early, family= negative.binomial(1), control=p_control)



AIC(early_mod, early_mod_2, early_mod_3, early_mod_4)

#mod 3 has lowest AIC , negative binomial with the interaction  but not significantly different from model 4 without interaction

anova(early_mod_3, early_mod_4)
library(car)
Anova(early_mod_3)


library(emmeans) #within treatements
early_post= emmeans(early_mod_3,  ~ lhs | compound)
early_outcome<- pairs(early_post)
early_outcome

#between treatments 

early_post2 = emmeans(early_mod_3,  ~ compound | lhs)
early_outcome_2<- pairs(early_post2)
early_outcome_2




#pupae graph
pupae_subset <- subset(summary,lhs != "eggs" & lhs != "first_inst" & lhs != "firtd_inst" & lhs != "fourth_inst")


pupae_plot_dissections<-ggplot(pupae_subset, aes(x = as.factor(lhs), y = mean, color = compound, group= compound)) +
  geom_point(aes(group = compound), size= 1.4, alpha= 1.0, position = position_dodge(0.6)) +
  geom_errorbar(aes(group = compound, ymin = mean - se, ymax = mean + se), width = 0.350, position=position_dodge(0.6), size=0.6)+
  scale_x_discrete(expand = c(1.0, 1.0)) +
  labs(x = " Pupal Development Stage", y = "Average Per Colony (Count)", title= "2. Pupal Stages 1-4" ,
       colour= 'Treatment') +
   theme_classic() +
  theme(legend.position = "right") +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), 
        legend.key.size = unit(1.0, 'lines')) +
 scale_x_discrete(labels = custom_breaks) +
  scale_color_manual(values = custom_palette, name= 'Treatment',labels= c("Control", "Flupyradifurone", "Sulfoxaflor", "Thiamethoxam"))+
  scale_y_continuous(breaks = seq(0.0,16.0, by = 2))+
  coord_cartesian(ylim= c(0.0, 17.0)) +
  theme(
    axis.title = element_text(size = 14), # Change size of axis titles
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14),
    legend.title = element_text(size = 12), # Change size of legend title
    legend.text = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 10)), # Adjust margin for x-axis title
    axis.title.y = element_text(margin = margin(r = 10))
  ) 
#title = "Counts (n) of Pupal Stages Present at Day 29 (End of Experiment)"
 
plot(pupae_plot_dissections) 

custom_breaks <- c("Stage 1", "Stage 2", "Stage 3", "Stage 4")

library(patchwork)

combined_dissection<- early_dev_dissections_plot + pupae_plot_dissections+  plot_layout(guides = "collect") + plot_annotation(
  title = "Average Number of Brood (count) per Colony at Each Development Observed Upon Dissection",
  theme = theme(plot.title = element_text(size= 16))  # Adjust the size here
)
plot(combined_dissection)

library(tidyr)

data.format_filtered<- data.format %>%
  subset(lhs %in% c("s1_pupae", "s2_pupae", "s3_pupae", "s4_pupae"))

library(lmerTest)

pupae_mod<-glmer(no ~ lhs * compound + (1|colony_ID), data= data.format_filtered, family= poisson, control=p_control)  
p_control<- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
summary(pupae_mod)
Anova(pupae_mod)

pupae_mod_2<-glmer(no ~ lhs + compound + (1|colony_ID), data= data.format_filtered, family= poisson, control=p_control)  



pupae_mod_3<-glmer(no ~ lhs * compound + (1|colony_ID), data= data.format_filtered, family= negative.binomial(1), control=p_control)  

pupae_mod_4<-glmer(no ~ lhs + compound + (1|colony_ID), data= data.format_filtered, family= negative.binomial(1), control=p_control)


anova(pupae_mod_3, pupae_mod_4)
AIC(pupae_mod, pupae_mod_2, pupae_mod_3, pupae_mod_4)
anova(pupae_mod_3)

#mod 3 has lowest AIC , negative binomial with the interaction  but not significantly different from model 4 without interaction


library(emmeans) #within treatements
pup_post_mod = emmeans(pupae_mod_3,  ~ lhs | compound)
p_outcome<- pairs(pup_post_mod)
p_outcome

#between treatments 
pup_post_mod2 = emmeans(pupae_mod_3,  ~ compound | lhs)
p_outcome2<- pairs(pup_post_mod2)
p_outcome2


#proportion data?
library(dplyr)

total_pupae <- data.format_filtered %>%
  group_by(compound) %>%
  summarise(total_pupae = sum(no))

# Merge total pupae back to the original data
df_merged <- data.format_filtered %>%
  left_join(total_pupae, by = "compound")

# Calculate the proportion of pupae at each stage for each treatment
proportions <- df_merged %>%
  group_by(compound, lhs) %>%
  summarise(proportion = sum(no) / total_pupae)  # Calculating proportion by dividing total pupae at each stage by total pupae for the treatment

# Plotting the proportions
ggplot(proportions, aes(x = lhs, y = proportion, fill = compound)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Pupae at Each Stage by Compound",
       x = "Stage",
       y = "Proportion") +
  coord_cartesian(ylim=c(0.0,1.0))
  theme_minimal()

library(stats)
  concise_dataset <- proportions %>%
    group_by(compound, lhs) %>%
    summarise(mean_proportion = mean(proportion))
  
  proportions_wide <- concise_dataset %>%
    pivot_wider(names_from = lhs, values_from = mean_proportion)
  
  proportions_wide$sample_size <- c("14", "10", "8", "10")
  
  s1 <- proportions_wide %>%
    select(compound, s1_pupae, sample_size) %>%
    group_by(compound) 
  
anova_result <- aov(formula = s1_pupae ~ compound, data = s1)
anova_result


counts<- read.csv("C:\\Users\\Windows\\Documents\\Masters\\key datasets for coding\\dissections.s1-4.csv")
head(counts)


#stage1
ctl = (0.2413793) * 14  # Assuming 14 samples for 'CTL'
flu = (0.6666667) * 10  # Assuming 10 samples for 'FLU'
slf = (0.4871795) * 8   # Assuming 8 samples for 'SLF'
tmx = (0.5675676) * 10  # Assuming 10 samples for 'TMX'


data <- c(ctl, flu, slf, tmx)

# Create a factor variable for treatment
treatment <- factor(rep(c('CTL', 'FLU', 'SLF', 'TMX'), c(length(ctl), length(flu), length(slf), length(tmx))))

# Perform one-way ANOVA
anova_result <- aov(data ~ treatment)
tukey_result <- TukeyHSD(anova_result)



# Summary of ANOVA
summary(anova_result)
tukey_result








library(emmeans)
# Perform emmeans pairwise comparisons
emmeans_result <- emmeans(kruskal_test_result, pairwise ~ compound)

# Print the emmeans pairwise comparisons
print(emmeans_result)
  
  
  
  
library(tidyverse)

data.format.pupaemerge<- data.format.pupaemerge %>%
  subset(lhs %in% c("eggs", "first_inst", "third_inst", "fourth_inst", "s1_pupae"))

library(lmerTest)

pupaeonly_mod<-glmer(no ~ lhs * compound + (1|colony_ID), data= data.format.pupaemerge, family= poisson, control=p_control)  
p_control<- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

pupaeonly_mod_2<-glmer(no ~ lhs + compound + (1|colony_ID), data= data.format.pupaemerge, family= poisson, control=p_control)  



pupaeonly_mod_3<-glmer(no ~ lhs * compound + (1|colony_ID), data= data.format.pupaemerge, family= negative.binomial(1), control=p_control)  

pupaeonly_mod_4<-glmer(no ~ lhs + compound + (1|colony_ID), data= data.format.pupaemerge, family= negative.binomial(1), control=p_control)



AIC(pupaeonly_mod, pupaeonly_mod_2, pupaeonly_mod_3,pupaeonly_mod_4)

#mod 3 has lowest AIC , negative binomial with the interaction  but not significantly different from model 4 without interaction

anova(early_mod_3, early_mod_4)
library(car)
Anova(pupaeonly_mod_4)
Anova(pupaeonly_mod_3)


library(emmeans) #within treatements
early_post= emmeans(early_mod_3,  ~ lhs | compound)
early_outcome<- pairs(early_post)
early_outcome

#between treatments 

pupaeonly_post2 = emmeans(pupaeonly_mod_3,  ~ compound | lhs)
pupaeonly_outcome_2<- pairs(pupaeonly_post2)
pupaeonly_outcome_2

  
  
  
  
 
 
 #boxplots - not sure if i need these after all :/
 #eggs 
 eggs<- read.csv( "C:\\Users\\Windows\\Documents\\Masters\\eggs.final.csv")
 library(tidyr)
 library(dplyr)
 
 # Reshape data from wide to long format
 eggs_long <- eggs %>%
   pivot_longer(cols = c(CTL, TMX, SLF, FLU), names_to = "Treatment", values_to = "Number_of_Eggs")
 
 eggs_long<-na.omit(eggs_long)
 
 mean_egg_data <- aggregate(Number_of_Eggs ~ Treatment, eggs_long, mean)
 
 egg_plot<-ggplot(eggs_long, aes(x = Treatment, y = Number_of_Eggs)) +
   geom_boxplot() +
   geom_point(data = mean_egg_data, aes(x = Treatment, y = Number_of_Eggs), shape= 3, size = 3) +
   geom_line(data = mean_egg_data, aes(x = Treatment, y = Number_of_Eggs, group = 1), color = "red") +
   labs(title = "Eggs",
        x = "Treatment",
        y = "Number of Eggs") +
   scale_color_manual(values = "red", labels = "Mean")+
   theme_bw()
 egg_plot
 #early larvae ( 1st-3rd instar)

 early_larvae<- read.csv( "C:\\Users\\Windows\\Documents\\Masters\\early_larvae.csv")
 
 library(tidyr)
 library(dplyr)
 
 # Reshape data from wide to long format
 larvae_long <- early_larvae%>%
   pivot_longer(cols = c(CTL, TMX, SLF, FLU), names_to = "Treatment", values_to = "Number_of_Larvae")
 
 larvae_long<-na.omit(larvae_long)
 
 mean_larvae_data <- aggregate(Number_of_Larvae ~ Treatment, larvae_long, mean)
 
 larvae_plot<-ggplot(larvae_long, aes(x = Treatment, y = Number_of_Larvae)) +
   geom_boxplot() +
   geom_point(data = mean_larvae_data, aes(x = Treatment, y = Number_of_Larvae), shape= 3, size = 3) +
   geom_line(data = mean_larvae_data, aes(x = Treatment, y = Number_of_Larvae, group = 1), color = "red") +
   labs(title = "Early Larval Stages (First--Third Instar)",
        x = "Treatment",
        y = "Number of Early Larvae") +
   scale_color_manual(values = "red", labels = "Mean")+
   theme_bw()
 
 #fourth instar

   forth<- read.csv( "C:\\Users\\Windows\\Documents\\Masters\\forth.csv")
 library(tidyr)
 library(dplyr)
 
 # Reshape data from wide to long format
 forth_long <- forth %>%
   pivot_longer(cols = c(CTL, TMX, SLF, FLU), names_to = "Treatment", values_to = "Number_of_Late_Larvae")
 
 forth_long<-na.omit(forth_long)
 
 mean_forth_data <- aggregate(Number_of_Late_Larvae ~ Treatment, forth_long, mean)
 
 fourth_plot<-ggplot(forth_long, aes(x = Treatment, y = Number_of_Late_Larvae)) +
   geom_boxplot() +
   geom_point(data = mean_forth_data, aes(x = Treatment, y = Number_of_Late_Larvae), shape= 3, size = 3) +
   geom_line(data = mean_forth_data, aes(x = Treatment, y = Number_of_Late_Larvae, group = 1), color = "red") +
   labs(title = "Late Larvae (Fourth Instar)",
        x = "Treatment",
        y = "Number of Fourth Instar Larvae") +
   scale_color_manual(values = "red", labels = "Mean")+
   theme_bw()
 

#pupae
boxplot_pupae<- read.csv( "C:\\Users\\Windows\\Documents\\Masters\\pupae.csv")
 library(tidyr)
 library(dplyr)
 
 # Reshape data from wide to long format
 boxplot_pupae_long <- boxplot_pupae %>%
   pivot_longer(cols = c(CTL, TMX, SLF, FLU), names_to = "Treatment", values_to = "Number_of_Pupae")
 
 boxplot_pupae_long<-na.omit(boxplot_pupae_long)
 
 mean_pupae_data <- aggregate(Number_of_Pupae ~ Treatment, boxplot_pupae_long, mean)
 
 boxplot_pupae_plot<-ggplot(boxplot_pupae_long, aes(x = Treatment, y = Number_of_Pupae)) +
   geom_boxplot() +
   geom_point(data = mean_pupae_data, aes(x = Treatment, y = Number_of_Pupae), shape= 3, size = 3) +
   labs(title = "Pupae",
        x = "Treatment",
        y = "Number of Pupae") +
   scale_color_manual(values = "red", labels = "Mean")+
   theme_bw()
 
 boxplot_pupae_plot
 #combined plot
 library(patchwork)

 combined_fig<- egg_plot + larvae_plot + fourth_plot + pupae_plot 

 combined_fig 
 
 #combined fig 2 (without pupae)
 
 combined_fig_nopupae<- egg_plot+  larvae_plot + fourth_plot

 combined_fig_nopupae 
 
 
 