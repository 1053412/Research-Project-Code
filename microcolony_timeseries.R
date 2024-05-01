library(ggplot2)
library(dplyr)

timeseries<- read.csv ( "C:\\Users\\Windows\\Documents\\Masters\\key datasets for coding\\timseries.csv")
#used later in stats and consumption
microcolony_timeseries<- read.csv ("C:\\Users\\Windows\\Documents\\Masters\\key datasets for coding\\micrcolony_timeseries.csv")
#day vs proportion lifestage graphs

#eggs 
eggs<- subset(timeseries, select = c(day_of_experiment, colony, dose, eggs))

proportion_eggs <- eggs %>%
  group_by(colony, day_of_experiment) %>%
  summarize(proportion = mean(eggs == "Y"))


egg_plot<- ggplot(proportion_eggs, aes(x = day_of_experiment, y = proportion, color = colony)) +
  geom_line(size= 0.5, alpha=0.6)+ 
  geom_point(size=0.8)+
  labs(x = "Day", y = "Proportion of Colonies", title= "1. Eggs") +
  theme_classic()+
   scale_color_manual(values = custom_palette_2, name= 'Treatment',labels= c("Control", "Flupyradifurone", "Sulfoxaflor", "Thiamethoxam")) +
  coord_cartesian(xlim = c(0, 30)) +
  scale_x_continuous(breaks = seq(0, max(proportion_eggs$day_of_experiment), by = 2))+
  theme(
    text = element_text(size = 8),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 9), 
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.margin = margin(0.5, 0.7, 0.5, 0.7, "cm")
  )+geom_text(data = proportion_eggs[proportion_eggs$colony %in% c("FLU"), ], aes(label = "*"), color = "red", size = 8.5, vjust = 0, hjust = 0, x= 9, y=0.49) +
geom_text(data = proportion_eggs[proportion_eggs$colony %in% c("SLF"), ], aes(label = "*"), color = "red", size = 8.5, vjust = 0, hjust = 0, x= 6.5, y=0.6) 

  plot(egg_plot)
  
 
custom_palette <- c("CTL" = "black", "TMX" = "#C77CFF", "SLF" = "#00AFBB", "FLU" = "red")
custom_palette_2<-c("CTL" = "black", "TMX" = "deeppink", "SLF" = "darkorange", "FLU" = "#00BFC4" )
library(viridis)


#larvae 
larvae<- subset(timeseries, select = c(day_of_experiment, colony, dose, early_larvae))

proportion_larvae <- larvae %>%
  group_by(colony, day_of_experiment) %>%
  summarize(proportion = mean(early_larvae == "Y"))


larvae_plot<- ggplot(proportion_larvae, aes(x = day_of_experiment, y = proportion, color = colony)) +
  geom_line(size= 0.5, alpha=0.6)+ 
  geom_point(size=0.8)+
  labs(x = "Day", y = "Proportion of Colonies", title =" 2. First-Third Instar Larvae") +
  theme_classic()+
  scale_color_manual(values = custom_palette_2)+
  coord_cartesian(xlim = c(0, 30)) +
  scale_x_continuous(breaks = seq(0, max(proportion_larvae$day_of_experiment), by = 2))+
  theme(legend.position = "none") +
  theme(
    text = element_text(size = 8),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
  )+ geom_text(data = proportion_eggs[proportion_eggs$colony %in% c("FLU"), ], aes(label = "*"), color = "red", size = 8.5, vjust = 0, hjust = 0, x= 13, y=0.35) 



plot(larvae_plot)

#custom_palette <- c("CTL" = "#00BFC4", "TMX" = "#F8766D", "SLF" = "#7CAE00", "FLU" = "#C77CFF")


#fourth larvae
fourth<- subset(timeseries, select = c(day_of_experiment, colony, dose, fourth_larvae))

proportion_fourth <- fourth %>%
  group_by(colony, day_of_experiment) %>%
  summarize(proportion = mean(fourth_larvae == "Y"))


  fourth_plot<- ggplot(proportion_fourth, aes(x = day_of_experiment, y = proportion, color = colony)) +
  geom_line(size= 0.5, alpha=0.6)+ 
  geom_point(size=0.8)+
  labs(x = "Day",y = "Proportion of Colonies", title = ' 3. Fourth Instar Larvae') +
  theme_classic()+
  scale_color_manual(values = custom_palette_2)+
  coord_cartesian(xlim = c(0, 30)) +
  scale_x_continuous(breaks = seq(0, max(proportion_fourth$day_of_experiment), by = 2))+
  theme(legend.position = "none", )+
  theme(
    text = element_text(size = 8),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 9), 
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
  ) + geom_text(data = proportion_eggs[proportion_eggs$colony %in% c("FLU"), ], aes(label = "*"), color = "red", size = 8.5, vjust = 0, hjust = 0, x= 22.3, y=0.49) +
    geom_text(data = proportion_eggs[proportion_eggs$colony %in% c("SLF"), ], aes(label = "*"), color = "red", size = 8.5, vjust = 0, hjust = 0, x= 19.5, y=0.49)+
    geom_text(data = proportion_eggs[proportion_eggs$colony %in% c("TMX"), ], aes(label = "*"), color = "red", size = 8.5, vjust = 0, hjust = 0, x= 18, y=0.49) 
  
  

plot(fourth_plot)

#custom_palette <- c("CTL" = "#00BFC4", "TMX" = "#F8766D", "SLF" = "#7CAE00", "FLU" = "#C77CFF")


#pupae
pupae<- subset(timeseries, select = c(day_of_experiment, colony, dose, pupae))

proportion_pupae <- pupae %>%
  group_by(colony, day_of_experiment) %>%
  summarize(proportion = mean(pupae == "Y"))


pupae_plot<- ggplot(proportion_pupae, aes(x = day_of_experiment, y = proportion, color = colony)) + # Set alpha value for 'Colony A'
  geom_line(size= 0.5, alpha=0.6)+ 
  geom_point(size=0.8)+
  labs(x = "Day", y = "Proportion of Colonies", title= "4. Pupae") +
  theme_classic()+
  scale_color_manual(values = custom_palette_2)+
  coord_cartesian(xlim = c(0, 30)) +
  scale_x_continuous(breaks = seq(0, max(proportion_pupae$day_of_experiment), by = 2))+
  theme(legend.position = "none")+
  coord_cartesian(ylim=c(0.0,1.0))+
  theme(
    text = element_text(size = 8),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
  ) +geom_text(data = proportion_eggs[proportion_eggs$colony %in% c("FLU"), ], aes(label = "*"), color = "red", size = 8.5, vjust = 0, hjust = 0, x= 26, y=0.25) +
    geom_text(data = proportion_eggs[proportion_eggs$colony %in% c("TMX"), ], aes(label = "*"), color = "red", size = 8.5, vjust = 0, hjust = 0, x= 21.8, y=0.25) 


plot(pupae_plot)

#custom_palette <- c("CTL" = "#00BFC4", "TMX" = "#F8766D", "SLF" = "#7CAE00", "FLU" = "#C77CFF")


custom_palette <- c("CTL" = "#00BFC4", "TMX" = "#F8766D", "SLF" = "#7CAE00", "FLU" = "#C77CFF")

library(patchwork)

combined_timeseries<-  egg_plot + larvae_plot + fourth_plot + pupae_plot + plot_layout(guides = "collect")  + plot_annotation(title = "Progression of Microcolony-Brood Through Four Key Developmental Stages", 
                                  theme= theme( plot.title = element_text(size = 12)))

                          
plot(combined_timeseries)

library(lme4)

timeseries_binary<-read.csv("C:\\Users\\Windows\\Documents\\Masters\\key datasets for coding\\timeseries.binary.csv")



library(lmerTest) # I am now trying a generalized linear mixed effect model (glmer better for binary count data) (logistic regression)
#A mixed-effects logistic regression model is a type of generalized linear mixed-effects model (GLMM) with a binomial family

# Choose one life history stage



# i think i need to reduce range of days to only where dveelopment occurs as the outputs are not great, dont match the visuals 
eggs.binary<- read.csv("C:\\Users\\Windows\\Documents\\Masters\\key datasets for coding\\eggs.binary.csv") # Example: days 3, 7, and 9 to exclude

# Filter out the excluded days from the dataset
eggs_binary <- eggs.binary %>%
  filter(day_of_experiment !=  "0", )

control<- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))#run these
p_control<- glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 100000))

model_eggs <- glmer(eggs ~ day_of_experiment * colony + (1|colony_ID), data = eggs.binary , 
               family=binomial, 
               control= control)

model_eggs_2 <- glmer(eggs ~ day_of_experiment + colony + (1|colony_ID), data = eggs.binary , 
                    family=binomial, 
                    control= control)


AIC(model_eggs, model_eggs_2) #without itneraction term better 


summary(model_eggs_2)
anova(model_eggs_2)
library(car)
Anova(model_eggs_2)

library(emmeans)
timeseries_post_mod = emmeans(model_eggs_2,  ~ colony)
timeseries_outcome<- pairs(timeseries_post_mod)
timeseries_outcome

timeseries_emm_interaction <- emmeans(model_eggs_2, specs = pairwise ~ day_of_experiment * colony)

contrast(timeseries_emm_interaction, method = "pairwise")

#larvae - filtered out day 0-3 as no larvae developed in any of the categories 

larvae.binary<- read.csv("C:\\Users\\Windows\\Documents\\Masters\\key datasets for coding\\larvae.binary.csv")
larvae.binary <- larvae.binary %>%
  filter(day_of_experiment !=  c("0", "1"))

model_larvae <- glmer(early_larvae ~ day_of_experiment * colony + (1|colony_ID), data = larvae.binary, 
                 family=binomial, 
                 control= control)

model_larvae_2 <- glmer(early_larvae ~ day_of_experiment + colony + (1|colony_ID), data = larvae.binary, 
                      family=binomial, 
                      control= control)

control<- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

anova(model_larvae, model_larvae_2)
AIC(model_larvae, model_larvae_2)

summary(model_larvae_2)
Anova(model_larvae_2)




larvae_post_mod = emmeans(model_larvae_2,  ~ colony)
larvae_outcome<- pairs(larvae_post_mod)
larvae_outcome

#fourth

fourth.binary<- read.csv("C:\\Users\\Windows\\Documents\\Masters\\key datasets for coding\\fourth. binary.csv")

excluded_days <- c("0", "1", "4", "8")  # Days to exclude

# Filter out the excluded days from the dataset
fourth.binary <- fourth.binary %>%
  filter(!day_of_experiment %in% excluded_days)

model_fourth <- glmer(fourth_larvae ~ day_of_experiment * colony + (1|colony_ID), data = fourth.binary, 
                      family=binomial, 
                      control= p_control)

model_fourth_2 <-glmer(fourth_larvae ~ day_of_experiment + colony + (1|colony_ID), data = fourth.binary, 
                     family=binomial, 
                     control= p_control)

anova(model_fourth, model_fourth_2)
AIC(model_fourth, model_fourth_2)

summary(model_fourth_2)
Anova(model_fourth_2)

library(lme4)
fourth_post_mod = emmeans(model_fourth_2,  ~ colony)
fourth_outcome<- pairs(fourth_post_mod)
fourth_outcome

#pupae - this one has issues because the treatments dont reach p=1.0, i dont think the ouputs are right 
library(glmmTMB)


pupae.binary<- read.csv("C:\\Users\\Windows\\Documents\\Masters\\key datasets for coding\\pupae. binary.csv")

p_excluded_days <- c("0", "1", "4", "8", "11")  # Days to exclude

# Filter out the excluded days from the dataset
pupae.binary <- pupae.binary %>%
  filter(!day_of_experiment %in% p_excluded_days)


library(pscl)
control_parameters <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2))

model_pupae <- glmer(pupae ~ day_of_experiment * colony + (1|colony_ID), data = pupae.binary, 
                      family=binomial,
                     control=p_control)

model_pupae_2 <- glmer(pupae ~ day_of_experiment + colony + (1|colony_ID), data = pupae.binary, 
                     family=binomial,
                     control= p_control)


anova(model_pupae, model_pupae_2)
AIC(model_pupae, model_pupae_2)

summary(model_pupae_2)
Anova(model_pupae_2)

pupae_post_mod = emmeans(model_pupae_2,  ~ colony)
pupae_outcome<- pairs(pupae_post_mod)
pupae_outcome


print(c(timeseries_outcome, larvae_outcome, fourth_outcome, pupae_outcome))





#part 2- consumption- sucrose 

#colony level consumption - might need correcting to standardise for colony per day 
microcolony_timeseries<- read.csv ("C:\\Users\\Windows\\Documents\\Masters\\key datasets for coding\\micrcolony_timeseries.csv")
#filtering out day 29 as no consumption
microcolony_timeseries <- microcolony_timeseries %>%
  filter(! day_of_experiment == 29)


consumption<- subset(microcolony_timeseries, select = c(day_of_experiment, colony, dose, amount_drank_g, density_adjusted_drank_ml, consumption_colony_day, consumption_bee_day_ml, consumption_bee_day_active_ngul))

consumption<- na.omit(consumption) #no feed given on 29th day so omitted 

col_avg_consumption <- aggregate(consumption_colony_day ~ day_of_experiment + colony, data = consumption, FUN = mean)

colony_consumption_plot<-ggplot(col_avg_consumption, aes(x = day_of_experiment, y = consumption_colony_day, color = colony)) +
  geom_line(size=0.8, alpha=0.8) +
  geom_point(size=0.8)+
  labs(title = "Colony Consumption of Sucrose over time ",
       x = "Day",
       y = "Average Consumption Per Colony per day (ml)") +
  scale_color_manual(values = custom_palette_2, name= 'Treatment',labels= c("Control", "Flupyradifurone", "Sulfoxaflor", "Thiamethoxam"))+
  coord_cartesian(ylim= c(1.0, 2.5))+
  theme_minimal()

plot(colony_consumption_plot)

#points alone rather than mean data? 
ggplot(consumption, aes(x= day_of_experiment, y=consumption_colony_day, colour=colony )) +
  geom_point()+
  geom_smooth(method= 'lm')+
  theme_minimal()

#individual  bee consumption

bee_avg_consumption<- aggregate(consumption_bee_day_ml ~ day_of_experiment + colony, data = consumption, FUN = mean)

  bee_consumption_plot<-ggplot(bee_avg_consumption, aes(x = day_of_experiment, y = consumption_bee_day_ml, color = colony)) +
  geom_line(size= 0.8, alpha=0.8) +
    scale_color_manual(values = custom_palette_2)+
  labs(title = "Average Consumption of Sucrose (+ Pesticide Treatment)",
       x = "Day",
       y = "Average Consumption per Bee per Day(ml)") +
    coord_cartesian(ylim= c(0.20, 0.50)) +
  theme_minimal()
  
plot(bee_consumption_plot)

overlayed_plot <- colony_consumption_plot + bee_consumption_plot

print(overlayed_plot)


  
#consumption- pollen from blank tube 
  #colony level also needs correcting to colony use per day 
  
  #individual bee level - think stick to colony level below 

  pollen_consumption<- subset(microcolony_timeseries, select = c(day_of_experiment, colony, colony_ID ,pol_used_bee_day_g))
  
  avg_pollen_consumption <- pollen_consumption%>%
    group_by(colony, day_of_experiment) %>%
    summarise(avg_pollen_consumed_per_bee = mean(pol_used_bee_day_g, na.rm = TRUE))

  
  ggplot(avg_pollen_consumption, aes(x = day_of_experiment, y = avg_pollen_consumed_per_bee, color = colony)) +
    geom_line(size=0.8, alpha = 2.0) +  # Set alpha value for 'Colony A'
    labs(title = "Individual Bee Consumption of Pollen Over Time",
         x = "Day",
         y = "Average Consumption per Bee per Day (g)") +
    scale_y_continuous(breaks = seq(0.00,0.10, by = 0.01))+
    theme_minimal()

  #colony level
 #adding a column that is just times 5, individual consumption times 5 for colony level 
  
  pollen_consumption <- pollen_consumption %>% 
    mutate(colony_consumption_per_day = pol_used_bee_day_g * 5)

  #mean
  avg_colony_pollen_consumption <- pollen_consumption%>%
    group_by(colony, day_of_experiment) %>%
    summarise(avg_pollen_consumed_per_colony_per_day = mean(colony_consumption_per_day, na.rm = TRUE))
    
  #se
  se_colony_pollen_consumption<- pollen_consumption %>%
    group_by(colony, day_of_experiment) %>%
    summarise(std_err = sd(colony_consumption_per_day, na.rm = TRUE) / sqrt(n()))
  
  
  #merging se and mean
  
  merged_se_mean<- full_join(se_colony_pollen_consumption, avg_colony_pollen_consumption, by = c("colony", "day_of_experiment")) 
  
pollen_colony_plot<-ggplot(merged_se_mean, aes(x = day_of_experiment, y = avg_pollen_consumed_per_colony_per_day, color = colony)) +
    geom_line( alpha = 0.6, size= 0.6) +
    geom_point(size=0.8) +
    labs(title = "Microcolony Consumption of Pollen Over Time",
         x = "Day",
         y = "Average Consumption per Colony per Day (g)") +
    geom_errorbar(data = merged_se_mean, aes(ymin = avg_pollen_consumed_per_colony_per_day -std_err, ymax = avg_pollen_consumed_per_colony_per_day + std_err), width = 0.2) +
    scale_color_manual(values = custom_palette_2, name= 'Treatment',labels= c("Control", "Flupyradifurone", "Sulfoxaflor", "Thiamethoxam"))+
    scale_x_continuous(breaks = seq(0, max(merged_se_mean$day_of_experiment), by = 2)) + 
    coord_cartesian(ylim= c(0.0, 0.5)) +
    theme_classic()+
  theme(
    axis.title = element_text(size = 10), # Change size of axis titles
    axis.text = element_text(size = 9),  # Change size of axis labels
    plot.title = element_text(size = 12), # Change size of plot title
    axis.title.x = element_text(margin = margin(t = 10)), # Adjust margin for x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  ) 
  
  plot(pollen_colony_plot)

  library (lmerTest) 
  
  #colony=treatment (CTL, TMX, SLF, FLU) Colony ID = CTL1, 2, 3 etc

    pol_cons_mod<-lmer(colony_consumption_per_day ~ day_of_experiment * colony + (1|colony_ID), data= pollen_consumption )  
  summary(pol_cons_mod)
  anova(pol_cons_mod)
  
  pol_cons_mod2<-lmer(colony_consumption_per_day ~ day_of_experiment + colony + (1|colony_ID), data= pollen_consumption )  
  #without interaction
  
  
  summary(pol_cons_mod2)
  anova(pol_cons_mod2)
  
 
  
  #HOWEVER, interaction is signfiicant so need to keep it in
  
  #post-hoc tests 
  
  anova (pol_cons_mod, pol_cons_mod2)
  library(emmeans)
  post_mod = emmeans(pol_cons_mod,  ~ colony |day_of_experiment)
  outcome<- pairs(post_mod)
  outcome
  
  emm_interaction <- emmeans(pol_cons_mod, specs = pairwise ~ day_of_experiment * colony)
  
  contrast(emm_interaction, method = "pairwise")
  

  contrast(emm_interaction, method = "pairwise")
  
  plot(emm_interaction)
  
  
  
