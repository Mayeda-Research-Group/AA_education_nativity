# input
# gen_edu_binary_cox_age_scale_2022_04_18_v2.xlsx
# gen_edu_binary_aalen_age_scale_2022_04_18_start_time60_v2.xlsx

# objective of this file
# Make figures for the interactions (education*nativity)

#path to working directories
path_to_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Result/"
path_to_figures <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/Revise_1/Figure/"

#load packages
library(openxlsx)
library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)

#Cox model
#read gen_edu_binary_cox_age_scale_2022_04_18_v2.xlsx -> data_edu_nativity_cox
data_edu_nativity_cox <- read.xlsx(paste0(path_to_data,"gen_edu_binary_cox_age_scale_2022_04_18_v2.xlsx"), sheet = 1)
data_edu_nativity_cox <- as.data.table(data_edu_nativity_cox)
data_edu_nativity_cox[,HR:= as.numeric(`Estimates.(HR)`)]
data_edu_nativity_cox$`Estimates.(HR)` <- NULL

#extract confidence limits
data_edu_nativity_cox$Lower_95 <- substr(data_edu_nativity_cox$`95%.CI`,3,6) %>% as.numeric()
data_edu_nativity_cox$Upper_95 <- substr(data_edu_nativity_cox$`95%.CI`,9,12) %>% as.numeric()

ci_ceiling <- 1.42
ci_floor <- 0.54

data_edu_nativity_cox[Upper_95 > ci_ceiling, upper_out:=ci_ceiling]
data_edu_nativity_cox[Lower_95 < ci_floor, lower_out:=ci_floor]

data_edu_nativity_cox[!is.na(upper_out), Upper_95:=upper_out-0.005]
data_edu_nativity_cox[!is.na(lower_out), Lower_95:=lower_out+0.003]

#variable prep
data_edu_nativity_cox$Nativity <- factor(data_edu_nativity_cox$Nativity,
                                           levels = c("US-born", "Foreign-born"))
data_edu_nativity_cox[Subpopulation=="Asian", Subpopulation:="Overall"]

fig_nativity_cox <- ggplot(
  data=data_edu_nativity_cox,
  aes(x = Subpopulation, y = HR, 
      ymin = Lower_95, ymax = Upper_95, group = Nativity)) +
  geom_pointrange(data=data_edu_nativity_cox,
                  fatten = 8,
                  aes(color = Nativity),
                  position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin = Lower_95, ymax = Upper_95, color = Nativity), position=position_dodge(width=0.6),
                width=0.1,cex = 1) +
  geom_hline(yintercept = 1, linetype = 2) +
  theme_bw() +
  ylab("Hazard ratio for dementia incidence (log scale) \n(college degree or higher versus less than college degree)") +
  scale_y_log10(breaks=seq(0.5,1.5,by=0.1)) +
  xlab('') +
  scale_x_discrete(limits=c("Overall", "Chinese", "Filipino", "Japanese")) +
  theme(legend.position= c(0.2, 0.8),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 10),
        strip.text = element_text(face = "bold", size = rel(1.5)),
        plot.title = element_text(size = 16, face = "bold"),
        strip.background = element_blank()) + guides(color = guide_legend(title = NULL)) +
  geom_point(aes(y = upper_out, colour = Nativity,fill = Nativity), shape = 24, 
             position = position_dodge(width = 0.6), size = 2.6, show.legend = F) +
  geom_point(aes(y = lower_out, colour = Nativity,fill = Nativity), shape = 25, 
             position = position_dodge(width = 0.6), size = 2.6, show.legend = F) +
  scale_color_manual(values = c("#E69F00","#0072B2")) +
  scale_fill_manual(values = c("#E69F00","#0072B2"))
fig_nativity_cox

#Aalen model
#read gen_edu_binary_aalen_age_scale_2022_04_18_start_time60_v2.xlsx -> data_edu_nativity_aalen
data_edu_nativity_aalen <- read.xlsx(paste0(path_to_data,"gen_edu_binary_aalen_age_scale_2022_04_18_start_time60_v2.xlsx"), sheet = 1)
data_edu_nativity_aalen <- as.data.table(data_edu_nativity_aalen)
data_edu_nativity_aalen$Estimates <- data_edu_nativity_aalen$`Estimates.per.1000.person-years` %>% as.numeric()
data_edu_nativity_aalen$`Estimates.per.1000.person-years` <- NULL

data_edu_nativity_aalen[Nativity=="Foreign-born" & 
                          Subpopulation=="Japanese",]$`95%.CI` # " (-3.3, 8.61)"
data_edu_nativity_aalen[Nativity=="Foreign-born" & 
                          Subpopulation=="Japanese",]$`95%.CI` <- " (-3.30, 8.61)"

#extract confidence limits
data_edu_nativity_aalen$Lower_95 <- substr(data_edu_nativity_aalen$`95%.CI`,3,7) %>% as.numeric()
data_edu_nativity_aalen$Upper_95 <- substr(data_edu_nativity_aalen$`95%.CI`,10, nchar(data_edu_nativity_aalen$`95%.CI`)-1) %>% as.numeric()

ci_ceiling <- 4.92
# ci_floor <- 0.54
# 
data_edu_nativity_aalen[Upper_95 > ci_ceiling, upper_out:=ci_ceiling]
# data_edu_nativity_aalen[Lower_95 < ci_floor, lower_out:=ci_floor]
# 
data_edu_nativity_aalen[!is.na(upper_out), Upper_95:=upper_out-0.07]
# data_edu_nativity_aalen[!is.na(lower_out), Lower_95:=lower_out+0.07]

#variable prep
data_edu_nativity_aalen$Nativity <- factor(data_edu_nativity_aalen$Nativity,
                                           levels = c("US-born", "Foreign-born"))
data_edu_nativity_aalen[Subpopulation=="Asian", Subpopulation:="Overall"]

# Point plots
fig_nativity_aalen <- ggplot(
  data=data_edu_nativity_aalen,
  aes(x = Subpopulation, y = Estimates, 
      ymin = Lower_95, ymax = Upper_95, group = Nativity)) +
  geom_pointrange(data=data_edu_nativity_aalen,
           stat = "identity",
           fatten = 8,
           aes(color = Nativity, fill = Nativity),
           position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin = Lower_95, ymax = Upper_95, color = Nativity), position=position_dodge(width=0.6),
                width=0.1,cex = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  ylab("Hazard difference for dementia incidence \n(cases per 1,000 person-years) \n(college degree or higher versus less than college degree)") + 
  scale_y_continuous(breaks=seq(-6,5,by=1)) +
  xlab('') +
  scale_x_discrete(limits=c("Overall", "Chinese", "Filipino", "Japanese")) +
  theme(legend.position= c(0.2,0.8),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 10),
        strip.text = element_text(face = "bold", size = rel(1.5)),
        plot.title = element_text(size = 16, face = "bold"),
        strip.background = element_blank()) +
  guides(color = guide_legend(NULL), fill = guide_legend(title = NULL)) +
  geom_point(aes(y = upper_out, colour = Nativity,fill = Nativity), shape = 24,
             position = position_dodge(width = 0.6), size = 2.6, show.legend = F) +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
  scale_fill_manual(values = c("#E69F00", "#0072B2"))
fig_nativity_aalen

#save the result
ggsave(paste0(path_to_figures, "Education_nativity_cox_2022_12_07.eps"), plot = fig_nativity_cox, width = 20, height = 12, units = "cm")
ggsave(paste0(path_to_figures, "Education_nativity_cox_2022_12_07.pdf"), plot = fig_nativity_cox, width = 20, height = 12, units = "cm")
ggsave(paste0(path_to_figures, "Education_nativity_aalen_2022_12_15.eps"), plot = fig_nativity_aalen, width = 20, height = 12, units = "cm")
ggsave(paste0(path_to_figures, "Education_nativity_aalen_2022_12_15.pdf"), plot = fig_nativity_aalen, width = 20, height = 12, units = "cm")

