# input
# gen_edu_three_cat_cox_age_scale_2022_04_18_v3.xlsx
# gen_edu_three_cat_aalen_age_scale_2022_04_18_start_time60_v4.xlsx

# objective of this file
# Make figures for the interactions (education*nativity)

#path to working directories
path_to_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Result/"
path_to_figures <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Figure/"

#load packages
library(openxlsx)
library(data.table)
library(ggplot2)
library(ggh4x)
library(gridExtra)
library(grid)
library(cowplot)
library(dplyr)

#Cox model
#read gen_edu_three_cat_cox_age_scale_2022_04_18_v3.xlsx -> data_edu_nativity_cox
data_edu_nativity_cox <- read.xlsx(paste0(path_to_data,"gen_edu_three_cat_cox_age_scale_2022_04_18_v3.xlsx"), sheet = 1)
data_edu_nativity_cox <- as.data.table(data_edu_nativity_cox)
data_edu_nativity_cox[,HR:= as.numeric(`Estimates.(HR)`)]
data_edu_nativity_cox$`Estimates.(HR)` <- NULL

data_edu_nativity_cox[Variable== "College" &
                        Nativity=="Foreign-born" & 
                          Subpopulation=="Chinese",]$`95%.CI`
data_edu_nativity_cox[Variable== "College" &
                        Nativity=="Foreign-born" & 
                        Subpopulation=="Chinese",]$`95%.CI` <- " (0.58, 0.90)"

data_edu_nativity_cox[Variable== "College" &
                        Nativity=="US-born" & 
                        Subpopulation=="Japanese",]$`95%.CI`
data_edu_nativity_cox[Variable== "College" &
                        Nativity=="US-born" & 
                        Subpopulation=="Japanese",]$`95%.CI` <- " (0.77, 1.20)"

data_edu_nativity_cox[Variable== "Less than high school" &
                        Nativity=="Foreign-born" & 
                        Subpopulation=="Filipino",]$`95%.CI`
data_edu_nativity_cox[Variable== "Less than high school" &
                        Nativity=="Foreign-born" & 
                        Subpopulation=="Filipino",]$`95%.CI` <- " (0.70, 1.20)"

#extract confidence limits
data_edu_nativity_cox$Lower_95 <- substr(data_edu_nativity_cox$`95%.CI`,3,6) %>% as.numeric()
data_edu_nativity_cox$Upper_95 <- substr(data_edu_nativity_cox$`95%.CI`,9,12) %>% as.numeric()

ci_ceiling <- 1.42
ci_floor <- 0.54

data_edu_nativity_cox[Upper_95 > ci_ceiling, upper_out:=ci_ceiling]
data_edu_nativity_cox[Lower_95 < ci_floor, lower_out:=ci_floor]

data_edu_nativity_cox[!is.na(upper_out), Upper_95:=upper_out-0.005]
data_edu_nativity_cox[!is.na(lower_out), Lower_95:=lower_out+0.005]

data_edu_nativity_cox[Subpopulation=="Asian", Subpopulation:="Overall"]
data_edu_nativity_cox$Subpopulation <- factor(data_edu_nativity_cox$Subpopulation,
                                         levels = c("Overall", "Chinese", "Filipino", "Japanese"))

data_edu_nativity_cox$Nativity <- factor(data_edu_nativity_cox$Nativity,
                                           levels = c("US-born", "Foreign-born"))

## Cox ver1
fig_nativity_cox_cat_asian <- ggplot(
  data=data_edu_nativity_cox,
  aes(x = Variable, y = HR, 
      ymin = Lower_95, ymax = Upper_95, group = Nativity)) +
  facet_nested(~factor(Subpopulation)+Nativity, scales = "free_x", space = "free_x") + #facet_nested
  geom_pointrange(data=data_edu_nativity_cox,
                  fatten = 8, aes(color = Nativity), position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin = Lower_95, ymax = Upper_95, color = Nativity), position=position_dodge(width=0.6),
                width=0.2,cex = 1) +
  geom_hline(yintercept = 1, linetype = 2) +
  theme_bw() +
  ylab("Hazard ratio for dementia incidence") +
  scale_x_discrete(breaks = c("College", "High school (ref.)", "Less than high school"),
                   labels=c("College degree or higher", "High school degree, GED, \ntechnical/trade school, or some college", "Less than high school degree")) + # updated on 07/17/2022
  scale_y_continuous(limits = c(0.5, 1.5), breaks=seq(0.5,1.5,by=0.1)) +
  xlab('') +
  # labs(shape = 'Education category') +
  theme(legend.position= "right",
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1,vjust = 0.5),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(face = "bold", size = rel(1)),
        plot.title = element_text(size = 16, face = "bold"),
        strip.background = element_blank()) + 
  guides(color = "none") +
  geom_point(aes(y = upper_out, colour = Nativity,fill = Nativity), shape = 24, 
             position = position_dodge(width = 0.6), size = 2.6, show.legend = F) +
  geom_point(aes(y = lower_out, colour = Nativity,fill = Nativity), shape = 25, 
             position = position_dodge(width = 0.6), size = 2.6, show.legend = F) +
  scale_color_manual(values = c("#E69F00","#0072B2")) +
  scale_fill_manual(values = c("#E69F00","#0072B2"))
fig_nativity_cox_cat_asian

#Aalen model
#read gen_edu_three_cat_aalen_age_scale_2022_04_18_start_time60_v4.xlsx -> data_edu_nativity_aalen
data_edu_nativity_aalen <- read.xlsx(paste0(path_to_data,"gen_edu_three_cat_aalen_age_scale_2022_04_18_start_time60_v4.xlsx"), sheet = 1)
data_edu_nativity_aalen <- as.data.table(data_edu_nativity_aalen)
data_edu_nativity_aalen$Estimates <- data_edu_nativity_aalen$`Estimates.per.1000.person-years` %>% as.numeric()
data_edu_nativity_aalen$`Estimates.per.1000.person-years` <- NULL

#extract confidence limits
data_edu_nativity_aalen$Lower_95 <- substr(data_edu_nativity_aalen$`95%.CI`,3,7) %>% as.numeric()
data_edu_nativity_aalen$Upper_95 <- substr(data_edu_nativity_aalen$`95%.CI`,10, nchar(data_edu_nativity_aalen$`95%.CI`)-1) %>% as.numeric()

ci_ceiling <- 9.92
ci_floor <- -4.92

data_edu_nativity_aalen[Upper_95 > ci_ceiling, upper_out:=ci_ceiling]
data_edu_nativity_aalen[Lower_95 < ci_floor, lower_out:=ci_floor]

data_edu_nativity_aalen[!is.na(upper_out), Upper_95:=upper_out-0.005]
data_edu_nativity_aalen[!is.na(lower_out), Lower_95:=lower_out+0.005]

data_edu_nativity_aalen[Subpopulation=="Asian", Subpopulation:="Overall"]
data_edu_nativity_aalen$Subpopulation <- factor(data_edu_nativity_aalen$Subpopulation,
                                              levels = c("Overall", "Chinese", "Filipino", "Japanese"))

data_edu_nativity_aalen$Nativity <- factor(data_edu_nativity_aalen$Nativity,
                                           levels = c("US-born", "Foreign-born"))

# data_edu_nativity_aalen_asian <- data_edu_nativity_aalen[Subpopulation=="Overall",]
# data_edu_nativity_aalen_asian_sub <- data_edu_nativity_aalen[Subpopulation!="Overall",]

## AAlen ver1
# Point plots (07/17/2022)
fig_nativity_aalen_asian <- ggplot(
  data=data_edu_nativity_aalen,
  aes(x = Variable, y = Estimates, 
      ymin = Lower_95, ymax = Upper_95, fill = Nativity)) +
  facet_nested(~factor(Subpopulation) + Nativity, scales = "free_x", space = "free_x") + #facet_nested
  geom_pointrange(stat = "identity",
           fatten = 8,
           aes(color = Nativity, fill = Nativity),
           position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin = Lower_95, ymax = Upper_95, color = Nativity), position=position_dodge(width=0.6),
                width=0.2,cex = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  ylab("Hazard difference for dementia incidence \n(per 1,000 person-years)") +
  scale_x_discrete(breaks = c("College", "High school (ref.)", "Less than high school"),
                   labels=c("College degree or higher", "High school degree, GED, \ntechnical/trade school, or some college", "Less than high school degree")) + # updated on 07/17/2022
  scale_y_continuous(breaks=seq(-10,10,by=1)) +
  xlab('') +
  theme(legend.position= "none",
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1,vjust = 0.5),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(face = "bold", size = rel(1)),
        plot.title = element_text(size = 16, face = "bold"),
        strip.background = element_blank()) +
  guides(color = "none", fill = "none") +
  geom_point(aes(y = upper_out, colour = Nativity,fill = Nativity), shape = 24,
             position = position_dodge(width = 0.6), size = 2.6, show.legend = F) +
  geom_point(aes(y = lower_out, colour = Nativity,fill = Nativity), shape = 25,
             position = position_dodge(width = 0.6), size = 2.6, show.legend = F) +
  scale_color_manual(values = c("#E69F00","#0072B2")) +
  scale_fill_manual(values = c("#E69F00","#0072B2"))
fig_nativity_aalen_asian

#save the result
ggsave(paste0(path_to_figures, "Education_nativity_three_cat_cox_2022_05_04_v5.png"), plot = fig_nativity_cox_cat_asian, width = 11.6, height = 8.2, units = "in")
ggsave(paste0(path_to_figures, "Education_nativity_three_cat_aalen_2022_05_04_v5.png"), plot = fig_nativity_aalen_asian, width = 11.6, height = 8.2, units = "in")
