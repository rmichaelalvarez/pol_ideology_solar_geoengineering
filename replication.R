
# Load packages:
library(ltm) # For vanilla IRT models
library(margins)
library(hIRT) # For hierarchical IRT models

library(plyr)
library(dplyr)
library(haven)
library(ggplot2)

library(naniar)
library(forcats)
library(marginaleffects)
library(ggeffects)
library(nnet)

library(ggpubr)
library(rcartocolor)

library(tidyr)

library(gtsummary)
library(survey)

library(scales)

#Load data

Survey_Nov2022_appended <- readRDS("subset_Survey_Nov2022_appended.rds")

## Descriptives

survey <- svydesign(id=~1, weights=~Weights,
                    data=Survey_Nov2022_appended)

prop.table(svytable(~solar_geo_heard,design=survey))

prop.table(svytable(~solar_geo_difference_cat,design=survey))

prop.table(svytable(~solar_geo_difference_cat,design=survey))

prop.table(svytable(~solar_geo_concerns_cat,design=survey))

prop.table(svytable(~solar_geo_reduce,design=survey))

#descriptive figures by heard of vs hasn't heard of

Survey_Nov2022_appended <- Survey_Nov2022_appended %>%
  mutate(solar_geo_heard_bin_cat = ifelse(solar_geo_heard_bin==1, "Heard little, some or a lot about SG",
                                          "Heard nothing about SG"))

Survey_Nov2022_appended$solar_geo_heard_bin_cat <- factor(Survey_Nov2022_appended$solar_geo_heard_bin_cat,
                                                          levels = c("Heard nothing about SG",
                                                                     "Heard little, some or a lot about SG"))

group.colors.2 <- c(`Heard nothing about SG` = "#E58606", `Heard little, some or a lot about SG` ="#5D69B1")

# Figure 1

p2_heard <- subset(Survey_Nov2022_appended, !is.na(solar_geo_difference_cat)) %>%
  pivot_longer(cols = c(solar_geo_difference_cat)) %>%
  dplyr::count(solar_geo_heard_bin_cat, value, wt = Weights) %>%
  dplyr::group_by(solar_geo_heard_bin_cat) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = value, y = prop, fill = as.factor(solar_geo_heard_bin_cat))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(.~ solar_geo_heard_bin_cat) +
  scale_fill_manual(values = group.colors.2, name = "Heard of SG", labels = c("Nothing or little", "Some or a lot")) +
  xlab("Solar geoengineering (SG) will make...") +
  ylab("Percentage of respondents") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 16, margin = margin(t = 15)),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16))

ggsave("Figure1.png", 
       plot = p2_heard, 
       width = 12,        # width in inches
       height = 8,        # height in inches
       dpi = 300,         # resolution (300 is recommended for publication)
       units = "in")      # units can be inches (in), centimeters (cm), or mill

# Figure 2
p3_heard <- subset(Survey_Nov2022_appended, !is.na(solar_geo_concerns_cat)) %>%
  pivot_longer(cols = c(solar_geo_concerns_cat)) %>%
  dplyr::count(solar_geo_heard_bin_cat, value,wt = Weights) %>%
  dplyr::group_by(solar_geo_heard_bin_cat) %>%
  dplyr:: mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = value, y = prop, fill = as.factor(solar_geo_heard_bin_cat))) +
  geom_bar(stat="identity")  +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(.~ solar_geo_heard_bin_cat) +
  scale_fill_manual(values=group.colors.2, name = "Heard of SG", labels = c("Nothing or little", "Some or a lot")) +
  xlab ("Concerned about solar geoengineering") + ylab ("percentage of respondents") +
  theme_minimal()+
  theme(legend.position = "none",
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 16, margin = margin(t = 15)),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16))

ggsave("Figure2.png", 
       plot = p3_heard, 
       width = 12,        # width in inches
       height = 8,        # height in inches
       dpi = 300,         # resolution (300 is recommended for publication)
       units = "in")      # units can be inches (in), centimeters (cm), or mill

# Figure 3
p4_heard <- subset(Survey_Nov2022_appended, !is.na(solar_geo_reduce)) %>%
  pivot_longer(cols = c(solar_geo_reduce)) %>%
  dplyr::count(solar_geo_heard_bin_cat, value,wt = Weights) %>%
  dplyr::group_by(solar_geo_heard_bin_cat) %>%
  dplyr:: mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = value, y = prop, fill = as.factor(solar_geo_heard_bin_cat))) +
  geom_bar(stat="identity")  +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(.~ solar_geo_heard_bin_cat) +
  scale_fill_manual(values=group.colors.2, name = "Heard of SG", labels = c("Nothing or little", "Some or a lot")) +
  xlab ("Solar geoengineering vs reducing carbon emissions") + ylab ("percentage of respondents") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(legend.position = "none",
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 16, margin = margin(t = 15)),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  scale_x_discrete(
    breaks = c('Invest in reducing emissions', 'Invest in geoengineering techniques', 'Do both'),
    labels = c('Invest in\nreducing emissions', 'Invest in geoengineering\ntechniques',  'Do both'))

ggsave("Figure3.png", 
       plot = p4_heard, 
       width = 12,        # width in inches
       height = 8,        # height in inches
       dpi = 300,         # resolution (300 is recommended for publication)
       units = "in")      # units can be inches (in), centimeters (cm), or mill


#####Models to create Fig4

group.colors.geo.heard <- c(`Heard nothing` = "#E58606",
                            `Heard little, some, or a lot` = "#5D69B1")

group.colors.politics <- c(`Extremely conservative` = "#CC6677",
                           `Extremely liberal` = "#88CCEE")


# by politics
# concerned
mod1_lm <- lm(solar_geo_concerns_bin ~ (better_off + Age + Woman + College +
                                          Religion + Ideo_num + cli_know_score + SocialMedia)*solar_geo_heard_bin,
              weights = Weights, data = Survey_Nov2022_appended)

pred_con_ideo <-predictions(
  mod1_lm,
  by=c("group", "Ideo_num","solar_geo_heard_bin"),
  newdata = datagridcf(solar_geo_heard_bin=c(0,1), Ideo_num = c(1, 7)))
pred_con_ideo$Ideo_num[pred_con_ideo$Ideo_num == '1'] <- "Extremely liberal"
pred_con_ideo$Ideo_num[pred_con_ideo$Ideo_num == '7'] <- "Extremely conservative"
colnames(pred_con_ideo)[colnames(pred_con_ideo) == "Ideo_num"] <- "contrast"
pred_con_ideo$term <-  "Ideology"

pred_con_ideo$solar_geo_heard_bin[pred_con_ideo$solar_geo_heard_bin==0] <- "Heard nothing"
pred_con_ideo$solar_geo_heard_bin[pred_con_ideo$solar_geo_heard_bin==1] <-  "Heard little, some, or a lot"
pred_con_ideo$solar_geo_heard_bin <- factor(pred_con_ideo$solar_geo_heard_bin, 
                                            levels = c("Heard nothing","Heard little, some, or a lot"))

pred_con_ideo$contrast <- factor(pred_con_ideo$contrast, levels = unique(pred_con_ideo$contrast))
pred_con_ideo$contrast <- fct_rev(pred_con_ideo$contrast)

p1 <- ggplot(pred_con_ideo,
             aes(x = solar_geo_heard_bin, y = estimate, color=contrast)) +
  geom_bar(aes(x = solar_geo_heard_bin, y=estimate, fill=contrast,alpha=2), 
           stat="identity",lwd=1, position=position_dodge())+
  #geom_point(aes(x = contrast,
  #y = estimate, color=solar_geo_heard_bin),position = position_dodge(width = .5))  +
  geom_errorbar(aes(x = solar_geo_heard_bin,
                    ymin = conf.low,
                    ymax = conf.high), lwd=1,
                width=.2,
                position=position_dodge(.9),alpha=2)  +
  xlab("") + ylab("predicted probability") +
  #scale_color_carto_d(palette="Safe", name="") +
  #scale_fill_carto_d(palette="Safe", name="")+
  scale_color_manual(values = group.colors.politics, name="") +
  scale_fill_manual(values = group.colors.politics, name="") +
  scale_alpha(guide = 'none') +  
  theme_minimal()  + scale_y_continuous(labels = percent) + ggtitle("Being concerned about SG") +
  theme(plot.title = element_text(size = 12))

#difference
mod2_lm <- lm(solar_geo_difference_bin ~ (better_off + Age + Woman + College +
                                            Religion + Ideo_num + cli_know_score + SocialMedia)*solar_geo_heard_bin,
              weights = Weights, data = Survey_Nov2022_appended)

pred_con_ideo <-predictions(
  mod2_lm,
  by=c("group", "Ideo_num","solar_geo_heard_bin"),
  newdata = datagridcf(solar_geo_heard_bin=c(0,1), Ideo_num = c(1, 7)))
pred_con_ideo$Ideo_num[pred_con_ideo$Ideo_num == '1'] <- "Extremely liberal"
pred_con_ideo$Ideo_num[pred_con_ideo$Ideo_num == '7'] <- "Extremely conservative"
colnames(pred_con_ideo)[colnames(pred_con_ideo) == "Ideo_num"] <- "contrast"
pred_con_ideo$term <-  "Ideology"

pred_con_ideo$solar_geo_heard_bin[pred_con_ideo$solar_geo_heard_bin==0] <- "Heard nothing"
pred_con_ideo$solar_geo_heard_bin[pred_con_ideo$solar_geo_heard_bin==1] <-  "Heard little, some, or a lot"

pred_con_ideo$solar_geo_heard_bin <- factor(pred_con_ideo$solar_geo_heard_bin, 
                                            levels = c("Heard nothing","Heard little, some, or a lot"))

pred_con_ideo$contrast <- factor(pred_con_ideo$contrast, levels = unique(pred_con_ideo$contrast))
pred_con_ideo$contrast <- fct_rev(pred_con_ideo$contrast)

p2 <- ggplot(pred_con_ideo,
             aes(x = solar_geo_heard_bin, y = estimate, color=contrast)) +
  geom_bar(aes(x = solar_geo_heard_bin, y=estimate, fill=contrast,alpha=2), 
           stat="identity",lwd=1, position=position_dodge())+
  #geom_point(aes(x = contrast,
  #y = estimate, color=solar_geo_heard_bin),position = position_dodge(width = .5))  +
  geom_errorbar(aes(x = solar_geo_heard_bin,
                    ymin = conf.low,
                    ymax = conf.high), lwd=1,
                width=.2,
                position=position_dodge(.9),alpha=2)  +
  xlab("") + ylab("predicted probability") +
  #scale_color_carto_d(palette="Safe", name="") +
  #scale_fill_carto_d(palette="Safe", name="")+
  scale_color_manual(values = group.colors.politics, name="") +
  scale_fill_manual(values = group.colors.politics, name="") +
  scale_alpha(guide = 'none') +  
  theme_minimal()  + scale_y_continuous(labels = percent) + ggtitle("Thinking SG will make a difference")+
  theme(plot.title = element_text(size = 12))

#policies
mod3 <- multinom(solar_geo_reduce ~ (better_off + Age + Woman + College +
                                       Religion + Ideo_num + cli_know_score + SocialMedia)*solar_geo_heard_bin,
                 weights = Weights, data = Survey_Nov2022_appended)

pred_con_ideo <-predictions(
  mod3,
  by=c("group", "Ideo_num","solar_geo_heard_bin"),
  newdata = datagridcf(solar_geo_heard_bin=c(0,1), Ideo_num = c(1, 7)))
pred_con_ideo$Ideo_num[pred_con_ideo$Ideo_num == '1'] <- "Extremely liberal"
pred_con_ideo$Ideo_num[pred_con_ideo$Ideo_num == '7'] <- "Extremely conservative"
colnames(pred_con_ideo)[colnames(pred_con_ideo) == "Ideo_num"] <- "contrast"
pred_con_ideo$term <-  "Ideology"

pred_con_ideo$solar_geo_heard_bin[pred_con_ideo$solar_geo_heard_bin==0] <- "Heard nothing"
pred_con_ideo$solar_geo_heard_bin[pred_con_ideo$solar_geo_heard_bin==1] <-  "Heard little, some, or a lot"

pred_con_ideo$solar_geo_heard_bin <- factor(pred_con_ideo$solar_geo_heard_bin, 
                                            levels = c("Heard nothing","Heard little, some, or a lot"))

pred_con_ideo$contrast <- factor(pred_con_ideo$contrast, levels = unique(pred_con_ideo$contrast))
pred_con_ideo$contrast <- fct_rev(pred_con_ideo$contrast)

comp_con_new_a <-avg_comparisons(mod3, variables = list(
  Ideo_num = c(1, 7)), newdata = datagridcf(solar_geo_heard_bin = 1))
comp_con_new_b <-avg_comparisons(mod3, variables = list(
  Ideo_num = c(1, 7)), newdata = datagridcf(solar_geo_heard_bin = 0))

p3 <- ggplot(pred_con_ideo,
             aes(x = solar_geo_heard_bin, y = estimate, color=contrast)) +
  geom_bar(aes(x = solar_geo_heard_bin, y=estimate, fill=contrast,alpha=2), 
           stat="identity",lwd=1, position=position_dodge())+
  #geom_point(aes(x = contrast,
  #y = estimate, color=solar_geo_heard_bin),position = position_dodge(width = .5))  +
  geom_errorbar(aes(x = solar_geo_heard_bin,
                    ymin = conf.low,
                    ymax = conf.high), lwd=1,
                width=.2,
                position=position_dodge(.9),alpha=2)  +
  xlab("") + ylab("predicted probability") +
  #scale_color_carto_d(palette="Safe", name="") +
  #scale_fill_carto_d(palette="Safe", name="")+
  scale_color_manual(values = group.colors.politics, name="") +
  scale_fill_manual(values = group.colors.politics, name="") +
  scale_alpha(guide = 'none') +
  facet_wrap(~group) +   
  theme_minimal()  + scale_y_continuous(labels = percent)+                                                                # Change font size
  theme(strip.text.x = element_text(size = 11))+
  scale_x_discrete(name = '', 
                   breaks = c('Heard nothing', 'Heard little, some, or a lot'), 
                   labels = c('Heard nothing', 'Heard little,\nsome, or a lot'))


# Figure 4 combined
library(ggpubr)
ggpubr::ggarrange(p2, p1, p3, nrow = 3, ncol = 1, common.legend = TRUE)



## Supplementary Information

## tables

### table S1 weighted and unweighted descriptives

weighted <- survey %>% 
  tbl_svysummary(
    # Use a character variable here. A factor leads to an error
    #by = smoker_char,
    # Use include to select variables
    include = c(Age, Gender, Region, Education, Ideology),
    statistic = list(
      all_categorical() ~ "{n}    ({p}%)")) %>%
  #digits = list(LBDGLUSI ~ c(2, 2),
  # BMXWAIST ~ c(1, 1),
  #RIDAGEYR ~ c(1, 1),
  #all_categorical() ~ c(0, 1))
  #) %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("Weighted descriptive statistics") %>%
  bold_labels()

unweighted <- Survey_Nov2022_appended %>% 
  tbl_summary(
    # Use a character variable here. A factor leads to an error
    #by = smoker_char,
    # Use include to select variables
    include = c(Age, Gender, Region, Education, Ideology),
    statistic = list(
      all_categorical() ~ "{n}    ({p}%)")) %>%
  #digits = list(LBDGLUSI ~ c(2, 2),
  # BMXWAIST ~ c(1, 1),
  #RIDAGEYR ~ c(1, 1),
  #all_categorical() ~ c(0, 1))
  #) %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("Unweighted descriptive statistics") %>%
  bold_labels()

##manually create table bc code below doesn't work

# merged <- tbl_merge(list(unweighted,weighted), tab_spanner = c("**Unweighted descriptive statistics**", "**Weighted descriptive statistics**")
# )%>%
#   modify_caption("")
# 
# merged %>%
#   as_gt() %>%
#   gt::as_latex() %>%
#   as.character() %>%
#   cat()

### table S2

##regression table

stargazer(mod1_lm, mod2_lm, mod3,
          dep.var.labels  = c("Concerns","Makes a Difference", "Invest in SG (ref. Reduce emissions)", "Do both"),
          type = "latex", covariate.labels=c(
            "The same financially (ref. Better off)", "Worse off financially", 
            "Age 45 to 64 (ref. over 65)","Age 30 to 44","Age 18 to 29", "Woman",
            "College Education or More",
            
            "Protestant (ref. No religion)", "Catholic", "Jewish", "Other religion",
            "Political ideology","Environ. Knowledge Score", "Rarely use social media (ref. Never)",
            "Sometimes use social media","Often use social media","Heard of SG (ref. has not)",
            "The same financially X Heard of SG", "Worse off financially X Heard of SG", 
            "Age 45 to 64 X Heard of SG","Age 30 to 44 X Heard of SG","Age 18 to 29 X Heard of SG", "Woman X Heard of SG",
            "College Education or More X Heard of SG",
            "Protestant X Heard of SG", "Catholic X Heard of SG", "Jewish X Heard of SG", "Other religion X Heard of SG",
            "Political ideology X Heard of SG","Environ. Knowledge Score X Heard of SG",
            "Rarely use social media X Heard of SG",
            "Sometimes use social media X Heard of SG","Often use social media X Heard of SG",
            "Constant"), star.cutoffs = c(0.05, 0.01, 0.001),
          star.char=c("*", "**", "***"),
          omit.stat = c("f", "rsq", "ser"),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE)

### figures

## plot histogram of weights figure S1

ggplot(data=Survey_Nov2022_appended, aes(x=Weights)) +
  geom_histogram(fill="steelblue")+
  labs( x="Weights")  +
  theme_minimal()



###########################################
## ordered probit figure S2
##########################################
## colors
group.colors <- c(solar_geo_heard_bin = "#E58606", cli_change_important = "#5D69B1", 
                  SocialMedia ="#52BCA3", pol_know_score ="#99C945", altruism_score ="#CC61B0", 
                  cli_know_score ="#24796C", Woman ="#DAA51B", College ="#2F8AC4", Age ="#764E9F",
                  Ideo_num ="#ED645A", better_off ="#CC3A8E", Religion ="#A5AA99", Trump ="#ED645A",
                  PID3 ="#ED645A")

mod_r <- polr(solar_geo_heard ~ better_off + Age + Woman + College + SocialMedia +
                Religion + Ideo_num + cli_know_score, weights = Weights, data = Survey_Nov2022_appended)

## plot comparisons of interest

comp_con <- avg_comparisons(mod_r, variables = list(
  better_off = "reference", 
  cli_know_score = c(-1.41, 0.34),
  Woman="reference",College="reference",
  Age = "reference",
  Religion = "reference", SocialMedia = "reference"
))

#conservative vs lib
comp_con_new <-avg_comparisons(mod_r, variables = list(
  Ideo_num = c(1, 7)))

colnames(comp_con_new)[colnames(comp_con_new) == "contrast_Ideo_num"] <- "contrast"

comp_con <- rbind.fill(comp_con, comp_con_new)

# plot 

comp_con$contrast[comp_con$contrast == 'The same - Better off'] <- "The Same Financially vs Better Off"
comp_con$contrast[comp_con$contrast == 'Worse off - Better off'] <- "Worse Off Financially vs Better Off"
#comp_con$contrast[comp_con$contrast == '0.76 - -1.19' & comp_con$term=="altruism_score"] <- "Altruism score high vs low"
comp_con$contrast[comp_con$contrast == '0.34 - -1.41' & comp_con$term=="cli_know_score"] <- "Environmental knowledge high vs low"
comp_con$contrast[comp_con$contrast == '7 - 1'] <- "Extremely Conservative vs Extremely Liberal"

comp_con$contrast[comp_con$contrast == 'Rarely use social media - Never use social media'] <- "Rarely use vs Never use social media"
comp_con$contrast[comp_con$contrast == 'Sometimes use social media - Never use social media'] <- "Sometimes use vs Never use social media"
comp_con$contrast[comp_con$contrast == 'Often use social media - Never use social media'] <- "Often use vs Never use social media"


#comp_con$contrast[comp_con$contrast == 'Married - Not Married' & comp_con$term=="married"] <- "Married vs not"
comp_con$contrast[comp_con$contrast == 'Woman - Not Woman'] <- "Woman vs Not Woman"
comp_con$contrast[comp_con$contrast == 'College - No College'] <- "College vs No College"
comp_con$contrast[comp_con$contrast == '45 to 64 - 65 or older'] <- "45 to 64 vs 65 or older"
comp_con$contrast[comp_con$contrast == '30 to 44 - 65 or older'] <- "30 to 44 vs 65 or older"
comp_con$contrast[comp_con$contrast == '18 to 29 - 65 or older'] <- "18 to 29 vs 65 or older"
comp_con$contrast[comp_con$contrast == 'Protestant - No religion'] <- "Protestant vs No religion"
comp_con$contrast[comp_con$contrast == 'Catholic - No religion'] <- "Catholic vs No religion"
comp_con$contrast[comp_con$contrast == 'Jewish - No religion'] <- "Jewish vs No religion"
comp_con$contrast[comp_con$contrast == 'Other - No religion'] <- "Other vs No religion"



# plot

comp_con$contrast <- factor(comp_con$contrast, levels = unique(comp_con$contrast))
comp_con$contrast <- fct_rev(comp_con$contrast)

comp_con$group = factor(comp_con$group , levels=c('Nothing','A little',
                                                  'Some','A lot'))

comp_con$group = factor(comp_con$group ,levels=c('Nothing','A little',
                                                 'Some','A lot'), labels=c('Heard Nothing about SG','A little',
                                                                           'Some','A lot'))

FD_heard_r <- ggplot(comp_con, 
                     aes(x = contrast, y = estimate, color=term)) +
  geom_hline(yintercept = 0, 
             colour = gray(1/2), lty = 2) +
  geom_point(aes(x = contrast, 
                 y = estimate, color=term)) + 
  geom_linerange(aes(x = contrast, 
                     ymin = conf.low,
                     ymax = conf.high, color=term),
                 lwd = 1) + 
  ylab("comparison") + xlab("Variable") +
  coord_flip()+ facet_wrap(~group) + theme_minimal() +
  scale_color_manual(values = group.colors) +
  theme(legend.position = "none")

# ordered probit figure S3

mod2 <- polr(solar_geo_difference_cat ~ (better_off + Age + Woman + College +
                                           Religion + Ideo_num + cli_know_score + SocialMedia)*solar_geo_heard_bin,
             weights = Weights, data = Survey_Nov2022_appended)

pred_con_ideo <-predictions(
  mod2,
  by=c("group", "Ideo_num","solar_geo_heard_bin"),
  newdata = datagridcf(solar_geo_heard_bin=c(0,1), Ideo_num = c(1, 7)))
pred_con_ideo$Ideo_num[pred_con_ideo$Ideo_num == '1'] <- "Extremely liberal"
pred_con_ideo$Ideo_num[pred_con_ideo$Ideo_num == '7'] <- "Extremely conservative"
colnames(pred_con_ideo)[colnames(pred_con_ideo) == "Ideo_num"] <- "contrast"
pred_con_ideo$term <-  "Ideology"

combined_df_con <- pred_con_ideo

combined_df_con$solar_geo_heard_bin[combined_df_con$solar_geo_heard_bin==0] <- "Heard nothing"
combined_df_con$solar_geo_heard_bin[combined_df_con$solar_geo_heard_bin==1] <-  "Heard little, some, or a lot"

combined_df_con$solar_geo_heard_bin <- factor(combined_df_con$solar_geo_heard_bin, 
                                              levels = c("Heard nothing","Heard little, some, or a lot"))

combined_df_con$contrast <- factor(combined_df_con$contrast, levels = unique(combined_df_con$contrast))
combined_df_con$contrast <- fct_rev(combined_df_con$contrast)

combined_df_con$group = factor(combined_df_con$group , levels=c('A big difference','Some difference',
                                                                'Little difference','No difference'))

#PV diff heard
ggplot(combined_df_con,
       aes(x = solar_geo_heard_bin, y = estimate, color=contrast)) +
  geom_point(aes(x = solar_geo_heard_bin, y = estimate, color=contrast),
             position = position_dodge(width = .5))  +
  geom_linerange(aes(x = solar_geo_heard_bin,
                     ymin = conf.low,
                     ymax = conf.high, color=contrast),
                 lwd = 1,position = position_dodge(width = .5))  +
  xlab("Variable") + ylab("predicted probability") +
  coord_flip() + facet_wrap(~group) +   theme_minimal()  +
  scale_color_manual(values = group.colors.politics, name="")


# ordered probit figure S4

mod1 <- polr(solar_geo_concerns_cat ~ (better_off + Age + Woman + College +
                                         Religion + Ideo_num + cli_know_score + SocialMedia)*solar_geo_heard_bin,
             weights = Weights, data = Survey_Nov2022_appended)

pred_con_ideo <-predictions(
  mod1,
  by=c("group", "Ideo_num","solar_geo_heard_bin"),
  newdata = datagridcf(solar_geo_heard_bin=c(0,1), Ideo_num = c(1, 7)))
pred_con_ideo$Ideo_num[pred_con_ideo$Ideo_num == '1'] <- "Extremely liberal"
pred_con_ideo$Ideo_num[pred_con_ideo$Ideo_num == '7'] <- "Extremely conservative"
colnames(pred_con_ideo)[colnames(pred_con_ideo) == "Ideo_num"] <- "contrast"
pred_con_ideo$term <-  "Ideology"

combined_df_con <- pred_con_ideo

combined_df_con$solar_geo_heard_bin[combined_df_con$solar_geo_heard_bin==0] <- "Heard nothing"
combined_df_con$solar_geo_heard_bin[combined_df_con$solar_geo_heard_bin==1] <-  "Heard little, some, or a lot"

combined_df_con$solar_geo_heard_bin <- factor(combined_df_con$solar_geo_heard_bin, 
                                              levels = c("Heard nothing","Heard little, some, or a lot"))

combined_df_con$contrast <- factor(combined_df_con$contrast, levels = unique(combined_df_con$contrast))
combined_df_con$contrast <- fct_rev(combined_df_con$contrast)


#PV concern heard
ggplot(combined_df_con,
       aes(x = solar_geo_heard_bin, y = estimate, color=contrast)) +
  geom_point(aes(x = solar_geo_heard_bin, y = estimate, color=contrast),
             position = position_dodge(width = .5))  +
  geom_linerange(aes(x = solar_geo_heard_bin,
                     ymin = conf.low,
                     ymax = conf.high, color=contrast),
                 lwd = 1,position = position_dodge(width = .5))  +
  xlab("Variable") + ylab("predicted probability") +
  coord_flip() + facet_wrap(~group) +   theme_minimal()  +
  scale_color_manual(values = group.colors.politics, name="")


#####Models by party Figure S5

group.colors <- c(Republican = "#E41A1C", Independent = "#4DAF4A", Democrat ="#377EB8")


mod1_lm <- lm(solar_geo_concerns_bin ~ (better_off + Age + Woman + College +
                                          Religion + PID2 + cli_know_score + SocialMedia)*solar_geo_heard_bin,
              weights = Weights, data = Survey_Nov2022_appended)

pred_con_ideo <-predictions(
  mod1_lm,
  by=c("group", "PID2","solar_geo_heard_bin"),
  newdata = datagridcf(solar_geo_heard_bin=c(0,1), PID2 = c("Democrat", "Republican")))

colnames(pred_con_ideo)[colnames(pred_con_ideo) == "PID2"] <- "contrast"
pred_con_ideo$term <-  "PartyID"

pred_con_ideo$solar_geo_heard_bin[pred_con_ideo$solar_geo_heard_bin==0] <- "Heard nothing"
pred_con_ideo$solar_geo_heard_bin[pred_con_ideo$solar_geo_heard_bin==1] <-  "Heard little, some, or a lot"
pred_con_ideo$solar_geo_heard_bin <- factor(pred_con_ideo$solar_geo_heard_bin, 
                                            levels = c("Heard nothing","Heard little, some, or a lot"))

pred_con_ideo$contrast <- factor(pred_con_ideo$contrast, levels = unique(pred_con_ideo$contrast))
pred_con_ideo$contrast <- fct_rev(pred_con_ideo$contrast)

p1 <- ggplot(pred_con_ideo,
             aes(x = solar_geo_heard_bin, y = estimate, color=contrast)) +
  geom_bar(aes(x = solar_geo_heard_bin, y=estimate, fill=contrast,alpha=2), 
           stat="identity",lwd=1, position=position_dodge())+
  #geom_point(aes(x = contrast,
  #y = estimate, color=solar_geo_heard_bin),position = position_dodge(width = .5))  +
  geom_errorbar(aes(x = solar_geo_heard_bin,
                    ymin = conf.low,
                    ymax = conf.high), lwd=1,
                width=.2,
                position=position_dodge(.9),alpha=2)  +
  xlab("") + ylab("predicted probability") +
  #scale_color_carto_d(palette="Safe", name="") +
  #scale_fill_carto_d(palette="Safe", name="")+
  scale_color_manual(values = group.colors, name="") +
  scale_fill_manual(values = group.colors, name="") +
  scale_alpha(guide = 'none') +  
  theme_minimal()  + scale_y_continuous(labels = scales::percent) + ggtitle("Being concerned about SG") +
  theme(plot.title = element_text(size = 12))


mod2_lm <- lm(solar_geo_difference_bin ~ (better_off + Age + Woman + College +
                                            Religion + PID2 + cli_know_score + SocialMedia)*solar_geo_heard_bin,
              weights = Weights, data = Survey_Nov2022_appended)

pred_con_ideo <-predictions(
  mod2_lm,
  by=c("group", "PID2","solar_geo_heard_bin"),
  newdata = datagridcf(solar_geo_heard_bin=c(0,1), PID2 = c("Democrat", "Republican")))

colnames(pred_con_ideo)[colnames(pred_con_ideo) == "PID2"] <- "contrast"
pred_con_ideo$term <-  "Ideology"

pred_con_ideo$solar_geo_heard_bin[pred_con_ideo$solar_geo_heard_bin==0] <- "Heard nothing"
pred_con_ideo$solar_geo_heard_bin[pred_con_ideo$solar_geo_heard_bin==1] <-  "Heard little, some, or a lot"

pred_con_ideo$solar_geo_heard_bin <- factor(pred_con_ideo$solar_geo_heard_bin, 
                                            levels = c("Heard nothing","Heard little, some, or a lot"))

pred_con_ideo$contrast <- factor(pred_con_ideo$contrast, levels = unique(pred_con_ideo$contrast))
pred_con_ideo$contrast <- fct_rev(pred_con_ideo$contrast)

p2 <- ggplot(pred_con_ideo,
             aes(x = solar_geo_heard_bin, y = estimate, color=contrast)) +
  geom_bar(aes(x = solar_geo_heard_bin, y=estimate, fill=contrast,alpha=2), 
           stat="identity",lwd=1, position=position_dodge())+
  #geom_point(aes(x = contrast,
  #y = estimate, color=solar_geo_heard_bin),position = position_dodge(width = .5))  +
  geom_errorbar(aes(x = solar_geo_heard_bin,
                    ymin = conf.low,
                    ymax = conf.high), lwd=1,
                width=.2,
                position=position_dodge(.9),alpha=2)  +
  xlab("") + ylab("predicted probability") +
  #scale_color_carto_d(palette="Safe", name="") +
  #scale_fill_carto_d(palette="Safe", name="")+
  scale_color_manual(values = group.colors, name="") +
  scale_fill_manual(values = group.colors, name="") +
  scale_alpha(guide = 'none') +  
  theme_minimal()  + scale_y_continuous(labels = scales::percent) + ggtitle("Thinking SG will make a difference")+
  theme(plot.title = element_text(size = 12))


mod3 <- multinom(solar_geo_reduce ~ (better_off + Age + Woman + College +
                                       Religion + PID2 + cli_know_score + SocialMedia)*solar_geo_heard_bin,
                 weights = Weights, data = Survey_Nov2022_appended)

pred_con_ideo <-predictions(
  mod3,
  by=c("group", "PID2","solar_geo_heard_bin"),
  newdata = datagridcf(solar_geo_heard_bin=c(0,1), PID2 = c("Democrat", "Republican")))

colnames(pred_con_ideo)[colnames(pred_con_ideo) == "PID2"] <- "contrast"
pred_con_ideo$term <-  "Ideology"

pred_con_ideo$solar_geo_heard_bin[pred_con_ideo$solar_geo_heard_bin==0] <- "Heard nothing"
pred_con_ideo$solar_geo_heard_bin[pred_con_ideo$solar_geo_heard_bin==1] <-  "Heard little, some, or a lot"

pred_con_ideo$solar_geo_heard_bin <- factor(pred_con_ideo$solar_geo_heard_bin, 
                                            levels = c("Heard nothing","Heard little, some, or a lot"))

pred_con_ideo$contrast <- factor(pred_con_ideo$contrast, levels = unique(pred_con_ideo$contrast))
pred_con_ideo$contrast <- fct_rev(pred_con_ideo$contrast)

p3 <- ggplot(pred_con_ideo,
             aes(x = solar_geo_heard_bin, y = estimate, color=contrast)) +
  geom_bar(aes(x = solar_geo_heard_bin, y=estimate, fill=contrast,alpha=2), 
           stat="identity",lwd=1, position=position_dodge())+
  #geom_point(aes(x = contrast,
  #y = estimate, color=solar_geo_heard_bin),position = position_dodge(width = .5))  +
  geom_errorbar(aes(x = solar_geo_heard_bin,
                    ymin = conf.low,
                    ymax = conf.high), lwd=1,
                width=.2,
                position=position_dodge(.9),alpha=2)  +
  xlab("") + ylab("predicted probability") +
  #scale_color_carto_d(palette="Safe", name="") +
  #scale_fill_carto_d(palette="Safe", name="")+
  scale_color_manual(values = group.colors, name="") +
  scale_fill_manual(values = group.colors, name="") +
  scale_alpha(guide = 'none') +
  facet_wrap(~group) +   
  theme_minimal()  + scale_y_continuous(labels = scales::percent)+                                                                # Change font size
  theme(strip.text.x = element_text(size = 11))+
  scale_x_discrete(name = '', 
                   breaks = c('Heard nothing', 'Heard little, some, or a lot'), 
                   labels = c('Heard nothing', 'Heard little,\nsome, or a lot'))

ggpubr::ggarrange(p2,p1,p3, nrow=3, ncol=1, common.legend = T)
