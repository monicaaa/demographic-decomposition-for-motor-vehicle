########################################################################
#### Perform preliminary analysis---------------------------------------
########################################################################

# calculate b-w % difference in death, exposure, and risk rates
bw_percent_diff_all <- calculate_percent_diff(age_stand_all)
bw_percent_diff_pveh <- calculate_percent_diff(age_stand_pveh)
bw_percent_diff_ped <- calculate_percent_diff(age_stand_ped)

# plots b-w absolute values
plot_exposure(exp.all)
plot_exposure(exp.pveh)
plot_exposure(exp.ped)

# plots risk and exposure effects
risk_exp_plot <- ggplot(data = decomp_combined %>% 
                          dplyr::select(-total_effect, -c(risk_percent_mile:exp_percent_mile)) %>%
                          melt(id.vars = c("gender", "age.cat", "type")), 
                        aes(x = age.cat, y = value, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(type ~ gender, scales = "free") +
  theme(legend.position="bottom") + geom_hline(yintercept=0) + 
  theme_bw() +
  scale_fill_discrete(name=" ",
                      breaks=c("risk_effect_mile", "exp_effect_mile"),
                      labels=c("Risk Effect", "Exposure Effect"))+
  scale_x_discrete("Age Categories", 
                   labels = c("5-14", "15-24", "25-44", "45-64", "65-84")) +
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0)) +
  theme(axis.text=element_text(size=10))+
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  theme(axis.title.y = element_blank()) +   # Remove x-axis label
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0)) +
  theme(legend.text = element_text(size = 10)) +
  theme(strip.text = element_text(size=10)) +
  theme(legend.position="bottom")

risk_exp_plot
