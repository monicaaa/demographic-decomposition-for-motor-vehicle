# age standardized death rate

age_stand_all <- exp.all %>%
  left_join(pop2000stand) %>%
  group_by(gender, race) %>%
  mutate(death = death_10000_pop*proportion,
         risk = risk_mile*proportion,
         exposure = exp_mile*proportion) %>%
  summarise(death_stand = sum(death),
            risk_stand = sum(risk),
            exp_stand = sum(exposure)) 

age_stand_pveh <- exp.pveh %>%
  left_join(pop2000stand) %>%
  group_by(gender, race) %>%
  mutate(death = death_10000_pop*proportion,
         risk = risk_mile*proportion,
         exposure = exp_mile*proportion) %>%
  summarise(death_stand = sum(death),
            risk_stand = sum(risk),
            exp_stand = sum(exposure)) 

age_stand_ped <- exp.ped %>%
  left_join(pop2000stand) %>%
  group_by(gender, race) %>%
  mutate(death = death_10000_pop*proportion,
         risk = risk_mile*proportion,
         exposure = exp_mile*proportion) %>%
  summarise(death_stand = sum(death),
            risk_stand = sum(risk),
            exp_stand = sum(exposure)) 

# Risk and Exposure effects
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