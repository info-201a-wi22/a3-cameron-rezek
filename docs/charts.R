source('C:/Users/rezek/Documents/Info_201/a3-cameron-rezek/docs/suminfo.R')
library(wesanderson)
library(reshape2)

adm_final_melt <- melt(adm_final, id.vars='year')

over_time_plot <- ggplot(adm_final_melt, aes(x=year, y=value, color=variable))+ 
  geom_line(size=1.3)+
  scale_color_manual(values= wes_palette("Rushmore1", n = 4), name="Race",
                     labels=c("total_prison_adm"="Total",
                              "black"="Black",
                              "white"="White",
                              "latinx"="Latinx"))+
  theme_classic()+
  labs(
    title = "Average Prison Admission in All Counties",
    subtitle = "(1985-2015)",
    y = "Average"
  )+
  theme(panel.background = element_rect(fill = 'Azure'))+
  theme(panel.grid.major = element_line(color = "grey",
                                        size = 0.5,
                                        linetype = 2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))


variable_comparison_chart <- ggplot(vc_final_melt, aes(x = variable, y = value, fill = race))+
  geom_bar(position = "fill", stat = "identity", color='black', width=0.9)+
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Percentage of Prisoners by Ethnicity in Each Region",
    subtitle = "(Data from 2015)"
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))

variable_comparison_chart



