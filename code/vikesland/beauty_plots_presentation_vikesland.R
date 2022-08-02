## plots for presenting

#  PAR changes over 24 h. ((NEE)) -----------------------------------------

filt_NEE_60 <- filter(co2_cut_60_keep, type == "NEE") # I am just filtering to make things easier

# quick plot

#plot(filt_NEE_60$PAR) # Plot the PAR values
plot(x= filt_NEE_60$datetime, y= filt_NEE_60$PAR,
     xlab = "Time of the day (hours)", 
     ylab = "Photosynthetically active radiation (PAR)",
     col = alpha("blue", 0.1), pch=16, main= "Vikesland (469 m a.s.l.)") # Plot the PAR vs time

abline(h=0, col="red")

## GPP over 24 h  -----------------------------------------------------------

# quickplot
# flux_virk_60 %>% 
#   ggplot(aes(x = datetime_NEE, y = GPP)) +
#   geom_point(aes(color=turfID)) +
#     geom_line(aes(color = turfID)) +
#     theme(axis.text=element_text(size=12),
#           axis.title=element_text(size=14,face="bold"))

EcoResp_vikesland <-

flux_vikesland_60 %>% 
  filter(
    ER>=0
  ) %>% 
  
  ggplot( aes(
      x = datetime_NEE, y = ER, group= turfID, color = warming)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 8),
              se = FALSE, size = 0.5, fullrange = FALSE) +

  geom_hline(
    yintercept = 0, linetype = "dashed", colour = "grey") +
  
  geom_vline(
    xintercept = as.numeric(flux_vikesland_60$datetime_NEE[102]),
             linetype = 4, colour = "orange") +
  geom_vline(
    xintercept = as.numeric(flux_vikesland_60$datetime_NEE[16]),
    linetype = 4, colour = "blue") +
  
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%b %d - %H:%M")) +
 
  JuliaTheme +
  theme(axis.ticks = element_line(size=1.5), 
        axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8)) +
   scale_color_manual(values = c(
     "ambient" = "#1e90ff",
     "transplant" = "#ff0800"
   )) 

  
  
  # 
  # facet_grid(type ~ ., scales = "free") +
  # geom_smooth(method = "lm",
  #             formula = y ~ poly(x, 2),
  #             se = TRUE, size = 0.5, fullrange = FALSE) +
  # scale_color_manual(values = c(
  #   "Control" = "#1e90ff",
  #   "Warming" = "#ff0800"
  # )) 



  # ggplot(
  #   aes(x = datetime_NEE, y = GPP)
  #   ) +
  # geom_line(
  #   aes(color = turfID)
  #   ) +
  # JuliaTheme


























# co2_cut_60_keep %>% 
#   filter(
#     type == "NEE"
#   ) %>% 
#   ggplot(aes(datetime, PAR)) +
#   geom_point(colour = "red", size = 3, alpha = 1/30) +
#   labs(
#     title ="Vikesland (469 m a.s.l.)", 
#     subtitle = "PAR values in 24 h"
#   ) +
#   
#   xlab(
#     "Date and time") + 
#   ylab(
#     "Photosynthetically active radiation (PAR)"
#   ) +
#   JuliaTheme
# 
