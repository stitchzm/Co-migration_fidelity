# PLOT ##
library(ggplot2)
library(ggThemeAssist)
library(ggthemes)
library("RColorBrewer")
library(ggiraph)
library(ggiraphExtra)
library(cowplot)
library(sjPlot)
library(ggpubr)
library(ggeffects)
library(gghighlight)
library(ggrepel)
library('gratia')
library(grid)
library(draw)
library(gamm4)
library(magrittr)
library(mgcv)
library(gratia)
library(dplyr)
# 1000 ####
#*******************************************************************************************************#
# Extract estimates from GAM 
se_1000 <- smooth_estimates(gam_model1000,level = 0.99) %>% add_confint()
DataGAM1000 <- DataGAM %>% add_partial_residuals(gam_model1000)
colnames(DataGAM1000)[16] <- "res_temp"  
colnames(DataGAM1000)[17] <- "res_tail" 
colnames(DataGAM1000)[18] <- "s_seasonal_nao" 
# tail 
tail_1000 <- se_1000 %>%  
  filter(.smooth == "s(anom_tailwind1000)") %>% 
  ggplot() +
  geom_rug(aes(x = anom_tailwind1000),data = DataGAM1000,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = anom_tailwind1000),
              alpha = 0.3, fill = "#99D3E2") +
  geom_point(aes(x = anom_tailwind1000, y = res_tail),
             data = DataGAM1000, shape = 21, size = 4, alpha = 0.7, colour = "#5F5D5E", bg="#27C2D4") +
  geom_line(aes(x = anom_tailwind1000, y = .estimate), 
            lwd = 1, linetype = "dashed", color = "#27C2D4") +
  geom_text_repel(aes(x = anom_tailwind1000, y = res_tail, label = YEAR),
    data = DataGAM1000,size = 5,max.overlaps = Inf,      # optional: show all labels
    #box.padding = 0.5,       # space around labels
    #point.padding = 0.3    # space around points
  ) +
  scale_size_continuous(name = "Q",range = c(1,9),
                        breaks = pretty(range(DataGAM1000$Q), n = 4)) +
  labs(subtitle = "Effects of tailwinds on co-migration fidelity",
       x = "Tailwind anomalies at 1000 mb", y = "Modularity Q") + 
  theme_minimal() + 
  theme(panel.background = element_rect(fill = NA),
      axis.title = element_text(size = 22),
      axis.text.x = element_text(size = 17),
      axis.text.y = element_text(size = 17),
      plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 22),
      plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
      legend.text = element_text(size = 17),
      legend.title = element_text(size = 22)) 
ggsave("tail_1000.png", tail_1000, width = 3000, height = 2000, units = "px", 
       pointsize = 22,bg = "white",dpi = 300)
# plot anomalies of tailwinds at 1000 mb 
tail1 <- ggplot(swtmNAO, aes(x = YEAR, y = anom_tailwind1000)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Tailwind anomalies across years",
       y = "Tailwind anomalies at 1000 mb", x = "Years") + 
  scale_x_continuous(breaks = 2007:2024) + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = NA),
        plot.subtitle = element_text(face = "italic", hjust = 0.5,size = 22),
        axis.text.x = element_text(angle = -90, size = 17, vjust = 0.6, hjust = 0.5),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 22,margin = margin(t = 15)),
        axis.title.y = element_text(size = 22,margin = margin(l = 10)),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 15))
# combine the two plots 
tail1000_plots <- cowplot::plot_grid(tail_1000,tail1,
                                    labels = c("a)", "b)"),ncol = 2,
                                    rel_widths = c(0.8, 0.8),label_size = 16,label_x = c(0.030, 0.05))
ggsave("tail1000_plots.png", tail1000_plots, width = 4000, height = 2600, units = "px", 
       pointsize = 22,bg = "white",dpi = 300)
#******************************************************************************************************
# temperature 
temperature_1000 <- se_1000 %>%  
  filter(.smooth == "s(anom_temperature1000)") %>% 
  ggplot() +
  geom_rug(aes(x = anom_temperature1000),data = DataGAM1000,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = anom_temperature1000),
              alpha = 0.3, fill = "#F5BE88") +
  geom_point(aes(x = anom_temperature1000, y = res_temp),
             data = DataGAM1000, shape = 21, size = 4,alpha = 0.7, colour = "#5F5D5E", bg="#FB5573") +
  geom_line(aes(x = anom_temperature1000, y = .estimate), 
            lwd = 1, linetype = "dashed", color = "#FB5573") +
  #geom_text(aes(x = anom_temperature1000, y = res_temp, label = YEAR),
            #data = DataGAM1000, vjust = -1, size = 5) + 
  geom_text_repel(aes(x = anom_temperature1000, y = res_temp, label = YEAR),
                  data = DataGAM1000,size = 5,max.overlaps = Inf) +
  scale_size_continuous(name = "Q",range = c(1,9),
                        breaks = pretty(range(DataGAM1000$Q), n = 4)) +
  labs(subtitle = "Effects of temperatures on co-migration fidelity",
       x = "Temperature anomalies at 1000 mb", y = "Modularity Q") + 
  theme_minimal() +  
  theme(panel.background = element_rect(fill = NA),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 22),
        plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 22)) 
ggsave("temperature_1000.png", temperature_1000, width = 3000, height = 2000, units = "px", 
       pointsize = 22,bg = "white",dpi = 300)
# plot anomalies of temperature at 1000 
temp1 <- ggplot(swtmNAO, aes(x = YEAR, y = anom_temperature1000)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Temperature anomalies across years",
       y = "Temperature anomalies at 1000 mb", x = "Years") + 
  scale_x_continuous(breaks = 2007:2024) + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = NA),
        plot.subtitle = element_text(face = "italic", hjust = 0.5,size = 22),
        axis.text.x = element_text(angle = -90, size = 17, vjust = 0.6, hjust = 0.4),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 22,margin = margin(t = 15)),
        axis.title.y = element_text(size = 22,margin = margin(r = 10)),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 15))
# combine the two plots
temp1000_plots <- cowplot::plot_grid(temperature_1000 ,temp1,
                                    labels = c("a)", "b)"),ncol = 2,
                                    rel_widths = c(0.85, 0.8),label_size = 16,label_x = c(0.030, 0.05))
ggsave("temp1000_plots.png", temp1000_plots, width = 4000, height = 2600, units = "px", 
       pointsize = 22,bg = "white",dpi = 300)

# 925 ####
se_925 <- smooth_estimates(gam_model925,level = 0.99) %>% add_confint()
DataGAM925 <- DataGAM %>% add_partial_residuals(gam_model925)
colnames(DataGAM925)[16] <- "res_temp"  
colnames(DataGAM925)[17] <- "res_tail" 
# tail 
tail_925 <- se_925 %>%  
  filter(.smooth == "s(anom_tailwind925)") %>% 
  ggplot() +
  geom_rug(aes(x = anom_tailwind925),data = DataGAM925,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = anom_tailwind925),
              alpha = 0.3, fill = "#99D3E2") +
  geom_point(aes(x = anom_tailwind925, y = res_tail),
             data = DataGAM925,shape = 21, size = 4,alpha = 0.7, colour = "#5F5D5E", bg="#27C2D4") +
  geom_line(aes(x = anom_tailwind925, y = .estimate), 
            lwd = 1, linetype = "dashed", color = "#27C2D4") +
  geom_text_repel(aes(x = anom_tailwind925, y = res_tail, label = YEAR),
                  data = DataGAM925,size = 5,max.overlaps = Inf) +
  scale_size_continuous(name = "Q",range = c(1,9),
                        breaks = pretty(range(DataGAM925$Q), n = 4)) +
  labs(subtitle = "Effects of tailwinds on co-migration fidelity",
       x = "Tailwind anomalies at 925 mb", y = "Modularity Q") + 
  theme_minimal() + 
  theme(panel.background = element_rect(fill = NA),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22,margin = margin(t = 15)),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 22),
        plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 22))
ggsave("tail_925.png", tail_925, width = 3000, height = 2000, units = "px", 
       pointsize = 22,bg = "white",dpi = 300)
# plot anomalies of tailwinds at 925 mb 
tail9 <- ggplot(swtmNAO, aes(x = YEAR, y = anom_tailwind925)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Tailwind anomalies across years",
       y = "Tailwind anomalies at 925 mb", x = "Years") + 
  scale_x_continuous(breaks = 2007:2023) + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = NA),
        plot.subtitle = element_text(face = "italic", hjust = 0.5,size = 22),
        axis.text.x = element_text(angle = -90, size = 16, vjust = 0.6, hjust = 0.5),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 22,margin = margin(t = 15)),
        axis.title.y = element_text(size = 22,margin = margin(l = 10)),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 15))
# combine the two plots 
tail925_plots <- cowplot::plot_grid(tail_925,tail9,
                                  labels = c("a)", "b)"),ncol = 2,
                                  rel_widths = c(0.8, 0.8),label_size = 16,label_x = c(0.030, 0.05))
ggsave("tail925_plots.png", tail925_plots, width = 4000, height = 2600, units = "px", 
       pointsize = 22,bg = "white",dpi = 300)
#********************************************************************************************************#
# temperature 
temperature_925 <- se_925 %>%  
  filter(.smooth == "s(anom_temperature925)") %>% 
  ggplot() +
  geom_rug(aes(x = anom_temperature925),data = DataGAM925,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = anom_temperature925),
              alpha = 0.3, fill = "#F5BE88") +
  geom_point(aes(x = anom_temperature925, y = res_temp),
             data = DataGAM925, shape = 21, size = 4,alpha = 0.7, colour = "#5F5D5E", bg="#FB5573") +
  geom_line(aes(x = anom_temperature925, y = .estimate), 
            lwd = 1, linetype = "dashed", color = "#FB5573") +
  geom_text_repel(aes(x = anom_temperature925, y = res_temp, label = YEAR),
                  data = DataGAM925,size = 5,max.overlaps = Inf) +
  scale_size_continuous(name = "Q",range = c(1,9),
                        breaks = pretty(range(DataGAM925$Q), n = 4)) + 
  labs(subtitle = "Effects of temperatures on co-migration fidelity",
       x = "Temperature anomalies at 925 mb", y = "Modularity Q") + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = NA),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 22),
        plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 22))  
ggsave("temperature_925.png", temperature_925, width = 3000, height = 2000, units = "px", 
       pointsize = 22,bg = "white",dpi = 300)
# plot anomalies of temperature at 925 
temp9 <- ggplot(swtmNAO, aes(x = YEAR, y = anom_temperature925)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Temperature anomalies across years",
       y = "Temperature anomalies at 925 mb", x = "Years") + 
  scale_x_continuous(breaks = 2007:2023) + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = NA),
        plot.subtitle = element_text(face = "italic", hjust = 0.5,size = 22),
        axis.text.x = element_text(angle = -90, size = 16, vjust = 0.6, hjust = 0.4),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 22,margin = margin(t = 15)),
        axis.title.y = element_text(size = 22,margin = margin(r = 10)),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 15))
# combine the two plots
temp925_plots <- cowplot::plot_grid(temperature_925 ,temp9,
                           labels = c("a)", "b)"),ncol = 2,
                           rel_widths = c(0.85, 0.8),label_size = 16,label_x = c(0.030, 0.05))
ggsave("temp925_plots.png", temp925_plots, width = 4000, height = 2600, units = "px", 
       pointsize = 22,bg = "white",dpi = 300)

# 850 ####
se_850 <- smooth_estimates(gam_model850,level = 0.99) %>% add_confint()
DataGAM850 <- DataGAM %>% add_partial_residuals(gam_model850)
colnames(DataGAM850)[16] <- "res_temp"  
colnames(DataGAM850)[17] <- "res_tail" 
# tail 
tail_850 <- se_850 %>%  
  filter(.smooth == "s(anom_tailwind850)") %>% 
  ggplot() +
  geom_rug(aes(x = anom_tailwind850),data = DataGAM850,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = anom_tailwind850),
              alpha = 0.3, fill = "#99D3E2") +
  geom_point(aes(x = anom_tailwind850, y = res_tail),
             data = DataGAM850, shape = 21,size = 4, alpha = 0.7, colour = "#5F5D5E", bg="#27C2D4") +
  geom_line(aes(x = anom_tailwind850, y = .estimate), lwd = 1, linetype = "dashed", color = "#27C2D4") +
  #geom_text(aes(x = anom_tailwind850, y = res_tail, label = YEAR),
  #data = DataGAM850, vjust = -1, size = 5) + 
  geom_text_repel(aes(x = anom_tailwind850, y = res_tail, label = YEAR),
                  data = DataGAM850,size = 5,max.overlaps = Inf) +
  scale_size_continuous(name = "Q",range = c(1,9),
                        breaks = pretty(range(DataGAM850$Q), n = 4)) +
  labs(subtitle = "Effects of tailwinds on co-migration fidelity",
       x = "Tailwind anomalies at 850 mb", y = "Modularity Q") + 
  theme_minimal() + 
  theme(panel.background = element_rect(fill = NA),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 22),
        plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 22)) 
ggsave("tail_850.png", tail_850, width = 3000, height = 2000, units = "px", 
       pointsize = 22,bg = "white",dpi = 300)
#********************************************************************************************************#
# temperature 
temperature_850 <- se_850 %>%  
  filter(.smooth == "s(anom_temperature850)") %>% 
  ggplot() +
  geom_rug(aes(x = anom_temperature850),data = DataGAM850,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = anom_temperature850),
              alpha = 0.3, fill = "#F5BE88") +
  geom_point(aes(x = anom_temperature850, y = res_temp),
             data = DataGAM850, shape = 21, size = 4,alpha = 0.7, colour = "#5F5D5E", bg="#FB5573") +
  geom_line(aes(x = anom_temperature850, y = .estimate), 
            lwd = 1, linetype = "dashed", color = "#FB5573") +
  geom_text_repel(aes(x = anom_temperature850, y = res_temp, label = YEAR),
                  data = DataGAM850,size = 5,max.overlaps = Inf) +
  scale_size_continuous(name = "Q",range = c(1,9),
                        breaks = pretty(range(DataGAM850$Q), n = 4)) +
  labs(subtitle = "Effects of temperatures on co-migration fidelity",
       x = "Temperature anomalies at 850 mb", y = "Modularity Q") + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = NA),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 22),
        plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 22)) 
ggsave("temperature_850.png", temperature_850, width = 3000, height = 2000, units = "px", 
       pointsize = 22,bg = "white",dpi = 300)
# EXTRACT OUTPUT GAM ####
library(mgcv)
library(openxlsx)
# Set up function to extract effect sizes and confidence intervals 
extract_all_effects <- function(gam_model, data, digits = 3, n_points = 100) {
  library(mgcv)
  
  gam_summary <- summary(gam_model)
  smooth_terms <- rownames(gam_summary$s.table)
  
  results_list <- list()
  
  # Get all variables used in the model from formula
  all_vars <- all.vars(formula(gam_model))
  
  for (term in smooth_terms) {
    preds <- gsub("s\\(|ti\\(|\\)", "", term)
    preds <- unlist(strsplit(preds, ","))
    preds <- trimws(preds)
    
    if (length(preds) == 1) {
      pred_seq <- seq(min(data[[preds]], na.rm=TRUE), max(data[[preds]], na.rm=TRUE), length.out = n_points)
      
      # Build new_data with *all* variables from model formula
      new_data <- as.data.frame(lapply(all_vars, function(v) mean(data[[v]], na.rm=TRUE)))
      names(new_data) <- all_vars
      new_data <- new_data[rep(1, n_points), , drop=FALSE]
      new_data[[preds]] <- pred_seq
      
      pred <- predict(gam_model, newdata = new_data, type = "link", se.fit = TRUE)
      
      mean_fit <- mean(pred$fit)
      ci_width <- (mean_fit + 1.96 * mean(pred$se.fit)) - (mean_fit - 1.96 * mean(pred$se.fit))
      
      pos_count <- sum(pred$fit > 0)
      neg_count <- sum(pred$fit < 0)
      directionality <- if (pos_count > length(pred$fit)/2) {
        "Positive"
      } else if (neg_count > length(pred$fit)/2) {
        "Negative"
      } else {
        "Mixed"
      }
      
    } else if (length(preds) == 2) {
      pred1_seq <- seq(min(data[[preds[1]]], na.rm=TRUE), max(data[[preds[1]]], na.rm=TRUE), length.out = sqrt(n_points))
      pred2_seq <- seq(min(data[[preds[2]]], na.rm=TRUE), max(data[[preds[2]]], na.rm=TRUE), length.out = sqrt(n_points))
      
      grid <- expand.grid(pred1_seq, pred2_seq)
      colnames(grid) <- preds
      
      # Build new_data grid with *all* vars, others at mean
      others <- setdiff(all_vars, preds)
      for (o in others) {
        grid[[o]] <- mean(data[[o]], na.rm=TRUE)
      }
      
      pred <- predict(gam_model, newdata = grid, type = "link", se.fit = TRUE)
      
      mean_fit <- mean(pred$fit)
      ci_width <- (mean_fit + 1.96 * mean(pred$se.fit)) - (mean_fit - 1.96 * mean(pred$se.fit))
      
      pos_count <- sum(pred$fit > 0)
      neg_count <- sum(pred$fit < 0)
      directionality <- if (pos_count > length(pred$fit)/2) {
        "Positive"
      } else if (neg_count > length(pred$fit)/2) {
        "Negative"
      } else {
        "Mixed"
      }
      
    } else {
      warning(paste("Term", term, "has unexpected number of predictors:", length(preds)))
      next
    }
    
    pval <- signif(gam_summary$s.table[term, "p-value"], digits)
    dev_exp <- round(gam_summary$dev.expl * 100, digits)
    
    results_list[[term]] <- data.frame(
      Term = term,
      EffectSize = round(mean_fit, digits = 3),
      Directionality = directionality,
      CI_Width = round(ci_width, digits = 3),
      DevianceExplained = round(dev_exp, digits = 3),
      PValue = round(pval, digits = 3)
    )
  }
  
  results_df <- do.call(rbind, results_list)
  rownames(results_df) <- NULL
  return(results_df)
}
      
# Extract effects for each
results_1000 <- extract_all_effects(gam_model1000, DataGAM)
results_925 <- extract_all_effects(gam_model925, DataGAM)
results_850 <- extract_all_effects(gam_model850, DataGAM)

# Combine all results
all_results <- rbind(
  cbind(Model = "1000", results_1000),
  cbind(Model = "925", results_925),
  cbind(Model = "850", results_850)
)
# Save results to an Excel file
write.xlsx(all_results, file = "allresultsGAM.xlsx", overwrite = TRUE)
