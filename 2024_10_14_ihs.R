pacman::p_load(tidyverse, rmi, here, 
               haven, infotheo, ggsci, stringr, 
               latex2exp, patchwork, ggpubr)

data <- read_sas(file.path(here(), "data", "IHSdata_ICPSRv1.sas7bdat")) 

data <-data %>% 
  select(colnames(data)[c(1, 2, 3, 4, 5, 
                          54, 55, 56, 57, 58, 
                          99, 100, 101, 102, 103)]) %>% 
  drop_na()


estim <- function(x, nboot = 10){
  male <- x %>% filter(Sex == 1)
  female <- x %>% filter(Sex == 2)
  
  dep_male <- male %>% pull(dep)
  p_dep_male <- table(dep_male)/length(dep_male)
  anx_male <- male %>% pull(anx)
  p_anx_male <- table(anx_male)/length(anx_male)
  
  male_pval <- chisq.test(dep_male, anx_male)$p.value
  
  c_dep_anx_male <- sum(-log(p_dep_male)*p_dep_male) - sum(-log(p_anx_male)*p_anx_male)
  
  dep_female <- female %>% pull(dep)
  p_dep_female <- table(dep_female)/length(dep_female)
  anx_female <- female %>% pull(anx)
  p_anx_female <- table(anx_female)/length(anx_female)
  
  female_pval <- chisq.test(dep_female, anx_female)$p.value
  c_dep_anx_female <- sum(-log(p_dep_female)*p_dep_female) - sum(-log(p_anx_female)*p_anx_female)
  
  #### bootstrapping CI
  boot <- NULL
  
  for(i in 1:nboot){
    indices_male <- sample(1:nrow(male), nrow(male), replace = TRUE)
    indices_female <- sample(1:nrow(female), nrow(female), replace = TRUE)
    
    p_dep_female_boot <- table(dep_female[indices_female])/length(dep_female[indices_female])
    p_anx_female_boot <- table(anx_female[indices_female])/length(anx_female[indices_female])
    c_dep_anx_female_boot <- sum(-log(p_dep_female_boot)*p_dep_female_boot) - sum(-log(p_anx_female_boot)*p_anx_female_boot)
    
    
    p_dep_male_boot <- table(dep_male[indices_male])/length(dep_male[indices_male])
    p_anx_male_boot <- table(anx_male[indices_male])/length(anx_male[indices_male])
    c_dep_anx_male_boot <- sum(-log(p_dep_male_boot)*p_dep_male_boot) - sum(-log(p_anx_male_boot)*p_anx_male_boot)
    
    boot <- rbind(boot, 
                  c(c_dep_anx_male_boot, c_dep_anx_female_boot)
                  )
  }
  
  
  op <- list("-log10(pval(male))" = -log10(male_pval), 
             "estim(male)" = c_dep_anx_male, 
             "ci(male)" = as.numeric(quantile(boot[,1], c(0.025, 0.975))),
             "-log10(pval(female))" = -log10(female_pval), 
             "estim(female)" = c_dep_anx_female, 
             "ci(female)" = as.numeric(quantile(boot[,2], c(0.025, 0.975))))
  return(op)
  
}

## moderate anxiety and depression

d2a_1 <- data %>%  filter(PHQtot1 >= 10 & GADtot1 >= 10) %>% 
  mutate(dep = round(PHQtot1), anx = round(GADtot1)) %>% 
  select(c(Sex, anx, dep))
d2a_1 <- estim(d2a_1, nboot = 5000)

d2a_2 <- data %>%  filter(PHQtot2 >= 10 & GADtot2 >= 10) %>% 
  mutate(dep = round(PHQtot2), anx = round(GADtot2)) %>% 
  select(c(Sex, anx, dep))
d2a_2 <- estim(d2a_2, nboot = 5000)

d2a_3 <- data %>%  filter(PHQtot3 >= 10 & GADtot3 >= 10) %>% 
  mutate(dep = round(PHQtot3), anx = round(GADtot3)) %>% 
  select(c(Sex, anx, dep))
d2a_3 <- estim(d2a_3, nboot = 5000)

d2a_4 <- data %>%  filter(PHQtot4 >= 10 & GADtot4 >= 10) %>% 
  mutate(dep = round(PHQtot4), anx = round(GADtot4)) %>% 
  select(c(Sex, anx, dep))
d2a_4 <- estim(d2a_4, nboot = 5000)

### p-value plots
op <- rbind(c(1, 1, d2a_1$`-log10(pval(male))`), 
            c(1, 2, d2a_2$`-log10(pval(male))`), 
            c(1, 3, d2a_3$`-log10(pval(male))`), 
            c(1, 4, d2a_4$`-log10(pval(male))`), 
            c(2, 1, d2a_1$`-log10(pval(female))`), 
            c(2, 2, d2a_2$`-log10(pval(female))`), 
            c(2, 3, d2a_3$`-log10(pval(female))`), 
            c(2, 4, d2a_4$`-log10(pval(female))`))

op <- as_tibble(op) %>% 
  rename(Sex = V1, Quarter = V2, pval = V3) %>% 
  mutate(Quarter = paste0("Quarter ", Quarter), 
         Sex = case_when(Sex == 1 ~ "Male", Sex == 2 ~ "Female"))

plot_indep <- 
  op %>% 
  ggplot(aes(y = Quarter, x = pval, color = Sex)) + 
  geom_point() + 
  geom_linerange(aes(xmin = 0, xmax = pval), linewidth = 1) +
  facet_grid(rows = vars(Sex)) + 
  theme_bw() + 
  scale_color_lancet() + 
  scale_fill_lancet() + 
  labs(x = latex2exp::TeX("$-log_{10}(p-value)$"), y = "", 
       color = "Sex", 
       fill = "Sex") + 
  geom_vline(xintercept = -log10(0.05/8), linetype = "dashed", color = "black") + 
  theme(legend.position = "bottom", 
        strip.background = element_rect(fill = "black"), 
        strip.text = element_text(size = 12, face = "bold", color = "white"),
        legend.text = element_text(size = 12, face = "bold", color = "black"),
        legend.title = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size = 10, face = "bold", color = "black"),
        axis.text.y = element_text(size = 10, face = "bold", color = "black"),
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"))





#### estim plot ####

op <- rbind(c(1, 1, d2a_1$`estim(male)`, d2a_1$`ci(male)`), 
            c(1, 2, d2a_2$`estim(male)`, d2a_2$`ci(male)`), 
            c(1, 3, d2a_3$`estim(male)`, d2a_3$`ci(male)`), 
            c(1, 4, d2a_4$`estim(male)`, d2a_4$`ci(male)`), 
            c(2, 1, d2a_1$`estim(female)`, d2a_1$`ci(female)`), 
            c(2, 2, d2a_2$`estim(female)`, d2a_2$`ci(female)`), 
            c(2, 3, d2a_3$`estim(female)`, d2a_3$`ci(female)`), 
            c(2, 4, d2a_4$`estim(female)`, d2a_4$`ci(female)`))

op <- as_tibble(op) %>% 
  rename(Sex = V1, Quarter = V2, estim = V3, low = V4, up = V5) %>% 
  mutate(Quarter = paste0("Quarter ", Quarter), 
         Sex = case_when(Sex == 1 ~ "Male", Sex == 2 ~ "Female")) %>% 
  mutate(text = paste0(sprintf("%05.3f", estim), " (", 
                       sprintf("%05.3f", low),  ", ", 
                       sprintf("%05.3f", up),  ")"))

plot_results <- op %>% 
  ggplot() + 
  geom_point(aes(x = Quarter, y = estim, color = Sex)) + 
  geom_errorbar(aes(x = Quarter, ymin = low, ymax = up, color = Sex), 
                linewidth = 1, width = 0.1) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_label(aes(x = Quarter, y = estim, label = text, color = Sex), size = 4, fontface = "bold", show.legend = FALSE) + 
  facet_grid(rows = vars(Sex)) + 
  theme_bw() + 
  scale_color_lancet() + 
  scale_fill_lancet() + 
  labs(x = "", y = "H(PHQ-9) - H(GAD-7)", 
       color = "Sex", 
       fill = "Sex") + 
  theme(legend.position = "bottom", 
        strip.background = element_rect(fill = "black"), 
        strip.text = element_text(size = 12, face = "bold", color = "white"),
        legend.text = element_text(size = 12, face = "bold", color = "black"),
        legend.title = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size = 10, face = "bold", color = "black"),
        axis.text.y = element_text(size = 10, face = "bold", color = "black"),
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"))



#### scattter plot of severe dep v severe anx ####
data_scatter <- as_tibble(cbind(data %>% select(c(Sex, IHSrid, PHQtot1, PHQtot2, PHQtot3, PHQtot4)) %>% pivot_longer(cols = -c(Sex, IHSrid)), 
      data %>% select(c(Sex, IHSrid, GADtot1, GADtot2, GADtot3, GADtot4)) %>% pivot_longer(cols = -c(Sex, IHSrid)))[,-c(5, 6, 7)]) %>% 
  mutate(Quarter = paste0("Quarter ", as.numeric(str_sub(name, -1)))) %>% 
  rename(dep = value, anx = value.1) %>% 
  filter(dep >= 10, anx >= 10) %>% 
  mutate(Sex = case_when(Sex == 1 ~ "Male", Sex == 2 ~ "Female")) %>% 
  select(-c("IHSrid", "name")) %>% 
  ggplot(aes(x = dep, y = anx, group = Quarter, color = Sex)) + 
  geom_count() + 
  facet_grid(cols = vars(Quarter), rows = vars(Sex)) +
  scale_color_lancet() + 
  scale_fill_lancet() + 
  labs(x = "Self-reported PHQ-9 scores", y = "Self-reported GAD-7 scores", 
       color = "Sex", 
       fill = "Sex") + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        strip.background = element_rect(fill = "black"), 
        strip.text = element_text(size = 12, face = "bold", color = "white"),
        legend.text = element_text(size = 12, face = "bold", color = "black"),
        legend.title = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size = 10, face = "bold", color = "black"),
        axis.text.y = element_text(size = 10, face = "bold", color = "black"),
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 12, face = "bold", color = "black")) + 
  guides(fill = "none", color = "none") + 
  geom_smooth(method = "gam", span = 0.75, se = FALSE, color = "black", linetype = "dashed")

p <- (data_scatter)/(plot_indep + plot_results)

factor <- 1.4
ggsave(file.path(here(), "images", "2024_10_14_ihs.pdf"), 
       height = factor*8.5, 
       width = factor*11, 
       units = "in", 
       device = "pdf")


