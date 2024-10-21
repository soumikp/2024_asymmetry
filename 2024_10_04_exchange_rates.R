require(pacman)
pacman::p_load(tidyverse, readxl, here, lubridate,
               rmi, KernSmooth,
               GGally
               )

data <- read_csv(file.path(here(), "data", "2024_10_04.csv")) %>% 
  select(c(1, 3, 7, 13, 40)) %>% 
  mutate(month = month(dmy(Date)),  year = year(dmy(Date))) %>%
  select(-c("Date")) %>% 
  group_by(year, month) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  drop_na() %>% 
  mutate(time = ym(paste0(year, "_", month))) %>% 
  select(-c("month", "year")) %>% 
  pivot_longer(cols = -time) 

data %>% 
  filter(time >= ymd("2010-01-01")) %>% 
  ggplot(aes(x = time, y = value, group = name)) + 
  geom_line(aes(color = name), linewidth = 1) + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12)) + 
  labs(color = "Currency")




