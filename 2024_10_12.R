pacman::p_load(haven, tidyverse, here, infotheo)

affine_unif <- function(x){
  (x - min(x))/(max(x) - min(x))
}

## demographic data
demo <- read_xpt(file.path(here(), "data/2024_10_11_DEMO_G.XPT"))
demo <- demo[colnames(demo) %in% c("SEQN", "RIAGENDR", "RIDAGEYR")] %>%
  drop_na()

## cognitive functioning data
cf <- read_xpt(file.path(here(), "data/2024_10_11_CFQ_G.XPT"))
cf <- cf[colnames(cf) %in% c("SEQN", "CFDDS", "CFDCSR", "CFDAST")] %>% 
  drop_na()

## grip strength data
hgs <- read_xpt(file.path(here(), "data/2024_10_11_MGX_G.XPT"))
hgs <- hgs[colnames(hgs) %in% c("SEQN", "MGDCGSZ")] %>% 
  drop_na()

## walking speed data
ws <- read_xpt(file.path(here(), "data/2024_10_11_MSX_B.XPT"))
ws <- ws[colnames(ws) %in% c("SEQN", "MSAEXLEN")] %>% 
  drop_na()

data <- inner_join(inner_join(demo, cf), hgs) %>% 
  mutate(CFDCSR = round(CFDCSR), 
         CFDAST = round(CFDAST), 
         CFDDS = round(CFDDS), 
         MGDCGSZ = round(MGDCGSZ))

cor.test(x = data %>% pull(c(CFDCSR)),
         y = data %>% pull(c(MGDCGSZ)), 
         method = "kendall")$p.value
## does not show association so i didn't go further. 

cor.test(x = data %>% pull(c(CFDAST)),
         y = data %>% pull(c(MGDCGSZ)), 
         method = "kendall")$p.value
estims <- replicate(1000, 
                    {
                      temp <- data[sample(1:nrow(data), nrow(data), replace = TRUE), ]
                      
                      p_CFDAST <- table(temp$CFDAST)
                      p_CFDAST <- p_CFDAST/sum(p_CFDAST)
                      
                      p_MGDCGSZ <- table(temp$MGDCGSZ)
                      p_MGDCGSZ <- p_MGDCGSZ/sum(p_MGDCGSZ)
                      
                      sum(log(p_CFDAST)*p_CFDAST) - sum(log(p_MGDCGSZ)*p_MGDCGSZ)
                    })
mean(estims)
quantile(estims, c(0.025, 0.975))
## evidence prompting HGS >> Cognitive functioning ()

cor.test(x = data %>% pull(c(CFDDS)),
         y = data %>% pull(c(MGDCGSZ)), 
         method = "kendall")$p.value
estims <- replicate(1000, 
                    {
                      temp <- data[sample(1:nrow(data), nrow(data), replace = TRUE), ]
                      
                      p_CFDDS <- table(temp$CFDDS)
                      p_CFDDS <- p_CFDDS/sum(p_CFDDS)
                      
                      p_MGDCGSZ <- table(temp$MGDCGSZ)
                      p_MGDCGSZ <- p_MGDCGSZ/sum(p_MGDCGSZ)
                      
                      sum(log(p_CFDDS)*p_CFDDS) - sum(log(p_MGDCGSZ)*p_MGDCGSZ)
                    })
mean(estims)
quantile(estims, c(0.025, 0.975))
## evidence prompting HGS >> Cognitive functioning ()
