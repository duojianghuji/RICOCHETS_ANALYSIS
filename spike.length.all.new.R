library(tidyverse)
library(ggpubr)
library(plotly)
library(broom)
library(InraeThemes)
theme_inrae()



spike.length.all <- read.csv("raw.data/spike.length.mm.all.tiller.csv", sep = ";", dec = ",", nrows = 225) |>  
  rename(
    plant = plante,
    # here no consider FLN 7 or 8
    spike_age_after_TS_BM = age_plante_stadeTS_BM,
    spike_length = lg.épi.réelle..mm.,
    tiller = talle,
    FLN.BM = F.drapeau.BM
         )
glimpse(spike.length.all)


# almost all tillers were sampled at the same day.
# In other words, the samepling date of other tillers is same as the main stem

spike.length.all |> 
  count(plant, tiller, date_6F_BM) |> 
  filter(n >1)


fitted_tiller <- spike.length.all |> 
  select(spike_age_after_TS_BM,
         spike_length,
         tiller,
         FLN.BM       
  ) |> 
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL, labels = c(levels(factor(FLN.BM)), "NA-7"))) |> 
  drop_na(spike_length) |> 
  filter(FLN.BM != "8") |>
  count(tiller) |> 
  filter(n > 6)
  
fitted_tiller

nested.spike.length.all <- spike.length.all |> 
  select(spike_age_after_TS_BM,
         spike_length,
         tiller,
         FLN.BM       
  ) |> 
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL, labels = c(levels(factor(FLN.BM)), "NA-7"))) |> 
  drop_na(spike_length) |> 
  # filter(spike_age_after_TS < 10) |> 
  filter(FLN.BM != "8") |>
  # filter(FLN.BM == "NA-7") |>
  filter(tiller %in% fitted_tiller$tiller) |>
  nest(data = -tiller)







nested.spike.length.all |>  
  # LG0:initial spike length coefficient?; SER: spike elongation rate;
  # TS.BM: Terminal spikelet stage of main stem
  mutate(
    exp_fit = map(data, ~ nls(spike_length ~ LG0 * exp(SER * spike_age_after_TS_BM),
                              data = .x,
                              start = list(LG0 = 2, SER = 0.2)),
                 ),
    tidied = map(exp_fit, tidy),
    glanced = map(exp_fit, glance),
    augmented = map(exp_fit, augment)
  ) |> 
  unnest(tidied) |> 
  filter(term == "SER")
  
  
  nls(spike_length ~ exp_model(spike_age_after_TS_BM, LG0, SER), data = .x, 
      start = list(LG0 = 2, SER = 0.15))  
  c("BM", "T1", "T2", "T3", "T4")
  
  
  
  
  
spike.length.mm.BM <- spike.length.all |> 
  select(spike_age_after_TS,
         spike_length,
         tiller,
         FLN.BM       
         ) |> 
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL, labels = c(levels(factor(FLN.BM)), "NA-7"))) |> 
  filter(tiller == "T1") |> 
  filter(spike_age_after_TS < 10) |> 
  filter(FLN.BM != "8")


spike.length.all |> 
  select(spike_age_after_TS,
         spike_length,
         tiller,
         FLN.BM       
  ) |> 
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL, labels = c(levels(factor(FLN.BM)), "NA-7"))) |> 
  # filter(tiller == "T1.1") |> 
  # filter(spike_age_after_TS < 40) |>
  # filter(FLN.BM != "8") |>
  filter(FLN.BM == "NA-7") |>
  ggplot(aes(spike_age_after_TS, spike_length,)) +
  geom_point(aes(group = tiller, color = tiller, shape = tiller), size = 3)

# Create the plot with a smooth line

# define a spike growth model(exponential)
exp_model <- function(delt_t, LG0, SER) {
  LG0 * exp(SER * (delt_t))
}

# Estimate the model parameters using nls
fit <- nls(spike_length ~ exp_model(spike_age_after_TS, a, b), data = spike.length.mm.BM, 
           start = list(a = 2, b = 0.15))

# Print the model summary
tidy(fit)

fit








  
  geom_smooth(data = . %>% filter(FLN.BM == "NA-7"), 
              method = "gam", formula = y ~ s(x, k=3), se = FALSE)  +
  stat_regline_equation(data = . %>% filter(FLN.BM == "NA-7"), 
                        formula = y ~ s(x, k=3), label.x = -1, label.y = 15)