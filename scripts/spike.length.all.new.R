library(tidyverse)
library(ggpubr)
library(plotly)
library(broom)
library(InraeThemes)
theme_inrae()
# For ChatGPT API
Sys.setenv(OPENAI_API_KEY = "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")


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
# In other words, the sampling date of other tillers is same as the main stem

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
  filter(n > 10)
  
fitted_tiller

# nested spike length by "tiller"
nested.spike.length.filtered_tiller <- spike.length.all |> 
  select(spike_age_after_TS_BM,
         spike_length,
         tiller,
         FLN.BM       
  ) |> 
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL, labels = c(levels(factor(FLN.BM)), "NA-7"))) |> 
  drop_na(spike_length) |> 
  # filter(spike_age_after_TS_BM < 10) |> 
  filter(FLN.BM != "8") |>
  # filter(FLN.BM == "NA-7") |>
  filter(tiller %in% fitted_tiller$tiller) |>
  nest(data = -tiller)
nested.spike.length.filtered_tiller





nested.spike.length.filtered_tiller |>  
  # LG0:initial spike length coefficient?; SER: spike elongation rate;
  # TS.BM: Terminal spikelet stage of main stem
  mutate(
    exp_fit = map(data, ~ nls(spike_length ~ LG0 * exp(SER * spike_age_after_TS_BM),
                              data = .x,
                              start = list(LG0 = 2, SER = 0.2)),
                 ),
    tidied = map(exp_fit, tidy),
    glanced = map(exp_fit, glance),
    augmented = map(exp_fit, augment),
  ) |> 
  unnest(tidied) |> 
  filter(p.value)


################################################################################
BM_NA_7 <- BM |> 
  filter(FLN.BM == "NA-7")
  

fit_NA_7 <- nls(data = BM_NA_7, 
           spike.length.mm ~ LG0 * exp(SER * day_after_leaf6_BM),
           start = list(LG0 = 1.5, SER = 0.2)) 
fit_7 <- nls(data = BM, 
                spike.length.mm ~ LG0 * exp(SER * day_after_leaf6_BM),
                start = list(LG0 = 1.5, SER = 0.2)) 

tidy(fit_7)
tidy(fit_NA_7)

t <- tibble(day_after_leaf6_BM = seq(-1.2, 15, length.out = 1000))
t
pred_t <- t|> 
  mutate(
    pred_NA_7 = predict(fit_7, newdata = list(day_after_leaf6_BM =  t$day_after_leaf6_BM)),
    pred_7  = predict(fit_NA_7, newdata = list(day_after_leaf6_BM =  t$day_after_leaf6_BM))
  )

predict(fit, newdata = list(day_after_leaf6_BM =  t$day_after_leaf6_BM))

pred_t

ggplot() +
  geom_point(data = BM, aes(x = day_after_leaf6_BM, y = spike.length.mm)) +
  geom_line(data = pred_t, aes(x=day_after_leaf6_BM, y =pred_NA_7)) +
  geom_line(data = pred_t, aes(x=day_after_leaf6_BM, y =pred_7))

################################################################################



  filter(term == "SER")
  
  augment()


  nls(spike_length ~ exp_model(spike_age_after_TS_BM, LG0, SER), data = .x, 
      start = list(LG0 = 2, SER = 0.15))  
  c("BM", "T1", "T2", "T3", "T4")
  
  
  
  
  
spike.length.mm.BM <- spike.length.all |> 
  select(spike_age_after_TS_BM,
         spike_length,
         tiller,
         FLN.BM       
         ) |> 
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL, labels = c(levels(factor(FLN.BM)), "NA-7"))) |> 
  filter(tiller == "T1") |> 
  filter(spike_age_after_TS_BM < 10) |> 
  filter(FLN.BM != "8")


spike.length.all |> 
  select(spike_age_after_TS_BM,
         spike_length,
         tiller,
         FLN.BM       
  ) |> 
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL, labels = c(levels(factor(FLN.BM)), "NA-7"))) |> 
  # filter(tiller == "T1.1") |> 
  # filter(spike_age_after_TS_BM < 40) |>
  # filter(FLN.BM != "8") |>
  filter(FLN.BM == "NA-7") |>
  ggplot(aes(spike_age_after_TS_BM, spike_length,)) +
  geom_point(aes(group = tiller, color = tiller, shape = tiller), size = 3)




# Create the plot with a smooth line

spike.length.all |> 
  select(spike_age_after_TS_BM,
         spike_length,
         tiller,
         FLN.BM       
  ) |> 
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL, labels = c(levels(factor(FLN.BM)), "NA-7"))) |> 
  # filter(tiller %in% c("BM", "T0", "T1", "T2", "T3", "T1.1", "T2.1")) |>
  filter(tiller == "BM") |> 
  # filter(spike_age_after_TS_BM < 10) |>
  filter(FLN.BM != "8") |>
  # filter(FLN.BM == "NA-7") |>
  ggplot(aes(spike_age_after_TS_BM, spike_length,)) +
  geom_point(aes(group = FLN.BM, color = FLN.BM, shape = FLN.BM), size = 2) + 
  # facet_wrap(~tiller, nrow = 3) +
  geom_smooth(method = "nls", formula = spike_length ~ LG0 * exp(SER * spike_age_after_TS_BM), start = list(LG0 = 0.9, SER = 0.1), se = FALSE)


ggsave("plots/spike_length/all_tillers/FLN_7_plus_8_before_age_day_10.Boris.png", width = 12, height = 8)

  stat_regline_equation(data = . %>% filter(FLN.BM == "NA-7"), 
                        formula = y ~ s(x, k=3), label.x = -1, label.y = 15)














# define a spike growth model(exponential)
exp_model <- function(delt_t, LG0, SER) {
  LG0 * exp(SER * (delt_t))
}

# Estimate the model parameters using nls
fit <- nls(spike_length ~ exp_model(spike_age_after_TS_BM, a, b), data = spike.length.mm.BM, 
           start = list(a = 2, b = 0.15))

# Print the model summary
tidy(fit)

fit








  
  geom_smooth(data = . %>% filter(FLN.BM == "NA-7"), 
              method = "gam", formula = y ~ s(x, k=3), se = FALSE)  +
  stat_regline_equation(data = . %>% filter(FLN.BM == "NA-7"), 
                        formula = y ~ s(x, k=3), label.x = -1, label.y = 15)