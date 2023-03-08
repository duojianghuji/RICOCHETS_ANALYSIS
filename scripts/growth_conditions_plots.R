
# Introduction of the analysis----

# There are 3 growth cabinets for the manipulation of different temperature treatments
# (Chamber A, Chamber B1 and Chamber B2): Chamber A is responsible for control and normal growth conditions,
# B1 and B2 are for priming stress and recurrent stress

# priming stress period (6 days) in B1 from 14 Mar to 19 Mar


# daytime saving occured between 2021-04-05 and 2021-04-06
# par(light intensity) value in chamber A (control) is almost double than chamber B1 and B2


# Libraries
library(tidyverse)
library(lubridate)
library(plantecophys)
library(plotly)
library(DataExplorer)
library(SmartEDA)
library(patchwork)
library(gridExtra)

# For ChatGPT API
Sys.setenv(OPENAI_API_KEY = "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")

# Import data ----

# meteo_B1_B2 <- c("raw.data/meteo_cabinetB1.csv",
#                  "raw.data/meteo_cabinetB2.csv")
# read_delim(meteo_B1_B2, delim = ";", id = "file")

# Data for B1
meteo_cabinetB1 <-
  read_delim("raw.data/meteo_cabinetB1.csv", 
             delim = ";",
             col_types = cols(
               date = col_datetime(format = "%d/%m/%Y %H:%M")
             )
  )
View(meteo_cabinetB1)
glimpse(meteo_cabinetB1)
head(meteo_cabinetB1)

# Data for B2
meteo_cabinetB2 <-
  read_delim("raw.data/meteo_cabinetB2.csv", 
             delim = ";",
             col_types = cols(
               date = col_datetime(format = "%d/%m/%Y %H:%M")
             )
  )

glimpse(meteo_cabinetB2)
view(meteo_cabinetB2)

# chamber A data 
meteo_cabinetA <-
  read_delim("raw.data/meteo_chambreA.csv",
             delim = ";",
             col_types = cols(
               date = col_datetime(format = "%d/%m/%Y %H:%M")
             )
  )
glimpse(meteo_cabinetA)
View(meteo_cabinetA)




meteo_B1 <-  meteo_cabinetB1 |>
  # select(ends_with("_b1")) |>
  rename_with(~ str_replace(., "_b1", "")) |> 
  select(
    scenario      = "traitements",
    stress_period = "periode_stress",
    chamber       = "site",
    tc_in_plant   = "branchement_thermocouples",
    datetime      = "date",
    time_of_day   = "heure_debut_jour",
    time          = "heure",
    t_air         = "tair01",
    tc_01         = "tc01",
    tc_02         = "tc02",
    tc_03         = "tc03",
    tc_04         = "tc04",
    hr_01         = "hr01",
    t_plant_mean  = "tc_mean",
    vpd_air       = "vpd_air",
    vpd_plant     = "vpd_plant",
    par           = "par01",
  ) |> 
  mutate(
      # datetime = dmy_hm(date),
    date     = lubridate::date(datetime),
    .before = time_of_day
  ) |> 
  # select data only when the thermocouple was attached in the plant
  filter(tc_in_plant == "oui")



col_names_B1 <- colnames(meteo_B1)

glimpse(meteo_B1)

view(meteo_B1)



meteo_B2 <-  meteo_cabinetB2 |>
  # select(ends_with("_b2")) |>
  rename_with(~ str_replace(., "_b2", "")) |> 
  select(
    scenario      = "traitements",
    stress_period = "periode_stress",
    chamber       = "site",
    tc_in_plant   = "branchement_thermocouples",
    datetime      = "date",
    time_of_day   = "heure_debut_jour",
    time          = "heure",
    t_air         = "tair01",
    tc_01         = "tc01",
    tc_02         = "tc02",
    tc_03         = "tc03",
    tc_04         = "tc04",
    hr_01         = "hr01",
    t_plant_mean  = "tc_mean",
    vpd_air       = "vpd_air",
    vpd_plant     = "vpd_plant",
    par           = "par01",
  ) |>
  mutate(
    # datetime = dmy_hm(date),
    date     = lubridate::date(datetime),
    .before = time_of_day
  ) |> 
  # select data only when the thermocouple was attached in the plant
  filter(tc_in_plant == "oui")

View(meteo_B2)
col_names_B2 <- colnames(meteo_B2)

glimpse(meteo_B2)

col_names_B1 == col_names_B2

# Combine meteo_B1 and meteo_B2

meteo_B1_B2 <- 
  bind_rows(meteo_B1, meteo_B2)

col_names_B1_B2 <- colnames(meteo_B1_B2)

# col_names_B1_B2
#
# view(meteo_B1_B2)

glimpse(meteo_B1_B2)


meteo_A <- meteo_cabinetA |> 
  select(
    datetime     = "date",
    time         = "heure",
    time_of_day  = "heure_debut_jour",
    chamber      = "site",
    co2          = "co2_01_cA",
    hr_01        = "hr01_cA",
    hr_02        = "hr02_cA",
    par_01       = "par01_cA",
    par_02       = "par02_cA",
    tair_01      = "tair01_cA",
    tair_02      = "tair02_cA",
    tc_01        = "tc01_cA",
    tc_02        = "tc02_cA",
    tc_03        = "tc03_cA",
    tc_04        = "tc04_cA",
    tc_05        = "tc05_cA",
    tc_06        = "tc06_cA",
    hr_mean      = "hr.moy",
    t_air_mean   = "t.air.moy",
    t_plant_mean = "tplant.moy",
    vpd_air      = "vpd.air",
    # vpd_plant  = "vpd.plant",
    par          = "par.moy",
    tc_in_plant  = "branchement_thermocouples"
  ) |>
  mutate(
    vpd_plant   = VPDairToLeaf(vpd_air, t_air_mean, t_plant_mean, Pa = 101),
    .after      = vpd_air,
    scenario    = "control",
    stress_period = "no_stress",
    date     = lubridate::date(datetime)
  ) |> 
  # select data only when the thermocouple was attached in the plant
  filter(tc_in_plant == "oui")

glimpse(meteo_A)
View(meteo_A)


# Data visualization ----

## Quick data exploring ----

# meteo_A |>
# # Data profile exploring using DataExplore package
# create_report(
#   output_file  = paste("Report", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), sep=" - "),
#   report_title = "EDA Report - Cardiovascular Disease Dataset",
#   y            = "cardio"
# )
# # Data profile exploring using SmartEDA package
# ExpReport(op_file = "report.html")


## Overall time series data exploring through visualization of interactive charts to spot the inregular data ----

# Checking the daytime saving between 2021-04-05 and 2021-04-06

time_saving_start <- as.Date("2021-04-05")
time_saving_end <- as.Date("2021-04-06")

## Interactive charts of tair, tplant and par for each chamber =============================================
# tair, tplant and par are all mean value directly retrived from the original datatable

temp_chamber_A <- meteo_A |> 
  # filter(between(date, time_saving_start, time_saving_end)) |>
  # filter(stress_period = priming_34) |> 
  
  # ggplot(aes(x = datetime)) +
  # geom_point(aes(y = t_air)) +
  # geom_line(aes(y = t_air)) +
  # scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M")
  plot_ly(x = ~datetime, 
          y = ~t_air_mean, 
          name = "t_air_A", type = "scatter",
          mode = "lines+markers"
  ) |> 
  add_trace(y = ~t_plant_mean, name = "t_plant_A")


# temp_chamber_A

temp_chamber_B1 <- meteo_B1 |> 
  # filter(between(date, time_saving_start, time_saving_end)) |> 
  # filter(stress_period == "priming_34") |> 
  
  # ggplot(aes(x = datetime)) +
  # geom_point(aes(y = t_air)) +
  # geom_line(aes(y = t_air)) +
  # scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M")
  plot_ly(x = ~datetime, 
          y = ~t_air, 
          name = "t_air_B1", type = "scatter",
          mode = "lines+markers"
  ) |> 
  add_trace(y = ~t_plant_mean, name = "t_plant_B1")
  
# temp_chamber_B1

temp_chamber_B2 <- meteo_B2 |> 
  # filter(between(date, time_saving_start, time_saving_end)) |> 
  # filter(stress_period == "priming_34") |> 
  
  # ggplot(aes(x = datetime)) +
  # geom_point(aes(y = t_air)) +
  # geom_line(aes(y = t_air)) +
  # scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M")
  plot_ly(x = ~datetime, 
          y = ~t_air, 
          name = "t_air_B2", type = "scatter",
          mode = "lines+markers"
  ) |> 
  add_trace(y = ~t_plant_mean, name = "t_plant_B2")

# combine plots
temp_all_chamber <- subplot(temp_chamber_A, temp_chamber_B1, temp_chamber_B2, nrows = 3, shareX = T)
temp_all_chamber

# output as an html file
htmlwidgets::saveWidget(
  widget = temp_all_chamber, #the plotly object
  file = "temp_all_chamber.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)

## par value for each chamber

par_A <- plot_ly(meteo_A, x = ~datetime, y = ~par, name = "par_A", type = "scatter", mode = "lines+markers")
par_B1 <- plot_ly(meteo_B1, x = ~datetime, y = ~par, name = "par_B1", type = "scatter", mode = "lines+markers") 
par_B2 <- plot_ly(meteo_B2, x = ~datetime, y = ~par, name = "par_B2", type = "scatter", mode = "lines+markers") 

par_all_chamber <- subplot(par_A, par_B1, par_B2, nrows = 3, shareX = TRUE)

# Notice the difference in par between chamber A and chamber B1 B2
par_all_chamber

# output as an html file
htmlwidgets::saveWidget(
  widget = par_all_chamber, #the plotly object
  file = "par_all_chamber.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)

# Different way to plot interactive chart for par value
par_plots <-
  ggplot() +
  geom_line(data = meteo_A, aes(datetime, par), color = "red") +
  geom_line(data = meteo_B1, aes(datetime, par), color = "blue") +
  geom_line(data = meteo_B2, aes(datetime, par), color = "green")
ggplotly(par_plots)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Chamber B2 interactive charts for analysis ----------------------------------
meteo_B2 |> 
  mutate(
    t_plant_recal_12   = (tc_01 + tc_02) / 2, # recalculate tplant
    t_plant_recal_34   = (tc_03 + tc_04) / 2, # recalculate tplant
    # t_air_recal     = mean(tair_01, tair_02),
  ) |> 
  plot_ly(x = ~datetime, 
          y = ~t_air, 
          name = "t_air", type = "scatter",
          mode = "lines+markers"
  ) |> 
  # add_trace(y = ~t_air_recal, name = "tair_recal") |>
  add_trace(y = ~t_plant_recal_12, name = "tplant_recal_12") |>
  add_trace(y = ~t_plant_recal_34, name = "tplant_recal_34") |>
  add_trace(y = ~t_plant_mean, name = "tplant_mean") |> 
  add_trace(y = ~tc_01, name = "tc_01") |> 
  add_trace(y = ~tc_02, name = "tc_02") |> 
  add_trace(y = ~tc_03, name = "tc_03") |> 
  add_trace(y = ~tc_04, name = "tc_04") |> 
  layout(title = "Chamber_B2")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Chamber A interactive charts for analysis -----------------------------------
meteo_A |> 
  mutate(
    # t_plant_recal   = mean(tc_04, tc_05, tc_06), # recalculate tplant
    # t_air_recal     = mean(tair_01, tair_02),
  ) |> 
  plot_ly(x = ~datetime, 
          y = ~t_air_mean, 
          name = "t_air_mean", type = "scatter",
          mode = "lines+markers"
  ) |> 
  # add_trace(y = ~t_air_recal, name = "tair_recal") |>
  # add_trace(y = ~t_plant_recal, name = "tplant_recal") |>
  add_trace(y = ~t_plant_mean, name = "tplant_mean") |> 
  add_trace(y = ~tair_01, name = "tair_01") |> 
  add_trace(y = ~tair_02, name = "tair_02") |> 
  add_trace(y = ~tc_01, name = "tc_01") |> 
  add_trace(y = ~tc_02, name = "tc_02") |> 
  add_trace(y = ~tc_03, name = "tc_03") |> 
  add_trace(y = ~tc_04, name = "tc_04") |> 
  add_trace(y = ~tc_05, name = "tc_05") |> 
  add_trace(y = ~tc_06, name = "tc_06") |> 
  layout(title = "Chamber_A")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




## Create subplots fo stress periods =============================================
glimpse(meteo_B1)

### priming stress ----
meteo_B1 |> 
  
  # select the priming stress period (6 days)
  filter(between(datetime, ymd_hm("2021-03-14 08:00"), ymd_hm("2021-03-19 21:30"))) |> 
  
  # filter specific time points when temp was inferenced due to the entering of people, sampling or measuring plants
  filter(!between(datetime, ymd_hm("2021-03-14 09:05"), ymd_hm("2021-03-14 09:50")),
         
         !between(datetime, ymd_hm("2021-03-19 10:50"), ymd_hm("2021-03-19 11:35"))
         
        ) |> 

  # plot_ly(x = ~datetime, 
  #         y = ~t_air, 
  #         name = "t_air_B1", type = "scatter",
  #         mode = "lines+markers"
  # ) |> 
  # add_trace(y = ~t_plant_mean, name = "t_plant_B1")
  
  # distinct(date)
  select(
    datetime,
    date,
    time,
    t_air,
    t_plant = t_plant_mean,
    vpd_air,
    vpd_plant,
  ) |> 

  group_by(time) |> 
  # summarise(across(where(is.numeric), ~ mean(.x ,na.rm = TRUE))) |> 
  # summarise(across(t_air:t_plant, ~ mean(.x , na.rm = TRUE))) |> 
  # summarize(across(t_air:t_plant, ~ sd(.x , na.rm = TRUE))) |> 
  
  # calculate the mean and standard deviation value for tair and tplant
  summarise(across(t_air:t_plant, list(mean = mean, sd = sd), .names = "{.col}.{.fn}")) |> 
  ggplot(aes(x = time)) +
  # Adding shading to differentiate day and night in a day using ggplot geom_rect
  # the daytime is between 06:30 to 21:30
  geom_rect(
    aes(
      xmin = hms::as_hms("07:30:00"),
      xmax = hms::as_hms("22:00:00"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey",
    alpha = 0.01
  ) +
  geom_line(aes(y = t_air.mean, color = "Tair"), size = 1) +
  # add standard deviation
  geom_ribbon(aes(ymin = t_air.mean - t_air.sd , ymax = t_air.mean + t_air.sd), alpha = 0.2) +
  
  geom_line(aes(y = t_plant.mean, color = "Tplant"), size = 1) +
  geom_ribbon(aes(ymin = t_plant.mean - t_plant.sd , ymax = t_plant.mean + t_plant.sd), alpha = 0.1) +
  labs(
    title = "Temperature",
    x = "Time",
    y = "Temperature (°C)"
  ) +
  scale_color_manual(values = c("Tair" = "#fc8d62", "Tplant" = "#66c2a5")) +
  scale_x_time(breaks = "4 hour", labels = "%H:%M") +
  scale_y_continuous(breaks = seq(10, 40, by = 5)) +
  # facet_wrap( ~ stress_period,
  #             ncol = 1,
  #             nrow = 4,
  #             switch = "y") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    element_line(size = 1),
    text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    legend.position = c(0.1, 0.95),
    legend.background = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    axis.ticks = element_line(linewidth = 0.5)
  )

### meiose 36 stress ----

meteo_B1 |>
  
  # select periods of meiose 36 stress (7 days)
  filter(between(
    datetime,
    ymd_hm("2021-03-24 08:00"),
    ymd_hm("2021-03-31 06:45")
  )) |>
  
  # filter specific time points when temp was inferenced due to the entering of people, sampling or measuring plants
  filter(
    !between(
      datetime,
      ymd_hm("2021-03-25 08:35"),
      ymd_hm("2021-03-25 09:50")
    ),
    !between(
      datetime,
        ymd_hm("2021-03-27 07:50"),
        ymd_hm("2021-03-27 08:05")
    ),
    !between(
      datetime,
      ymd_hm("2021-03-30 07:50"),
      ymd_hm("2021-03-30 08:05")
    ),
    # filter specific date when tplant was abnomal
    date != ymd("2021-03-28")
  ) |> 
  
  # plot_ly(x = ~datetime,
  #         y = ~t_air,
  #         name = "t_air_B1", type = "scatter",
  #         mode = "lines+markers"
  # ) |>
  # add_trace(y = ~t_plant_mean, name = "t_plant_B1")
  
  select(datetime,
         date,
         time,
         t_air,
         t_plant = t_plant_mean,
         vpd_air,
         vpd_plant,) |>
  
  group_by(time) |>
  # summarise(across(where(is.numeric), ~ mean(.x ,na.rm = TRUE))) |>
  # summarise(across(t_air:t_plant, ~ mean(.x , na.rm = TRUE))) |>
  # summarize(across(t_air:t_plant, ~ sd(.x , na.rm = TRUE))) |>
  
  # calculate the mean and standard deviation value for tair and tplant
  summarise(across(t_air:vpd_plant, list(mean = mean, sd = sd), .names = "{.col}.{.fn}")) |>
  
  # temp plots
  ggplot(aes(x = time)) +
  # Adding shading to differentiate day and night in a day using ggplot geom_rect
  # the daytime is between 06:30 to 21:30
  geom_rect(
    aes(
      xmin = hms::as_hms("07:30:00"),
      xmax = hms::as_hms("22:00:00"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey",
    alpha = 0.01
  ) +
  geom_line(aes(y = t_air.mean, color = "Tair"), size = 1) +
  # add standard deviation
  geom_ribbon(aes(ymin = t_air.mean - t_air.sd , ymax = t_air.mean + t_air.sd),
              alpha = 0.2) +
  
  geom_line(aes(y = t_plant.mean, color = "Tplant"), size = 1) +
  geom_ribbon(aes(ymin = t_plant.mean - t_plant.sd , ymax = t_plant.mean + t_plant.sd),
              alpha = 0.1) +
  labs(title = "Temperature",
       x = "Time",
       y = "Temperature (°C)") +
  scale_color_manual(values = c("Tair" = "#fc8d62", "Tplant" = "#66c2a5")) +
  scale_x_time(breaks = "4 hour", labels = "%H:%M") +
  scale_y_continuous(breaks = seq(10, 40, by = 5)) +
  # facet_wrap( ~ stress_period,
  #             ncol = 1,
  #             nrow = 4,
  #             switch = "y") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    element_line(size = 1),
    text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    legend.position = c(0.1, 0.95),
    legend.background = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    axis.ticks = element_line(linewidth = 0.5)
  )


### meiose 32 stress----

meteo_B2 |>
  
  # select periods of meiose 32 stress (6 days)
  filter(between(
    datetime,
    ymd_hm("2021-03-25 09:15"),
    ymd_hm("2021-03-31 07:00")
  )) |> 

  # 
  # filter specific time points when temp was inferenced due to the entering of people, sampling or measuring plants
  filter(!datetime == ymd_hm("2021-03-27 08:15") & !datetime == ymd_hm("2021-03-30 08:00")) |>
  
  # plot_ly(x = ~datetime,
  #         y = ~t_air,
  #         name = "t_air_B2", type = "scatter",
  #         mode = "lines+markers"
  # ) |>
  # add_trace(y = ~t_plant_mean, name = "t_plant_B2")
  
  select(datetime,
         date,
         time,
         t_air,
         t_plant = t_plant_mean,
         vpd_air,
         vpd_plant,) |>
  
  group_by(time) |>
  # summarise(across(where(is.numeric), ~ mean(.x ,na.rm = TRUE))) |>
  # summarise(across(t_air:t_plant, ~ mean(.x , na.rm = TRUE))) |>
  # summarize(across(t_air:t_plant, ~ sd(.x , na.rm = TRUE))) |>
  
  # calculate the mean and standard deviation value for tair and tplant
  summarise(across(t_air:vpd_plant, list(mean = mean, sd = sd), .names = "{.col}.{.fn}")) |>
  
  # temp plots
  ggplot(aes(x = time)) +
  # Adding shading to differentiate day and night in a day using ggplot geom_rect
  # the daytime is between 06:30 to 21:30
  geom_rect(
    aes(
      xmin = hms::as_hms("07:30:00"),
      xmax = hms::as_hms("22:00:00"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey",
    alpha = 0.01
  ) +
  geom_line(aes(y = t_air.mean, color = "Tair"), size = 1) +
  # add standard deviation
  geom_ribbon(aes(ymin = t_air.mean - t_air.sd , ymax = t_air.mean + t_air.sd),
              alpha = 0.2) +
  
  geom_line(aes(y = t_plant.mean, color = "Tplant"), size = 1) +
  geom_ribbon(aes(ymin = t_plant.mean - t_plant.sd , ymax = t_plant.mean + t_plant.sd),
              alpha = 0.1) +
  labs(title = "Temperature",
       x = "Time",
       y = "Temperature (°C)") +
  scale_color_manual(values = c("Tair" = "#fc8d62", "Tplant" = "#66c2a5")) +
  scale_x_time(breaks = "4 hour", labels = "%H:%M") +
  scale_y_continuous(breaks = seq(10, 40, by = 5)) +
  # facet_wrap( ~ stress_period,
  #             ncol = 1,
  #             nrow = 4,
  #             switch = "y") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    element_line(size = 1),
    text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    legend.position = c(0.1, 0.95),
    legend.background = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    axis.ticks = element_line(linewidth = 0.5)
  )

### control treatment ----
glimpse(meteo_A)

meteo_A |>
  
  # select periods of meiose 32 stress (6 days)
  filter(between(
    datetime,
    ymd_hm("2021-03-25 07:30"),
    ymd_hm("2021-03-31 07:00")
  )) |> 
  
  # 
  # # filter specific time points when temp was inferenced due to the entering of people, sampling or measuring plants
  # filter(
  #   !between(
  #     datetime,
  #     ymd_hm("2021-03-25 08:35"),
  #     ymd_hm("2021-03-25 09:50")
  #   ),
  #   !between(
  #     datetime,
#     ymd_hm("2021-03-27 07:50"),
#     ymd_hm("2021-03-27 08:05")
#   ),
#   !between(
#     datetime,
#     ymd_hm("2021-03-30 07:50"),
#     ymd_hm("2021-03-30 08:05")
#   ),
#   # filter specific date when tplant was abnomal
#   date != ymd("2021-03-28")
# ) |> 

plot_ly(x = ~datetime,
        y = ~t_air_mean,
        name = "t_air_A", type = "scatter",
        mode = "lines+markers"
) |>
add_trace(y = ~t_plant_mean, name = "t_plant_A")

select(datetime,
       date,
       time,
       t_air   = t_air_mean,
       t_plant = t_plant_mean,
       vpd_air,
       vpd_plant,) |>
  
  group_by(time) |>
  # summarise(across(where(is.numeric), ~ mean(.x ,na.rm = TRUE))) |>
  # summarise(across(t_air:t_plant, ~ mean(.x , na.rm = TRUE))) |>
  # summarize(across(t_air:t_plant, ~ sd(.x , na.rm = TRUE))) |>
  
  # calculate the mean and standard deviation value for tair and tplant
  summarise(across(t_air:vpd_plant, list(mean = mean, sd = sd), .names = "{.col}.{.fn}")) |>
  
  # temp plots
  ggplot(aes(x = time)) +
  # Adding shading to differentiate day and night in a day using ggplot geom_rect
  # the daytime is between 06:30 to 21:30
  geom_rect(
    aes(
      xmin = hms::as_hms("07:30:00"),
      xmax = hms::as_hms("22:00:00"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey",
    alpha = 0.01
  ) +
  geom_line(aes(y = t_air.mean, color = "Tair"), size = 1) +
  # add standard deviation
  geom_ribbon(aes(ymin = t_air.mean - t_air.sd , ymax = t_air.mean + t_air.sd),
              alpha = 0.2) +
  
  geom_line(aes(y = t_plant.mean, color = "Tplant"), size = 1) +
  geom_ribbon(aes(ymin = t_plant.mean - t_plant.sd , ymax = t_plant.mean + t_plant.sd),
              alpha = 0.1) +
  labs(title = "Temperature",
       x = "Time",
       y = "Temperature (°C)") +
  scale_color_manual(values = c("Tair" = "#fc8d62", "Tplant" = "#66c2a5")) +
  scale_x_time(breaks = "4 hour", labels = "%H:%M") +
  scale_y_continuous(breaks = seq(10, 40, by = 5)) +
  # facet_wrap( ~ stress_period,
  #             ncol = 1,
  #             nrow = 4,
  #             switch = "y") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    element_line(size = 1),
    text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    legend.position = c(0.1, 0.95),
    legend.background = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    axis.ticks = element_line(linewidth = 0.5)
  )


## Final plotting ----
# This is intended to plot only for meiosis period
# select variables of each dataframe
glimpse(meteo_B1_B2)
meteo_B1 |> 
  distinct(stress_period)


meteo_A_for_merge <- meteo_A |> 
  select(
    datetime,
    date,
    time,
    stress_period,
    t_air   = t_air_mean,
    t_plant = t_plant_mean,
    vpd_air,
    vpd_plant
  )

meteo_B1_B2_for_merge <- meteo_B1_B2 |> 
  select(
    datetime,
    date,
    time,
    stress_period,
    t_air,
    t_plant = t_plant_mean,
    vpd_air,
    vpd_plant
  )

# merge datatable A B1 and B2
meteo_merged_for_plot <- bind_rows(meteo_B1_B2_for_merge, meteo_A_for_merge)

meteo_merged_for_plot |> 
  distinct(stress_period)
### Temp plots ----

temp_plots_meiosis <- meteo_merged_for_plot |>
  # Only plotting the periods during meosis stage
  # distinct(stress_period)
  # filter(stress_period != "anthese_36" & stress_period != "anthese_32") |> 
  filter(!stress_period %in% c("anthese_36", "anthese_32")) |> 
  # also selection partial period for control treatment
  filter(!(stress_period == "no_stress" & (datetime <= ymd_hm("2021-03-25 07:30") | datetime >= ymd_hm("2021-03-31 07:00")))) |> 
  # # to chack how many samples per stress_period
  # group_by(stress_period) |>
  # summarise(n = n())
  
  # filter out some points for priming stress 
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # filter specific time points when temp was inferenced due to the entering of people, sampling or measuring plants
  filter(!(stress_period == "priming_34" & between(datetime, ymd_hm("2021-03-14 09:05"), ymd_hm("2021-03-14 09:50")))) |>
  filter(!(stress_period == "priming_34" & between(datetime, ymd_hm("2021-03-19 10:50"), ymd_hm("2021-03-19 11:35")))) |>

  # filter out some points for meiose 36 stress
  filter(!(stress_period == "meiose_36" & between(datetime, ymd_hm("2021-03-25 08:35"),ymd_hm("2021-03-25 09:50")))) |>
  filter(!(stress_period == "meiose_36" & between(datetime, ymd_hm("2021-03-27 07:50"), ymd_hm("2021-03-27 08:05")))) |>
  filter(!(stress_period == "meiose_36" & between(datetime, ymd_hm("2021-03-30 07:50"), ymd_hm("2021-03-30 08:05")))) |>
  # filter specific date for meiose 36 when tplant was obviously abnomal
  filter(!(stress_period == "meiose_36" & date == ymd("2021-03-28"))) |>

  # filter out some points for meiose 32 stress
  filter(!(stress_period == "meiose_32" & datetime == ymd_hm("2021-03-27 08:15"))) |>
  filter(!(stress_period == "meiose_32" & datetime == ymd_hm("2021-03-30 08:00"))) |>
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # change the time format to datetime format
  mutate(
    stress_period = fct_recode(
      stress_period,
      "control_24" = "no_stress",
      "meiosis_32" = "meiose_32",
      "meiosis_36" = "meiose_36"
    ),
    stress_period = fct_relevel(
      stress_period,
      levels = c("control_24", "priming_34", "meiosis_32", "meiosis_36")
    ),
    dtime = as.POSIXct(time)
  ) |> 
  group_by(stress_period, dtime) |> 
  summarise(across(t_air:t_plant, list(mean = mean, sd = sd), .names = "{.col}.{.fn}")) |> 
  
  ggplot(aes(x = dtime)) +
  # Adding shading to differentiate day and night in a day using ggplot geom_rect
  # the daytime is between 06:30 to 21:30
  geom_rect(
    aes(
      xmin = ymd_hms("1970-01-01 07:30:00"),
      xmax = ymd_hms("1970-01-01 22:00:00"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey",
    alpha = 0.01
  ) +
  # geom_rect(
  #   aes(
  #     xmin = hms::as_hms("07:30:00"),
  #     xmax = hms::as_hms("22:00:00"),
  #     ymin = -Inf,
  #     ymax = Inf
  #   ),
  #   fill = "grey",
  #   alpha = 0.01
  # ) +
  geom_line(aes(y = t_air.mean, color = "Tair"), size = 1) +
  # add standard deviation
  geom_ribbon(aes(ymin = t_air.mean - t_air.sd , ymax = t_air.mean + t_air.sd), fill = "#fc8d62", alpha = 0.5) +
  
  geom_line(aes(y = t_plant.mean, color = "Tplant"), size = 1) +
  geom_ribbon(aes(ymin = t_plant.mean - t_plant.sd , ymax = t_plant.mean + t_plant.sd), fill = "#66c2a5", alpha = 0.5) +
  labs(
    title = "Temperature",
    x = "Time of the Day",
    y = "Temperature (°C)"
  ) +
  scale_color_manual(values = c("Tair" = "#fc8d62", "Tplant" = "#66c2a5")) +
  scale_x_datetime(date_breaks = "4 hour", date_labels = "%H:%M") +
  # scale_x_time(breaks = "6 hour", labels = "%H:%M")) +
  scale_y_continuous(breaks = seq(10, 40, by = 5)) +
  facet_wrap( ~ stress_period,
              ncol = 1,
              nrow = 4,
              strip.position = "left") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    element_line(linewidth = 1),
    text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    legend.position = c(0.1, 0.95),
    legend.background = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    strip.placement = "outside",
    axis.ticks = element_line(linewidth = 0.5)
  )
### VPD plots ---- 

vpd_plots_meiosis <- meteo_merged_for_plot |>
  # Only plotting the periods during meosis stage
  # distinct(stress_period)
  # filter(stress_period != "anthese_36" & stress_period != "anthese_32") |> 
  filter(!stress_period %in% c("anthese_36", "anthese_32")) |> 
  # also selection partial period for control treatment
  filter(!(stress_period == "no_stress" & (datetime <= ymd_hm("2021-03-25 07:30") | datetime >= ymd_hm("2021-03-31 07:00")))) |> 
  # # to chack how many samples per stress_period
  # group_by(stress_period) |>
  # summarise(n = n())
  
  # filter out some points for priming stress 
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # filter specific time points when temp was inferenced due to the entering of people, sampling or measuring plants
  filter(!(stress_period == "priming_34" & between(datetime, ymd_hm("2021-03-14 09:05"), ymd_hm("2021-03-14 09:50")))) |>
  filter(!(stress_period == "priming_34" & between(datetime, ymd_hm("2021-03-19 10:50"), ymd_hm("2021-03-19 11:35")))) |>



  # filter out some points for meiose 36 stress
  filter(!(stress_period == "meiose_36" & between(datetime, ymd_hm("2021-03-25 08:35"),ymd_hm("2021-03-25 09:50")))) |>
  filter(!(stress_period == "meiose_36" & between(datetime, ymd_hm("2021-03-27 07:50"), ymd_hm("2021-03-27 08:05")))) |>
  filter(!(stress_period == "meiose_36" & between(datetime, ymd_hm("2021-03-30 07:50"), ymd_hm("2021-03-30 08:05")))) |>
  # filter specific date for meiose 36 when tplant was obviously abnomal
  filter(!(stress_period == "meiose_36" & date == ymd("2021-03-28"))) |>

  # filter out some points for meiose 32 stress
  filter(!(stress_period == "meiose_32" & datetime == ymd_hm("2021-03-27 08:15"))) |>
  filter(!(stress_period == "meiose_32" & datetime == ymd_hm("2021-03-30 08:00"))) |>
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # change the time format to datetime format
  mutate(
    stress_period = fct_recode(
      stress_period,
      "control_24" = "no_stress",
      "meiosis_32" = "meiose_32",
      "meiosis_36" = "meiose_36"
    ),
    
    stress_period = fct_relevel(
      stress_period,
      levels = c("control_24", "priming_34", "meiosis_32", "meiosis_36")
    ),
    dtime = as.POSIXct(time)
  ) |> 
  #calculate the mean, standard deviation
  group_by(stress_period, dtime) |> 
  summarise(across(vpd_air:vpd_plant, list(mean = mean, sd = sd), .names = "{.col}.{.fn}")) |> 
  
  ggplot(aes(x = dtime)) +
  # Adding shading to differentiate day and night in a day using ggplot geom_rect
  # the daytime is between 06:30 to 21:30
  geom_rect(
    aes(
      xmin = ymd_hms("1970-01-01 07:30:00"),
      xmax = ymd_hms("1970-01-01 22:00:00"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey",
    alpha = 0.01
  ) +
  geom_line(aes(y = vpd_air.mean, color = "VPDair"), size = 1) +
  # add standard deviation
  geom_ribbon(aes(ymin = vpd_air.mean - vpd_air.sd , ymax = vpd_air.mean + vpd_air.sd), fill = "#fc8d62", alpha = 0.5) +
  
  geom_line(aes(y = vpd_plant.mean, color = "VPDplant"), size = 1) +
  geom_ribbon(aes(ymin = vpd_plant.mean - vpd_plant.sd , ymax = vpd_plant.mean + vpd_plant.sd), fill = "#66c2a5", alpha = 0.5) +
  labs(
    title = "VPD",
    x = "Time of the Day",
    y = "Vapor Pressure Deficit (kPa)"
  ) +
  scale_color_manual(values = c("VPDair" = "#fc8d62", "VPDplant" = "#66c2a5")) +
  scale_x_datetime(date_breaks = "4 hour", date_labels = "%H:%M") +
  scale_y_continuous(breaks = seq(0, 2.0, by = 0.4)) +
  facet_wrap( ~ stress_period,
              ncol = 1,
              nrow = 4,
              strip.position = "left") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    element_line(size = 1),
    text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    legend.position = c(0.1, 0.95),
    legend.background = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    strip.placement = "outside",
    axis.ticks = element_line(linewidth = 0.5)
  )


meteo_all_plots_meiosis <- temp_plots_meiosis + vpd_plots_meiosis

# meteo_all_meiosis <- grid.arrange(temp_plots_meiosis, vpd_plots_meiosis, ncol = 2)

meteo_all_plots_meiosis


ggsave("plots/meteo_all_plots_meiosis_filtered.png", width = 12.5, height = 12 * 0.618)



  
# Other old plotting tries ---------------------------------------------------------------



#***********************************************************************
#________________________________________
col_names_A <- colnames(meteo_A)

col_names_B1_B2 == col_names_A
#________________________________________

meteo_all <- bind_rows(meteo_B1_B2, meteo_A)
meteo_all
# Temp and VPD within a day (average grouped by time)

meteo_by_time <- meteo_all |>
  select(scenario, stress_period, time:par) |>
  mutate(
    daynight = if_else(par < 20, "night", "day"),
    stress_period = fct_collapse(
      stress_period,
      "control_24" = "no_stress",
      "priming_34" = "priming_34",
      "stress_32"  = c("meiose_32", "anthese_32"),
      "stress_36"  = c("meiose_36", "anthese_36")
    ),
    .after = scenario,
    stress_period = fct_relevel(
      stress_period,
      levels = c("control_24", "priming_34", "stress_32", "stress_36")
    ),
    dtime = as.POSIXct(time)
  ) |>
  relocate(time, dtime, daynight)


# hist(meteo_by_time$par)
# view(meteo_by_time)
glimpse(meteo_all)




# Set start and end time for day and night
day_start <- ymd_hms("1970-01-01 06:30:00")
day_end <- ymd_hms("1970-01-01 21:45:00")

# Plotting for the whole experimental period
# when the thermocouple is attached in plant's stem
temp_plots_season <- meteo_by_time |>
  # group_by(stress_period, daynight, time) |>
  group_by(stress_period, dtime) |>
  # summarise(across(where(is.numeric), ~ mean(.x ,na.rm = TRUE)))
  summarise(across(t_air:t_plant, ~ mean(.x , na.rm = TRUE))) |>
  ggplot(aes(x = dtime)) +
  # Adding shading to differentiate day and night in a day using ggplot geom_rect
  # the daytime is between 06:30 to 21:30
  geom_rect(
    aes(
      xmin = day_start,
      xmax = day_end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "gray",
    alpha = 0.01
  ) +
  geom_line(aes(y = t_air, color = "Tair"), size = 1) +
  geom_line(aes(y = t_plant, color = "Tplant"), size = 1) +
  labs(
    title = "Temperature",
    x = "Time",
    y = "Temperature (°C)"
  ) +
  scale_color_manual(values = c("Tair" = "#fc8d62", "Tplant" = "#66c2a5")) +
  scale_x_datetime(date_breaks = "4 hour", date_labels = "%H:%M") +
  scale_y_continuous(breaks = seq(10, 40, by = 5)) +
  facet_wrap( ~ stress_period,
              ncol = 1,
              nrow = 4,
              switch = "y") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    element_line(size = 1),
    text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    legend.position = c(0.1, 0.95),
    legend.background = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    axis.ticks = element_line(linewidth = 0.5)
  )

# VPD plot
vpd_plots_season <- meteo_by_time |>
  # group_by(stress_period, daynight, time) |>
  group_by(stress_period, dtime) |>
  # summarise(across(where(is.numeric), ~ mean(.x ,na.rm = TRUE)))
  summarise(across(vpd_air:vpd_plant, ~ mean(.x , na.rm = TRUE))) |>
  ggplot(aes(x = dtime)) +
  geom_rect(
    aes(
      xmin = day_start,
      xmax = day_end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "gray",
    alpha = 0.01
  ) +
  geom_line(aes(y = vpd_air, color = "VPDair"), size = 1) +
  geom_line(aes(y = vpd_plant, color = "VPDplant"), size = 1) +
  labs(
    title = "VPD",
    x = "Time",
    y = "Vapor Pressure Deficit (kPa)"
  ) +
  scale_color_manual(values = c("VPDair" = "#fc8d62", "VPDplant" = "#66c2a5")) +
  scale_x_datetime(date_breaks = "4 hour", date_labels = "%H:%M") +
  scale_y_continuous(breaks = seq(0, 2.0, by = 0.4)) +
  facet_wrap(
    ~ stress_period,
    ncol = 1,
    nrow = 4,
    strip.position = "left"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    element_line(size = 1.5),
    text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.1, 0.95),
    legend.background = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text = element_text(size = 12),
    axis.ticks = element_line(linewidth = 0.5)
  )



# Plotting grouped by hour for the whole experimental period
# when the thermocouple is attached in plant's stem


# Temp plot
temp_plots <- meteo_by_time |>
  # group_by(stress_period, daynight, time) |>
  group_by(stress_period, dtime) |>
  # summarise(across(where(is.numeric), ~ mean(.x ,na.rm = TRUE)))
  summarise(across(t_air:t_plant, ~ mean(.x , na.rm = TRUE))) |>
  ggplot(aes(x = dtime)) +
  # Adding shading to differentiate day and night in a day using ggplot geom_rect
  # the daytime is between 06:30 to 21:30
  geom_rect(
    aes(
      xmin = day_start,
      xmax = day_end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "gray",
    alpha = 0.01
  ) +
  geom_line(aes(y = t_air, color = "Tair"), size = 1) +
  geom_line(aes(y = t_plant, color = "Tplant"), size = 1) +
  labs(
    title = "Temperature",
    x = "Time",
    y = "Temperature (°C)"
  ) +
  scale_color_manual(values = c("Tair" = "#fc8d62", "Tplant" = "#66c2a5")) +
  scale_x_datetime(date_breaks = "4 hour", date_labels = "%H:%M") +
  scale_y_continuous(breaks = seq(10, 40, by = 5)) +
  facet_wrap( ~ stress_period,
              ncol = 1,
              nrow = 4,
              switch = "y") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    element_line(size = 1),
    text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    legend.position = c(0.1, 0.95),
    legend.background = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    axis.ticks = element_line(linewidth = 0.5)
  )

# VPD plot
vpd_plots <- meteo_by_time |>
  # group_by(stress_period, daynight, time) |>
  group_by(stress_period, dtime) |>
  # summarise(across(where(is.numeric), ~ mean(.x ,na.rm = TRUE)))
  summarise(across(vpd_air:vpd_plant, ~ mean(.x , na.rm = TRUE))) |>
  ggplot(aes(x = dtime)) +
  geom_rect(
    aes(
      xmin = day_start,
      xmax = day_end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "gray",
    alpha = 0.01
  ) +
  geom_line(aes(y = vpd_air, color = "VPDair"), size = 1) +
  geom_line(aes(y = vpd_plant, color = "VPDplant"), size = 1) +
  labs(
    title = "VPD",
    x = "Time",
    y = "Vapor Pressure Deficit (kPa)"
  ) +
  scale_color_manual(values = c("VPDair" = "#fc8d62", "VPDplant" = "#66c2a5")) +
  scale_x_datetime(date_breaks = "4 hour", date_labels = "%H:%M") +
  scale_y_continuous(breaks = seq(0, 2.0, by = 0.4)) +
  facet_wrap(
    ~ stress_period,
    ncol = 1,
    nrow = 4,
    strip.position = "left"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    element_line(size = 1.5),
    text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.1, 0.95),
    legend.background = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text = element_text(size = 12),
    axis.ticks = element_line(linewidth = 0.5)
  )

# Interactive Charts 

ggplotly(temp_plots)

ggplotly(vpd_plots)

vpd_plots



meteo_plots <- grid.arrange(temp_plots, vpd_plots, ncol = 2)
meteo_plots

save.image()

#****************************************************************************
























# geom_point(aes(y = Desired_T),size = 0.8) +
geom_line(aes(y = Tair, color = "Tair"), size = 1) +
  geom_line(aes(y = Tplant, color = "Tplant"), size = 1) +
  labs(
    title = "Temperature",
    x = "Time",
    y = "Temperature (°C)",
    colour = ""
  ) +
  scale_color_manual(values = c(
    "Desired_T" = "#999999",
    "Tair" = "#E69F00",
    "Tplant" = "#56B4E9"
  )) +
  # scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  # scale_x_date(date_labels = "%Y (%b)") +
  facet_wrap(~ Treats, ncol = 1, nrow = 4) +
  
  theme(
    plot.title = element_text(hjust = 0.5),
    element_line(size = 1),
    text = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12)
  )











# ordered(c("control_24", "priming_34", "stress_32", "stress_34")) |>
# count(stress_period)

# group_by(stress_period, time) |>
# filter(stress_period == "stress_36") |>
# group_by(time) |>

# summarise(n = n())
# summarise(across(where(is.numeric), ~ mean(.x ,na.rm = TRUE)))

meteo_by_time
# summarise(across(everything(), mean))





meteo_by_time |>
  summarise(across(where(is.numeric), ~ mean(.x , na.rm = TRUE)))

meteo_by_time |>
  distinct(stress_period)




view(meteo_by_time)



view(meteo_all)





# meteo_temp_by_time <- meteo_by_time |>
#
#
# meteo_vpd_by_time
