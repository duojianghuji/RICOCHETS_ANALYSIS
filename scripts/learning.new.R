library(ggplot2)
library(tidyverse)
# Load the CO2 dataset
data(CO2)
CO2
# Define the exponential function as the model
exp_model <- function(x, a, b, c) {
  a * exp(-b * x) + c
}

filtered_CO2 <- CO2 |> 
  filter(Treatment == "nonchilled", Plant == "Qn3")
summary(filtered_CO2)

# Estimate the model parameters using nls
fit <- nls(conc ~ exp_model(uptake, a, b, c), data = filtered_CO2, 
           start = list(a = 3, b = 0.1, c = 2))

# Print the model summary
summary(fit)

# Plot the data and model
ggplot(aes(x = uptake, y = conc)) +
  geom_point() +
  geom_line(aes(y = predict(fit)))








# Print the model summary
summary(fit_ss)

# Plot the data and model
ggplot(aes(x = uptake, y = conc)) +
  geom_point() +
  geom_line(aes(y = predict(fit_ss)))

























filtered_CO2 <- CO2 |> 
  filter(Treatment == "nonchilled", Plant == "Qn3")
summary(filtered_CO2)

# Estimate the model parameters using nls
fit <- nls(conc ~ exp_model(uptake, a, b, c), data = filtered_CO2, 
           start = list(a = 3, b = 0.1, c = 2), algorithm = "port")

# Print the model summary
summary(fit)

# Plot the data and model
str(CO2)
CO2 |> 
  filter(Treatment == "nonchilled", Plant == "Qn3") |> 
  ggplot(aes(x = uptake, y = conc)) +
  geom_point()
geom_line(aes(y = predict(fit)))
##########################



t <-  1:100

y1 <-  22 + (53 - 22) * exp(-0.02 * t) %>% jitter(10)

y2 <-  24 + (60 - 24) * exp(-0.01 * t) %>% jitter(10)


df <- tibble(t = t, y = y1, sensor = 'sensor1') %>% 
  rbind(. , data.frame(t = t, y = y2, sensor = 'sensor2'))
df 

qplot(t, y, data = df, colour = sensor)


sensor1 <- df %>% filter(sensor == 'sensor1')

sensor1

nls(conc ~ yf + (y0 - yf) * exp(-alpha * uptake), 
    data = filtered_CO2,
    start = list(y0 = 54, yf = 25, alpha = 1))

fit <- nls(conc ~ SSasymp(uptake, yf, y0, log_alpha), data = filtered_CO2)
fit









##########################################
require(ggplot2)
library(nlraa)
set.seed(1234)
x <- 1:15
y <- expf(x, 10, -0.3) + rnorm(15, 0, 0.2)
dat <- data.frame(x = x, y = y)
fit <- nls(conc ~ SSexpf(uptake, a, c), data = filtered_CO2)
## plot
ggplot(data = filtered_CO2, aes(x = uptake, y = conc)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fit)))

###########################################################



