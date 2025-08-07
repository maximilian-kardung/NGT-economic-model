library(ggplot2)
library(tidyverse)
library(dplyr)
library(vtable)


wdir <-
  "C:/Users/kardu001/OneDrive - Wageningen University & Research/Economic Impact of NGTs"

setwd(paste0(wdir))

set.seed(1234)

n <- 10000  # Number of random samples

#### Research phase & Other variables (same for all options) ####

R <- runif(n, min = 3, max = 5)
r <- runif(n, min = 0.2, max = 0.6)

h1_min <- 1 / 6.5
h1_max <- 1 / 4.5
h1 <- runif(n, min = h1_min, max = h1_max)

mu <- 0.105
q <- 0.5

#### Option 1: Status Quo - Authorization after Data request ####

a1 <- runif(n, min = 0.05, max = 0.1)
A1 <- runif(n, min = 4.5, max = 18.5)
a2 <- runif(n, min = 0.05, max = 0.1)
A2 <- runif(n, min = 0.5, max = 3)
a3 <- runif(n, min = 0.05, max = 0.1)
A3 <- runif(n, min = 0.2, max = 0.4)
a4 <- runif(n, min = 0.05, max = 0.1)
A4 <- runif(n, min = 0.2, max = 0.4)

theta <- runif(n, min = 0.1, max = 0.2)

h2_min <- 1 / 1.5
h2_max <- 1 / 0.5
h2 <- runif(n, min = h2_min, max = h2_max)
h3_min <- 1 / 1.5
h3_max <- 1 / 0.001
h3 <- runif(n, min = h3_min, max = h3_max)
h4_min <- 1 / 1.5
h4_max <- 1 / 0.5
h4 <- runif(n, min = h4_min, max = h4_max)
h5_min <- 1 / 1.5
h5_max <- 1 / 0.5
h5 <- runif(n, min = h5_min, max = h5_max)
h_star_min <- 1 / 10
h_star_max <- 1 / 8
h_star <- runif(n, min = h_star_min, max = h_star_max)

p3 <- 0
p4 <- 0
p5 <- 0
p6 <- 0


#### Hurdle rate ####

#### Calculating hurdle rate ####


E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1) / ((mu + h1) * (mu + h2))

E3_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3)) -
  theta * h1 * h2 * h3 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h_star))

E4_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4))

E5_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  a4 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5))

E6_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  a4 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))

E3_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
                (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3)) -
                theta * h1 * h2 * h3 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h_star))) * exp(-mu)

E6_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
                (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                a4 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
                theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))) * exp(-mu)


V1 <- (1 - q * exp(-mu)) * p3 * E2_V0 +
  (1 - q * exp(mu)) * (1 - p3) * (1 - p4) * p5 * E4_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * p6 * E5_V0 +
  (1 - p3) * p4 * E3_V0 -
  (1 - p3) * p4 * E3_Vp +
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * E6_V0 -
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * E6_Vp

C3 <- h1 * h2 * h3 / ((mu + h1) * (mu + h2) * (mu + h3))

C6 <- h1 * h2 * h3 * h4 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5))


HR <- -V1 / ((1 - p3) * p4 * C3 + (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * C6)



O1_HR_2 <- data.frame(HR)

O1_HR_2_bw <- 2 * IQR(O1_HR_2$HR) / length(O1_HR_2$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O1_HR_2 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O1_HR_2_stats

ggplot(O1_HR_2, aes(x = HR)) +
  geom_histogram(binwidth = O1_HR_2_bw) +
  theme_minimal() +
  geom_vline(data = O1_HR_2_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 1: Status Quo") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O1_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O1_HR_2)


#### Calculating hurde rate for changing mu for Option 1 ####

# Define the range of p3 values

mu_values <- seq(from = 0.01, to = 0.2, by = 0.01)


# Initialize a list to store the results
O1_HR_2_mu <- list()

# Initialize a counter
counter <- 1

# Loop over the mu values

for (i in 1:length(mu_values)) {
  mu <- mu_values[i]
  
  #### Hurdle rate ####
  
  #### Calculating hurdle rate ####
  
  
  E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1) / ((mu + h1) * (mu + h2))
  
  E3_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
    (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
    a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3)) -
    theta * h1 * h2 * h3 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h_star))
  
  E4_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
    (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
    (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
    a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4))
  
  E5_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
    (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
    (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
    (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
    a4 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5))
  
  E6_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
    (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
    (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
    (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
    a4 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
    theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))
  
  E3_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
                  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                  a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3)) -
                  theta * h1 * h2 * h3 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h_star))) * exp(-mu)
  
  E6_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
                  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                  a4 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
                  theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))) * exp(-mu)
  
  
  V1 <- (1 - q * exp(-mu)) * p3 * E2_V0 +
    (1 - q * exp(mu)) * (1 - p3) * (1 - p4) * p5 * E4_V0 +
    (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * p6 * E5_V0 +
    (1 - p3) * p4 * E3_V0 -
    (1 - p3) * p4 * E3_Vp +
    (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * E6_V0 -
    (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * E6_Vp
  
  C3 <- h1 * h2 * h3 / ((mu + h1) * (mu + h2) * (mu + h3))
  
  C6 <- h1 * h2 * h3 * h4 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5))
  
  
  HR <- -V1 / ((1 - p3) * p4 * C3 + (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * C6)
  
  # Store the result
  O1_HR_2_mu[[counter]] <- list("mu" = mu,
                              "HR" = HR)
  counter <- counter + 1
}

# Convert the results to a data frame for easier viewing
O1_HR_2_mu_df <- do.call(rbind, lapply(O1_HR_2_mu, as.data.frame))

O1_HR_2_mu_df$mu <- as.factor(O1_HR_2_mu_df$mu)


# Calculate the mean of HR for each value of p6
O1_HR_2_mu_df_mean <- aggregate(HR ~ mu, data = O1_HR_2_mu_df, FUN = mean)

# Print the results
print(O1_HR_2_mu_df_mean)

O1_HR_2_mu_df_mean$HR <- round(O1_HR_2_mu_df_mean$HR, 2)


ggplot(O1_HR_2_mu_df_mean, aes(x = mu, y = HR, group = 1)) + 
  geom_step(direction = "mid",
            size = 1) + 
  geom_text(aes(x = mu, y = HR, label = HR), vjust= -1) +
  theme_minimal(base_size = 14) +
  scale_x_discrete(breaks = seq(0.0, 0.2, by = 0.02)) +
  xlab("\nDiscount rate") +
  ylab("Hurdle value\n")

ggsave('mu_sens.tiff', width=10, height=6)