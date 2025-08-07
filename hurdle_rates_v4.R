library(ggplot2)
library(tidyverse)
library(dplyr)
library(vtable)

wdir <-
  "C:/Users/kardu001/OneDrive - Wageningen University & Research/Economic Impact of NGTs"

setwd(paste0(wdir))

set.seed(1234)

n <- 10000 # Number of random samples

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
p4 <- 1
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



O1_HR_1 <- data.frame(HR)

O1_HR_1_bw <- 2 * IQR(O1_HR_1$HR) / length(O1_HR_1$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O1_HR_1 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O1_HR_1_stats

ggplot(O1_HR_1, aes(x = HR)) +
  geom_histogram(binwidth = O1_HR_1_bw) +
  theme_minimal() +
  geom_vline(data = O1_HR_1_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 1: Status Quo") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O1_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O1_HR_1)

#### Option 1: Status Quo - Authorization after Re-evaluation ####

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


#### Option 3: Risk profiles - 0 & 1 ####

a1 <- runif(n, min = 0.05, max = 0.1)
A1 <- runif(n, min = 0.0026, max = 0.0098)
a2 <- runif(n, min = 0.05, max = 0.1)
A2 <- runif(n, min = 0.007, max = 0.0252)
a3 <- runif(n, min = 0.05, max = 0.1)
A3 <- runif(n, min = 2.314, max = 16.022)

theta <- runif(n, min = 0.25, max = 0.35)


h2_min <- 1 / 0.5
h2_max <- 1 / 0.33
h2 <- runif(n, min = h2_min, max = h2_max)
h3_min <- 1 / 1.5
h3_max <- 1 / 0.5
h3 <- runif(n, min = h3_min, max = h3_max)
h4_min <- 1 / 1.5
h4_max <- 1 / 0.5
h4 <- runif(n, min = h4_min, max = h4_max)
h_star_min <- 1 / 10
h_star_max <- 1 / 8
h_star <- runif(n, min = h_star_min, max = h_star_max)

p3 <- 1
p4 <- 0
p5 <- 0


#### Hurdle rate ####

#### Calculating hurdle rate ####


E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  a1 * h1 / ((mu + h1) * (mu + h2)) -
  theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))

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
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  theta * h1 * h2 * h3 * h4 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h_star))

E2_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
                a1 * h1 / ((mu + h1) * (mu + h2)) -
                theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))) * exp(-mu)


E3_Vp <- q *(-R - (r + A1 * h1) / (mu + h1) - 
  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3)) -
  theta * h1 * h2 * h3 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h_star)))*exp(-mu)

E5_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
                (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                theta * h1 * h2 * h3 * h4 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h_star))) * exp(-mu)



V2 <- ((1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * p5 * E4_V0 +
         (1 - p3) * (1 - p4) * (1 - p5) * E5_V0 -
         (1 - p3) * (1 - p4) * (1 - p5) * E5_Vp +
         (1 - p3) * p4 * E3_V0 -
         (1 - p3) * p4 * E3_Vp +
         p3 * E2_V0 -
         p3 * E2_Vp)

C2 <- h1 * h2 / ((mu + h1) * (mu + h2))
C3 <- h1 * h2 * h3 / ((mu + h1) * (mu + h2) * (mu + h3))
C5 <- h1 * h2 * h3 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4))


HR <- -V2 / (p3 * C2 + (1 - p3) * p4 * C3 + (1 - p3) * (1 - p4) * (1 - p5) * C5)

O3_HR_1 <- data.frame(HR)

O3_HR_1_bw <- 2 * IQR(O3_HR_1$HR) / length(O3_HR_1$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O3_HR_1 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O3_HR_1_stats

ggplot(O3_HR_1, aes(x = HR)) +
  geom_histogram(binwidth = O3_HR_1_bw) +
  theme_minimal() +
  geom_vline(data = O3_HR_1_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 3: Risk Profiles") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O3_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O3_HR_1)


#### Option 3: Risk profiles - 0 & 1 as safe as ####

a1 <- runif(n, min = 0.05, max = 0.1)
A1 <- runif(n, min = 0.0026, max = 0.0098)
a2 <- runif(n, min = 0.05, max = 0.1)
A2 <- runif(n, min = 0.007, max = 0.0252)
a3 <- runif(n, min = 0.05, max = 0.1)
A3 <- runif(n, min = 2.314, max = 16.022)

theta <- runif(n, min = 0.25, max = 0.35)


h2_min <- 1 / 0.5
h2_max <- 1 / 0.33
h2 <- runif(n, min = h2_min, max = h2_max)
h3_min <- 1 / 1.5
h3_max <- 1 / 0.5
h3 <- runif(n, min = h3_min, max = h3_max)
h4_min <- 1 / 1.5
h4_max <- 1 / 0.5
h4 <- runif(n, min = h4_min, max = h4_max)
h_star_min <- 1 / 10
h_star_max <- 1 / 8
h_star <- runif(n, min = h_star_min, max = h_star_max)

p3 <- 0
p4 <- 1
p5 <- 0


#### Hurdle rate ####

#### Calculating hurdle rate ####


E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  a1 * h1 / ((mu + h1) * (mu + h2)) -
  theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))

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
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  theta * h1 * h2 * h3 * h4 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h_star))

E2_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
                a1 * h1 / ((mu + h1) * (mu + h2)) -
                theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))) * exp(-mu)


E3_Vp <- q *(-R - (r + A1 * h1) / (mu + h1) - 
               (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
               a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3)) -
               theta * h1 * h2 * h3 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h_star)))*exp(-mu)

E5_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
                (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                theta * h1 * h2 * h3 * h4 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h_star))) * exp(-mu)



V2 <- ((1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * p5 * E4_V0 +
         (1 - p3) * (1 - p4) * (1 - p5) * E5_V0 -
         (1 - p3) * (1 - p4) * (1 - p5) * E5_Vp +
         (1 - p3) * p4 * E3_V0 -
         (1 - p3) * p4 * E3_Vp +
         p3 * E2_V0 -
         p3 * E2_Vp)

C2 <- h1 * h2 / ((mu + h1) * (mu + h2))
C3 <- h1 * h2 * h3 / ((mu + h1) * (mu + h2) * (mu + h3))
C5 <- h1 * h2 * h3 * h4 / ((mu + h1) * (mu + h2) * (mu + h3)* (mu + h4))


HR <- -V2 / (p3 * C2 + (1 - p3) * p4 * C3 + (1 - p3) * (1 - p4) * (1 - p5) * C5)


O3_HR_2 <- data.frame(HR)

O3_HR_2_bw <- 2 * IQR(O3_HR_2$HR) / length(O3_HR_2$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O3_HR_2 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O3_HR_2_stats

ggplot(O3_HR_2, aes(x = HR)) +
  geom_histogram(binwidth = O3_HR_2_bw) +
  theme_minimal() +
  geom_vline(data = O3_HR_2_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 3: Risk Profiles") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O3_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O3_HR_2)


#### Option 3: Risk profiles - Not as safe as ####

a1 <- runif(n, min = 0.05, max = 0.1)
A1 <- runif(n, min = 0.0026, max = 0.0098)
a2 <- runif(n, min = 0.05, max = 0.1)
A2 <- runif(n, min = 0.007, max = 0.0252)
a3 <- runif(n, min = 0.05, max = 0.1)
A3 <- runif(n, min = 2.314, max = 16.022)

theta <- runif(n, min = 0.25, max = 0.35)


h2_min <- 1 / 0.5
h2_max <- 1 / 0.33
h2 <- runif(n, min = h2_min, max = h2_max)
h3_min <- 1 / 1.5
h3_max <- 1 / 0.5
h3 <- runif(n, min = h3_min, max = h3_max)
h4_min <- 1 / 1.5
h4_max <- 1 / 0.5
h4 <- runif(n, min = h4_min, max = h4_max)
h_star_min <- 1 / 10
h_star_max <- 1 / 8
h_star <- runif(n, min = h_star_min, max = h_star_max)

p3 <- 0
p4 <- 0
p5 <- 0


#### Hurdle rate ####

#### Calculating hurdle rate ####


E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  a1 * h1 / ((mu + h1) * (mu + h2)) 
  theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))

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
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  theta * h1 * h2 * h3 * h4 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h_star))

E2_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
                a1 * h1 / ((mu + h1) * (mu + h2)) -
                theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))) * exp(-mu)


E3_Vp <- q *(-R - (r + A1 * h1) / (mu + h1) - 
               (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
               a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3)) -
               theta * h1 * h2 * h3 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h_star)))*exp(-mu)

E5_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
                (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                theta * h1 * h2 * h3 * h4 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h_star))) * exp(-mu)



V2 <- ((1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * p5 * E4_V0 +
         (1 - p3) * (1 - p4) * (1 - p5) * E5_V0 -
         (1 - p3) * (1 - p4) * (1 - p5) * E5_Vp +
         (1 - p3) * p4 * E3_V0 -
         (1 - p3) * p4 * E3_Vp +
         p3 * E2_V0 -
         p3 * E2_Vp)

C2 <- h1 * h2 / ((mu + h1) * (mu + h2))
C3 <- h1 * h2 * h3 / ((mu + h1) * (mu + h2) * (mu + h3))
C5 <- h1 * h2 * h3 * h4 / ((mu + h1) * (mu + h2) * (mu + h3)* (mu + h4))


HR <- -V2 / (p3 * C2 + (1 - p3) * p4 * C3 + (1 - p3) * (1 - p4) * (1 - p5) * C5)

O3_HR_3 <- data.frame(HR)

O3_HR_3_bw <- 2 * IQR(O3_HR_3$HR) / length(O3_HR_3$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O3_HR_3 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O3_HR_3_stats

ggplot(O3_HR_3, aes(x = HR)) +
  geom_histogram(binwidth = O3_HR_3_bw) +
  theme_minimal() +
  geom_vline(data = O3_HR_3_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 3: Risk Profiles") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O3_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O3_HR_3)

#### Option 4: Novel trait - Novel trait and enough data or not novel trait ####

a1 <- runif(n, min = 0.05, max = 0.1)
A1 <- runif(n, min = 0.0048, max = 0.0173)
a2 <- runif(n, min = 0.05, max = 0.1)
A2 <- runif(n, min = 1.827, max = 8.520)
a3 <- runif(n, min = 0.05, max = 0.1)
A3 <- runif(n, min = 0.2, max = 0.4)
a4 <- runif(n, min = 0.05, max = 0.1)
A4 <- runif(n, min = 0.2, max = 0.4)

theta <- runif(n, min = 0.25, max = 0.35)


h2_min <- 1 / 0.5
h2_max <- 1 / 0.33
h2 <- runif(n, min = h2_min, max = h2_max)
h3_min <- 1 / 1.5
h3_max <- 1 / 0.5
h3 <- runif(n, min = h3_min, max = h3_max)
h4_min <- 1 / 1.5
h4_max <- 1 / 0.5
h4 <- runif(n, min = h4_min, max = h4_max)
h_star_min <- 1 / 10
h_star_max <- 1 / 8
h_star <- runif(n, min = h_star_min, max = h_star_max)

p3 <- 1
p4 <- 0
p5 <- 0


E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  a1 * h1 / ((mu + h1) * (mu + h2)) -
  theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))

E3_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3))

E4_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4))

E5_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  theta * h1 * h2 * h3 * h4 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h_star))

E2_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
               a1 * h1 / ((mu + h1) * (mu + h2)) -
               theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))) * exp(-mu)

E3_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                  a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3))) -
     theta * h1 * h2 * h3 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h_star)) * exp(-mu)

E5_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
               (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
               (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
               a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
               theta * h1 * h2 * h3 * h4 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h_star))) * exp(-mu)

C2 <- h1 * h2 / ((mu + h1) * (mu + h2))

C5 <- h1 * h2 * h3 * h4 / ((mu + h1) * (mu + h2) * (mu + h3)* (mu + h4))

V3 <- (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * p5 * E4_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * p4 * E3_V0 +
  p3 * E2_V0 - p3 * E2_Vp +
  (1 - p3) * (1 - p4) * (1 - p5) * E5_V0 -
  (1 - p3) * (1 - p4) * (1 - p5) * E5_Vp


HR <- -V3 / (p3 * C2 + (1 - p3) * (1 - p4) * (1  - p5) * C5)


O4_HR_1 <- data.frame(HR)

O4_HR_1_bw <- 2 * IQR(O4_HR_1$HR) / length(O4_HR_1$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O4_HR_1 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O4_HR_1_stats

ggplot(O4_HR_1, aes(x = HR)) +
  geom_histogram(binwidth = O4_HR_1_bw) +
  theme_minimal() +
  geom_vline(data = O4_HR_1_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 4: Novel Trait") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O4_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O4_HR_1)


#### Option 4: Novel trait - Novel trait and acceptable risk ####

a1 <- runif(n, min = 0.05, max = 0.1)
A1 <- runif(n, min = 0.0048, max = 0.0173)
a2 <- runif(n, min = 0.05, max = 0.1)
A2 <- runif(n, min = 1.827, max = 8.520)
a3 <- runif(n, min = 0.05, max = 0.1)
A3 <- runif(n, min = 0.2, max = 0.4)
a4 <- runif(n, min = 0.05, max = 0.1)
A4 <- runif(n, min = 0.2, max = 0.4)

theta <- runif(n, min = 0.25, max = 0.35)


h2_min <- 1 / 0.5
h2_max <- 1 / 0.33
h2 <- runif(n, min = h2_min, max = h2_max)
h3_min <- 1 / 1.5
h3_max <- 1 / 0.5
h3 <- runif(n, min = h3_min, max = h3_max)
h4_min <- 1 / 1.5
h4_max <- 1 / 0.5
h4 <- runif(n, min = h4_min, max = h4_max)
h_star_min <- 1 / 10
h_star_max <- 1 / 8
h_star <- runif(n, min = h_star_min, max = h_star_max)

p3 <- 0
p4 <- 0
p5 <- 0


E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  a1 * h1 / ((mu + h1) * (mu + h2)) -
  theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))

E3_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3))

E4_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4))

E5_V0 <- -R - (r + A1 * h1) / (mu + h1) - 
  (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  theta * h1 * h2 * h3 * h4 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h_star))

E2_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
                a1 * h1 / ((mu + h1) * (mu + h2)) -
                theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))) * exp(-mu)

E3_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3))) -
  theta * h1 * h2 * h3 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h_star)) * exp(-mu)

E5_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - 
                (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                theta * h1 * h2 * h3 * h4 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h_star))) * exp(-mu)

C2 <- h1 * h2 / ((mu + h1) * (mu + h2))

C5 <- h1 * h2 * h3 * h4 / ((mu + h1) * (mu + h2) * (mu + h3)* (mu + h4))

V3 <- (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * p5 * E4_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * p4 * E3_V0 +
  p3 * E2_V0 - p3 * E2_Vp +
  (1 - p3) * (1 - p4) * (1 - p5) * E5_V0 -
  (1 - p3) * (1 - p4) * (1 - p5) * E5_Vp

HR <- -V3 / (p3 * C2 + (1 - p3) * (1 - p4) * (1  - p5) * C5)


O4_HR_2 <- data.frame(HR)

O4_HR_2_bw <- 2 * IQR(O4_HR_2$HR) / length(O4_HR_2$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O4_HR_2 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O4_HR_2_stats

ggplot(O4_HR_2, aes(x = HR)) +
  geom_histogram(binwidth = O4_HR_2_bw) +
  theme_minimal() +
  geom_vline(data = O4_HR_2_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 4: Novel Trait") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O4_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O4_HR_2)

#### Option 5: Foreign DNA - No Foreign DNA ####

a1 <- runif(n, min = 0.05, max = 0.1)
A1 <- runif(n, min = 0.0014, max = 0.0061)
a2 <- runif(n, min = 0.05, max = 0.1)
A2 <- runif(n, min = 4.5, max = 18.5)
a3 <- runif(n, min = 0.05, max = 0.1)
A3 <- runif(n, min = 0.5, max = 3)
a4 <- runif(n, min = 0.05, max = 0.1)
A4 <- runif(n, min = 0.2, max = 0.4)
a5 <- runif(n, min = 0.05, max = 0.1)
A5 <- runif(n, min = 0.2, max = 0.4)

theta <- runif(n, min = 0.4, max = 0.5)

h2_min <- 1 / 0.33
h2_max <- 1 / 0.16
h2 <- runif(n, min = h2_min, max = h2_max)
h3_min <- 1 / 1.5
h3_max <- 1 / 0.5
h3 <- runif(n, min = h3_min, max = h3_max)
h4_min <- 1 / 1.5
h4_max <- 1 / 0.001
h4 <- runif(n, min = h4_min, max = h4_max)
h5_min <- 1 / 1.5
h5_max <- 1 / 0.5
h5 <- runif(n, min = h5_min, max = h5_max)
h6_min <- 1 / 1.5
h6_max <- 1 / 0.5
h6 <- runif(n, min = h6_min, max = h6_max)
h_star_min <- 1 / 10
h_star_max <- 1 / 8
h_star <- runif(n, min = h_star_min, max = h_star_max)

p3 <- 1
p4 <- 0
p5 <- 0
p6 <- 0
p7 <- 0


E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - a1 * h1 / ((mu + h1) * (mu + h2)) -
  theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))

E3_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3))

E4_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))

E5_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  a4 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5))

E6_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6))

E7_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6)) -
  theta * h1 * h2 * h3 * h4 * h5 * h6 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6) * (mu + h_star))

E2_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - a1 * h1 / ((mu + h1) * (mu + h2)) -
                theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))) * exp(-mu)

E4_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))) * exp(-mu)

E7_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
                a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6)) -
                theta * h1 * h2 * h3 * h4 * h5 * h6 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6) * (mu + h_star))) * exp(-mu)

C2 <- h1 * h2 / ((mu + h1) * (mu + h2))

C4 <- h1 * h2 * h3 * h4 / ((mu + h1) * (mu + h2) * (mu + h3)* (mu + h4))

C7 <- h1 * h2 * h3 * h4 * h5 * h6 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6))


V4 <- (1 - q * exp(-mu)) * (1 - p3) * p4 * E3_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * p6 * E5_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * p7 * E6_V0 +
  (1 - p3) * (1 - p4) * p5 * E4_V0 - (1 - p3) * (1 - p4) * p5 * E4_Vp +
  p3 * E2_V0 - p3 * E2_Vp +
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * E7_V0 -
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * E7_Vp


HR <- -V4 / (p3 * C2 + (1 - p3) * (1 - p4) * p5 * C4 + (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * C7)


O5_HR_1 <- data.frame(HR)

O5_HR_1_bw <- 2 * IQR(O5_HR_1$HR) / length(O5_HR_1$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O5_HR_1 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O5_HR_1_stats

ggplot(O5_HR_1, aes(x = HR)) +
  geom_histogram(binwidth = O5_HR_1_bw) +
  theme_minimal() +
  geom_vline(data = O5_HR_1_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 5: Foreign DNA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O5_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O5_HR_1)


#### Option 5: Foreign DNA - Foreign DNA Authorization after Data Request ####

a1 <- runif(n, min = 0.05, max = 0.1)
A1 <- runif(n, min = 0.0014, max = 0.0061)
a2 <- runif(n, min = 0.05, max = 0.1)
A2 <- runif(n, min = 4.5, max = 18.5)
a3 <- runif(n, min = 0.05, max = 0.1)
A3 <- runif(n, min = 0.5, max = 3)
a4 <- runif(n, min = 0.05, max = 0.1)
A4 <- runif(n, min = 0.2, max = 0.4)
a5 <- runif(n, min = 0.05, max = 0.1)
A5 <- runif(n, min = 0.2, max = 0.4)

theta <- runif(n, min = 0.4, max = 0.5)


h2_min <- 1 / 0.33
h2_max <- 1 / 0.16
h2 <- runif(n, min = h2_min, max = h2_max)
h3_min <- 1 / 1.5
h3_max <- 1 / 0.5
h3 <- runif(n, min = h3_min, max = h3_max)
h4_min <- 1 / 1.5
h4_max <- 1 / 0.001
h4 <- runif(n, min = h4_min, max = h4_max)
h5_min <- 1 / 1.5
h5_max <- 1 / 0.5
h5 <- runif(n, min = h5_min, max = h5_max)
h6_min <- 1 / 1.5
h6_max <- 1 / 0.5
h6 <- runif(n, min = h6_min, max = h6_max)
h_star_min <- 1 / 10
h_star_max <- 1 / 8
h_star <- runif(n, min = h_star_min, max = h_star_max)

p3 <- 0
p4 <- 0
p5 <- 1
p6 <- 0
p7 <- 0


E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - a1 * h1 / ((mu + h1) * (mu + h2)) -
  theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))

E3_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3))

E4_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))

E5_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  a4 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5))

E6_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6))

E7_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6)) -
  theta * h1 * h2 * h3 * h4 * h5 * h6 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6) * (mu + h_star))

E2_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - a1 * h1 / ((mu + h1) * (mu + h2)) -
                theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))) * exp(-mu)

E4_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))) * exp(-mu)

E7_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
                a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6)) -
                theta * h1 * h2 * h3 * h4 * h5 * h6 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6) * (mu + h_star))) * exp(-mu)

C2 <- h1 * h2 / ((mu + h1) * (mu + h2))

C4 <- h1 * h2 * h3 * h4 / ((mu + h1) * (mu + h2) * (mu + h3)* (mu + h4))

C7 <- h1 * h2 * h3 * h4 * h5 * h6 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6))


V4 <- (1 - q * exp(-mu)) * (1 - p3) * p4 * E3_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * p6 * E5_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * p7 * E6_V0 +
  (1 - p3) * (1 - p4) * p5 * E4_V0 - (1 - p3) * (1 - p4) * p5 * E4_Vp +
  p3 * E2_V0 - p3 * E2_Vp +
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * E7_V0 -
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * E7_Vp


HR <- -V4 / (p3 * C2 + (1 - p3) * (1 - p4) * p5 * C4 + (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * C7)


O5_HR_2 <- data.frame(HR)

O5_HR_2_bw <- 2 * IQR(O5_HR_2$HR) / length(O5_HR_2$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O5_HR_2 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O5_HR_2_stats

ggplot(O5_HR_2, aes(x = HR)) +
  geom_histogram(binwidth = O5_HR_2_bw) +
  theme_minimal() +
  geom_vline(data = O5_HR_2_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 5: Foreign DNA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O5_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O5_HR_2)


#### Option 5: Foreign DNA - Foreign DNA Authorization after Re-evaluation ####

a1 <- runif(n, min = 0.05, max = 0.1)
A1 <- runif(n, min = 0.0014, max = 0.0061)
a2 <- runif(n, min = 0.05, max = 0.1)
A2 <- runif(n, min = 4.5, max = 18.5)
a3 <- runif(n, min = 0.05, max = 0.1)
A3 <- runif(n, min = 0.5, max = 3)
a4 <- runif(n, min = 0.05, max = 0.1)
A4 <- runif(n, min = 0.2, max = 0.4)
a5 <- runif(n, min = 0.05, max = 0.1)
A5 <- runif(n, min = 0.2, max = 0.4)

theta <- runif(n, min = 0.4, max = 0.5)


h2_min <- 1 / 0.33
h2_max <- 1 / 0.16
h2 <- runif(n, min = h2_min, max = h2_max)
h3_min <- 1 / 1.5
h3_max <- 1 / 0.5
h3 <- runif(n, min = h3_min, max = h3_max)
h4_min <- 1 / 1.5
h4_max <- 1 / 0.001
h4 <- runif(n, min = h4_min, max = h4_max)
h5_min <- 1 / 1.5
h5_max <- 1 / 0.5
h5 <- runif(n, min = h5_min, max = h5_max)
h6_min <- 1 / 1.5
h6_max <- 1 / 0.5
h6 <- runif(n, min = h6_min, max = h6_max)
h_star_min <- 1 / 10
h_star_max <- 1 / 8
h_star <- runif(n, min = h_star_min, max = h_star_max)

p3 <- 0
p4 <- 0
p5 <- 0
p6 <- 0
p7 <- 0


E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - a1 * h1 / ((mu + h1) * (mu + h2)) -
  theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))

E3_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3))

E4_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))

E5_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  a4 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5))

E6_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6))

E7_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6)) -
  theta * h1 * h2 * h3 * h4 * h5 * h6 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6) * (mu + h_star))

E2_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - a1 * h1 / ((mu + h1) * (mu + h2)) -
                theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))) * exp(-mu)

E4_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))) * exp(-mu)

E7_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
                a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6)) -
                theta * h1 * h2 * h3 * h4 * h5 * h6 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6) * (mu + h_star))) * exp(-mu)

C2 <- h1 * h2 / ((mu + h1) * (mu + h2))

C4 <- h1 * h2 * h3 * h4 / ((mu + h1) * (mu + h2) * (mu + h3)* (mu + h4))

C7 <- h1 * h2 * h3 * h4 * h5 * h6 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6))


V4 <- (1 - q * exp(-mu)) * (1 - p3) * p4 * E3_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * p6 * E5_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * p7 * E6_V0 +
  (1 - p3) * (1 - p4) * p5 * E4_V0 - (1 - p3) * (1 - p4) * p5 * E4_Vp +
  p3 * E2_V0 - p3 * E2_Vp +
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * E7_V0 -
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * E7_Vp


HR <- -V4 / (p3 * C2 + (1 - p3) * (1 - p4) * p5 * C4 + (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * C7)


O5_HR_3 <- data.frame(HR)

O5_HR_3_bw <- 2 * IQR(O5_HR_3$HR) / length(O5_HR_3$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O5_HR_3 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O5_HR_3_stats

ggplot(O5_HR_3, aes(x = HR)) +
  geom_histogram(binwidth = O5_HR_3_bw) +
  theme_minimal() +
  geom_vline(data = O5_HR_3_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 5: Foreign DNA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O5_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O5_HR_3)


#### Option 6: REACH-like legislation - Low Hazard ####

a1 <- runif(n, min = 0.05, max = 0.1)
A1 <- runif(n, min = 0.0051, max = 0.008)
a2 <- runif(n, min = 0.05, max = 0.1)
A2 <- runif(n, min = 4.5, max = 18.5)
a3 <- runif(n, min = 0.05, max = 0.1)
A3 <- runif(n, min = 0.5, max = 3)
a4 <- runif(n, min = 0.05, max = 0.1)
A4 <- runif(n, min = 0.2, max = 0.4)
a5 <- runif(n, min = 0.05, max = 0.1)
A5 <- runif(n, min = 0.2, max = 0.4)

theta <- runif(n, min = 0.7, max = 0.8)


h2_min <- 1 / 0.1
h2_max <- 1 / 0.08
h2 <- runif(n, min = h2_min, max = h2_max)
h3_min <- 1 / 1.5
h3_max <- 1 / 0.5
h3 <- runif(n, min = h3_min, max = h3_max)
h4_min <- 1 / 1.5
h4_max <- 1 / 0.001
h4 <- runif(n, min = h4_min, max = h4_max)
h5_min <- 1 / 1.5
h5_max <- 1 / 0.5
h5 <- runif(n, min = h5_min, max = h5_max)
h6_min <- 1 / 1.5
h6_max <- 1 / 0.5
h6 <- runif(n, min = h6_min, max = h6_max)
h_star_min <- 1 / 10
h_star_max <- 1 / 8
h_star <- runif(n, min = h_star_min, max = h_star_max)

p3 <- 1
p4 <- 0
p5 <- 0
p6 <- 0
p7 <- 0


E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - a1 * h1 / ((mu + h1) * (mu + h2)) -
  theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))

E3_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3))

E4_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))

E5_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  a4 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5))

E6_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6))

E7_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6)) -
  theta * h1 * h2 * h3 * h4 * h5 * h6 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6) * (mu + h_star))

E2_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - a1 * h1 / ((mu + h1) * (mu + h2)) -
                theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))) * exp(-mu)

E4_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))) * exp(-mu)

E7_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
                a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6)) -
                theta * h1 * h2 * h3 * h4 * h5 * h6 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6) * (mu + h_star))) * exp(-mu)

C2 <- h1 * h2 / ((mu + h1) * (mu + h2))

C4 <- h1 * h2 * h3 * h4 / ((mu + h1) * (mu + h2) * (mu + h3)* (mu + h4))

C7 <- h1 * h2 * h3 * h4 * h5 * h6 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6))


V4 <- (1 - q * exp(-mu)) * (1 - p3) * p4 * E3_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * p6 * E5_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * p7 * E6_V0 +
  (1 - p3) * (1 - p4) * p5 * E4_V0 - (1 - p3) * (1 - p4) * p5 * E4_Vp +
  p3 * E2_V0 - p3 * E2_Vp +
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * E7_V0 -
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * E7_Vp


HR <- -V4 / (p3 * C2 + (1 - p3) * (1 - p4) * p5 * C4 + (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * C7)


O6_HR_1 <- data.frame(HR)

O6_HR_1_bw <- 2 * IQR(O6_HR_1$HR) / length(O6_HR_1$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O6_HR_1 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O6_HR_1_stats

ggplot(O6_HR_1, aes(x = HR)) +
  geom_histogram(binwidth = O6_HR_1_bw) +
  theme_minimal() +
  geom_vline(data = O6_HR_1_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 5: Foreign DNA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O5_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O6_HR_1)


#### #### Option 6: REACH-like legislation - High Hazard Authorization after Data Request ####

a1 <- runif(n, min = 0.05, max = 0.1)
A1 <- runif(n, min = 0.0051, max = 0.008)
a2 <- runif(n, min = 0.05, max = 0.1)
A2 <- runif(n, min = 4.5, max = 18.5)
a3 <- runif(n, min = 0.05, max = 0.1)
A3 <- runif(n, min = 0.5, max = 3)
a4 <- runif(n, min = 0.05, max = 0.1)
A4 <- runif(n, min = 0.2, max = 0.4)
a5 <- runif(n, min = 0.05, max = 0.1)
A5 <- runif(n, min = 0.2, max = 0.4)

theta <- runif(n, min = 0.1, max = 0.2)


h2_min <- 1 / 0.1
h2_max <- 1 / 0.08
h2 <- runif(n, min = h2_min, max = h2_max)
h3_min <- 1 / 1.5
h3_max <- 1 / 0.5
h3 <- runif(n, min = h3_min, max = h3_max)
h4_min <- 1 / 1.5
h4_max <- 1 / 0.001
h4 <- runif(n, min = h4_min, max = h4_max)
h5_min <- 1 / 1.5
h5_max <- 1 / 0.5
h5 <- runif(n, min = h5_min, max = h5_max)
h6_min <- 1 / 1.5
h6_max <- 1 / 0.5
h6 <- runif(n, min = h6_min, max = h6_max)
h_star_min <- 1 / 10
h_star_max <- 1 / 8
h_star <- runif(n, min = h_star_min, max = h_star_max)

p3 <- 0
p4 <- 0
p5 <- 1
p6 <- 0
p7 <- 0

E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - a1 * h1 / ((mu + h1) * (mu + h2)) -
  theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))

E3_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3))

E4_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))

E5_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  a4 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5))

E6_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6))

E7_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6)) -
  theta * h1 * h2 * h3 * h4 * h5 * h6 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6) * (mu + h_star))

E2_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - a1 * h1 / ((mu + h1) * (mu + h2)) -
                theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))) * exp(-mu)

E4_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))) * exp(-mu)

E7_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
                a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6)) -
                theta * h1 * h2 * h3 * h4 * h5 * h6 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6) * (mu + h_star))) * exp(-mu)

C2 <- h1 * h2 / ((mu + h1) * (mu + h2))

C4 <- h1 * h2 * h3 * h4 / ((mu + h1) * (mu + h2) * (mu + h3)* (mu + h4))

C7 <- h1 * h2 * h3 * h4 * h5 * h6 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6))


V4 <- (1 - q * exp(-mu)) * (1 - p3) * p4 * E3_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * p6 * E5_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * p7 * E6_V0 +
  (1 - p3) * (1 - p4) * p5 * E4_V0 - (1 - p3) * (1 - p4) * p5 * E4_Vp +
  p3 * E2_V0 - p3 * E2_Vp +
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * E7_V0 -
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * E7_Vp


HR <- -V4 / (p3 * C2 + (1 - p3) * (1 - p4) * p5 * C4 + (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * C7)


O6_HR_2 <- data.frame(HR)

O6_HR_2_bw <- 2 * IQR(O6_HR_2$HR) / length(O6_HR_2$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O6_HR_2 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O6_HR_2_stats

ggplot(O6_HR_2, aes(x = HR)) +
  geom_histogram(binwidth = O6_HR_2_bw) +
  theme_minimal() +
  geom_vline(data = O6_HR_2_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 5: Foreign DNA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O5_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O6_HR_2)


#### #### Option 6: REACH-like legislation - High hazard Authorization after Re-evaluation ####

a1 <- runif(n, min = 0.05, max = 0.1)
A1 <- runif(n, min = 0.0051, max = 0.008)
a2 <- runif(n, min = 0.05, max = 0.1)
A2 <- runif(n, min = 4.5, max = 18.5)
a3 <- runif(n, min = 0.05, max = 0.1)
A3 <- runif(n, min = 0.5, max = 3)
a4 <- runif(n, min = 0.05, max = 0.1)
A4 <- runif(n, min = 0.2, max = 0.4)
a5 <- runif(n, min = 0.05, max = 0.1)
A5 <- runif(n, min = 0.2, max = 0.4)

theta <- runif(n, min = 0.1, max = 0.2)


h2_min <- 1 / 0.1
h2_max <- 1 / 0.08
h2 <- runif(n, min = h2_min, max = h2_max)
h3_min <- 1 / 1.5
h3_max <- 1 / 0.5
h3 <- runif(n, min = h3_min, max = h3_max)
h4_min <- 1 / 1.5
h4_max <- 1 / 0.001
h4 <- runif(n, min = h4_min, max = h4_max)
h5_min <- 1 / 1.5
h5_max <- 1 / 0.5
h5 <- runif(n, min = h5_min, max = h5_max)
h6_min <- 1 / 1.5
h6_max <- 1 / 0.5
h6 <- runif(n, min = h6_min, max = h6_max)
h_star_min <- 1 / 10
h_star_max <- 1 / 8
h_star <- runif(n, min = h_star_min, max = h_star_max)

p3 <- 0
p4 <- 0
p5 <- 0
p6 <- 0
p7 <- 0


E2_V0 <- -R - (r + A1 * h1) / (mu + h1) - a1 * h1 / ((mu + h1) * (mu + h2)) -
  theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))

E3_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  a2 * h2 / ((mu + h1) * (mu + h2) * (mu + h3))

E4_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))

E5_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  a4 * h4 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5))

E6_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6))

E7_V0 <- -R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
  (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
  (a3 * h3 + A4 * h1 * h2 * h3 * h4) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
  (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
  a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6)) -
  theta * h1 * h2 * h3 * h4 * h5 * h6 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6) * (mu + h_star))

E2_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - a1 * h1 / ((mu + h1) * (mu + h2)) -
                theta * h1 * h2 * h_star / ((mu + h1) * (mu + h2) * (mu + h_star))) * exp(-mu)

E4_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                theta * h1 * h2 * h3 * h4 * h5 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h_star))) * exp(-mu)

E7_Vp <- q * (-R - (r + A1 * h1) / (mu + h1) - (a1 * h1 + A2 * h1 * h2) / ((mu + h1) * (mu + h2)) -
                (a2 * h2 + A3 * h1 * h2 * h3) / ((mu + h1) * (mu + h2) * (mu + h3)) -
                a3 * h3 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4)) -
                (a4 * h4 + A5 * h1 * h2 * h3 * h4 * h5) / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5)) -
                a5 * h5 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6)) -
                theta * h1 * h2 * h3 * h4 * h5 * h6 * h_star / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6) * (mu + h_star))) * exp(-mu)

C2 <- h1 * h2 / ((mu + h1) * (mu + h2))

C4 <- h1 * h2 * h3 * h4 / ((mu + h1) * (mu + h2) * (mu + h3)* (mu + h4))

C7 <- h1 * h2 * h3 * h4 * h5 * h6 / ((mu + h1) * (mu + h2) * (mu + h3) * (mu + h4) * (mu + h5) * (mu + h6))


V4 <- (1 - q * exp(-mu)) * (1 - p3) * p4 * E3_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * p6 * E5_V0 +
  (1 - q * exp(-mu)) * (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * p7 * E6_V0 +
  (1 - p3) * (1 - p4) * p5 * E4_V0 - (1 - p3) * (1 - p4) * p5 * E4_Vp +
  p3 * E2_V0 - p3 * E2_Vp +
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * E7_V0 -
  (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * E7_Vp


HR <- -V4 / (p3 * C2 + (1 - p3) * (1 - p4) * p5 * C4 + (1 - p3) * (1 - p4) * (1 - p5) * (1 - p6) * (1 - p7) * C7)


O6_HR_3 <- data.frame(HR)

O6_HR_3_bw <- 2 * IQR(O6_HR_3$HR) / length(O6_HR_3$HR) ^ (1 / 3) #### Freedman–Diaconis rule ####

O6_HR_3 %>% summarize(Mean = mean(HR)) %>%
  pivot_longer(Mean, names_to = "Stat", values_to = "Value") -> O6_HR_3_stats

ggplot(O6_HR_3, aes(x = HR)) +
  geom_histogram(binwidth = O6_HR_3_bw) +
  theme_minimal() +
  geom_vline(data = O6_HR_3_stats,
             mapping = aes(xintercept = Value, color = Stat)) +
  labs(title = "Option 5: Foreign DNA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hurdle rate")

ggsave(paste0(wdir, '/Figures/O5_Hist.tiff'), width = 180, height = 38, units = "mm")

summary(O6_HR_3)


#### Summary Statistics ####

sumtable(O1_HR_1,
         digits = 4,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O1_HR_1",
         out = "viewer")

sumtable(O1_HR_2,
         digits = 4,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O1_HR_2",
         out = "viewer")

sumtable(O3_HR_1,
         digits = 4,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O3_HR_1",
         out = "viewer")

sumtable(O3_HR_2,
         digits = 4,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O3_HR_2",
         out = "viewer")

sumtable(O3_HR_3,
         digits = 4,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O3_HR_3",
         out = "viewer")

sumtable(O4_HR_1,
         digits = 4,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O4_HR_1",
         out = "viewer")

sumtable(O4_HR_2,
         digits = 4,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O4_HR_2",
         out = "viewer")

sumtable(O5_HR_1,
         digits = 4,
         fixed.digits = TRUE,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O5_HR_1",
         out = "viewer")

sumtable(O5_HR_2,
         digits = 4,
         fixed.digits = TRUE,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O5_HR_2",
         out = "viewer")

sumtable(O5_HR_3,
         digits = 4,
         fixed.digits = TRUE,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O5_HR_3",
         out = "viewer")

sumtable(O6_HR_1,
         digits = 4,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O6_HR_1",
         out = "viewer")

sumtable(O6_HR_2,
         digits = 4,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O6_HR_2",
         out = "viewer")

sumtable(O6_HR_3,
         digits = 4,
         summ = c('min(x)',
                  'mean(x)',
                  'median(x)',
                  'max(x)'),
         title = "O6_HR_3",
         out = "viewer")