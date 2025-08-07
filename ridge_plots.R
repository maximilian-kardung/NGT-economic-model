library(ggridges)
library(ggplot2)

#### O1: Status quo ####

O1_HR_ridge <- bind_cols(O1_HR_1, O1_HR_2)

O1_HR_ridge %>% rename('Data request' = 'HR...1', 'Re-evaluation' = 'HR...2') %>%
  pivot_longer(cols = c(1, 2), names_to = "Outcome") -> O1_HR_ridge

O1_HR_ridge$Outcome <- factor(O1_HR_ridge$Outcome, levels = c('Re-evaluation', 'Data request'))

ggplot(O1_HR_ridge, aes(x = value, y = Outcome, fill = Outcome)) +
  geom_density_ridges(
    alpha = 0.6,
    stat = "binline",
    bins = 40,
    scale = 1
  ) +
  theme_minimal(base_size = 16) +
  xlab("\nHurdle value (Mio Euro)") +
  labs(title = "Options 1 & 2: Status Quo\n") +
  theme(plot.title = element_text(hjust = 0.4)) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(10, 40, by = 5))

ggsave('Ridge_O1.tiff', width=10, height=3)

#### O3: Risk Profiles ####

O3_HR_ridge <- bind_cols(O3_HR_1, O3_HR_2) %>% bind_cols(O3_HR_3)

O3_HR_ridge %>% rename(
  'Risk profile 0 & 1' = 'HR...1',
  'As safe as' = 'HR...2',
  'Not as safe as' = 'HR...3'
) %>%
  pivot_longer(cols = c(1, 2, 3), names_to = "Outcome") -> O3_HR_ridge

O3_HR_ridge$Outcome <- factor(O3_HR_ridge$Outcome,
                              levels = c('Not as safe as', 'As safe as', 'Risk profile 0 & 1'))

ggplot(O3_HR_ridge, aes(x = value, y = Outcome, fill = Outcome)) +
  geom_density_ridges(
    alpha = 0.6,
    stat = "binline",
    bins = 40,
    scale = 3
  ) +
  theme_minimal(base_size = 16) +
  xlab("\nHurdle value (Mio Euro)") +
  labs(title = "Option 3: Risk Profiles\n") +
  theme(plot.title = element_text(hjust = 0.3)) +
  scale_fill_discrete(breaks = c('Risk profile 0 & 1', 'As safe as', 'Not as safe as')) +
  theme(legend.position = "none")  +
  scale_x_continuous(breaks = seq(0, 30, by = 5))

ggsave('Ridge_O3.tiff', width=10, height=3)

#### O4: Novel Trait ####

O4_HR_ridge <- bind_cols(O4_HR_1, O4_HR_2)

O4_HR_ridge %>% rename('Not novel trait' = 'HR...1', 'Novel trait' = 'HR...2') %>%
  pivot_longer(cols = c(1, 2), names_to = "Outcome") -> O4_HR_ridge

O4_HR_ridge$Outcome <- factor(O4_HR_ridge$Outcome, levels = c('Novel trait', 'Not novel trait'))

ggplot(O4_HR_ridge, aes(x = value, y = Outcome, fill = Outcome)) +
  geom_density_ridges(
    alpha = 0.6,
    stat = "binline",
    bins = 40,
    scale = 2
  ) +
  theme_minimal(base_size = 16) +
  xlab("\nHurdle value (Mio Euro)") +
  labs(title = "Option 4: Novel Trait\n") +
  theme(plot.title = element_text(hjust = 0.3)) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 20, by = 5))

ggsave('Ridge_O4.tiff', width=10, height=3)

#### O5: Foreign DNA Sequence ####

O5_HR_ridge <- bind_cols(O5_HR_1, O5_HR_2) %>% bind_cols(O5_HR_3)

O5_HR_ridge %>% rename(
  'No foreign DNA' = 'HR...1',
  'Data request' = 'HR...2',
  'Re-evaluation' = 'HR...3'
) %>%
  pivot_longer(cols = c(1, 2, 3), names_to = "Outcome") -> O5_HR_ridge

O5_HR_ridge$Outcome <- factor(O5_HR_ridge$Outcome,
                              levels = c('Re-evaluation', 'Data request', 'No foreign DNA'))

ggplot(O5_HR_ridge, aes(x = value, y = Outcome, fill = Outcome)) +
  geom_density_ridges(
    alpha = 0.6,
    stat = "binline",
    bins = 40,
    scale = 3
  ) +
  theme_minimal(base_size = 16) +
  xlab("\nHurdle value (Mio Euro)") +
  labs(title = "Option 5: Foreign DNA Sequence\n") +
  theme(plot.title = element_text(hjust = 0.3)) +
  scale_fill_discrete(breaks = c('Risk profile 0 & 1', 'As safe as', 'Not as safe as')) +
  theme(legend.position = "none")  +
  scale_x_continuous(breaks = seq(0, 50, by = 5))

ggsave('Ridge_O5.tiff', width=10, height=3)

#### O6: REACH-like legistlation ####

O6_HR_ridge <- bind_cols(O6_HR_1, O6_HR_2) %>% bind_cols(O6_HR_3)

O6_HR_ridge %>% rename(
  'Low hazard' = 'HR...1',
  'Data request' = 'HR...2',
  'Re-evaluation' = 'HR...3'
) %>%
  pivot_longer(cols = c(1, 2, 3), names_to = "Outcome") -> O6_HR_ridge

O6_HR_ridge$Outcome <- factor(O6_HR_ridge$Outcome,
                              levels = c('Re-evaluation', 'Data request', 'Low hazard'))

ggplot(O6_HR_ridge, aes(x = value, y = Outcome, fill = Outcome)) +
  geom_density_ridges(
    alpha = 0.6,
    stat = "binline",
    bins = 40,
    scale = 3
  ) +
  theme_minimal(base_size = 16) +
  xlab("\nHurdle value (Mio Euro)") +
  labs(title = "Option 6: REACH-like legistlation\n") +
  theme(plot.title = element_text(hjust = 0.3)) +
  scale_fill_discrete(breaks = c('Risk profile 0 & 1', 'As safe as', 'Not as safe as')) +
  theme(legend.position = "none")  +
  scale_x_continuous(breaks = seq(0, 50, by = 5))

ggsave('Ridge_O6.tiff', width=10, height=3)


#### Cumulative distribution functions ####

O1_HR_ridge$Outcome <- paste0("Option 1: ", O1_HR_ridge$Outcome)
O3_HR_ridge$Outcome <- paste0("Option 3: ", O3_HR_ridge$Outcome)
O4_HR_ridge$Outcome <- paste0("Option 4: ", O4_HR_ridge$Outcome)
O5_HR_ridge$Outcome <- paste0("Option 5: ", O5_HR_ridge$Outcome)
O6_HR_ridge$Outcome <- paste0("Option 6: ", O6_HR_ridge$Outcome)

HR_CD <- bind_rows(O1_HR_ridge, O3_HR_ridge) %>% 
  bind_rows(O4_HR_ridge) %>% 
  bind_rows(O5_HR_ridge) %>% 
  bind_rows(O6_HR_ridge)


ggplot(HR_CD, aes(value, colour = Outcome)) +
  stat_ecdf(linewidth = 0.9) +
  theme_minimal(base_size = 14) +
  theme(legend.position="bottom")