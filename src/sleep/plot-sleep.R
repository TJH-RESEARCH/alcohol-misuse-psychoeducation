
library(patchwork)

set.seed(9)
# Plot Knowledge of Effects
plot_effects <-
  data_sleep %>% 
  mutate(after = factor(after, levels = c(0,1), labels = c('Before', 'After'))) %>% 
  filter(match_before_after == 1) %>% 
  ggplot(aes(after, know_effects)) +1
  geom_jitter(
    height = .125, 
    width = .4,
    alpha = .25, 
    color = '#440154FF') +
  stat_summary(fun.data = 'mean_sdl', 
               geom = "errorbar", 
               fun.args = list(mult = 1.96), 
               width = .15) +
  geom_point(stat = "summary", 
             fun = "mean", 
             color = 'black', 
             fill = '#FDE725FF',
             shape = 23, 
             size = 3.5) + 
  labs(x = '', y = 'Score', subtitle = "Sleep's Effects on Body") +
  theme_minimal()
plot_effects

# Hygiene
plot_hygiene <-
  data_sleep %>% 
  mutate(after = factor(after, levels = c(0,1), labels = c('Before', 'After'))) %>% 
  filter(match_before_after == 1) %>% 
  ggplot(aes(factor(after), know_hygiene)) +
  geom_jitter(
    height = .125,
    width = .4,
    alpha = .25, 
    color = '#440154FF') +
  stat_summary(fun.data = 'mean_sdl', 
               geom = "errorbar", 
               fun.args = list(mult = 1.96), 
               width = .15) +
  geom_point(stat = "summary", 
             fun = "mean", 
             color = 'black', 
             fill = '#FDE725FF',
             shape = 23, 
             size = 3.5) + 
  labs(x = '', y = '', subtitle = "Sleep Hygiene") +
  theme_minimal()

plot_sleep <-
  plot_effects + 
  plot_hygiene + 
  patchwork::plot_annotation(title = 'Self Reported Sleep Knowledge',
        caption = 'Raw data and mean and 95% error bars')

plot_sleep

ggsave(plot = plot_sleep,
       filename = 'plot-sleep.pdf',
       path = here::here('output'),
       bg = "transparent", width = 6, height = 4, dpi = 300)
