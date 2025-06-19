
data_alcohol %>%
  filter(match_before_after == 1) %>% 
  mutate(after = factor(after, labels = c('Pre', 'Post'), levels = c(0,1))) %>% 
  select(id, after, know_signs, know_reduction, know_effects) %>% 
  pivot_longer(cols = !c(id, after)) %>% 
  ggplot(aes(after, value, color = name)) +
  geom_jitter(height = .25, width = .4, alpha = .25) +
  facet_grid(~name) +
  MetBrewer::scale_color_met_d('Egypt') +
  labs(x = NULL, y = 'Response') +
  theme(legend.position = 'none')

data_alcohol %>%
  filter(match_before_after == 1) %>% 
  mutate(after = factor(after, labels = c('Pre', 'Post'), levels = c(0,1))) %>% 
  select(id, after, know_signs, know_reduction, know_effects) %>% 
  pivot_longer(cols = !c(id, after)) %>% 
  pivot_wider(id_cols = id, names_from = c(after, name),values_from = value) %>% 
  transmute(id,
           diff_know_signs = Post_know_signs - Pre_know_signs,
         diff_know_reduction = Post_know_reduction - Pre_know_reduction,
         diff_know_effects = Post_know_effects - Pre_know_effects) %>% 
  pivot_longer(!id) %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_grid(~name) +
  lims(x = c(-5, 5))
  

data_mean_diff <-
data_alcohol %>%
  filter(match_before_after == 1) %>% 
  mutate(after = factor(after, labels = c('Pre', 'Post'), levels = c(0,1))) %>% 
  select(id, after, know_signs, know_reduction, know_effects) %>% 
  pivot_longer(cols = !c(id, after)) %>% 
  pivot_wider(id_cols = id, names_from = c(after, name),values_from = value) %>% 
  transmute(id,
            diff_know_signs = Post_know_signs - Pre_know_signs,
            diff_know_reduction = Post_know_reduction - Pre_know_reduction,
            diff_know_effects = Post_know_effects - Pre_know_effects) %>% 
  left_join(data_alcohol, by = c("id" = "id"))

# Ceiling effect?
data_mean_diff %>% 
  filter(after == 0) %>% 
  mutate(final_score = know_signs + diff_know_signs) %>% 
  ggplot(aes(know_signs, diff_know_signs, color = final_score)) +
  geom_jitter(height = .15, width = .15, alpha = .5) +
  labs(x = 'Pre- Score', y = 'Change', color = 'Post- Score') +
  MetBrewer::scale_color_met_c('Egypt')

data_mean_diff %>% 
  filter(after == 0) %>% 
  mutate(final_score = know_reduction + diff_know_reduction) %>% 
  ggplot(aes(know_reduction, diff_know_reduction, color = final_score)) +
  geom_jitter(height = .15, width = .15, alpha = .5) +
  labs(x = 'Pre- Score', y = 'Change', color = 'Post- Score') +
  MetBrewer::scale_color_met_c('Egypt')

data_mean_diff %>% 
  filter(after == 0) %>% 
  mutate(final_score = know_effects + diff_know_effects) %>% 
  ggplot(aes(know_effects, diff_know_effects, color = final_score)) +
  geom_jitter(height = .15, width = .15, alpha = .5) +
  labs(x = 'Pre- Score', y = 'Change', color = 'Post- Score') +
  MetBrewer::scale_color_met_c('Egypt') 

data_wide <-
  data_alcohol %>%
  filter(match_before_after == 1) %>% 
  mutate(after = factor(after, labels = c('Pre', 'Post'), levels = c(0,1))) %>% 
  select(id, after, know_signs, know_reduction, know_effects) %>% 
  pivot_longer(cols = !c(id, after)) %>% 
  pivot_wider(id_cols = id, names_from = c(after, name),values_from = value)

data_wide %>% 
  ggplot(aes(Pre_know_signs, Post_know_signs)) +
  geom_jitter(height = .15, width = .15, alpha = .5)

data_wide %>% 
  ggplot(aes(Pre_know_reduction, Post_know_reduction)) +
  geom_jitter(height = .15, width = .15, alpha = .5)

data_wide %>% 
  ggplot(aes(Pre_know_effects, Post_know_effects)) +
  geom_jitter(height = .15, width = .15, alpha = .5)
