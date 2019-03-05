library(tidyverse)
library(lme4)

movie <- rep(c("M1", "M2"), 4)
state <- rep(c("sober", "sober", "drunk", "drunk"), 2)
tomatometer <- c(40, 70, 68, 82, 5, 52, 35, 75)
subject <- rep(c("S1", "S2"), each = 4)
grand_mean <- mean(tomatometer)

df <- tibble(movie, state, tomatometer, subject, grand_mean)


# fixed effects model prediction
df <- df %>%
  group_by(state) %>%
  summarise(FE = mean(tomatometer)) %>%
  right_join(df, by = c("state"))

# subjects random intercepts only prediction
df <- df %>%
  group_by(subject) %>%
  summarise(subj_mean_diff = (mean(tomatometer) - grand_mean[1])) %>%
  right_join(df, by = c("subject")) %>%
  mutate(subj_RI = FE + subj_mean_diff)


# item random intercepts only prediction
df <- df %>%
  group_by(movie) %>%
  summarise(mov_mean_diff = (mean(tomatometer) - grand_mean[1])) %>%
  right_join(df, by = c("movie")) %>%
  mutate(mov_RI = FE + mov_mean_diff)


# subjects random intercepts and random slopes
df <- df %>%
  group_by(subject, state) %>%
  summarise(subj_RI_RS = mean(tomatometer)) %>%
  right_join(df, by = c("subject", "state"))

# movie random intercepts and random slopes
df <- df %>%
  group_by(movie, state) %>%
  summarise(mov_RI_RS = mean(tomatometer)) %>%
  right_join(df, by = c("movie", "state"))

# crossed random intercepts and slopes  
  
df <- df %>%
  group_by(subject, movie, state) %>%
  summarise(cr_RI_RS = mean(tomatometer)) %>%
  ungroup() %>%
  right_join(df, by = c("subject", "movie", "state")) %>%
  select(-mov_mean_diff, -subj_mean_diff) %>%
  mutate(x_cat = c(1,2, 4, 5, 8, 9, 11, 12))

write_csv(df, "_sessions/MixedModels/MEM_example.csv")

# Plots ========================================

# Fixed effects only plot --------------------
g_size <- 6
dat_plot <- ggplot(df, aes(x_cat, tomatometer)) +
  geom_hline(yintercept = grand_mean[1], lty = 2, alpha = .35, size = 1.25) +
  geom_point(size = g_size, col = "#606061")  +
  annotate(geom = "text", x = c(1,2, 4, 5, 8, 9, 11, 12), y = -5,
           label = rep(c("M1", "M2"), 4), size = 6) +
  annotate(geom = "text", x = c(1.5, 4.5, 8.5, 11.5),
           y = -12, label = rep(c("Sober", "Drunk"), 2), size = 6) +
  annotate(geom = "text", x = c(3, 10), y = -20, label = c("Subject 1", "Subject 2"),
           size = 7, fontface = 2) +
  geom_segment(x = c(1,2, 4, 5, 8, 9, 11, 12), y = rep(0),
               xend = c(1,2, 4, 5, 8, 9, 11, 12), yend = rep(-1),
               col = "black") +
  geom_vline(xintercept = 6.5) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 100), xlim = c(0, 13), expand = FALSE, clip = "off") +
  scale_x_discrete(breaks = "none") +
  labs(y = "Tomatometer",
       x = " ") +
  theme(
    plot.margin = unit(c(.5, .5, .5, .5), "lines"),
    axis.title.x = element_text(margin = margin(t = 50, r = 0, b = 0, l = 0)),
    axis.text.x = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18,face = "bold"),
    strip.background = element_rect(fill="#a5d7d2")
  )

dat_plot
ggsave("_sessions/MixedModels/image/dat_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)

# Fixed Effects plot -----------------------------
dat_plot + geom_point(aes(y = FE), shape = 17, col = "#EA4B68",
                      size = g_size)
ggsave("_sessions/MixedModels/image/FE_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)

# Subjects Random Intercepts only plot -----------
dat_plot + geom_point(aes(y = subj_RI), shape = 17, col = "#EA4B68",
                      size = g_size)
ggsave("_sessions/MixedModels/image/subj_RI_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)

# Movie Random Intercepts only plot -----------
dat_plot + geom_point(aes(y = mov_RI), shape = 17, col = "#EA4B68",
                      size = g_size)
ggsave("_sessions/MixedModels/image/mov_RI_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)


# Subject Random Intercepts and Slopes plot -----------
dat_plot + geom_point(aes(y = subj_RI_RS), shape = 17, col = "#EA4B68",
                      size = g_size)
ggsave("_sessions/MixedModels/image/subj_RI_RS_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)

# Movie Random Intercepts and Slopes plot -----------
dat_plot + geom_point(aes(y = mov_RI_RS), shape = 17, col = "#EA4B68",
                      size = g_size)
ggsave("_sessions/MixedModels/image/mov_RI_RS_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)

# Crossed Random Intercepts and Slopes plot -----------
dat_plot + geom_point(aes(y = cr_RI_RS), shape = 17, col = "#EA4B68",
                      size = g_size)
ggsave("_sessions/MixedModels/image/cr_RI_RS_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)

