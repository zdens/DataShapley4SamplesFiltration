library(stringi)
library(ggplot2)
library(ggpmisc)
library(plyr)
library(dplyr)
library(latex2exp)
library(patchwork)
library(ggtext)
library(ggsci)

set.seed(20261005)

 all_preds$mz_fact <- factor(
  all_preds$MZ, levels = sort(unique(levels(all_preds$MZ)))
)
exp_mode <- unique(all_preds$exp_mode)
groups <- levels(all_preds$group)

gid <- 4  # Смотрим на группу AboveMedian

group_preds <- all_preds |> filter(group == groups[gid])

# positive_selected_mz <- c(780.56, 808.59)
negative_selected_mz <- 629.55

selected_mz <- negative_selected_mz


idx <- which(group_preds$MZ %in% selected_mz)

col_idx <- which(attributes(de$all_fm_data$result_fm)$mass %in% selected_mz)
selected_mz_data <- as.data.frame(de$all_fm_data$result_fm[, col_idx])
names(selected_mz_data) <- selected_mz
selected_mz_data <- selected_mz_data |>
  mutate(
    class = as.factor(de$all_fm_data$result_meta_data$diagnosis)
  )

pca_data <- prcomp(selected_mz_data[, c(1, 2)])

fviz_eig(
  pca_data, addlabels = TRUE
)
fviz_pca_ind(
  pca_data,
  geom.ind =  "point",
  col.ind = selected_mz_data[, 3]
)

ggplot(group_preds[idx,], aes(x = MZ, y = intensity, color = class)) +
  geom_point()

idx2 <- which(all_preds$MZ %in% selected_mz)
ggplot(all_preds[idx2,]) +
  labs(
    title = paste("Predictors", "intens")
  ) +
  facet_grid(. ~ group) +
  geom_violin(aes(y = mz_fact, color = class, x = intensity)) +
  labs(
    y = "MZ"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

plot_data2 <- rbind(data.frame(
  x = seq_len(nrow(pos_ds)),
  y = pos_ds[, col_idx[[1]]],
  class = as.factor(pos_ds$class),
  group = selected_mz[[1]]
), data.frame(
  x = seq_len(nrow(pos_ds)),
  y = -1 * pos_ds[, col_idx[[2]]],
  class = as.factor(pos_ds$class),
  group = selected_mz[[2]]
))

ggplot(plot_data2, aes(color = class)) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = y)) +
  theme_bw()

dens_data <- pos_ds[, c(selected_mz, "class")]
dens_data$class <- factor(
  paste0("diag_", pos_ds$class),
  levels = c("diag_6", "diag_32")
)
names(dens_data) <- c("var1", "class")
ggplot(dens_data, aes(x = x, color = class)) +
  geom_density(aes(x = var1, y = ..density..)) +
  annotate(
    "text", x = 0.3, y = 25, size = 5,
    label = selected_mz[[1]], color="#69b3a2"
  ) +
  xlab("Predictor's intensity") + theme_bw() +
  labs(title = "Positive Shapley data")

ggplot(dens_data, aes(x = x, color = class)) +
  geom_density(aes(x = var1, y = ..density..)) +
  annotate(
    "text", x = 0.025, y = 250, size = 5,
    label = selected_mz[[1]], color="#69b3a2"
  ) +
  xlab("Predictor's intensity") + theme_bw() +
  labs(title = "Positive Shapley data")

dens_all_data <- as.data.frame(de$all_fm_data$result_fm[, col_idx])
dens_all_data$class <- factor(
  paste0("diag_", de$all_fm_data$result_meta_data$diagnosis),
  levels = c("diag_6", "diag_32", "neg_shap")
)
names(dens_all_data) <- c("var1", "var2", "class")
ggplot(dens_all_data, aes(x = x, color = class)) +
  geom_density(aes(x = var1, y = ..density..)) +
  annotate(
    "text", x = 0.3, y = 25, size = 5,
    label = selected_mz[[1]], color="#69b3a2"
  ) +
  geom_density(aes(x = var2, y = -..density..)) +
  annotate(
    "text", x = 0.3, y = -20, size = 5,
    label = selected_mz[[2]], color="#404080"
  ) +
  xlab("Predictor's intensity") + theme_bw() +
  labs(title = "All data set")

ggplot(
  dens_all_data,
  aes(x = var1, y = var2, color = class)
) + geom_point() + theme_bw() +
  labs(x = selected_mz[[1]], y = selected_mz[[2]])


ggplot(
  dens_data,
  aes(x = seq_len(nrow(dens_data)), y = var1, color = class)
) + geom_point() + theme_bw() +
  labs(title = paste("Scatter plot for ", selected_mz[[1]]))
ggplot(
  dens_ldata,
  aes(x = seq_len(nrow(dens_data)), y = var2, color = class)
) + geom_point() + theme_bw() +
  labs(title = paste("Scatter plot for ", selected_mz[[2]]))

(ggplot(dens_data, aes(x = x, color = class)) +
  geom_density(aes(x = var1, y = ..density..)) +
  annotate(
    "text", x = 0.3, y = 25, size = 5,
    label = selected_mz[[1]], color="#69b3a2"
  ) +
  geom_density(aes(x = var2, y = -..density..)) +
  annotate(
    "text", x = 0.3, y = -20, size = 5,
    label = selected_mz[[2]], color="#404080"
  ) +
  xlab("Predictor's intensity") + theme_bw() +
ggplot(
  dens_data,
  aes(x = var1, y = var2, color = class)
) + geom_point() + theme_bw() +
  labs(x = selected_mz[[1]], y = selected_mz[[2]])) + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Positive Shapley",
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  )


true_dens_data <- true_ds[, c(selected_mz, "class", "shapley")]
neg_shap_idx <- which(true_dens_data$shapley <= 0)
true_dens_data$class <- paste0("diag_", true_dens_data$class)
true_dens_data$diag <- true_dens_data$class
true_dens_data$class[neg_shap_idx] <- "neg_shapley"
true_dens_data$class <- factor(
  true_dens_data$class,
  levels = c("diag_6", "diag_32", "neg_shapley")
)
true_dens_data$diag <- factor(
  true_dens_data$diag,
  levels = c("diag_6", "diag_32", "neg_shapley")
)
names(true_dens_data)[1:2] <- c("var1", "var2")
p_scatter <- ggplot(
  true_dens_data,
  aes(x = var1, y = var2, color = class)
) + geom_point() + theme_bw() +
  labs(
    x = selected_mz[[1]], y = selected_mz[[2]],
    title = "Diagnosis with negative Shapley values"
  ) + scale_color_manual(
    name = "Class",
    values = c(
      "diag_6" = "red",
      "diag_32" = "#00BA38",
      "neg_shapley" = "#619CFF"
    )
  )
p_diag_scatter <- ggplot(
  true_dens_data[-neg_shap_idx,],
  aes(x = var1, y = var2, color = diag)
) + geom_point() + theme_bw() +
  labs(
    x = selected_mz[[1]], y = selected_mz[[2]],
    title = "Diagnosis"
  ) +
  theme(legend.position = "none") + scale_color_manual(
    name = "Class",
    values = c(
      "diag_6" = "red",
      "diag_32" = "#00BA38",
      "neg_shapley" = "#619CFF"
    )
  )
p_dens <- ggplot(true_dens_data, aes(x = x, color = class)) +
  geom_density(aes(x = var1, y = ..density..)) +
  annotate(
    "text", x = 0.3, y = 25, size = 5,
    label = selected_mz[[1]], color="#69b3a2"
  ) +
  geom_density(aes(x = var2, y = -..density..)) +
  annotate(
    "text", x = 0.3, y = -20, size = 5,
    label = selected_mz[[2]], color="#404080"
  ) +
  xlab("Predictor's intensity") + theme_bw() +
  theme(legend.position = "none")

p_dens + p_scatter  + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Positive Shapley",
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  )
xlim_range = range(
  true_dens_data$var1
)
ylim_range = range(
  true_dens_data$var2
)
p_diag_scatter + xlim(xlim_range) + ylim(ylim_range) + p_scatter  + plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  )

dens_data4class <- true_ds[, c(selected_mz, "class", "shapley")]
dens_data4class <- dens_data4class |>
  mutate(
    diag = class,
    class = factor(
      ifelse(
        shapley > 0,
        paste0("diag_", class),
        paste0("neg_", class)
      ),
      levels = c("diag_6", "diag_32", "neg_6", "neg_32")
    )
  )
names(dens_data4class)[1:2] <- c("var1", "var2")

my_colors_neg_shap <- c(
  "diag_6"   = "red",
  "diag_32"  = "#00BA38",
  "neg_6" = "#00bfc4",
  "neg_32" = "#c4009a"
)

p_scatter4class <- ggplot(
  dens_data4class,
  aes(x = var1, y = var2, color = class)
) + geom_point() + theme_bw() +
  labs(
    x = selected_mz[[1]], y = selected_mz[[2]],
    title = "Diagnosis with negative Shapley values"
  ) + scale_color_manual(
    name = "Class",
    values = my_colors_neg_shap
  )

p_diag_scatter + xlim(xlim_range) + ylim(ylim_range) + p_scatter4class  + plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  )

## Двумерие

dens_data_2d <- true_ds[, c(selected_mz, "class", "shapley")]
neg_shap_idx <- which(dens_data_2d$shapley <= 0)
dens_data_2d$class <- paste0("diag_", dens_data_2d$class)
dens_data_2d$diag <- dens_data_2d$class
dens_data_2d$class <- factor(
  dens_data_2d$class,
  levels = c("diag_6", "diag_32", "neg_shapley")
)
dens_data_2d$diag <- factor(
  dens_data_2d$diag,
  levels = c("diag_6", "diag_32", "neg_shapley")
)
names(dens_data_2d)[1:2] <- c("var1", "var2")

ggplot(dens_data_2d, aes(x = x, color = class)) +
  geom_density(aes(x = var1, y = ..density..)) +
  annotate(
    "text", x = 0.3, y = 25, size = 7,
    label = selected_mz[[1]], color="#404080"
  ) +
  geom_density(aes(x = var2, y = -..density..)) +
  annotate(
    "text", x = 0.3, y = -20, size = 7,
    label = selected_mz[[2]], color="#404080"
  ) +
  scale_color_manual(
    name = "Class",
    values = my_colors_neg_shap
  ) +
  labs(
    x = "Predictor's intensity",
    y = "Density distribution"
  ) + theme_bw()
