library(stringi)
library(ggplot2)
library(ggpmisc)
library(plyr)
library(dplyr)
library(latex2exp)
library(patchwork)
library(viridisLite)
library(ggtext)
library(ggsci)
library(factoextra)

set.seed(20261005)

fviz_plot_ind_manual0 <- function(
  true_ds_pca, axes, col_inds_f, neg_pca_data = NULL,
  new_color = NULL, title_txt = NULL,
  axes_label = "Dim"
) {
  col_names <- paste0(axes_label, axes)
  base_data <- as.data.frame(
    true_ds_pca$x[, axes]
  )
  names(base_data) <- col_names
  base_data$class <- col_inds_f
  if (!is.null(neg_pca_data)) {
    new_data <- as.data.frame(neg_pca_data[, axes])
    colnames(new_data) <- col_names
    new_data$class <- factor(
      new_color,
      levels = levels(col_inds_f)
    )
    all_pca_data <- rbind(base_data, new_data)
  } else {
    all_pca_data <- base_data
  }
  names(all_pca_data) <- c("var1", "var2", "class")
  
  var_explained <- (true_ds_pca$sdev^2) / sum(true_ds_pca$sdev^2) * 100
  pc_labs <- sapply(seq_along(axes), function(i) {
    ax = axes[[i]]
    paste0(axes_label, ax, sprintf(" (%.1f%%)", var_explained[ax]))
  })

  ellipses <- do.call(
    rbind,
    lapply(
      split(all_pca_data, all_pca_data$class), 
      function(d) {
        if (nrow(d) < 3) return(NULL)
        cov_m  <- cov(d[, 1:2])
        center <- colMeans(d[, 1:2])
        rad    <- sqrt(qchisq(0.95, df = 2))
        theta  <- seq(0, 2 * pi, length.out = 100)
        circle <- cbind(cos(theta), sin(theta)) * rad
        eig    <- eigen(cov_m)
        ell    <- circle %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
        ell    <- sweep(ell, 2, center, "+")
        data.frame(var1 = ell[, 1], var2 = ell[, 2], class = unique(d$class))
      }
    )
  )
  ggplot() +
    # --- Эллипсы ---
    geom_path(data = ellipses,
              aes(x = var1, y = var2, color = class),
              linetype = "dashed", linewidth = 0.6) +
    
    # --- Точки индивидов ---
    geom_point(data = all_pca_data,
              aes(x = var1, y = var2, color = class),
              size = 1, alpha = 0.85) +
    
    # --- Нулевые оси ---
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    
    # --- Подписи осей с долей вариабельности ---
    labs(x = pc_labs[[1]],
        y = pc_labs[[2]],
        color = "Class",
        title = title_txt) +
    # coord_equal() +
    theme_minimal() +
    theme(
      title = element_text(size = 12)
    )
  
}

fviz_plot_ind_manual1 <- function(
  true_ds_pca, axes, col_inds_f, neg_pca_data = NULL,
  new_color = NULL, title_txt = NULL,
  axes_label = "Dim",
  fixed_colors = NULL,        # <-- именованный вектор class -> color
  fixed_limits = NULL         # <-- c(xmin, xmax, ymin, ymax)
) {
  col_names <- paste0(axes_label, axes)
  
  base_data <- as.data.frame(true_ds_pca$x[, axes])
  names(base_data) <- col_names
  base_data$class <- col_inds_f
  
  if (!is.null(neg_pca_data)) {
    new_data <- as.data.frame(neg_pca_data[, axes])
    colnames(new_data) <- col_names
    new_data$class <- factor(new_color, levels = levels(col_inds_f))
    all_pca_data <- rbind(base_data, new_data)
  } else {
    all_pca_data <- base_data
  }
  names(all_pca_data) <- c("var1", "var2", "class")
  
  var_explained <- (true_ds_pca$sdev^2) / sum(true_ds_pca$sdev^2) * 100
  pc_labs <- sapply(seq_along(axes), function(i) {
    ax = axes[[i]]
    paste0(axes_label, ax, sprintf(" (%.1f%%)", var_explained[ax]))
  })
  
  # --- Эллипсы ---
  ellipses <- do.call(
    rbind,
    lapply(split(all_pca_data, all_pca_data$class), function(d) {
      if (nrow(d) < 3) return(NULL)
      cov_m  <- cov(d[, 1:2])
      center <- colMeans(d[, 1:2])
      rad    <- sqrt(qchisq(0.95, df = 2))
      theta  <- seq(0, 2 * pi, length.out = 100)
      circle <- cbind(cos(theta), sin(theta)) * rad
      eig    <- eigen(cov_m)
      ell    <- circle %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
      ell    <- sweep(ell, 2, center, "+")
      data.frame(var1 = ell[, 1], var2 = ell[, 2], class = unique(d$class))
    })
  )
  
  p <- ggplot() +
    geom_path(data = ellipses,
              aes(x = var1, y = var2, color = class),
              linetype = "dashed", linewidth = 0.6, show.legend = FALSE) +
    geom_point(data = all_pca_data,
               aes(x = var1, y = var2, color = class),
               size = 1, alpha = 0.85) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    labs(x = pc_labs[[1]], y = pc_labs[[2]],
         color = "Class", title = title_txt) +
    theme_minimal()
  
  # --- Фиксированные цвета ---
  if (!is.null(fixed_colors)) {
    p <- p + scale_color_manual(values = fixed_colors,
                                limits = names(fixed_colors),
                                drop = FALSE)
  }
  
  # --- Фиксированные пределы осей ---
  if (!is.null(fixed_limits)) {
    p <- p + coord_equal(
      xlim = fixed_limits[1:2],
      ylim = fixed_limits[3:4]
    )
  } else {
    p <- p + coord_equal()
  }
  
  p
}

load("pca.RData")

non_zeros_preds <- downsmpl_pos_data$predictors |>
  filter(intensity != 0) |>
  select(MZ) |> distinct()

col_idx <- which(
  names(
    true_ds
  ) %in% levels(unlist(non_zeros_preds))
)

data_levels <- c("diag_6", "diag_32", "neg_shap")

preds_true_ds <- true_ds[, col_idx]
pos_idx <- which(true_ds$shapley > 0)
true_ds_pca <- prcomp(preds_true_ds[pos_idx,], scale = FALSE)
col_inds <- paste0("diag_", true_ds[pos_idx, "class"])
col_inds_f <- factor(
  col_inds,
  levels = data_levels
)

p_scree <- fviz_eig(
  true_ds_pca, addlabels = TRUE
)
p12 <- fviz_pca_ind(
  true_ds_pca,
  geom.ind =  "point",
  col.ind = col_inds_f,
  addEllipses=TRUE, ellipse.level=0.95
) + labs(
  title = "Dim1-2"
)

p12m <- fviz_plot_ind_manual0(
  true_ds_pca,
  axes = c(1, 2),
  col_inds_f,
  title = "Positive Shapley scans"
  ) + theme(legend.position = "none")

p_scree / p12 + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Positive Shapley DataSet",
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  )

## Add negative Spaley scans

neg_pca_data <- predict(
  true_ds_pca,
  true_ds[-pos_idx,]
)

p12_negshap <- fviz_plot_ind_manual0(
  true_ds_pca, axes = c(1, 2),
  col_inds_f = col_inds_f,
  neg_pca_data = neg_pca_data,
  new_color = "neg_shap",
  title = "Positive+Negative Shapley scans"
)

p_scree / (p12m + p12_negshap) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Positive Shapley DataSet",
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  )

my_colors <- c(
  "diag_6"   = "#F8766D",
  "diag_32"  = "#7CAE00",
  "neg_shap" = "#00BFC4"
)

axes <- c(1, 2)
x_all <- c(true_ds_pca$x[, axes[1]], neg_pca_data[, axes[1]])
y_all <- c(true_ds_pca$x[, axes[2]], neg_pca_data[, axes[2]])

pad <- 0.15  # небольшой запас
lims <- c(
  -0.4,
  0.5,
  -0.4,
  0.3
)
scale_fix <- scale_color_manual(
  name    = "Class",
  values  = my_colors,
  limits  = names(my_colors),
  breaks  = names(my_colors),   # <-- явно
  labels  = names(my_colors),   # <-- явно
  drop    = FALSE,
  na.value = "grey80"
)

p12m <- fviz_plot_ind_manual1(
  true_ds_pca,
  axes = c(1, 2),
  col_inds_f,
  title = "Positive Shapley scans",
  fixed_colors = my_colors,
  fixed_limits = lims
) + theme(legend.position = "none")
p12_negshap <- fviz_plot_ind_manual1(
  true_ds_pca, axes = c(1, 2),
  col_inds_f = col_inds_f,
  neg_pca_data = neg_pca_data,
  new_color = "neg_shap",
  title = "Positive+Negative Shapley scans",
  fixed_colors = my_colors,
  fixed_limits = lims
) + theme(legend.position = "none")
p_all <- p_scree / (p12m + p12_negshap) +
  plot_layout(
    guides = "keep"
  ) +
  plot_annotation(
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  ) +
  theme(
    axis.title   = element_text(size = 9),
    axis.text    = element_text(size = 7),
    plot.title   = element_text(size = 10),
    legend.position = "none"
  )
legend <- cowplot::get_legend(
  p12_negshap + theme(legend.position = "right")
)

(wrap_elements(p_all) | legend) + plot_layout(widths = c(12, 1))

## Add negative Shapley scans

neg32_idx <- which(true_ds[-pos_idx, "class"] == 32)
neg6_idx <- which(true_ds[-pos_idx, "class"] == 6)
new_labels <- true_ds$class[-pos_idx]

new_labels[neg32_idx] <- "neg_32"
new_labels[neg6_idx] <- "neg_6"
labels_neg_shap <- factor(
  col_inds,
  levels = c("diag_6", "diag_32", "neg_6", "neg_32")
)

my_colors_neg_shap <- c(
  "diag_6"   = "#F8766D",
  "diag_32"  = "#7CAE00",
  "neg_6" = "#00bfc4",
  "neg_32" = "#c4009a"
)
fviz_plot_ind_manual1(
  true_ds_pca, axes = c(1, 2),
  col_inds_f = labels_neg_shap,
  neg_pca_data = neg_pca_data,
  new_color = new_labels,
  title = "Positive+Negative Shapley scans",
  fixed_colors = my_colors_neg_shap,
  fixed_limits = lims
)
