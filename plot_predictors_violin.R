library(pander)
library(stringi)
library(knitr)
library(ggplot2)
library(ggpmisc)
library(data.table)
library(plyr)
library(dplyr)
library(xtable)
library(datasets)
library(latex2exp)
library(patchwork)
library(ggtext)
library(ggsci)


data_set_name <- "Positive"
gid <- 1
load(file.path(
  "~",
  paste0(data_set_name, "modelqualcv_downsmpl.RData")
))

all_preds$mz_fact <- factor(
  all_preds$MZ, levels = sort(unique(levels(all_preds$MZ)))
)
all_preds <- all_preds |>
  mutate(
    Diagnosis = as.factor(
      ifelse(class == "class6", "Glioblastoma", "Unmodified")
    )
  )
exp_mode <- unique(all_preds$exp_mode)
groups <- levels(all_preds$group)

cairo_pdf("predictors_violin.pdf", width = 8)
ggplot(all_preds |> filter(group == groups[gid])) +
  facet_grid(. ~ data_set) +
  geom_violin(aes(y = mz_fact, color = Diagnosis, x = intensity)) +
  labs(
    y = "MZ", x = "Intensity"
  ) +
  theme_bw()
dev.off()
