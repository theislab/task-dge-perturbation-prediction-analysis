library(tidyverse)

source("results_scripts/helpers.R")

# read results
de_test <- anndata::read_h5ad("data/resources/neurips-2023-data/de_test.h5ad")

task_info <- jsonlite::read_json("results/task_info.json")
method_info <- jsonlite::read_json("results/method_info.json", simplifyVector = TRUE)
metric_info <- jsonlite::read_json("results/metric_info.json", simplifyVector = TRUE)
dataset_info <- jsonlite::read_json("results/dataset_info.json", simplifyVector = TRUE)
results <- jsonlite::read_json("results/results.json", simplifyVector = TRUE) %>% tibble()
qc <- jsonlite::read_json("results/quality_control.json", simplifyVector = TRUE)
stability <- yaml::read_yaml("results/stability_uns.yaml")
overall_ranking <- read_tsv("results/overall_ranking.tsv")
custom_method_info <- tribble(
  ~method_id, ~kaggle_rank, ~shape, ~color,
  "nn_retraining_with_pseudolabels", 3, "triangle", "#de8f05",
  "scape", 16, "pentagon", "#d55e00",
  "lgc_ensemble", 1, "square", "#029e73",
  "pyboost", 18, "hexagon", "#cc78bc",
  "jn_ap_op2", 20, "circle", "#0173b2",
  "transformer_ensemble", 2, "star", "#fbafe4",
  "mean_across_celltypes", NA, "diamond", "#ca9161"
)

method_info <- method_info %>% filter(method_id %in% custom_method_info$method_id)
overall_ranking <- overall_ranking %>% filter(method_id %in% custom_method_info$method_id)

summary_all <- read_tsv("results/summary_all.tsv") %>%
  inner_join(custom_method_info, by = "method_id") %>%
  mutate(
    shape_path = paste0("plots/shapes/", shape, ".png")
  )



##################################################
# FIGURE 3a
##################################################


# create column info
column_info <-
  bind_rows(
    tribble(
      ~id, ~id_color, ~name, ~group, ~geom, ~palette, ~options,
      "shape_path", NA_character_, "Shape", "method", "image", NA_character_, list(width = 1),
      "kaggle_rank", NA_character_, "LB ranking", "method", "text", NA_character_, list(width = 1),
      "method_name", NA_character_, "Name", "method", "text", NA_character_, list(width = 10, hjust = 0),
      "overall_score", "overall_score_rank", "Score", "overall", "bar", "overall", list(width = 4),
    ),
    tribble(
      ~id, ~name,
      # "metric_mean_rowwise_cosine", "Mean Rowwise Cosine",
      # "metric_mean_rowwise_mae", "Mean Rowwise MAE",
      # "metric_mean_rowwise_pearson", "Mean Rowwise Pearson",
      # "metric_mean_rowwise_rmse", "Mean Rowwise RMSE",
      # "metric_mean_rowwise_spearman", "Mean Rowwise Spearman"
      "metric_mean_rowwise_cosine", "MR Cosine",
      "metric_mean_rowwise_mae", "MR MAE",
      "metric_mean_rowwise_pearson", "MR Pearson",
      "metric_mean_rowwise_rmse", "MR RMSE",
      "metric_mean_rowwise_spearman", "MR Spearman"
    ) %>%
      mutate(
        id_color = paste0(id, "_rank"),
        group = "metric",
        geom = "funkyrect",
        palette = "metric"
      ),
    tribble(
      ~id, ~name,
      "stability_mean_rowwise_cosine_var", "Var(MR Cosine)",
      "stability_mean_rowwise_mae_var", "Var(MR MAE)",
      "stability_mean_rowwise_pearson_var", "Var(MR Pearson)",
      "stability_mean_rowwise_rmse_var", "Var(MR RMSE)",
      "stability_mean_rowwise_spearman_var", "Var(MR Spearman)"
    ) %>%
      mutate(
        id_color = paste0(id, "_rank"),
        group = "stability",
        geom = "funkyrect",
        palette = "stability"
      ),
    tribble(
      ~id, ~name, ~geom,
      "mean_cpu_pct_scaled", "%CPU", "funkyrect",
      "mean_peak_memory_log_scaled", "Peak memory", "rect",
      "mean_peak_memory_str", "", "text",
      "mean_disk_read_log_scaled", "Disk read", "rect",
      "mean_disk_read_str", "", "text",
      "mean_disk_write_log_scaled", "Disk write", "rect",
      "mean_disk_write_str", "", "text",
      "mean_duration_log_scaled", "Duration", "rect",
      "mean_duration_str", "", "text"
    ) %>% mutate(
      group = "resources",
      palette = ifelse(geom == "text", NA_character_, "resources"),
      options = map(geom, function(geom) {
        if (geom == "text") {
          list(overlay = TRUE, size = 2.5)
        } else {
          list()
        }
      })
    )
  )

# create column groups
column_groups <- tribble(
  ~group, ~palette, ~level1,
  "method", NA_character_, "",
  "overall", "overall", "Overall",
  # "dataset", "dataset", "Datasets",
  "metric", "metric", "Metrics",
  "stability", "stability", "Stability",
  "resources", "resources", "Resources"
)

# create palettes
palettes <- list(
  overall = "Greys",
  # dataset = "Blues",
  metric = "Reds",
  stability = "Greens",
  resources = "YlOrBr"
)

# create palettes
legends <- list(
  list(
    title = "Rank",
    palette = "overall",
    geom = "rect",
    labels = c("", "", "worst", "", "", "", "best", ""),
    label_hjust = rep(.5, 8),
    size = c(0, 0, 1, 1, 1, 1, 1, 0)
  ),
  list(
    title = "Scaled score",
    palette = "overall",
    geom = "funkyrect",
    labels = c("", "0", "", "", "", "0.4", "", "0.6", "", "0.8", "", "1"),
    size = c(0, seq(0, 1, by = .1)),
    label_hjust = rep(.5, 12)
  ),
  list(palette = "metric", enabled = FALSE),
  list(palette = "stability", enabled = FALSE),
  list(
    title = "Resources",
    palette = "resources",
    geom = "rect",
    labels = c("min", "", "", "", "max"),
    label_hjust = c(0, .5, .5, .5, 1),
    color = colorRampPalette(rev(funkyheatmap:::default_palettes$numerical$YlOrBr))(5),
    size = c(1, 1, 1, 1, 1)
  )
)


# create funkyheatmap
g_funky_heatmap <- funky_heatmap(
  data = summary_all,
  column_info = column_info %>% filter(id %in% colnames(summary_all)),
  column_groups = column_groups,
  palettes = palettes,
  position_args = position_arguments(
    # determine xmax expand heuristically
    expand_xmax = 2,
    # determine offset heuristically
    col_annot_offset = max(str_length(column_info$name)) / 5
  ),
  add_abc = FALSE,
  scale_column = FALSE,
  legends = legends,
)
ggsave(
  "plots/figure3a.pdf",
  g_funky_heatmap,
  width = g_all$width,
  height = g_all$height
)

# convert pdf to svg
system("pdftocairo -svg plots/figure3a.pdf plots/figure3a.svg")


##################################################
# FIGURE 3d
##################################################
predictions <- map_dfr(
  method_info$method_id,
  function(mid) {
    adata <- anndata::read_h5ad(paste0("data/predictions/neurips-2023-data.", mid, ".", mid, ".output.h5ad"))
    adata$layers[["prediction"]] %>%
      reshape2::melt(
        varnames = c("cell_type_sm_ix", "gene_id"),
        value.name = "prediction"
      ) %>%
      mutate(
        cell_type_sm_ix = cell_type_sm_ix + 1L,
        method_id = factor(mid, levels = overall_ranking$method_id)
      )
  }
) %>%
  as_tibble()

sign_log10_pval_df <- de_test$layers[["sign_log10_pval"]] %>%
  reshape2::melt(
    varnames = c("cell_type_sm", "gene_id"),
    value.name = "sign_log10_pval"
  ) %>%
  mutate(
    cell_type_sm_ix = as.integer(cell_type_sm)
  )

pred_annotated <- bind_cols(
  predictions,
  de_test$obs[predictions$cell_type_sm_ix, c("cell_type", "sm_name")]
) %>%
  left_join(
    sign_log10_pval_df %>% select(cell_type_sm_ix, gene_id, sign_log10_pval),
    by = c("cell_type_sm_ix", "gene_id")
  )

fraction_of_degs <- bind_cols(
  sign_log10_pval_df,
  de_test$obs[sign_log10_pval_df$cell_type_sm_ix, c("cell_type", "sm_name")]
) %>%
  as_tibble() %>%
  group_by(sm_name) %>%
  summarise(
    fraction_of_degs = mean(abs(sign_log10_pval) > -log10(.01)),
    .groups = "drop"
  ) %>%
  arrange(fraction_of_degs) %>%
  mutate(
    sm_name = factor(sm_name, levels = sm_name)
  )
pred_summ <-
  pred_annotated %>%
  group_by(method_id, sm_name, cell_type) %>%
  summarise(
    rmse = sqrt(mean((prediction - sign_log10_pval)^2)),
    .groups = "drop_last"
  ) %>%
  summarise(
    mrrmse = mean(rmse),
    .groups = "drop"
  ) %>%
  mutate(
    sm_name = factor(sm_name, levels = fraction_of_degs$sm_name)
  )

g4d1 <- ggplot(fraction_of_degs) +
  geom_bar(aes(sm_name, fraction_of_degs), stat = "identity") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
  ) +
  labs(x = "Drug", y = "Fraction of DEGs (P-value < 0.01)")
g4d2 <- ggplot(pred_summ) +
  geom_point(aes(sm_name, mrrmse, color = method_id, shape = method_id)) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "Drug", y = "MR RMSE")
patchwork::wrap_plots(g4d1, g4d2, ncol = 1)












