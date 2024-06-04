library(tidyverse)
library(funkyheatmap)

source("results_scripts/helpers.R")

# read results
overall_ranking <- read_tsv("results/overall_ranking.tsv")

custom_method_info <- tribble(
  ~method_id, ~kaggle_rank, ~shape, ~color, ~name_wrap,
  "nn_retraining_with_pseudolabels", "3", "triangle", "#de8f05", "NN retraining\nwith pseudolabels",
  "scape", "16", "pentagon", "#d55e00", "ScAPE",
  "pyboost", "18", "hexagon", "#cc78bc", "Py-boost",
  "lgc_ensemble", "1", "square", "#029e73", "LSTM-GRU-CNN\nEnsemble",
  "jn_ap_op2", "20", "circle", "#0173b2", "JN-AP-OP2",
  "mean_across_celltypes", "/", "diamond", "#ca9161", "Mean per cell\ntype and gene",
  "transformer_ensemble", "2", "star", "#fbafe4", "Transformer\nensemble"
) %>%
  mutate(
    method_id = factor(method_id, levels = overall_ranking$method_id),
    name_wrap = factor(name_wrap, levels = name_wrap)
  )

overall_ranking <- overall_ranking %>% slice(match(custom_method_info$method_id, method_id))

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
g3 <- funky_heatmap(
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
  add_abc = TRUE,
  scale_column = FALSE,
  legends = legends
)
ggsave(
  "plots/figure3.pdf",
  g3,
  width = g3$width,
  height = g3$height
)
