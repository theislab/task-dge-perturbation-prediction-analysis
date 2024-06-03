library(tidyverse)
library(funkyheatmap)

summary_all <- read_tsv("results/summary_all.tsv") %>%
  mutate(
    is_control_str = case_when(
      !is_baseline ~ "",
      method_id == "ground_truth" ~ "+",
      TRUE ~ "-"
    )
  )

# create column info
column_info <-
  bind_rows(
    tribble(
      ~id, ~id_color, ~name, ~group, ~geom, ~palette, ~options,
      "method_name", NA_character_, "Name", "method", "text", NA_character_, list(width = 10, hjust = 0),
      "is_control_str", NA_character_, "Control method", "method", "text", NA_character_, list(width = 2),
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
    labels = c("worst", " ", "", " ", "best"),
    size = c(1, 1, 1, 1, 1)
  ),
  list(
    title = "Scaled score",
    palette = "overall",
    geom = "funkyrect",
    labels = c("0", "", "", "", "0.4", "", "0.6", "", "0.8", "", "1"),
    size = seq(0, 1, by = .1)
  ),
  list(palette = "metric", enabled = FALSE),
  list(palette = "stability", enabled = FALSE),
  list(palette = "resources", enabled = FALSE)
)

# create funkyheatmap
g_all <- funky_heatmap(
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
  scale_column = TRUE,
  legends = legends,
)
ggsave(
  "plots/figure4b.pdf",
  g_all,
  width = g_all$width,
  height = g_all$height
)

  

# create funkyheatmap
g_all <- funky_heatmap(
  data = summary_all %>% filter(!method_id %in% c("sample", "zeros", "ground_truth")),
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
  scale_column = TRUE,
  legends = legends,
)
ggsave(
  "plots/figure4b_alt.pdf",
  g_all,
  width = g_all$width,
  height = g_all$height
)


# create funkyheatmap
g_all1 <- funky_heatmap(
  data = summary_all %>% filter(!method_id %in% c("sample", "zeros", "ground_truth", "mean_outcome", "mean_across_compounds")),
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
g_all2 <- funky_heatmap(
  data = summary_all %>% filter(!method_id %in% c("sample", "zeros", "ground_truth", "mean_outcome", "mean_across_compounds")),
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
  scale_column = TRUE,
  legends = legends,
)
ggsave(
  "plots/figure4b_altbis.pdf",
  patchwork::wrap_plots(g_all1, patchwork::plot_spacer(), g_all2, ncol = 1, heights = c(1, .05, 1)),
  width = max(g_all1$width, g_all2$width),
  height = (g_all1$height + g_all2$height) / 2 * 2.05
)
