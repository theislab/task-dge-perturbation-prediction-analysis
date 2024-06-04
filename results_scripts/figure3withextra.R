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
results_stability_derived <- read_tsv("results/results_stability_derived.tsv")
results_stability_scaled <- read_tsv("results/results_stability_scaled.tsv")


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
    method_id = factor(method_id, levels = method_info$method_id),
    name_wrap = factor(name_wrap, levels = custom_method_info$name_wrap)
  )

method_info <- method_info %>% slice(match(custom_method_info$method_id, method_id))
results_stability_scaled <- results_stability_scaled %>% filter(method_id %in% custom_method_info$method_id)
results_stability_derived <- results_stability_derived %>% filter(method_id %in% custom_method_info$method_id)
overall_ranking <- overall_ranking %>% slice(match(custom_method_info$method_id, method_id))

summary_all <- read_tsv("results/summary_all.tsv") %>%
  inner_join(custom_method_info, by = "method_id") %>%
  mutate(
    shape_path = paste0("plots/shapes/", shape, ".png")
  )


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

sign_log10_pval_df <- de_test$layers[["clipped_sign_log10_pval"]] %>%
  reshape2::melt(
    varnames = c("cell_type_sm", "gene_id"),
    value.name = "clipped_sign_log10_pval"
  ) %>%
  mutate(
    cell_type_sm_ix = as.integer(cell_type_sm)
  )

pred_annotated <- bind_cols(
  predictions,
  de_test$obs[predictions$cell_type_sm_ix, c("cell_type", "sm_name")]
) %>%
  left_join(
    sign_log10_pval_df %>% select(cell_type_sm_ix, gene_id, clipped_sign_log10_pval),
    by = c("cell_type_sm_ix", "gene_id")
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
g3a <- funky_heatmap(
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


##################################################
# FIGURE 3b
##################################################

df3b <- inner_join(
  method_info,
  results_stability_scaled %>% select(-is_baseline),
  by = "method_id"
) %>%
  left_join(custom_method_info, by = "method_id") %>%
  mutate(
    method_id = factor(method_id, levels = method_info$method_id),
    method_name = factor(method_name, levels = rev(method_info$method_name))
  ) %>%
  filter(metric_id == "mean_rowwise_rmse")

# compute 2 sigma error bars
df3b_stat <- df3b %>%
  group_by(method_id, method_name) %>%
  summarise(
    mean = mean(value),
    sig = 2 * sd(value) / sqrt(n()),
    .groups = "drop"
  ) %>%
  left_join(custom_method_info, by = "method_id")



# todo: add error bar
g3b <- ggplot() +
  geom_errorbarh(
    aes(xmin = mean - sig, xmax = mean + sig, y = forcats::fct_rev(name_wrap)),
    df3b_stat,
    height = .5
  ) +
  geom_point(
    aes(mean, name_wrap),
    df3b_stat,
    size = 2
  ) +
  ggbeeswarm::geom_quasirandom(
    aes(value, name_wrap, colour = color),
    df3b,
    orientation = "y"
  ) +
  theme_bw() +
  scale_color_identity() +
  labs(x = "MR RMSE", y = NULL)
g3b

##################################################
# FIGURE 3c
##################################################
df3c <- pred_annotated %>%
  filter(method_id == "nn_retraining_with_pseudolabels") %>%
  mutate(
    xbin = cut(clipped_sign_log10_pval, breaks = seq(-4, 4, length.out = 1001), labels = FALSE),
    ybin = cut(prediction, breaks = seq(-4, 4, length.out = 1001), labels = FALSE)
  ) %>%
  group_by(xbin, ybin) %>%
  mutate(
    n = n(),
    w = sqrt(1 / n)
  ) %>%
  ungroup()

df3c_sampled <- df3c %>% sample_n(50000, weight = w)

# sample based on density in (clipped_sign_log10_pval, prediction) space
g3c_tl <- ggplot() +
  geom_density(aes(clipped_sign_log10_pval), df3c, fill = "#423479") +
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "#00000000"),
    axis.ticks = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
g3c_bl <- ggplot() +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  geom_point(aes(clipped_sign_log10_pval, prediction, colour = sqrt(n)), df3c_sampled, size = .5) + #, colour = "darkgray") +
  theme_bw() +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  labs(x = "Ground truth", y = "Prediction") +
  viridis::scale_color_viridis() +
  theme(legend.position = "none")

g3c_br <- ggplot() +
  geom_density(aes(prediction), df3c, fill = "#423479") +
  theme_bw() +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "#00000000"),
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
g3c <- patchwork::wrap_plots(
  g3c_tl,
  patchwork::plot_spacer(),
  g3c_bl,
  g3c_br,
  nrow = 2,
  widths = c(1, .2),
  heights = c(.2, 1)
)
g3c
##################################################
# FIGURE 3d
##################################################

fraction_of_degs <- bind_cols(
  sign_log10_pval_df,
  de_test$obs[sign_log10_pval_df$cell_type_sm_ix, c("cell_type", "sm_name")]
) %>%
  as_tibble() %>%
  group_by(sm_name) %>%
  summarise(
    fraction_of_degs = mean(abs(clipped_sign_log10_pval) > -log10(.01)),
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
    rmse = sqrt(mean((prediction - clipped_sign_log10_pval)^2)),
    .groups = "drop_last"
  ) %>%
  summarise(
    mrrmse = mean(rmse),
    .groups = "drop"
  ) %>%
  mutate(
    sm_name = factor(sm_name, levels = fraction_of_degs$sm_name)
  ) %>%
  left_join(custom_method_info, by = "method_id")

g3d1 <- ggplot(fraction_of_degs) +
  geom_bar(aes(sm_name, fraction_of_degs), stat = "identity") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
  ) +
  labs(x = "Drug", y = "Fraction of DEGs (P-value < 0.01)")
g3d2 <- ggplot(pred_summ) +
  geom_point(aes(sm_name, mrrmse, color = color)) +
  theme_bw() +
  scale_color_identity() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "Drug", y = "MR RMSE")

g3d <- patchwork::wrap_plots(g3d1 + labs(x = NULL), g3d2, ncol = 1)

##################################################
# FIGURE 3
##################################################

# TODO: add annotations
g <- patchwork::wrap_plots(
  patchwork::wrap_plots(
    g3a,
    g3b,
    nrow = 1,
    widths = c(.6, .2),
    tag_level = "keep"
  ),
  patchwork::wrap_plots(
    g3c,
    g3d,
    nrow = 1,
    widths = c(.3, .5),
    tag_level = "keep"
  ),
  ncol = 1
)
ggsave("plots/figure3withextra.pdf", g, width = 14, height = 10)
