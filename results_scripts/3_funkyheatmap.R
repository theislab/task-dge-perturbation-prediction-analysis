library(tidyverse)
library(funkyheatmap)
library(kableExtra)

# code interpreted from
# https://github.com/openproblems-bio/website/blob/a5412e97fc564f73c4f22ab2d6b7f076c203ba2e/results/_include/_load_data.qmd

# helper functions
`%|%` <- function(x, y) {
  ifelse(is.na(x), y, x)
}
aggregate_scores <- function(score) {
  mean(pmin(1, pmax(0, score)) %|% 0)
}

# read results
params <- list(data_dir = "results")
task_info <- jsonlite::read_json(paste0(params$data_dir, "/task_info.json"))
method_info <- jsonlite::read_json(paste0(params$data_dir, "/method_info.json"), simplifyVector = TRUE)
metric_info <- jsonlite::read_json(paste0(params$data_dir, "/metric_info.json"), simplifyVector = TRUE)
dataset_info <- jsonlite::read_json(paste0(params$data_dir, "/dataset_info.json"), simplifyVector = TRUE)
results <- jsonlite::read_json(paste0(params$data_dir, "/results.json"), simplifyVector = TRUE) %>% tibble()
qc <- jsonlite::read_json(paste0(params$data_dir, "/quality_control.json"), simplifyVector = TRUE)
stability <- yaml::read_yaml(paste0(params$data_dir, "/stability_uns.yaml"))

# transform results
results_long <-
  results %>%
    select(method_id, dataset_id, metric_values) %>%
    unnest(metric_values) %>%
    gather(metric_id, value, any_of(metric_info$metric_id)) %>%
    mutate(value = ifelse(!is.finite(value), NA_real_, value)) %>%
    left_join(method_info %>% select(method_id, is_baseline), "method_id") %>%
    left_join(metric_info %>% select(metric_id, maximize), by = "metric_id")

# compute scaling
norm <- results_long %>%
  group_by(dataset_id, metric_id) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  )

results_long <- results_long %>%
  left_join(norm, by = c("dataset_id", "metric_id")) %>%
  mutate(
    score = (value - min) / (max - min),
    score = ifelse(maximize, score, 1 - score),
    score = score %|% 0
  )

# process stability results
stability_long <- stability %>%
  map_dfr(as.data.frame) %>%
  as_tibble() %>%
  rename(metric_id = metric_ids, value = metric_values) %>%
  mutate(
    value = ifelse(!is.finite(value), NA_real_, value),
    bootstrap = gsub(".*[-_]bootstrap([0-9]+).*", "\\1", dataset_id),
    dataset_id = gsub("[-_]bootstrap[0-9]+", "", dataset_id)
  ) %>%
  left_join(metric_info %>% select("metric_id", "maximize"), by = "metric_id") %>%
  left_join(norm, by = c("dataset_id", "metric_id")) %>%
  mutate(
    score = (value - min) / (max - min),
    score = ifelse(maximize, score, 1 - score),
    score = score %|% 0
  )

# compute metrics from stability_long
stability_long <- stability_long %>%
  left_join(results_long %>% select(method_id, dataset_id, metric_id, full_score = score), by = c("method_id", "dataset_id", "metric_id")) %>%
  group_by(method_id, metric_id, dataset_id) %>%
  summarise(
    var = var(score),
    pct_diff = mean((full_score - score) / full_score),
    .groups = "drop"
  ) %>%
  gather(key = "stat", value = "value", var, pct_diff) %>%
  mutate(orig_metric_id = metric_id, metric_id = paste0(metric_id, "_", stat)) %>%
  mutate(maximize = c("var" = FALSE, "pct_diff" = FALSE)[stat]) %>%
  left_join(method_info %>% select(method_id, is_baseline), "method_id")
stability_norm <- stability_long %>%
  filter(is_baseline) %>%
  group_by(dataset_id, metric_id) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  )
stability_long <- stability_long %>%
  left_join(stability_norm, by = c("dataset_id", "metric_id")) %>%
  group_by(dataset_id, metric_id) %>%
  mutate(
    score = (value - min) / (max - min),
    score = ifelse(maximize, score, 1 - score),
    score = score %|% 0
  ) %>%
  ungroup()


# filter metrics
metric_ids <- c("mean_rowwise_mae_r", "mean_cosine_sim_r")
metric_info <- metric_info %>% filter(metric_id %in% metric_ids)
results_long <- results_long %>% filter(metric_id %in% metric_ids)
stability_long <- stability_long %>% filter(orig_metric_id %in% metric_ids)

# combine stability results info main results
stab_met_id <- unique(stability_long$metric_id)
stability_info <- tibble(
  metric_id = stab_met_id,
  metric_name = gsub("_", " ", stab_met_id),
  maximize = FALSE
)

# compute ranking
overall_ranking <-
  results_long %>%
    group_by(method_id) %>%
    summarise(benchmark_score = aggregate_scores(score)) %>%
  left_join(
    stability_long %>%
      group_by(method_id) %>%
      summarise(stability = aggregate_scores(score)),
    by = "method_id"
  ) %>%
  mutate(
    overall_score = (benchmark_score + stability) / 2
  ) %>%
  arrange(desc(benchmark_score))

# order by ranking
results_long$method_id <- factor(results_long$method_id, levels = rev(overall_ranking$method_id))
# results$method_id <- factor(results$method_id, levels = rev(overall_ranking$method_id))
method_info$method_id <- factor(method_info$method_id, levels = rev(overall_ranking$method_id))

# create aggregated results
per_dataset <- results_long %>%
  group_by(method_id, dataset_id) %>%
  summarise(score = aggregate_scores(score), .groups = "drop") %>%
  mutate(dataset_id = paste0("dataset_", dataset_id)) %>%
  spread(dataset_id, score)
per_metric <- results_long %>%
  group_by(method_id, metric_id) %>%
  summarise(score = aggregate_scores(score), .groups = "drop") %>%
  mutate(metric_id = paste0("metric_", metric_id)) %>%
  spread(metric_id, score)
per_stability <- stability_long %>%
  group_by(method_id, metric_id) %>%
  summarise(score = aggregate_scores(score), .groups = "drop") %>%
  mutate(metric_id = paste0("metric_", metric_id)) %>%
  spread(metric_id, score)

# combine into summary table
summary_all <-
  method_info %>%
  filter(!method_id %in% c("sample", "zeros", "ground_truth")) %>%
  select(method_id, method_name) %>%
  inner_join(overall_ranking, by = "method_id") %>%
  left_join(per_dataset, by = "method_id") %>%
  left_join(per_metric, by = "method_id") %>%
  left_join(per_stability, by = "method_id") %>%
  arrange(desc(method_id))

# create column info
column_info <-
  bind_rows(
    tribble(
      ~id, ~name, ~group, ~geom, ~palette,
      "method_name", "Name", "method", "text", NA_character_,
      "overall_score", "Score", "overall", "bar", "overall",
      "benchmark_score", "Benchmark", "overall", "bar", "overall",
      "stability", "Stability", "overall", "bar", "overall",
    ),
    dataset_info %>% transmute(
      id = paste0("dataset_", dataset_id),
      name = dataset_name,
      group = "dataset",
      geom = "funkyrect",
      palette = "dataset"
    ),
    metric_info %>% transmute(
      id = paste0("metric_", metric_id),
      name = metric_name,
      group = "metric",
      geom = "funkyrect",
      palette = "metric"
    ),
    stability_info %>% transmute(
      id = paste0("metric_", metric_id),
      name = metric_name,
      group = "stability",
      geom = "funkyrect",
      palette = "stability"
    )
  ) %>%
  mutate(
    options = map2(id, geom, function(id, geom) {
      if (id == "method_name") {
        list(width = 15, hjust = 0)
      } else if (id == "is_baseline") {
        list(width = 1)
      } else if (geom == "bar") {
        list(width = 4)
      } else {
        list()
      }
    }
  )
)

# create column groups
column_groups <- tribble(
  ~group, ~palette, ~level1,
  "method", NA_character_, "",
  "overall", "overall", "Overall",
  "dataset", "dataset", "Datasets",
  "metric", "metric", "Metrics",
  "stability", "stability", "Stability"
)

# create palettes
palettes <- list(
  overall = "Greys",
  dataset = "Blues",
  metric = "Reds",
  stability = "YlOrBr"
)

g_all <- funky_heatmap(
  data = summary_all,
  column_info = column_info %>% filter(id %in% colnames(summary_all)),
  column_groups = column_groups,
  palettes = palettes,
  position_args = position_arguments(
    # determine xmax expand heuristically
    expand_xmax = max(str_length(tail(column_info$name, 4))) / 5,
    # determine offset heuristically
    col_annot_offset = max(str_length(column_info$name)) / 5
  ),
  add_abc = FALSE,
  scale_column = FALSE
)
g_all
