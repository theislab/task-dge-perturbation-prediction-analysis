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
label_time <- function(time) {
  case_when(
    time < 1e-5 ~ "0s",
    time < 1 ~ "<1s",
    time < 60 ~ paste0(floor(time), "s"),
    time < 3600 ~ paste0(floor(time / 60), "m"),
    time < 3600 * 24 ~ paste0(floor(time / 3600), "h"),
    time < 3600 * 24 * 7 ~ paste0(floor(time / 3600 / 24), "d"),
    !is.na(time) ~ ">7d",
    TRUE ~ NA_character_
  )
}
label_memory <- function(x, include_mb = FALSE) {
  case_when(
    x < 1e9 ~ "<1G",
    x < 1e12 ~ paste0(round(x / 1e9), "G"),
    !is.na(x) ~ ">1T",
    TRUE ~ NA_character_
  )
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
metric_ids <- c("mean_rowwise_mae_r", "mean_rowwise_rmse_clipped_0001_r", "mean_cosine_sim_r")
dataset_ids <- c("neurips-2023-data")

dataset_info <- dataset_info %>% filter(dataset_id %in% dataset_ids)
metric_info <- metric_info %>% filter(metric_id %in% metric_ids)
results_long <- results_long %>% filter(metric_id %in% metric_ids, dataset_id %in% dataset_ids)
stability_long <- stability_long %>% filter(orig_metric_id %in% metric_ids, dataset_id %in% dataset_ids)

# compute ranking
overall_ranking <-
  results_long %>%
    group_by(method_id) %>%
    summarise(overall_score = aggregate_scores(score)) %>%
    arrange(desc(overall_score))

# order by ranking
results_long$method_id <- factor(results_long$method_id, levels = rev(overall_ranking$method_id))
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
  mutate(metric_id = paste0("stability_", metric_id)) %>%
  spread(metric_id, score)

# resources
results_resources <- results %>%
  select(method_id, dataset_id, resources) %>%
  unnest(resources)

resources <- results_resources %>%
  group_by(method_id) %>%
  summarise(
    error_pct_oom = mean(exit_code %|% 0 %in% c(137)),
    error_pct_timeout = mean(exit_code %|% 0 %in% c(143)),
    error_pct_error = mean(exit_code %|% 0 != 0) - error_pct_oom - error_pct_timeout,
    error_pct_ok = 1 - error_pct_oom - error_pct_timeout - error_pct_error,
    error_reason = list(c(
      "Memory limit exceeded" = error_pct_oom,
      "Time limit exceeded" = error_pct_timeout,
      "Execution error" = error_pct_error,
      "No error" = error_pct_ok
    )),
    mean_cpu_pct = mean(cpu_pct, na.rm = TRUE),
    mean_peak_memory_b = mean(peak_memory_mb, na.rm = TRUE) * 1000,
    mean_peak_memory_log = -log10(mean_peak_memory_b),
    mean_peak_memory_str = label_memory(mean_peak_memory_b * 1000),
    mean_disk_read_b = mean(disk_read_mb, na.rm = TRUE) * 1000,
    mean_disk_read_log = -log10(mean_disk_read_b),
    mean_disk_read_str = label_memory(mean_disk_read_b * 1000),
    mean_disk_write_mb = mean(disk_write_mb, na.rm = TRUE) * 1000,
    mean_disk_write_log = -log10(mean_disk_write_mb),
    mean_disk_write_str = label_memory(mean_disk_write_mb * 1000),
    mean_duration_sec = mean(duration_sec %|% 0),
    mean_duration_log = -log10(mean_duration_sec),
    mean_duration_str = label_time(mean_duration_sec),
    .groups = "drop"
  ) %>%
  mutate_at(vars(ends_with("_str")), function(x) paste0(" ", x, " "))

# combine into summary table
summary_all <-
  overall_ranking %>%
  filter(!method_id %in% c("sample", "zeros", "ground_truth")) %>%
  inner_join(method_info %>% transmute(method_id, method_name, is_control_str = ifelse(is_baseline, "Yes", "No")), by = "method_id") %>%
  left_join(per_dataset, by = "method_id") %>%
  left_join(per_metric, by = "method_id") %>%
  left_join(per_stability, by = "method_id") %>%
  left_join(resources, by = "method_id")

for (col in colnames(summary_all)) {
  if (is.numeric(summary_all[[col]])) {
    summary_all[[paste0(col, "_scaled")]] <- dynutils::scale_minmax(summary_all[[col]])
    summary_all[[paste0(col, "_rank")]] <- dynutils::scale_minmax(rank(summary_all[[col]]))
  }
}

# create column info
column_info <-
  bind_rows(
    tribble(
      ~id, ~id_color, ~name, ~group, ~geom, ~palette, ~options,
      "method_name", NA_character_, "Name", "method", "text", NA_character_, list(width = 10, hjust = 0),
      "is_control_str", NA_character_, "Is control?", "method", "text", NA_character_, list(width = 2),
      "overall_score", "overall_score_rank", "Score", "overall", "bar", "overall", list(width = 4),
    ),
    metric_info %>% transmute(
      id = paste0("metric_", metric_id, ""),
      id_color = paste0("metric_", metric_id, "_rank"),
      name = metric_name,
      group = "metric",
      geom = "funkyrect",
      palette = "metric"
    ),
    tibble(
      col_id = sort(unique(stability_long$metric_id)),
      id = paste0("stability_", col_id),
      id_color = paste0("stability_", col_id, "_rank"),
      name = stringr::str_to_title(gsub("_", " ", gsub("_r_", "_", col_id))),
      group = "stability",
      geom = "funkyrect",
      palette = "stability"
    ) %>% select(-col_id),
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
# legend examples:
# funkyheatmap::verify_legends(legends, palettes, column_info, summary_all)

# create funkyheatmap
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
  scale_column = FALSE,
  legends = legends,
)
ggsave(
  "plots/figure3a.pdf",
  g_all,
  width = g_all$width,
  height = g_all$height
)

