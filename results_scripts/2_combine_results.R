library(tidyverse)

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
  inner_join(method_info %>% select(-method_summary, -method_description), by = "method_id") %>%
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

write_tsv(summary_all, "results/summary_all.tsv")
