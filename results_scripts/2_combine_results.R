library(tidyverse)

# code interpreted from
# https://github.com/openproblems-bio/website/blob/a5412e97fc564f73c4f22ab2d6b7f076c203ba2e/results/_include/_load_data.qmd

# helper functions
source("results_scripts/helpers.R")

# read results
task_info <- jsonlite::read_json("results/task_info.json")
method_info <- jsonlite::read_json("results/method_info.json", simplifyVector = TRUE)
metric_info <- jsonlite::read_json("results/metric_info.json", simplifyVector = TRUE)
dataset_info <- jsonlite::read_json("results/dataset_info.json", simplifyVector = TRUE)
results <- jsonlite::read_json("results/results.json", simplifyVector = TRUE) %>% tibble()
qc <- jsonlite::read_json("results/quality_control.json", simplifyVector = TRUE)
stability <- yaml::read_yaml("results/stability_uns.yaml")

# normalise results
results_long <-
  results %>%
    select(method_id, dataset_id, metric_values) %>%
    unnest(metric_values) %>%
    gather(metric_id, value, any_of(metric_info$metric_id)) %>%
    mutate(value = ifelse(!is.finite(value), NA_real_, value)) %>%
    left_join(method_info %>% select(method_id, is_baseline), "method_id") %>%
    left_join(metric_info %>% select(metric_id, maximize), by = "metric_id")

results_long <- normalize_scores(results_long)$scaled

write_tsv(results_long, "results/results_scaled.tsv")

# normalise stability results
stability_long <- stability %>%
  map_dfr(as.data.frame) %>%
  as_tibble() %>%
  rename(metric_id = metric_ids, value = metric_values) %>%
  mutate(
    value = ifelse(!is.finite(value), NA_real_, value),
    bootstrap = gsub(".*[-_]bootstrap([0-9]+).*", "\\1", dataset_id),
    dataset_id = gsub("[-_]bootstrap[0-9]+", "", dataset_id)
  ) %>%
  left_join(method_info %>% select(method_id, is_baseline), "method_id") %>%
  left_join(metric_info %>% select(metric_id, maximize), by = "metric_id")

stability_long <- normalize_scores(stability_long, groups = c("dataset_id", "metric_id", "bootstrap"))$scaled

write_tsv(stability_long, "results/results_stability_scaled.tsv")

# filter data
metric_ids <- c("mean_rowwise_mae", "mean_rowwise_rmse", "mean_rowwise_cosine")
dataset_ids <- c("neurips-2023-data")
dataset_info <- dataset_info %>% filter(dataset_id %in% dataset_ids)
metric_info <- metric_info %>% filter(metric_id %in% metric_ids)
results_long <- results_long %>% filter(metric_id %in% metric_ids, dataset_id %in% dataset_ids)
stability_long <- stability_long %>% filter(metric_id %in% metric_ids, dataset_id %in% dataset_ids)


# compute derived metrics from stability scores
stability_derived <- stability_long %>%
  inner_join(results_long %>% select(method_id, dataset_id, metric_id, full_score = score), by = c("method_id", "dataset_id", "metric_id")) %>%
  group_by(method_id, metric_id, dataset_id) %>%
  summarise(
    var = var(score),
    mean = mean(score),
    # pct_diff = mean(ifelse(full_score == 0, NA_real_, (full_score - score) / full_score)),
    .groups = "drop"
  ) %>%
  gather(key = "stat", value = "value", var) %>%
  mutate(orig_metric_id = metric_id, metric_id = paste0(metric_id, "_", stat)) %>%
  mutate(maximize = c("mean" = FALSE, "var" = FALSE, "pct_diff" = FALSE)[stat]) %>%
  left_join(method_info %>% select(method_id, is_baseline), "method_id")
stability_derived <- normalize_scores(stability_derived, groups = c("dataset_id", "metric_id"), metric_value = "value")$scaled

write_tsv(stability_derived, "results/results_stability_derived.tsv")

# compute ranking
overall_ranking <-
  results_long %>%
    group_by(method_id) %>%
    summarise(overall_score = aggregate_scores(score)) %>%
    arrange(desc(overall_score))

write_tsv(overall_ranking, "results/overall_ranking.tsv")

# order by ranking
results_long$method_id <- factor(results_long$method_id, levels = rev(overall_ranking$method_id))
stability_long$method_id <- factor(stability_long$method_id, levels = rev(overall_ranking$method_id))
stability_derived$method_id <- factor(stability_derived$method_id, levels = rev(overall_ranking$method_id))
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
per_stability <- stability_derived %>%
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


# Plot stability suppfig
stability_sel <- stability_long %>%
  filter(!method_id %in% c("ground_truth", "sample", "zeros"))
results_sel <- results_long %>%
  filter(!method_id %in% c("ground_truth", "sample", "zeros"))

g <- ggplot() +
  geom_boxplot(aes(score, method_id, colour = "bootstraps"), stability_sel) +
  geom_point(aes(score, method_id, colour = "full"), results_sel, size = 2) +
  facet_wrap(~metric_id, scales = "free_x", ncol = 1) +
  theme_bw() +
  scale_colour_manual(values = c(full = "red", bootstraps = "black")) +
  labs(x = "Scaled score", y = NULL)

ggsave("plots/suppfig_stability.pdf", g, width = 10, height = 10)




# Plot stability suppfig
g <- ggplot() +
  geom_boxplot(aes(score, method_id, colour = "bootstraps"), stability_long) +
  geom_point(aes(score, method_id, colour = "full"), results_long, size = 2) +
  facet_wrap(~metric_id, scales = "free_x", ncol = 1) +
  theme_bw() +
  scale_colour_manual(values = c(full = "red", bootstraps = "black")) +
  labs(x = "Scaled score", y = NULL)

ggsave("plots/suppfig_stability_all.pdf", g, width = 10, height = 10)


# Plot stability suppfig
g <- ggplot() +
  ggbeeswarm::geom_quasirandom(aes(score, method_id, colour = "bootstraps"), stability_long, size = 1, alpha = .5) +
  geom_point(aes(score, method_id, colour = "full"), results_long, size = 2) +
  facet_wrap(~metric_id, scales = "free_x", ncol = 1) +
  theme_bw() +
  scale_colour_manual(values = c(full = "red", bootstraps = "black")) +
  labs(x = "Scaled score", y = NULL)

ggsave("plots/suppfig_stability_scatter.pdf", g, width = 10, height = 10)
