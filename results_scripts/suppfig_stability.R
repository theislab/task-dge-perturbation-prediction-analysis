library(tidyverse)
library(funkyheatmap)
library(kableExtra)

# read results
params <- list(data_dir = "results")
# task_info <- jsonlite::read_json(paste0(params$data_dir, "/task_info.json"))
# method_info <- jsonlite::read_json(paste0(params$data_dir, "/method_info.json"), simplifyVector = TRUE)
metric_info <- jsonlite::read_json(paste0(params$data_dir, "/metric_info.json"), simplifyVector = TRUE)
# dataset_info <- jsonlite::read_json(paste0(params$data_dir, "/dataset_info.json"), simplifyVector = TRUE)
results <- jsonlite::read_json(paste0(params$data_dir, "/results.json"), simplifyVector = TRUE) %>% tibble()
# qc <- jsonlite::read_json(paste0(params$data_dir, "/quality_control.json"), simplifyVector = TRUE)
stability <- yaml::read_yaml(paste0(params$data_dir, "/stability_uns.yaml"))

# based on the figure4b result
method_ids <- c(
  "ground_truth",
  "nn_retraining_with_pseudolabels",
  "pyboost",
  "lgc_ensemble",
  "scape",
  "jn_ap_op2",
  "mean_across_compounds",
  "transformer_ensemble",
  "mean_outcome",
  "mean_across_celltypes",
  "zeros",
  "sample"
)
dataset_ids <- c("neurips-2023-data")

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
  mutate(method_id = factor(method_id, levels = rev(method_ids)))
results_long <-
  results %>%
    select(method_id, dataset_id, metric_values) %>%
    unnest(metric_values) %>%
    gather(metric_id, value, any_of(metric_info$metric_id)) %>%
    mutate(value = ifelse(!is.finite(value), NA_real_, value)) %>%
    mutate(method_id = factor(method_id, levels = rev(method_ids)))


metric_ids <- unique(stability_long$metric_id) %>%
  .[grep("_r", .)]

stability_sel <- stability_long %>%
  filter(
    !method_id %in% c("ground_truth", "sample", "zeros"),
    metric_id %in% metric_ids,
    dataset_id %in% dataset_ids
  )
results_sel <- results_long %>%
  filter(
    !method_id %in% c("ground_truth", "sample", "zeros"),
    metric_id %in% metric_ids,
    dataset_id %in% dataset_ids
  )

g <- ggplot(stability_sel) +
  geom_boxplot(aes(value, method_id)) +
  geom_point(aes(value, method_id), results_sel, colour = "red", size = 2) +
  facet_wrap(~metric_id, scales = "free_x") +
  theme_bw()

ggsave("plots/suppfig_stability.pdf", g, width = 12, height = 10)
