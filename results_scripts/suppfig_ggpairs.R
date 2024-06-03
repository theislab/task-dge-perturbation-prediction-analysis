library(tidyverse)


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
    left_join(method_info %>% transmute(method_id, method_type = ifelse(is_baseline, "Control", "Method")), "method_id") %>%
    left_join(metric_info %>% select(metric_id, maximize), by = "metric_id")

results_spr <- results_long %>%
  select(-maximize) %>%
  spread(metric_id, value)

g <- GGally::ggpairs(
  results_spr %>% filter(!method_id %in% c("zeros", "ground_truth", "sample")),
  columns = setdiff(colnames(results_spr), c("method_id", "method_type", "dataset_id")),
  mapping = aes(colour = method_type)
) +
  theme_bw()
ggsave("plots/ggpairs.pdf", g, width = 10, height = 10)
