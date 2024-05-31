#!/bin/bash

BENCHMARK_RUN=s3://openproblems-data/resources/dge_perturbation_prediction/results/run_2024-05-30_15-51-56
STABILITY_RUN=s3://openproblems-data/resources/dge_perturbation_prediction/results/stability_2024-05-30_23-13-30

# fetch and process results
NXF_VER=23.10.0 nextflow run \
  openproblems-bio/openproblems-v2 \
  -r main_build \
  -main-script target/nextflow/common/process_task_results/run/main.nf \
  -profile docker \
  -resume \
  -latest \
  --id "process" \
  --input_scores "$BENCHMARK_RUN/score_uns.yaml" \
  --input_dataset_info "$BENCHMARK_RUN/dataset_uns.yaml" \
  --input_method_configs "$BENCHMARK_RUN/method_configs.yaml" \
  --input_metric_configs "$BENCHMARK_RUN/metric_configs.yaml" \
  --input_execution "$BENCHMARK_RUN/trace.txt" \
  --input_task_info "$BENCHMARK_RUN/task_info.yaml" \
  --output_state "state.yaml" \
  --publish_dir "results"

# fetch stability run
aws s3 cp \
  "$STABILITY_RUN/stability_uns.yaml" \
  results/stability_uns.yaml