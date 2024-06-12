# Open Problems Perturbation Prediction task data analysis
This repository is a supplement to the manuscript "A benchmark for prediction of transcriptomic responses to chemical perturbations across cell types", the benchmark [github.com/openproblems-bio/task\_perturbation\_prediction](https://github.com/openproblems-bio/task_perturbation_prediction) and the platform [https://openproblems.bio/results/perturbation\_prediction](https://openproblems.bio/results/perturbation_prediction).

## Cell type annotation and initial filtering
Use the conda environment `environment.yaml`.
Execute `data_preprocessing/celltype_analysis_1.ipynb` and `data_preprocessing/celltype_analysis_2.ipynb` in sequence to reproduce the cell type annotation and filtering of the three compounds: `'CEP-18770 (Delanzomib)', 'MLN 2238', 'Oprozomib (ONX 0912)'`.

The resulting file `sc_counts_reannotated_with_counts.h5ad` is further processed on the platform [github.com/openproblems-bio/task\_perturbation\_prediction](https://github.com/openproblems-bio/task_perturbation_prediction). The analysis that justifies the resulting filtering is shown in `data_preprocessing/molecule_filtering.ipynb` .

The notebook `data_preprocessing/multiome_cell_type_annotation.ipynb` applies this same cell type annotation approach to the multimodal snRNA-seq/scATAC-seq baseline data.

## Fig1d
The umap plots shown in Fig1d are generated with `data_preprocessing/dataset_umap.ipynb`

## Fig2
`representation_analysis/representation_analysis.ipynb` reproduces the plots from figure 2

## Fig3
```
results_scripts/1_download.sh
results_scripts/2_combine_results.R
results_scripts/figure3.R
```
reproduce figure 3

## Fig9
`results_analysis/results_analysis.ipynb`, `results_analysis/results_stability_plot.ipynb` reproduce the plots from figure 9

## Supplementary table with T cell coefficient of variation
`data_preprocessing/celltype_analysis_2.ipynb`

