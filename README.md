# open-problems-23-sc-perturbation-prediction reproducibility

## Cell type annotation and initial filtering
Use the conda environment `environment.yaml`.
Execute `data_preprocessing/celltype_analysis_1.ipynb` and `data_preprocessing/celltype_analysis_2.ipynb` in sequence to reproduce the cell type annotation and filtering of the three compounds: `'CEP-18770 (Delanzomib)', 'MLN 2238', 'Oprozomib (ONX 0912)'`.

The resulting file `sc_counts_reannotated_with_counts.h5ad` is further processed on the platform [github.com/openproblems-bio/task\_perturbation\_prediction](https://github.com/openproblems-bio/task_perturbation_prediction).

## Fig1d
The umap plots shown in Fig1d are generated with `data_preprocessing/dataset_umap.ipynb`

## Fig2
`representation_analysis/representation_analysis.ipynb` reproduces the plots from figure 2

## Fig9
`results_analysis/results_analysis.ipynb` and `results_analysis/results_stability_plot.ipynb` reproduces the plots from figure 9