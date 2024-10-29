import anndata as ad
import pandas as pd
import cpa
import scanpy as sc
import numpy as np
import torch
torch.set_float32_matmul_precision('medium')

## VIASH START
par = {
  'sc_train_h5ad': '../data/neurips-2023-data/sc_train.h5ad',
  'sc_test_h5ad': '../data/neurips-2023-data/sc_test.h5ad',
  'output_sc': '../data/output/output_sc.h5ad',
}
meta = {
  'name': 'cpa'
}
## VIASH END

ae_hparams = {'n_latent': 64,
 'recon_loss': 'gauss',
 'doser_type': 'linear',
 'n_hidden_encoder': 256,
 'n_layers_encoder': 3,
 'n_hidden_decoder': 512,
 'n_layers_decoder': 2,
 'use_batch_norm_encoder': True,
 'use_layer_norm_encoder': False,
 'use_batch_norm_decoder': True,
 'use_layer_norm_decoder': False,
 'dropout_rate_encoder': 0.25,
 'dropout_rate_decoder': 0.25,
 'variational': False,
 'seed': 6478}

trainer_params = {'n_epochs_kl_warmup': None,
 'n_epochs_pretrain_ae': 50,
 'n_epochs_adv_warmup': 100,
 'n_epochs_mixup_warmup': 10,
 'mixup_alpha': 0.1,
 'adv_steps': None,
 'n_hidden_adv': 128,
 'n_layers_adv': 3,
 'use_batch_norm_adv': False,
 'use_layer_norm_adv': False,
 'dropout_rate_adv': 0.2,
 'reg_adv': 10.0,
 'pen_adv': 0.1,
 'lr': 0.0003,
 'wd': 4e-07,
 'adv_lr': 0.0003,
 'adv_wd': 4e-07,
 'adv_loss': 'cce',
 'doser_lr': 0.0003,
 'doser_wd': 4e-07,
 'do_clip_grad': False,
 'gradient_clip_value': 1.0,
 'step_size_lr': 10}

print('Reading input files', flush=True)
sc_train_h5ad = ad.read_h5ad(par['sc_train_h5ad'])
sc_test_h5ad = ad.read_h5ad(par['sc_test_h5ad'])

# subsample for testing
sc.pp.subsample(sc_train_h5ad, n_obs=10000)
sc.pp.subsample(sc_test_h5ad, n_obs=10000)

# remove the counts from the test set to prevent leakage
sc_test_h5ad.X[:] = 0

print('Preprocess data for CPA', flush=True)
sc_h5ad = ad.concat([sc_train_h5ad, sc_test_h5ad], axis=0)
sc_h5ad.obs['control'] = sc_h5ad.obs['sm_name'].eq("Dimethyl Sulfoxide").astype(int)
sc_h5ad.layers["counts"] = sc_h5ad.X.copy()
sc.pp.normalize_total(sc_h5ad, target_sum=1e4)
sc.pp.log1p(sc_h5ad)
cpa.CPA.setup_anndata(sc_h5ad,
                      perturbation_key='sm_name',
                      dosage_key='dose_uM',
                      control_group='Dimethyl Sulfoxide',
                      batch_key="donor_id",
                      smiles_key='SMILES',
                      is_count_data=False,
                      categorical_covariate_keys=['cell_type'],
                      max_comb_len=1,
                     )

print('Train model', flush=True)
model = cpa.CPA(sc_h5ad,
                split_key="split",
                train_split="train",
                test_split="test",
                use_rdkit_embeddings=True,
                **ae_hparams)

model.train(max_epochs=2, #do 2000 for real training
            use_gpu=True,
            batch_size=512,
            plan_kwargs=trainer_params,
           )
model.save("../data/cpa2", save_anndata=True, overwrite=True)