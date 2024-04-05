rm(list=ls())
library(dplR)
load("dataVault/process_itrdb_has_run.Rdata")
load("dataVault/rwls.Rdata")
rm(itrdb_crn)
rm(itrdb_meta)
mask_gt_1 <- rwls_meta$RWL_Count == 1
summary(mask_gt_1)
meta2 <- rwls_meta[mask_gt_1,]
rwls2 <- rwls[mask_gt_1]
