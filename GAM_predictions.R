setwd("~/GitHub/MRM_mini_project")
source('GAM_utils.R')
# load saved fitted models
load("C:/Users/lacko/OneDrive - epfl.ch/Modern Regression Methods/Project/gam_qp_tr_and_val.RData")

CNT_predicted <- predict.bam(gam.qp.full, df_predict, type="response", discrete=TRUE, n.threads=8)