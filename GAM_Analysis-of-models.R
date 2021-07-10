# # GAM Analysis of models

# ### Load fitted models and run GAM_utils.R
setwd("~/GitHub/MRM_mini_project")
source('GAM_utils.R')
# load saved fitted models
load("C:/Users/lacko/OneDrive - epfl.ch/Modern Regression Methods/Project/models_full_dataset.RData")
load("C:/Users/lacko/OneDrive - epfl.ch/Modern Regression Methods/Project/gam_qp_tr_and_val.RData")


# ## Poisson GAM
diagnostics.gam(gam.poiss, df_val, 'poiss')


# ## Negative Binomial GAM
diagnostics.gam(gam.NB, df_val, 'NB')


# ## Quasipoisson GAM
diagnostics.gam(gam.qp, df_val, 'qp')


# ## Quasipoisson GAM - full model
diagnostics.gam(gam.qp.full, NA, 'qp_full')


# ### Plots (individually)
p <- plot(gam.qp.full, scale=0, pers=FALSE, se=TRUE, scheme=0)


# ### Plots (more plots on one page)
plot(gam.qp.full, scale=0, scheme=1, pers=FALSE, se=TRUE, pages=5, family="B")


# Location smoothing separately
plot(gam.qp.full, scale=0, pers=FALSE, se=TRUE, scheme=0, select=1, theta=50)


# ### Show the same plots using ggplot2
# extract data from plots
df_smooths <- data.frame(x=double(),
                         fit=double(),
                         se=double(),
                         xlab=character(),
                         ylab=character())

df_ti <- data.frame(x=double(),
                    y=double(),
                    fit=double(),
                    se=double(),
                    xlab=character(),
                    ylab=character(),
                    main=character())

z <- 1.96
for (i in 1:length(p)){
  if (i == 1){
    df_la_lon <- data.frame(x=p[[i]]$lo,
                            y=p[[i]]$la,
                            fit=p[[i]]$fit,
                            xlab="latitude",
                            ylab="longitude",
                            main="position")
  } else if ((i > 1 & i < 15) | (i > 20 & i < 32)){
    d <- data.frame(x=p[[i]]$x,
                    fit=p[[i]]$fit,
                    se=p[[i]]$se,
                    xlab=p[[i]]$xlab,
                    ylab=p[[i]]$ylab)
    
    df_smooths <- rbind(df_smooths, d)
  } else {
    d <- data.frame(x=rep(p[[i]]$x, 40),
                    y=as.vector(t(matrix(rep(p[[i]]$y, 40), nrow=40))),
                    fit=p[[i]]$fit,
                    se=p[[i]]$se,
                    xlab=p[[i]]$xlab,
                    ylab=p[[i]]$ylab,
                    main=p[[i]]$main)

    df_ti <- rbind(df_ti, d)
  }
}
df_ti <- df_ti %>% mutate(main=gsub(',', ', ', main))


# Plot individual smooths of main effects


df_smooths %>% 
    ggplot() + 
    geom_line(aes(x=x, y=fit), colour="black") + 
    geom_ribbon(aes(x=x, ymin=fit-z*se, ymax=fit+z*se), alpha=0.25) + 
    facet_wrap(~ylab, scales="free", ncol=4) + 
    theme_minimal() + xlab("variable, edf") + ylab("s(variable) +/- 1.96 SE") + 
    theme(aspect.ratio = 1/2, text=element_text(size=6)) #+ 
#     guides(x = guide_axis(angle = 90))
# ggsave('Figures/smooths.pdf')


# Contour plots for tensor product smooth interactions


df_ti %>% 
    ggplot(aes(x, y)) + 
    geom_contour_filled(aes(z=fit), colour="black", na.rm=TRUE, size=0.25) + 
    geom_contour(aes(z=fit+z*se), colour="red", na.rm=TRUE, size=0.25) + 
    geom_contour(aes(z=fit-z*se), colour="blue", na.rm=TRUE, size=0.25) + 
    facet_wrap(~main, scales="free", ncol=4, labeller = labeller(main=label_wrap_gen(10))) + 
    theme_minimal() + ylab("var2") + xlab("var1") + 
    theme(aspect.ratio = 1/2, text=element_text(size=4))
# ggsave('Figures/Interactions.pdf')


