# Purpose: To employ OSA residuals, and compare to Pearson residuals
# Creator: Matthew LH. Cheng
# Date 9/27/23

# Set up theme for ggplot
theme_reg = function() {
  theme_bw() +
    theme(axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 14),
          legend.text = element_text(color = "black", size = 12),
          legend.title = element_text(color = "black", size = 14),
          legend.background = element_blank(),
          strip.text = element_text(size = 12))
}

# Installation
# https://github.com/fishfollower/compResidual
# TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
# remotes::install_github("fishfollower/compResidual/compResidual", force=TRUE)

# Notes for Dan:
# Observed matrix needs to be in numbers (need to multiply by ESS)
# Predicted matrix can be either (probs or numbers)
# Matrices for comps for resMulti function need to be nrow = ages, ncol = years

# Set up ------------------------------------------------------------------
library(here)
library(tidyverse)
library(compResidual) 
library(cowplot)

# Read in ctl file to get weights from multinomial
source(here("Model", "Code", "residual_functions.r"))
ctl_wts <- readLines(here("Model", "Code", "tem.ctl"))
sab_curr <- dget(here("Results", "tem.rdat")) # read in observed and predicted comps
iss = 20 # input sample size
lens = seq(43, 99, 2)  # start at 43 bc dropping first bin
ages = seq(3, 31, 1) # start at 3 bc dropping first bin

# Fishery LL Age Comps ----------------------------------------------------
# get effective sample size weights
iter_wt = parse_number(ctl_wts[str_detect(ctl_wts, "#wt fish1 age comp iter")]) 

# Get fishery ages OSA
fish1_age_osa = get_osa_res(obs = sab_curr$oac.fish1, pred = sab_curr$eac.fish1, 
                               iss = iss, iter_wt = iter_wt, index = ages, drop_bin = 1)

# Get fishery ages Pearson
fish1_age_pearson = get_pearson_res(iter_wt = iter_wt, iss = iss,
                                       obs = sab_curr$oac.fish1, pred = sab_curr$eac.fish1)

# Plot OSA and Pearson Comparison
osa_plot = res_plot(data = fish1_age_osa[[1]], comp_type = "Age", res_type = "OSA w/o first bin (Fishery LL Age Comps)")
pearson_plot = res_plot(data = fish1_age_pearson, comp_type = "Age", res_type = "Pearson (Fishery LL Age Comps)")
plot_grid(osa_plot , pearson_plot, ncol = 1) 

# Domestic Survey LL Age Comps ----------------------------------------------------
# get effective sample size weights
iter_wt = parse_number(ctl_wts[str_detect(ctl_wts, "#wt surv1 age comp iter")]) 

# Get domestic survey ages OSA
srv1_age_osa = get_osa_res(obs = sab_curr$oac.srv1, pred = sab_curr$eac.srv1, 
                            iss = iss, iter_wt = iter_wt, index = ages, drop_bin = 1)

# Get domestic survey ages Pearson
srv1_age_pearson = get_pearson_res(iter_wt = iter_wt, iss = iss,
                                    obs = sab_curr$oac.srv1, pred = sab_curr$eac.srv1)

# Plot OSA and Pearson Comparison
osa_plot = res_plot(data = srv1_age_osa[[1]], comp_type = "Age", res_type = "OSA w/o first bin (Domestic LL Survey Age Comps)")
pearson_plot = res_plot(data = srv1_age_pearson, comp_type = "Age", res_type = "Pearson (Domestic LL Survey Age Comps)")
plot_grid(osa_plot , pearson_plot, ncol = 1) 

# Japanese Survey LL Age Comps ----------------------------------------------------
# get effective sample size weights
iter_wt = parse_number(ctl_wts[str_detect(ctl_wts, "#wt surv2 age comp iter")]) 

# Get Japanese Survey ages OSA
srv2_age_osa = get_osa_res(obs = sab_curr$oac.srv2, pred = sab_curr$eac.srv2, 
                           iss = iss, iter_wt = iter_wt, index = ages, drop_bin = 1)

# Get Japanese Survey ages Pearson
srv2_age_pearson = get_pearson_res(iter_wt = iter_wt, iss = iss,
                                   obs = sab_curr$oac.srv2, pred = sab_curr$eac.srv2)

# Plot OSA and Pearson Comparison
osa_plot = res_plot(data = srv2_age_osa[[1]], comp_type = "Age", res_type = "OSA w/o first bin (Japanese LL Survey Age Comps)")
pearson_plot = res_plot(data = srv2_age_pearson, comp_type = "Age", res_type = "Pearson (Japanese LL Survey Age Comps)")
plot_grid(osa_plot , pearson_plot, ncol = 1) 

# Domestic LL Survey (Length Females) -----------------------------------------------------
# get effective sample size weights
iter_wt = parse_number(ctl_wts[str_detect(ctl_wts, "#wt surv1 size comp female iter")]) 

# Get Survey length females residuals (OSA)
srv1_fem_len_osa = get_osa_res(obs = sab_curr$olc.srv1.f, pred = sab_curr$elc.srv1.f, 
                           iss = iss, iter_wt = iter_wt, index = lens, drop_bin = 1)

# Get Survey length females residuals (Pearson)
srv1_fem_len_pearson = get_pearson_res(iter_wt = iter_wt, iss = iss,
                                       obs = sab_curr$olc.srv1.f, pred = sab_curr$elc.srv1.f)

# Plot OSA and Pearson Comparison
osa_plot = res_plot(data = srv1_fem_len_osa[[1]], comp_type = "Length", res_type = "OSA w/o first bin (Domestic LL Survey Female Length Comps)")
pearson_plot = res_plot(data = srv1_fem_len_pearson, comp_type = "Length", res_type = "Pearson (Domestic LL Survey Female Length Comps)")
plot_grid(osa_plot, pearson_plot, ncol = 1)

# Domestic LL Survey (Length Males) -----------------------------------------------------
# get effective sample size weights
iter_wt = parse_number(ctl_wts[str_detect(ctl_wts, "#wt surv1 size comp male iter")]) 

# Get Survey length males residuals (OSA)
srv1_male_len_osa = get_osa_res(obs = sab_curr$olc.srv1.m, pred = sab_curr$elc.srv1.m, 
                               iss = iss, iter_wt = iter_wt, index = lens, drop_bin = 1)

# Get Survey length males residuals (Pearson)
srv1_male_len_pearson = get_pearson_res(iter_wt = iter_wt, iss = iss,
                                       obs = sab_curr$olc.srv1.m, pred = sab_curr$elc.srv1.m)

# Plot OSA and Pearson Comparison
osa_plot = res_plot(data = srv1_male_len_osa[[1]], comp_type = "Length", res_type = "OSA w/o first bin (Domestic LL Survey Male Length Comps)")
pearson_plot = res_plot(data = srv1_male_len_pearson, comp_type = "Length", res_type = "Pearson (Domestic LL Survey Male Length Comps)")
plot_grid(osa_plot, pearson_plot, ncol = 1)

