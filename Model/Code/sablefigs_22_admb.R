# Purpose: To update the graphics code base for the sablefish assessment
# Creator: Matthew LH. Cheng (UAF-CFOS)
# Date 9/27/23

# Set up ------------------------------------------------------------------
library(here)
library(tidyverse)
library(reshape2)
library(data.table)
library(R2admb)

# Read in .rdat
sab_curr <- dget(here("Results", "tem.rdat")) 
sab_rep <- readLines(here("Results", "sable.rep"))
ages = 2:31

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

# Likelihood components ---------------------------------------------------
# Likelihood component names
like_names<-c("LL Fish Age","LL Srv Age", "LL Fish Size_F","LL Fish Size_M","TRWL Fish Size_F","TRWL Fish Size_M",
              "LL Srv Size_F","LL Srv Size_M","Coop Srv Size_F","Coop Srv Size_M", "TRWL Srv Size_F","TRWL Srv Size_M",
              "LL Srv RPN","Coop Srv RPN","LL CPUE RPN", "JPN CPUE RPN", "Trawl Survey RPW","Catch",
              "Recruit_Pen","F_Pen","M_Prior")     

likecomps = sab_curr$likecomp # extract out likelihood components
likecomps_df = data.frame(nLL = likecomps, component = names(likecomps)) # dataframe for plotting

# Do some residual munging (getting rid of data not fit to and objfun, and assign groups to components)
likecomps_df = likecomps_df %>% 
  filter(nLL != 0, component != "obj.fun") %>% 
  mutate(component = like_names,
         # Setting up grouping structure for different likelihood types
         groups = case_when(
           str_detect(component, "Catch") ~ "Catch",
           str_detect(component, "Age") ~ "Age Comps",
           str_detect(component, "Size") ~ "Length Comps",
           str_detect(component, "RPW") ~ "Survey Indices",
           str_detect(component, "RPN") ~ "Survey Indices",
           str_detect(component, "CPUE") ~ "CPUE",
           str_detect(component, "Pen") ~ "Penalties",
           str_detect(component, "Prior") ~ "Penalties"),
         groups = factor(groups, levels = c("Age Comps", "Length Comps", "Survey Indices", "CPUE",
                                            "Catch", "Penalties"))) # set up order of plotting here

# Plot likelihood components
pdf(here("Results", "Like_Comps.pdf"))
ggplot(likecomps_df, aes(x = component, y = nLL, fill = groups)) +
  geom_col() +
  labs(x = "Likelihood Component", y = "Likelihood", fill = "") +
  scale_x_discrete(guide = guide_axis(angle = 90), limits = unique(likecomps_df$component))  +
  theme_reg() +
  theme(legend.position = c(0.85, 0.85))
dev.off()
  
# Fits to indices ---------------------------------------------------------
idx_names = c("Domestic LL Survey Relative Population Weight", "Japanese LL Survey Relative Population Weight",
              "Domestic LL Survey Relative Population Numbers", "Japanese LL Survey Relative Population Numbers",
              "Domestic Fishery CPUE Index", "Japanese Fishery CPUE Index", "GOA Trawl Survey Biomass (kt)") # index names

# extract out index data
idx_df = data.frame()
idx_list = sab_curr[str_detect(names(sab_curr), "obssrv")]

# Loop through to extract stuff out
for(i in 1:length(idx_list)) {
  # extract out index dataframe (just extracting out components from a list)
  idx_tmp = data.frame(year = as.numeric(rownames(idx_list[[i]])), obs = idx_list[[i]][[1]],
                       lci = idx_list[[i]][[2]], uci = idx_list[[i]][[3]],
                       pred = idx_list[[i]][[4]], type = idx_names[i])
  # bind together
  idx_df = rbind(idx_df, idx_tmp)
} # end i loop

# relvel by index name
idx_df$type = factor(idx_df$type, levels = idx_names)

# Now plot index data
pdf(here("Results", "Index_Fits.pdf"), width = 13, height = 8)
ggplot(idx_df) +
  geom_line(mapping = aes(x = year, y = pred), size = 1.5, col = "red") +
  geom_pointrange(mapping = aes(x = year, y = obs, ymin = lci, ymax = uci), 
                  alpha = 0.75, size = 0.75, col = "blue") +
  facet_wrap(~type, scales = "free") +
  scale_y_continuous(limits = c(0, NA)) +  # NA for no upper limit
  labs(x = "Year", y = "Index") +
  theme_reg() +
  theme(legend.position = "top")
dev.off()

# Fits to compositions ---------------------------------------------------------
pdf(here("Results", "Composition_Fits.pdf"), width = 10, height = 20)
### Domestic LL Fishery Age Compositions -------------------------------
obs_ac_fish1 = data.frame(reshape2::melt(sab_curr$oac.fish1), type = "obs")
pred_ac_fish1 = data.frame(reshape2::melt(sab_curr$eac.fish1), type = "pred")

# Put these into a dataframe
ac_fish1 = rbind(obs_ac_fish1, pred_ac_fish1)
names(ac_fish1) = c("year", "age", "prop", "type")
ac_fish1$broodYear = ac_fish1$year - ac_fish1$age # create brood year to track cohort over time
ac_fish1 = ac_fish1 %>% mutate(broodYear = ifelse(age == 31, "31", broodYear)) # make plus group consistent color

ggplot() +
  geom_col(ac_fish1 %>% filter(type == "obs"), mapping = aes(x = age, y = prop, fill = factor(broodYear))) +
  geom_line(ac_fish1 %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Domestic LL Fishery Age Compositions") +
  theme_reg() +
  theme(legend.position = "none")

### Domestic LL Survey Age Compositions -------------------------------
obs_ac_srv1 = data.frame(reshape2::melt(sab_curr$oac.srv1), type = "obs")
pred_ac_srv1 = data.frame(reshape2::melt(sab_curr$eac.srv1), type = "pred")

# Put these into a dataframe
ac_srv1 = rbind(obs_ac_srv1, pred_ac_srv1)
names(ac_srv1) = c("year", "age", "prop", "type")
ac_srv1$broodYear = ac_srv1$year - ac_srv1$age # create brood year to track cohort over time
ac_srv1 = ac_srv1 %>% mutate(broodYear = ifelse(age == 31, "31", broodYear)) # make plus group consistent color

ggplot() +
  geom_col(ac_srv1 %>% filter(type == "obs"), mapping = aes(x = age, y = prop, fill = factor(broodYear))) +
  geom_line(ac_srv1 %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Domestic LL Survey Age Compositions") +
  theme_reg() +
  theme(legend.position = "none")

### Japanese LL Survey Age Compositions -------------------------------
obs_ac_srv2 = data.frame(reshape2::melt(sab_curr$oac.srv2), type = "obs")
pred_ac_srv2 = data.frame(reshape2::melt(sab_curr$eac.srv2), type = "pred")

# Put these into a dataframe
ac_srv2 = rbind(obs_ac_srv2, pred_ac_srv2)
names(ac_srv2) = c("year", "age", "prop", "type")
ac_srv2$broodYear = ac_srv2$year - ac_srv2$age # create brood year to track cohort over time
ac_srv2 = ac_srv2 %>% mutate(broodYear = ifelse(age == 31, "31", broodYear)) # make plus group consistent color

ggplot() +
  geom_col(ac_srv2 %>% filter(type == "obs"), mapping = aes(x = age, y = prop, fill = factor(broodYear))) +
  geom_line(ac_srv2 %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Japanese LL Survey Age Compositions") +
  theme_reg() +
  theme(legend.position = "none")

### Domestic LL Fishery Length Compositions (Female) -------------------------------
obs_lc_fish1_female = data.frame(reshape2::melt(sab_curr$olc.fish1.f), type = "obs")
pred_lc_fish1_female = data.frame(reshape2::melt(sab_curr$elc.fish1.f), type = "pred")

# Put these into a dataframe
lc_fish1_female = rbind(obs_lc_fish1_female, pred_lc_fish1_female)
names(lc_fish1_female) = c("year", "age", "prop", "type")

ggplot() +
  geom_col(lc_fish1_female %>% filter(type == "obs"), mapping = aes(x = age, y = prop), fill = "darkgreen", alpha = 0.85) +
  geom_line(lc_fish1_female %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Domestic LL Fishery Length Compositions (Female)") +
  theme_reg() +
  theme(legend.position = "none")

### Domestic LL Fishery Length Compositions (Male) -------------------------------
obs_lc_fish1_male = data.frame(reshape2::melt(sab_curr$olc.fish1.m), type = "obs")
pred_lc_fish1_male = data.frame(reshape2::melt(sab_curr$elc.fish1.m), type = "pred")

# Put these into a dataframe
lc_fish1_male = rbind(obs_lc_fish1_male, pred_lc_fish1_male)
names(lc_fish1_male) = c("year", "age", "prop", "type")

ggplot() +
  geom_col(lc_fish1_male %>% filter(type == "obs"), mapping = aes(x = age, y = prop), fill = "darkgreen", alpha = 0.85) +
  geom_line(lc_fish1_male %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Domestic LL Fishery Length Compositions (Male)") +
  theme_reg() +
  theme(legend.position = "none")

### Domestic Trawl Fishery Length Compositions (Female) -------------------------------
obs_lc_fish3_female = data.frame(reshape2::melt(sab_curr$olc.fish3.f), type = "obs")
pred_lc_fish3_female = data.frame(reshape2::melt(sab_curr$elc.fish3.f), type = "pred")

# Put these into a dataframe
lc_fish3_female = rbind(obs_lc_fish3_female, pred_lc_fish3_female)
names(lc_fish3_female) = c("year", "age", "prop", "type")

ggplot() +
  geom_col(lc_fish3_female %>% filter(type == "obs"), mapping = aes(x = age, y = prop), fill = "darkgreen", alpha = 0.85) +
  geom_line(lc_fish3_female %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Domestic Trawl Fishery Length Compositions (Female)") +
  theme_reg() +
  theme(legend.position = "none")

### Domestic Trawl Fishery Length Compositions (Male) -------------------------------
obs_lc_fish3_male = data.frame(reshape2::melt(sab_curr$olc.fish3.m), type = "obs")
pred_lc_fish3_male = data.frame(reshape2::melt(sab_curr$elc.fish3.m), type = "pred")

# Put these into a dataframe
lc_fish3_male = rbind(obs_lc_fish3_male, pred_lc_fish3_male)
names(lc_fish3_male) = c("year", "age", "prop", "type")

ggplot() +
  geom_col(lc_fish3_male %>% filter(type == "obs"), mapping = aes(x = age, y = prop), fill = "darkgreen", alpha = 0.85) +
  geom_line(lc_fish3_male %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Domestic Trawl Fishery Length Compositions (Male)") +
  theme_reg() +
  theme(legend.position = "none")

### Domestic LL Survey Length Compositions (Female) -------------------------------
obs_lc_srv1_female = data.frame(reshape2::melt(sab_curr$olc.srv1.f), type = "obs")
pred_lc_srv1_female = data.frame(reshape2::melt(sab_curr$elc.srv1.f), type = "pred")

# Put these into a dataframe
lc_srv1_female = rbind(obs_lc_srv1_female, pred_lc_srv1_female)
names(lc_srv1_female) = c("year", "age", "prop", "type")

ggplot() +
  geom_col(lc_srv1_female %>% filter(type == "obs"), mapping = aes(x = age, y = prop), fill = "darkgreen", alpha = 0.85) +
  geom_line(lc_srv1_female %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Domestic LL Survey Length Compositions (Female)") +
  theme_reg() +
  theme(legend.position = "none")

### Domestic LL Survey Length Compositions (Male) -------------------------------
obs_lc_srv1_male = data.frame(reshape2::melt(sab_curr$olc.srv1.m), type = "obs")
pred_lc_srv1_male = data.frame(reshape2::melt(sab_curr$elc.srv1.m), type = "pred")

# Put these into a dataframe
lc_srv1_male = rbind(obs_lc_srv1_male, pred_lc_srv1_male)
names(lc_srv1_male) = c("year", "age", "prop", "type")

ggplot() +
  geom_col(lc_srv1_male %>% filter(type == "obs"), mapping = aes(x = age, y = prop), fill = "darkgreen", alpha = 0.85) +
  geom_line(lc_srv1_male %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Domestic LL Survey Length Compositions (Male)") +
  theme_reg() +
  theme(legend.position = "none")

### Japanese LL Survey Length Compositions (Female) -------------------------------
obs_lc_srv2_female = data.frame(reshape2::melt(sab_curr$olc.srv2.f), type = "obs")
pred_lc_srv2_female = data.frame(reshape2::melt(sab_curr$elc.srv2.f), type = "pred")

# Put these into a dataframe
lc_srv2_female = rbind(obs_lc_srv2_female, pred_lc_srv2_female)
names(lc_srv2_female) = c("year", "age", "prop", "type")

ggplot() +
  geom_col(lc_srv2_female %>% filter(type == "obs"), mapping = aes(x = age, y = prop), fill = "darkgreen", alpha = 0.85) +
  geom_line(lc_srv2_female %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Japanese LL Survey Length Compositions (Female)") +
  theme_reg() +
  theme(legend.position = "none")

### Japanese LL Survey Length Compositions (Male) -------------------------------
obs_lc_srv2_male = data.frame(reshape2::melt(sab_curr$olc.srv2.m), type = "obs")
pred_lc_srv2_male = data.frame(reshape2::melt(sab_curr$elc.srv2.m), type = "pred")

# Put these into a dataframe
lc_srv2_male = rbind(obs_lc_srv2_male, pred_lc_srv2_male)
names(lc_srv2_male) = c("year", "age", "prop", "type")

ggplot() +
  geom_col(lc_srv2_male %>% filter(type == "obs"), mapping = aes(x = age, y = prop), fill = "darkgreen", alpha = 0.85) +
  geom_line(lc_srv2_male %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Japanese LL Survey Length Compositions (Male)") +
  theme_reg() +
  theme(legend.position = "none")

### Domestic Trawl Survey Length Compositions (Female) -------------------------------
obs_lc_srv7_female = data.frame(reshape2::melt(sab_curr$olc.srv7.f), type = "obs")
pred_lc_srv7_female = data.frame(reshape2::melt(sab_curr$elc.srv7.f), type = "pred")

# Put these into a dataframe
lc_srv7_female = rbind(obs_lc_srv7_female, pred_lc_srv7_female)
names(lc_srv7_female) = c("year", "age", "prop", "type")

ggplot() +
  geom_col(lc_srv7_female %>% filter(type == "obs"), mapping = aes(x = age, y = prop), fill = "darkgreen", alpha = 0.85) +
  geom_line(lc_srv7_female %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Domestic Trawl Survey Length Compositions (Female)") +
  theme_reg() +
  theme(legend.position = "none")

### Domestic Trawl Survey Length Compositions (Male) -------------------------------
obs_lc_srv7_male = data.frame(reshape2::melt(sab_curr$olc.srv7.m), type = "obs")
pred_lc_srv7_male = data.frame(reshape2::melt(sab_curr$elc.srv7.m), type = "pred")

# Put these into a dataframe
lc_srv7_male = rbind(obs_lc_srv7_male, pred_lc_srv7_male)
names(lc_srv7_male) = c("year", "age", "prop", "type")

ggplot() +
  geom_col(lc_srv7_male %>% filter(type == "obs"), mapping = aes(x = age, y = prop), fill = "darkgreen", alpha = 0.85) +
  geom_line(lc_srv7_male %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion", title = "Domestic Trawl Survey Length Compositions (Male)") +
  theme_reg() +
  theme(legend.position = "none")

dev.off()


# Selectivity -------------------------------------------------------------
selex_names = c("Derby fishery female","Derby fishery male","Trawl fishery female" ,"Trawl fishery male" ,"IFQ fishery female" ,"IFQ fishery male", "IFQ Recent fishery female" ,
                "IFQ Recent fishery male", "Domestic LL survey female","Domestic LL survey male", "Domestic LL Recent survey female","Domestic LL Recent survey male","Cooperative LL survey female" ,
                "Cooperative LL survey male" ,"GOA trawl survey female","GOA trawl survey male" ) # selectivity names

# pivot longer for plotting
selex = data.frame(sab_curr$agesel, ages = ages) 
names(selex) = c(selex_names, "ages") # rename columns for plotting

# pivot longer for plotting purposes
selex_df = selex %>%  
  pivot_longer(!ages, names_to = "type", values_to = "selex") %>% 
  mutate(type = factor(type, levels = selex_names),
         sex = case_when( # differentiate sexes
           str_detect(type, "female") ~ "Female",
           str_detect(type, "male") ~ "Male" ))

# plot selectivities!
pdf(here("Results", "Selectivity.pdf"), width = 10)
ggplot(selex_df, aes(x = ages, y = selex, color = sex)) +
  geom_line() +
  geom_point() +
  facet_wrap(~type) +
  labs(x = "Ages", y = "Selectivity") +
  theme_reg() +
  theme(legend.position = "none")
dev.off()

# Recruitment ~ SSB -------------------------------------------------------
rec_ssb_df = data.frame(year = sab_curr$t.series$year[-c(1:2)], rec = sab_curr$t.series$Recr[-c(1:2)],
                        ssb = sab_curr$t.series$spbiom[-c(1:2)]) # removing first 2 years from time series

pdf(here("Results", "Rec_SSB.pdf"))
ggplot(rec_ssb_df, aes(x = ssb, y = rec, label = year - 2)) +
  geom_text(color = "blue") +
  scale_x_continuous(limits = c(0, NA)) +  # NA for no upper limit
  theme_reg() +
  labs(y = "Age 2 Recruits (millions)", x = "SSB (kt)")
dev.off()


# Recruitment, SSB, Catch -------------------------------------------------
rec_ssb_catch = data.frame(year = sab_curr$t.series$year, rec = sab_curr$t.series$Recr,
                           ssb = sab_curr$t.series$spbiom, catch = sab_curr$t.series$Catch_HAL + sab_curr$t.series$Catch_TWL)

# Plot!
pdf(here("Results", "Rec_SSB_Catch.pdf"), height = 5)
ggplot(rec_ssb_catch) +
  geom_col(mapping = aes(x = year, y = rec, color = "Recruitment")) +
  geom_line(mapping = aes(x = year, y = ssb / 5, color = "SSB"), size = 1.75) +
  geom_line(mapping = aes(x = year, y = catch / 5, color = "Catch"), size = 1.75) +
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "SSB or Catch (kt)") ) +
  scale_color_manual(values = c("Recruitment" = "lightblue3", "SSB" = "orange", 
                                "Catch" = "yellow2"),  name = "") + 
  labs(x = "Year", y = "Recruitment (millions of fish)") +
  theme_reg() + theme(legend.position = c(0.15, 0.9)) 
dev.off()


# Numbers at age ----------------------------------------------------------
# Females
n_at_age_f = data.frame(year = rownames(sab_curr$natage.female), sab_curr$natage.female) %>% 
  pivot_longer(!year, names_to = "age", values_to = "N") %>% 
  mutate(age = as.numeric(str_remove(age, "X")), year = as.numeric(year),
         sex = "Female") 

# Males
n_at_age_m = data.frame(year = rownames(sab_curr$natage.male),  sab_curr$natage.male) %>% 
  pivot_longer(!year, names_to = "age", values_to = "N") %>% 
  mutate(age = as.numeric(str_remove(age, "X")), year = as.numeric(year),
         sex = "Male")

n_at_age = rbind(n_at_age_f, n_at_age_m) # bind together

# Create proportions to look at age structure
n_at_age = n_at_age %>% 
  group_by(year) %>% 
  mutate(prop = N / sum(N))

pdf(here("Results", "NAA_plots.pdf"), width = 10)

# Plot numbers at age (females)
ggplot(n_at_age %>% filter(sex == "Female"), aes(x = year, y = N)) +
  geom_line(size = 1) +
  facet_wrap(~age, scales = "free") +
  theme_reg() +
  labs(x = "Year", y = "Numbers at age (Females)")

# Plot numbers at age (males)
ggplot(n_at_age %>% filter(sex == "Male"), aes(x = year, y = N)) +
  geom_line(size = 1) +
  facet_wrap(~age, scales = "free") +
  theme_reg() +
  labs(x = "Year", y = "Numbers at age (Males)")

# Plot age-structure of the population (filter to recent years)
ggplot(n_at_age %>% filter(year %in% c(2011:2022)), aes(x = age, y = prop, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year) +
  theme_reg() +
  theme(legend.position = "top") +
  labs(x = "Age", y = "Proportion", fill = "Sex")

dev.off()
