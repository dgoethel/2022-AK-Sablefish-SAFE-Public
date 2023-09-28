# Purpose: To update figures for sablefish assessment
# Creator: Matthew LH. Cheng (UAF-CFOS)
# Date 9/27/23


# Set up ------------------------------------------------------------------
library(here)
library(tidyverse)
library(reshape2)
library(data.table)

# Read in .rdat
sab_curr <- dget(here("Results", "tem.rdat")) 
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
ggplot(likecomps_df, aes(x = component, y = nLL, fill = groups)) +
  geom_col() +
  labs(x = "Likelihood Component", y = "Likelihood", fill = "") +
  scale_x_discrete(guide = guide_axis(angle = 90), limits = unique(likecomps_df$component))  +
  theme_reg() +
  theme(legend.position = c(0.85, 0.85))
  
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
ggplot(idx_df) +
  geom_line(mapping = aes(x = year, y = pred), size = 1.5, col = "red") +
  geom_pointrange(mapping = aes(x = year, y = obs, ymin = lci, ymax = uci), 
                  alpha = 0.75, size = 0.75, col = "blue") +
  facet_wrap(~type, scales = "free") +
  scale_y_continuous(limits = c(0, NA)) +  # NA for no upper limit
  labs(x = "Year", y = "Index") +
  theme_reg() +
  theme(legend.position = "top")

# Fits to compositions ---------------------------------------------------------
### Domestic LL Fishery Age Compositions -------------------------------
obs_ac_fish1 = data.frame(melt(sab_curr$oac.fish1), type = "obs")
pred_ac_fish1 = data.frame(melt(sab_curr$eac.fish1), type = "pred")

# Put these into a dataframe
ac_fish1 = rbind(obs_ac_fish1, pred_ac_fish1)
names(ac_fish1) = c("year", "age", "prop", "type")
ac_fish1$broodYear = ac_fish1$year - ac_fish1$age # create brood year to track cohort over time
ac_fish1 = ac_fish1 %>% mutate(broodYear = ifelse(age == 31, "31", broodYear)) # make plus group consistent color

ggplot() +
  geom_col(ac_fish1 %>% filter(type == "obs"), mapping = aes(x = age, y = prop, fill = factor(broodYear))) +
  geom_line(ac_fish1 %>% filter(type == "pred"), mapping = aes(x = age, y = prop), size = 1) +
  facet_wrap(~year, scales = "free", ncol = 2, dir = "v") +
  labs(x = "Ages", y = "Proportion") +
  theme_reg() +
  theme(legend.position = "none")
 
# Recruitment ~ SSB -------------------------------------------------------
rec_ssb_df = data.frame(year = sab_curr$t.series$year[-c(1:2)], rec = sab_curr$t.series$Recr[-c(1:2)],
                        ssb = sab_curr$t.series$spbiom[-c(1:2)]) # removing first 2 years from time series

ggplot(rec_ssb_df, aes(x = ssb, y = rec, label = year - 2)) +
  geom_text(color = "blue") +
  scale_x_continuous(limits = c(0, NA)) +  # NA for no upper limit
  theme_reg() +
  labs(y = "Age 2 Recruits (millions)", x = "SSB (kt)")

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
ggplot(selex_df, aes(x = ages, y = selex, color = sex)) +
  geom_line() +
  geom_point() +
  facet_wrap(~type) +
  labs(x = "Ages", y = "Selectivity") +
  theme_reg() +
  theme(legend.position = "none")


  
