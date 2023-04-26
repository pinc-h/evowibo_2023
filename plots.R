# Required packages
library(tidyverse)
setwd("/Users/alexpinch/GitHub/inversion_model")

# Load locally adaptive data
la_data <- tibble()
files <- length(list.files("/Users/alexpinch/GitHub/inversion_model/data_040623/full_runs"))
full_runs <- list.files("/Users/alexpinch/GitHub/inversion_model/data_040623/full_runs")
for (i in 1:files) {
  run <- (full_runs[i])
  typeof(run)
  setwd(file.path("/Users/alexpinch/GitHub/inversion_model/data_040623/full_runs",run))
  la_run_data <- read.csv(file = paste(run,".csv",sep=""),skip=1,header=F) %>%
    rename(gen=V1,pop=V2,sample=V3,fitness=V4,inv_genotype=V5)
  la_run_data <- la_run_data %>% mutate(sim_run=run)
  la_data <- rbind(la_data, la_run_data)
}

# Load overdominant data
od_data <- tibble()
files <- length(list.files("/Users/alexpinch/GitHub/inversion_model/data_041823_od/full_runs"))
full_runs <- list.files("/Users/alexpinch/GitHub/inversion_model/data_041823_od/full_runs")
for (i in 1:files) {
  run <- (full_runs[i])
  typeof(run)
  setwd(file.path("/Users/alexpinch/GitHub/inversion_model/data_041823_od/full_runs",run))
  od_run_data <- read.csv(file = paste(run,".csv",sep=""),skip=1,header=F) %>%
    rename(gen=V1,pop=V2,sample=V3,fitness=V4,inv_genotype=V5)
  od_run_data <- od_run_data %>% mutate(sim_run=run)
  od_data <- rbind(od_data, od_run_data)
}

# |--------------------------------|
# | Preliminary data formatting    |
# |--------------------------------|

# This removes the strict fitness changes coded in the SLiM model, this way we can assess fitness changes to look for build-up of deleterious mutations
la_data <- la_data %>% mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                                        inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                                        inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                                        inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                                        TRUE ~ fitness))
od_data <- od_data %>% mutate(fixed_fitness = case_when(inv_genotype == 2 ~ fitness - 0.05,
                                                        inv_genotype == 1 ~ fitness - 0.1,
                                                        TRUE ~ fitness))

# Calculate deleterious load
la_data <- la_data %>% mutate(del_load = case_when(fixed_fitness > 0.5 ~ 1 - fixed_fitness,
                                                   TRUE ~ fitness))
od_data <- od_data %>% mutate(del_load = case_when(fixed_fitness > 0.5 ~ 1 - fixed_fitness,
                                                   TRUE ~ fitness))

# |-------------------|
# | Figures           |
# |-------------------|

# Figure 1: Locally adaptive model deleterious load at 51,000 and 100k gen.
la_data %>%
  filter(!is.na(inv_genotype), gen==1e5, del_load < 0.5) %>% # Change gen=1e5 to compare to last generation
  group_by(sim_run, pop, inv_genotype) %>%
  summarize(mean_load = mean(del_load,na.rm=T)) %>%
  group_by(sim_run,inv_genotype) %>%
  mutate(qnt_90 = quantile(mean_load, 0.9),
         qnt_10 = quantile(mean_load, 0.1),
         mean_sim_load = quantile(mean_load, 0.9)) %>%  ## 0.5 = median
  ggplot(.,aes(x=as.factor(inv_genotype),y=mean_sim_load,group=inv_genotype,color=as.factor(inv_genotype))) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  labs(x = "Genotype", y = "Deleterial load") +
  scale_color_discrete(name = "Inversion Genotype") + 
  ylim(0, 0.3) +
  theme(text = element_text(size = 20)) 

# Figure 2: Overdominant model deleterious load at 51,000 and 100k gen.
od_data %>%
  filter(!is.na(inv_genotype), gen==1e5, del_load < 0.5) %>% # Change gen=1e5 to compare to last generation
  group_by(sim_run, pop, inv_genotype) %>%
  summarize(mean_load = mean(del_load,na.rm=T)) %>%
  group_by(sim_run,inv_genotype) %>%
  mutate(qnt_90 = quantile(mean_load, 0.9),
         qnt_10 = quantile(mean_load, 0.1),
         mean_sim_load = quantile(mean_load, 0.9)) %>%  ## 0.5 = median
  ggplot(.,aes(x=as.factor(inv_genotype),y=mean_sim_load,group=inv_genotype,color=as.factor(inv_genotype))) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  labs(x = "Genotype", y = "Deleterial load") + 
  scale_color_discrete(name = "Inversion Genotype") + 
  ylim(0, 0.3) +
  theme(text = element_text(size = 20)) 