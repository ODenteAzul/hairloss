library(tidyverse)
library(tidymodels)
library(cluster)
library(reshape2)
library(fpc)
library(dendextend)
library(patchwork)

df <- read.csv("hairloss.csv")
head(df)

df_f <- df

names(df_f) <- c("id","genetics","hormonal_change","medical_conditions",
                 "medical_treatments","nutritional_deficiencies","stress",
                 "age","poor_hair_care_habits","environmental_factors",
                 "smoking","weight_loss","hair_loss")

df_f$age_group <- case_when(df_f$age <= 25 ~ "<= 25 years",
                            df_f$age <= 35 ~ "25 - 35 years",
                            df_f$age <= 45 ~ "35 - 45 years",
                            df_f$age > 45 ~ "> 45 years")

df_f$age_group <- factor(df_f$age_group, levels = c("<= 25 years", "25 - 35 years", "35 - 45 years", "> 45 years"))

df_f$hair_loss_desc <- ifelse(df_f$hair_loss == 0, "No", "Yes")


# exploratory data analysis, trying to find some trends

age_mean <- round(mean(df_f$age))


df_f %>%
  ggplot(aes(age, fill = hair_loss_desc, colour = hair_loss_desc)) +
  geom_density(alpha = 0.3, linewidth = 1.1) +
  theme_light() +
  scale_fill_manual(values = c("purple4","steelblue2")) +
  scale_colour_manual(values = c("purple4","steelblue2")) +
  labs(title = "Hair Loss by Age",
       x = "Hair Loss",
       y = "Total") +
  guides(colour = guide_legend(title="Hair Loss")) +
  guides(fill = guide_legend(title="Hair Loss"))


df_f %>%
  group_by(age_group, hair_loss_desc) %>%
  summarize(total = n()) %>%
  mutate(prop = round(total / sum(total) * 100,2)) %>%
  ggplot(aes(age_group, total, fill = hair_loss_desc)) +
  geom_col(position = "dodge", alpha = .8) +
  theme_light() +
  scale_fill_manual(values = c("purple4","steelblue2")) +
  geom_text(aes(label = str_c(as.character(prop), "%"), y = total + 4), position = position_dodge(0.9), size = 4) +
  annotate("text", 1, 170, label = str_c("Mean Age: ",as.character(age_mean)," years")) +
  labs(title = "Cases of Hair Loss by Age Group",
       x = "Age Group",
       y = "Total") +
  guides(fill=guide_legend(title="Hair Loss"))

df_f %>%
  group_by(hair_loss_desc, genetics) %>%
  summarize(total = n()) %>%
  mutate(prop = round(total / sum(total) * 100,2)) %>%
  ggplot(aes(fct_reorder(genetics, total, .desc = TRUE), total, fill = hair_loss_desc)) +
  geom_col(position = "dodge", alpha = .8) +
  theme_light() +
  scale_fill_manual(values = c("purple4","steelblue2")) +
  geom_text(aes(label = str_c(as.character(prop), "%"), y = total + 2), position = position_dodge(0.9), size = 4) +
  theme(axis.text.x = element_text(angle = 45, size = 9)) +
  labs(title = "Hormonal Change Summary by Genetics",
       x = "Hormonal Change",
       y = "Total") +
  guides(fill=guide_legend(title="Hair Loss"))

df_f %>%
  group_by(hair_loss_desc, hormonal_change) %>%
  summarize(total = n()) %>%
  mutate(prop = round(total / sum(total) * 100,2)) %>%
  ggplot(aes(fct_reorder(hormonal_change, total, .desc = TRUE), total, fill = hair_loss_desc)) +
  geom_col(position = "dodge", alpha = .8) +
  theme_light() +
  scale_fill_manual(values = c("purple4","steelblue2")) +
  geom_text(aes(label = str_c(as.character(prop), "%"), y = total + 2), position = position_dodge(0.9), size = 4) +
  theme(axis.text.x = element_text(angle = 45, size = 9)) +
  labs(title = "Hormonal Change Summary by Hormonal Change",
       x = "Hormonal Change",
       y = "Total") +
  guides(fill=guide_legend(title="Hair Loss"))

df_f %>%
  group_by(hair_loss_desc, medical_treatments) %>%
  summarize(total = n()) %>%
  mutate(prop = round(total / sum(total) * 100,2)) %>%
  ggplot(aes(fct_reorder(medical_treatments, total, .desc = TRUE), total, fill = hair_loss_desc)) +
  geom_col(position = "dodge", alpha = .8) +
  theme_light() +
  scale_fill_manual(values = c("purple4","steelblue2")) +
  theme(axis.text.x = element_text(angle = 45, size = 7)) +
  labs(title = "Medical Treatments Summary by Medical Treatment",
       x = "Medical Treatment",
       y = "Total") +
  guides(fill=guide_legend(title="Hair Loss"))

df_f %>%
  group_by(hair_loss_desc, medical_conditions) %>%
  summarize(total = n()) %>%
  mutate(prop = round(total / sum(total) * 100,2)) %>%
  ggplot(aes(fct_reorder(medical_conditions, total, .desc = TRUE), total, fill = hair_loss_desc)) +
  geom_col(position = "dodge", alpha = .8) +
  theme_light() +
  scale_fill_manual(values = c("purple4","steelblue2")) +
  theme(axis.text.x = element_text(angle = 45, size = 7)) +
  labs(title = "Medical Conditions Summary by Medical Conditions",
       x = "Medical Conditions",
       y = "Total") +
  guides(fill=guide_legend(title="Hair Loss"))

df_f %>%
  group_by(hair_loss_desc, nutritional_deficiencies) %>%
  summarize(total = n()) %>%
  mutate(prop = round(total / sum(total) * 100,2)) %>%
  ggplot(aes(fct_reorder(nutritional_deficiencies, total, .desc = TRUE), total, fill = hair_loss_desc)) +
  geom_col(position = "dodge", alpha = .8) +
  theme_light() +
  scale_fill_manual(values = c("purple4","steelblue2")) +
  theme(axis.text.x = element_text(angle = 45, size = 7))  +
  labs(title = "Nutritional Deficiencies Summary by Nutritional Deficiencies",
       x = "Nutritional Deficiencies",
       y = "Total") +
  guides(fill=guide_legend(title="Hair Loss"))

df_f %>%
  group_by(hair_loss_desc, stress) %>%
  summarize(total = n()) %>%
  mutate(prop = round(total / sum(total) * 100,2)) %>%
  ggplot(aes(fct_reorder(stress, total, .desc = TRUE), total, fill = hair_loss_desc)) +
  geom_col(position = "dodge", alpha = .8) +
  theme_light() +
  scale_fill_manual(values = c("purple4","steelblue2")) +
  geom_text(aes(label = str_c(as.character(prop), "%"), y = total + 2), position = position_dodge(0.9), size = 4) +
  theme(axis.text.x = element_text(angle = 45, size = 9)) +
  labs(title = "Stress Summary by Stress Level",
       x = "Stress",
       y = "Total") +
  guides(fill=guide_legend(title="Hair Loss"))

df_f %>%
  group_by(hair_loss_desc, poor_hair_care_habits) %>%
  summarize(total = n()) %>%
  mutate(prop = round(total / sum(total) * 100,2)) %>%
  ggplot(aes(fct_reorder(poor_hair_care_habits, total, .desc = TRUE), total, fill = hair_loss_desc)) +
  geom_col(position = "dodge", alpha = .8) +
  theme_light() +
  scale_fill_manual(values = c("purple4","steelblue2")) +
  geom_text(aes(label = str_c(as.character(prop), "%"), y = total + 2), position = position_dodge(0.9), size = 4) +
  theme(axis.text.x = element_text(angle = 45, size = 9)) +
  labs(title = "Poor Hair Care Habits Summary by Poor Hair Care Habits",
       x = "Poor Hair Care Habits",
       y = "Total") +
  guides(fill=guide_legend(title="Hair Loss"))

df_f %>%
  group_by(hair_loss_desc, environmental_factors) %>%
  summarize(total = n()) %>%
  mutate(prop = round(total / sum(total) * 100,2)) %>%
  ggplot(aes(fct_reorder(environmental_factors, total, .desc = TRUE), total, fill = hair_loss_desc)) +
  geom_col(position = "dodge", alpha = .8) +
  theme_light() +
  scale_fill_manual(values = c("purple4","steelblue2")) +
  geom_text(aes(label = str_c(as.character(prop), "%"), y = total + 2), position = position_dodge(0.9), size = 4) +
  theme(axis.text.x = element_text(angle = 45, size = 9)) +
  labs(title = "Environmental Factors Summary by Environmental Factors",
       x = "Environmental Factors",
       y = "Total") +
  guides(fill=guide_legend(title="Hair Loss"))

df_f %>%
  group_by(hair_loss_desc, smoking) %>%
  summarize(total = n()) %>%
  mutate(prop = round(total / sum(total) * 100,2)) %>%
  ggplot(aes(fct_reorder(smoking, total, .desc = TRUE), total, fill = hair_loss_desc)) +
  geom_col(position = "dodge", alpha = .8) +
  theme_light() +
  scale_fill_manual(values = c("purple4","steelblue2")) +
  geom_text(aes(label = str_c(as.character(prop), "%"), y = total + 2), position = position_dodge(0.9), size = 4) +
  theme(axis.text.x = element_text(angle = 45, size = 9)) +
  labs(title = "Smoking Summary by Smoking",
       x = "Smoking Summary",
       y = "Total") +
  guides(fill=guide_legend(title="Hair Loss"))

df_f %>%
  group_by(hair_loss_desc, weight_loss) %>%
  summarize(total = n()) %>%
  mutate(prop = round(total / sum(total) * 100,2)) %>%
  ggplot(aes(fct_reorder(weight_loss, total, .desc = TRUE), total, fill = hair_loss_desc)) +
  geom_col(position = "dodge", alpha = .8) +
  theme_light() +
  scale_fill_manual(values = c("purple4","steelblue2")) +
  geom_text(aes(label = str_c(as.character(prop), "%"), y = total + 2), position = position_dodge(0.9), size = 4) +
  theme(axis.text.x = element_text(angle = 45, size = 9)) +
  labs(title = "Weight Loss Summary by Weight Loss",
       x = "Weight Loss Summary",
       y = "Total") +
  guides(fill=guide_legend(title="Hair Loss"))



# changes variable names for something more explanatory, for future visualization in the heatmap

df_f$hair_loss_desc <- ifelse(df_f$hair_loss == 0,
                              "Hair Loss - No",
                              "Hair Loss - Yes")

df_f$genetics <- ifelse(df_f$genetics == "Yes",
                        "Gen - Yes",
                        "Gen - No")

df_f$hormonal_change <- ifelse(df_f$hormonal_change == "Yes",
                               "Horm. Change - Yes",
                               "Horm. Change - No")

df_f$poor_hair_care_habits <- ifelse(df_f$poor_hair_care_habits == "Yes",
                                     "Poor Hair Hab. - Yes",
                                     "Poor Hair Hab. - No")

df_f$environmental_factors <- ifelse(df_f$environmental_factors == "Yes",
                                     "Environm. Factors - Yes",
                                     "Environm. Factors - No")

df_f$smoking <- ifelse(df_f$smoking == "Yes",
                       "Smoking - Yes",
                       "Smoking - No")

df_f$weight_loss <- ifelse(df_f$weight_loss == "Yes",
                           "Weight Loss - Yes",
                           "Weight Loss - No")

df_f$stress <- case_when(df_f$stress == "Low" ~ "Stress Low",
                         df_f$stress == "Moderate" ~ "Stress Moderate",
                         df_f$stress == "High" ~ "Stress High")


df_f_m <- df_f %>%
  dplyr::select(-c(hair_loss, age))


df_f_m$genetics <- as.factor(df_f_m$genetics)
df_f_m$hormonal_change <- as.factor(df_f_m$hormonal_change)
df_f_m$medical_conditions <- as.factor(df_f_m$medical_conditions)
df_f_m$medical_treatments <- as.factor(df_f_m$medical_treatments)
df_f_m$nutritional_deficiencies <- as.factor(df_f_m$nutritional_deficiencies)
df_f_m$stress <- as.factor(df_f_m$stress)
df_f_m$poor_hair_care_habits <- as.factor(df_f_m$poor_hair_care_habits)
df_f_m$environmental_factors <- as.factor(df_f_m$environmental_factors)
df_f_m$smoking <- as.factor(df_f_m$smoking)
df_f_m$weight_loss <- as.factor(df_f_m$weight_loss)
df_f_m$age_group <- as.factor(df_f_m$age_group)
df_f_m$hair_loss_desc <- as.factor(df_f_m$hair_loss_desc)


gower.dist <- daisy(df_f_m, metric = c("gower"), wei)



# First I'll transform this data using GOWER, to create a dissimilarity matrix
# from the categorical data provided, this will be the input in our clustering algorithm

gower.dist <- daisy(df_f_m, metric = c("gower"), wei)

# I'll perform two clustering methods, both using Hierarchical Clustering: Divisive and Agglomerative.

# Divisive
divisive.clust <- diana(as.matrix(gower.dist), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive Clustering")

# Agglomerative
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative Clustering, complete linkages")
aggl.clust.c <- hclust(gower.dist, method = "complete")




# Generating and grouping some statistics seeking orientation and good format to analyse

cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number",
                    "n",
                    "within.cluster.ss",
                    "average.within",
                    "average.between",
                    "wb.ratio",
                    "dunn2",
                    "avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am limiting the maximum amout of clusters by 10
# Looking at the Dendogram I already have a final number of clusters in mind: 6 - let's seek for confirmation.
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 10)
stats.df.divisive

#stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, 10)
#stats.df.aggl

#At the end of this I feel the Divisive approuch has more in commom with the data.




# Looking at the dendrograms and Statistics, as mentioned above, I am already 
# leaning towards the Divisive aprouch, as it looks mode natural and according to the data. 

#But, let's follow a little further, with the Elbow and Silhouette plots for next steps. 

# Elbow - Divisive clustering
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point() +
  geom_line() +
  theme_light() +
  labs(title = "Elbows: Divisive clustering",
       x = "Number of clusters",
       y = "Within Clusters Sum of Squares") +
  theme(plot.title = element_text(hjust = 0.5))

# Silhouette - Divisive
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point() +
  geom_line() +
  theme_light() +
  labs(title = "Silhouette: Divisive clustering",
       x = "Number of clusters",
       y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))


# So, both the Elbow Plot and the Silhouette Plot tells me that 6 is a good number
# for clusters. But I will try also 5 or for, to see if it tells more about the data. 



# And finally, to make things visual a Heatmap to see were things fit. 
clust.num <- cutree(aggl.clust.c, k = 6)
df_f_m_cl <- cbind(df_f_m, clust.num)
cust.long <- melt(data.frame(lapply(df_f_m_cl, as.character), stringsAsFactors=FALSE), 
                  id = c("id", "clust.num"), factorsAsStrings=T)
cust.long.q <- cust.long %>%
  group_by(clust.num, variable, value) %>%
  mutate(count = n_distinct(id)) %>%
  distinct(clust.num, variable, value, count)


# heatmap.c will be suitable in case you want to go for absolute counts - but it doesn't tell much to my taste

heatmap.c <- ggplot(cust.long.q, aes(x = clust.num, y = factor(value,
                                                               levels = c("<= 25 years",
                                                                          "> 45 years",
                                                                          "25 - 35 years",
                                                                          "35 - 45 years",
                                                                          "Alopecia Areata",
                                                                          "Androgenetic Alopecia",
                                                                          "Dermatitis",
                                                                          "Dermatosis",
                                                                          "Eczema",
                                                                          "Psoriasis",
                                                                          "Thyroid Problems",
                                                                          "Ringworm",
                                                                          "Scalp Infection", 
                                                                          "Seborrheic Dermatitis", 
                                                                          "Rogaine", 
                                                                          "Antibiotics",
                                                                          "Antidepressants", 
                                                                          "Antifungal Cream", 
                                                                          "Blood Pressure Medication",
                                                                          "Accutane","Heart Medication", 
                                                                          "Steroids", 
                                                                          "Immunomodulators",
                                                                          "Chemotherapy", 
                                                                          "Environm. Factors - No", 
                                                                          "Environm. Factors - Yes",
                                                                          "Gen - No", 
                                                                          "Gen - Yes", 
                                                                          "Hair Loss - No", 
                                                                          "Hair Loss - Yes", 
                                                                          "Horm. Change - No", 
                                                                          "Horm. Change - Yes", 
                                                                          "Iron deficiency", 
                                                                          "Magnesium deficiency", 
                                                                          "Omega-3 fatty acids", 
                                                                          "Vitamin D Deficiency",
                                                                          "Vitamin E deficiency", 
                                                                          "Zinc Deficiency", 
                                                                          "Protein deficiency", 
                                                                          "Selenium deficiency", 
                                                                          "Biotin Deficiency", 
                                                                          "Vitamin A Deficiency", 
                                                                          "Poor Hair Hab. - No", 
                                                                          "Poor Hair Hab. - Yes", 
                                                                          "Smoking - No", 
                                                                          "Smoking - Yes", 
                                                                          "Stress High", 
                                                                          "Stress Moderate",
                                                                          "Stress Low", 
                                                                          "Weight Loss - No", 
                                                                          "Weight Loss - Yes", 
                                                                          "No Data"), ordered = T))) +
  geom_tile(aes(fill = count))+
  scale_fill_gradient2(low = "yellow2", mid = "darkorange", high = "purple4")


# calculating the percent of each factor level in the absolute count of cluster members
cust.long.p <- cust.long.q %>%
  group_by(clust.num, variable) %>%
  mutate(perc = count / sum(count)) %>%
  arrange(clust.num)
heatmap.p <- ggplot(cust.long.p, aes(x = clust.num, y = factor(value, 
                                                               levels = c("<= 25 years",
                                                                          "> 45 years",
                                                                          "25 - 35 years",
                                                                          "35 - 45 years",
                                                                          "Alopecia Areata",
                                                                          "Androgenetic Alopecia",
                                                                          "Dermatitis",
                                                                          "Dermatosis",
                                                                          "Eczema",
                                                                          "Psoriasis",
                                                                          "Thyroid Problems",
                                                                          "Ringworm",
                                                                          "Scalp Infection", 
                                                                          "Seborrheic Dermatitis", 
                                                                          "Rogaine", 
                                                                          "Antibiotics",
                                                                          "Antidepressants", 
                                                                          "Antifungal Cream", 
                                                                          "Blood Pressure Medication",
                                                                          "Accutane","Heart Medication", 
                                                                          "Steroids", 
                                                                          "Immunomodulators",
                                                                          "Chemotherapy", 
                                                                          "Environm. Factors - No", 
                                                                          "Environm. Factors - Yes",
                                                                          "Gen - No", 
                                                                          "Gen - Yes", 
                                                                          "Hair Loss - No", 
                                                                          "Hair Loss - Yes", 
                                                                          "Horm. Change - No", 
                                                                          "Horm. Change - Yes", 
                                                                          "Iron deficiency", 
                                                                          "Magnesium deficiency", 
                                                                          "Omega-3 fatty acids", 
                                                                          "Vitamin D Deficiency",
                                                                          "Vitamin E deficiency", 
                                                                          "Zinc Deficiency", 
                                                                          "Protein deficiency", 
                                                                          "Selenium deficiency", 
                                                                          "Biotin Deficiency", 
                                                                          "Vitamin A Deficiency", 
                                                                          "Poor Hair Hab. - No", 
                                                                          "Poor Hair Hab. - Yes", 
                                                                          "Smoking - No", 
                                                                          "Smoking - Yes", 
                                                                          "Stress High", 
                                                                          "Stress Moderate",
                                                                          "Stress Low", 
                                                                          "Weight Loss - No", 
                                                                          "Weight Loss - Yes", 
                                                                          "No Data"), ordered = T))) +
  
  geom_tile(aes(fill = perc), alpha = 0.85)+
  labs(title = "Distribution of characteristics across clusters", x = "Cluster number", y = NULL) +
  geom_hline(yintercept = 4.5) + 
  geom_hline(yintercept = 13.5) + 
  geom_hline(yintercept = 21.5) + 
  geom_hline(yintercept = 23.5) + 
  geom_hline(yintercept = 25.5) +
  geom_hline(yintercept = 27.5) +
  geom_hline(yintercept = 29.5) +
  geom_hline(yintercept = 38.5) +
  geom_hline(yintercept = 40.5) + 
  geom_hline(yintercept = 42.5) +
  geom_hline(yintercept = 45.5) +
  geom_hline(yintercept = 47.5) +
  scale_fill_gradient2(low = "yellow2", mid = "darkorange", high = "purple4")
heatmap.p
