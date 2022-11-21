library(tidyverse)

art_csv <- rio::import(file="data/articles.csv")

art_csv <- art_csv %>%
  dplyr::mutate(excl_reason = str_extract(.$notes, "RAYYAN-EXCLUSION-REASONS:.*?(\\||$)"), # extract exclusion reasons
                labels = str_extract(.$notes, "RAYYAN-LABELS:.*?(\\||$)"),       # extract labels coded
                decision_j = str_extract(.$notes, "Jürgen\"\"=>\"\".*?\"\""),    # extract decision from coder 1
                decision_s = str_extract(.$notes, "Salome\"\"=>\"\".*?\"\""))%>% # extract decision from coder 2
  # remove prefixes or recode
  dplyr::mutate(excl_reason = str_remove_all(excl_reason, "RAYYAN-EXCLUSION-REASONS:\\s"),
                labels = str_remove_all(labels, "RAYYAN-LABELS:\\s|\\s\\|"),
                decision_j = case_when(                                      
                  str_detect(decision_j, "Excluded") ~ "Excluded",           
                  str_detect(decision_j, "Included") ~ "Included",           
                  str_detect(decision_j, "Maybe") ~ "Maybe"),                
                decision_s = case_when(                                      
                  str_detect(decision_s, "Excluded") ~ "Excluded",           
                  str_detect(decision_s, "Included") ~ "Included",           
                  str_detect(decision_s, "Maybe") ~ "Maybe")                 
  )



# crosstab of coding results
table(art_csv$decision_j, art_csv$decision_s)
table(art_csv$decision_j)
table(art_csv$decision_s)

# write table with clean data set to data folder
write_csv(art_csv, file = "data/articles_clean.csv")
