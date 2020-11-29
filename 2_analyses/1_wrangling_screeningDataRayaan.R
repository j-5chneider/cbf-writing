library(tidyverse)

art_csv <- rio::import(file="../data/articles.csv")

art_csv <- art_csv %>%
  dplyr::mutate(excl_reason = str_extract(.$note, "RAYYAN-EXCLUSION-REASONS:.*?(\\||$)"),
                labels = str_extract(.$note, "RAYYAN-LABELS:.*?(\\||$)"),
                decision_j = str_extract(.$note, "juergen.schneider\"\"=>\"\".*?\"\""),
                decision_s = str_extract(.$note, "Salome\"\"=>\"\".*?\"\""))%>%
  dplyr::mutate(excl_reason = str_remove_all(excl_reason, "RAYYAN-EXCLUSION-REASONS:\\s"),
                labels = str_remove_all(labels, "RAYYAN-LABELS:\\s|\\s\\|"),
                decision_j = case_when(                                                    # Diese Zeilen für die Neue Variable
                  str_detect(decision_j, "Excluded") ~ "Excluded",           # ebenfalls kopieren und
                  str_detect(decision_j, "Included") ~ "Included",           # den neuen Variablennamen
                  str_detect(decision_j, "Maybe") ~ "Maybe"),                # einfügen
                decision_s = case_when(                                                    # Diese Zeilen für die Neue Variable
                  str_detect(decision_s, "Excluded") ~ "Excluded",           # ebenfalls kopieren und
                  str_detect(decision_s, "Included") ~ "Included",           # den neuen Variablennamen
                  str_detect(decision_s, "Maybe") ~ "Maybe")                 # einfügen
  )




table(art_csv$decision_j, art_csv$decision_s)
table(art_csv$decision_j)
table(art_csv$decision_s)