
library(tidyverse)
library(rvest)

wikipedia_osfi <- c("https://en.wikipedia.org/w/index.php?title=Office_of_the_Superintendent_of_Financial_Institutions&action=info")
wikipedia_bsif <- c("https://fr.wikipedia.org/w/index.php?title=Bureau_du_surintendant_des_institutions_financi%C3%A8res&action=info")
wikipedia_bsif_form <- "https://fr.wikipedia.org/w/index.php?returnto=Bureau+du+surintendant+des+institutions+financi%C3%A8res&returntoquery=action%3Dinfo&title=Sp%C3%A9cial:Connexion&centralAuthAutologinTried=1&centralAuthError=Not+centrally+logged+in"
wikipedia_bdc <- c("https://fr.wikipedia.org/w/index.php?title=Banque_du_Canada&action=info")
wikipedia_boc <- c("https://en.wikipedia.org/w/index.php?title=Bank_of_Canada&action=info")
wikipedia_fcac <- c("https://en.wikipedia.org/w/index.php?title=Financial_Consumer_Agency_of_Canada&action=info")
wikipedia_acfc <- c("https://fr.wikipedia.org/w/index.php?title=Agence_de_la_consommation_en_mati%C3%A8re_financi%C3%A8re_du_Canada&action=info")
wikipedia_finance_canada <- c("https://fr.wikipedia.org/w/index.php?title=Minist%C3%A8re_des_Finances_(Canada)&action=info")
wikipedia_finances_canada <- c("https://en.wikipedia.org/w/index.php?title=Department_of_Finance_Canada&action=info")
wikipedia_cdic <- c("https://en.wikipedia.org/w/index.php?title=Canada_Deposit_Insurance_Corporation&action=info")
wikipedia_sadc <- c("https://fr.wikipedia.org/w/index.php?title=Soci%C3%A9t%C3%A9_d%27assurance-d%C3%A9p%C3%B4ts_du_Canada&action=info")
wikipedia_cmhc <- c("https://en.wikipedia.org/w/index.php?title=Canada_Mortgage_and_Housing_Corporation&action=info")
wikipedia_schl <- c("https://fr.wikipedia.org/w/index.php?title=Soci%C3%A9t%C3%A9_canadienne_d%27hypoth%C3%A8ques_et_de_logement&action=info")

wikipedia <- c(wikipedia_osfi, wikipedia_bsif)
#names_partners("BdC", "BoC", "acfc", "")


extraction_noms_partenaires <- function(wikipedia_partners) {
  for (i in wikipedia_partners){
    x <<- deparse(substitute(i))
    return(x)}
}
wikipedia_partners <- c(wikipedia_bdc, wikipedia_boc, wikipedia_acfc, wikipedia_fcac,wikipedia_finance_canada, wikipedia_finances_canada,wikipedia_sadc,  wikipedia_cdic, wikipedia_schl, wikipedia_cmhc)

wikipedia_names <- extraction_noms_partenaires(wikipedia_partners)

langue <- c("en", "fr")

scraping_wikipedia <- function(site) {
  site_html <- read_html(site)
  tableau_liens_a_extraire <- site_html %>% html_table()
  nombre_de_vues <- tableau_liens_a_extraire[[1]][nrow(tableau_liens_a_extraire[[1]]),2]
  nombre_de_vues <- as.numeric(gsub(",", "", nombre_de_vues))
  return(nombre_de_vues)

}

wikipedia_df <- sapply(wikipedia, scraping_wikipedia)
wikipedia_partners_df <- sapply(wikipedia_partners, scraping_wikipedia)



création_data_frame_wikipedia <- function(wikipedia_df) {
  langue <- data.frame(c("en", "fr"))
  date <- floor_date(Sys.Date(), "month")
  
  
  wikipedia_df <- cbind(date, wikipedia_df, langue)
  colnames(wikipedia_df) <- c("date", "impressions", "langue")
  
  
  wikipedia_archive <- read.csv("wikipedia.csv")
  wikipedia_archive <- wikipedia_archive %>% select(date, impressions, langue, plateforme, organisation)  
  wikipedia_df <- wikipedia_df %>% select(date, impressions, langue) %>% mutate(plateforme = "wikipedia", organisation = "BSIF") 
  
  #View(wikipedia_archive[length(wikipedia_archive)-3:length(wikipedia_archive)])
  wikipedia_df <- rbind(wikipedia_df, wikipedia_archive)
  write_csv(wikipedia_df, "wikipedia.csv")
}



création_data_frame_partenaires <- function(wikipedia_partners_df) {
  library(tibble)
  wikipedia_partners_df <- as.data.frame(wikipedia_partners_df)
  wikipedia_partners_df <- tibble::rownames_to_column(wikipedia_partners_df, "adresse")
  
  wikipedia_partners_df <- wikipedia_partners_df %>% mutate(langue = str_extract_all(adresse, "\\D{2}(?=\\.wikipedia)")) %>% 
    mutate(nom = str_remove_all(adresse, ".*?title="))%>%
    mutate(nom = str_remove_all(nom, "&action=info")) %>%
    mutate(nom = str_replace_all(nom, "%C3%A8", "è")) %>% 
    mutate(nom = str_replace_all(nom, "%C3%A9", "é")) %>% 
    mutate(nom = str_replace_all(nom, "%C3%B4", "ô")) %>% 
    mutate(nom = str_replace_all(nom, "%27", "'")) %>% 
    mutate(date = floor_date(Sys.Date(), "month")) %>% 
    mutate(plateforme = "wikipedia")%>% 
    mutate(organisation = case_when( grepl("Banque_du_Canada|Bank_of_Canada", nom) ~ "BdC",
                                     grepl("Agence_de_la_consommation_en_matière_financière_du_Canada", nom) ~ "FCAC",
                                     grepl("Financial_Consumer_Agency_of_Canada", nom)~ "FCAC",
                                     grepl("Ministère_des_Finances_", nom)~ "Finance",             
                                     grepl("Department_of_Finance_Canada", nom)~ "Finance",                
                                     grepl("Société_d'assurance-dépôts_du_Canada", nom)  ~ "SADC",                   
                                     grepl("Canada_Deposit_Insurance_Corporation", nom) ~ "SADC",           
                                     grepl("Société_canadienne_d'hypothèques_et_de_logement", nom)~ "SCHL",
                                     grepl("Canada_Mortgage_and_Housing_Corporation", nom) ~ "SCHL" ))
  
  wikipedia_partners_df <- wikipedia_partners_df %>%rename("impressions" = "wikipedia_partners_df")
  wikipedia_partners_df <<- wikipedia_partners_df %>% select(date, impressions, langue, plateforme, organisation)
  
  #ajout du fichier partenaires au fichier "wikipedia"
  
  wikipedia_archive <- read.csv("wikipedia.csv")
  wikipedia_archive <- wikipedia_archive %>% select(date, impressions, langue, plateforme, organisation)  
  wikipedia_partners_df <- wikipedia_partners_df %>% select(date, impressions, langue, plateforme, organisation)
  
  wikipedia_partners_df <- rbind(wikipedia_partners_df, wikipedia_archive)
  write_csv(wikipedia_partners_df, "wikipedia.csv")
}
création_data_frame_wikipedia(wikipedia_df)
création_data_frame_partenaires(wikipedia_partners_df)
