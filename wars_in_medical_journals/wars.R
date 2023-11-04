library(openxlsx)
library(europepmc)

wars = read.xlsx("wars_in_medical_journals/list_of_wars.xlsx", sheet = 2)


issns = '(ISSN:"0959-8138" OR ISSN:"1756-1833" OR ISSN:"0140-6736" OR ISSN:"1474-547X" OR ISSN:"0028-4793" OR ISSN:"1533-4406" OR ISSN:"0098-7484" OR ISSN:"1538-3598")'

db_all = NULL


for(i in 1:nrow(wars)) {
        # number of hits per year
        db = epmc_hits_trend(query = paste0(
                "(",
                wars$query[i],
                ")",
                " AND ",
                issns
        ), 
        period = wars$start[i]:ifelse(wars$end[i] == "Ongoing", 2023, wars$end[i]))
        
        # deleting the total hits column
        db$all_hits = NULL
        
        # converting to wide format
        db = tidyr::spread(db, year, query_hits)
        
        db_all = dplyr::bind_rows(db, db_all)
}


db_all = db_all %>% 
        dplyr::rowwise() %>%
        dplyr::mutate(articles_sum = sum(dplyr::c_across(dplyr::everything()), na.rm = T))

db_all$war = wars$name
db_all$deaths = wars$deaths
db_all$region = wars$region

lm_wars = lm(articles_sum~deaths+region, data = db_all)
summary(lm_wars)


require(MASS)
nb_wars = glm.nb(articles_sum~deaths+region, data = db_all)
summary(nb_wars)
