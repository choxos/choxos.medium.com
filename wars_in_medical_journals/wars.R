library(europepmc)

wars = read.csv("wars_in_medical_journals/list_of_wars.csv")

issns = '(ISSN:"0959-8138" OR ISSN:"1756-1833" OR ISSN:"0140-6736" OR ISSN:"1474-547X" OR ISSN:"0028-4793" OR ISSN:"1533-4406" OR ISSN:"0098-7484" OR ISSN:"1538-3598")'
# journals = '("Lancet (London, England)"[Journal] OR "The New England journal of medicine"[Journal] OR "BMJ (Clinical research ed.)"[Journal] OR "JAMA"[Journal])'

db_wars = NULL

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
        
        db_wars = dplyr::bind_rows(db, db_wars)
}


db_wars = db_wars %>% 
        dplyr::rowwise() %>%
        dplyr::mutate(articles_sum = sum(dplyr::c_across(dplyr::everything()), na.rm = T))

db_wars$war = wars$name
db_wars$deaths = wars$deaths
db_wars$region = wars$region


# Plots

library(data.table)
long_db_wars = melt(setDT(db_wars), id.vars = c("war", "region", "deaths", "articles_sum"), variable.name = "year")
long_db_wars$year = as.numeric(as.character(long_db_wars$year))

library(ggplot2)
library(dplyr)

long_db = aggregate(value~year+region, data = long_db_wars, FUN = sum)
long_db_deaths = aggregate(deaths~region, data = long_db_wars, FUN = sum)
long_db = merge(long_db, long_db_deaths, by = "region")


long_db %>%
        ggplot(aes(x = year, y = value, color = region)) + 
        geom_line(size = 1.5) + 
        geom_point(size = 3) +
        scale_x_continuous(breaks = unique(long_db_wars$year)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Number of articles") +
        xlab("Year")

# Save to PNG
ggsave("wars_in_medical_journals/articles_region.png", width = 10, height = 6, dpi = 300)



long_db %>%
        ggplot(aes(x = year, y = value/deaths*1000000, color = region)) + 
        geom_line(size = 1.5) + 
        geom_point(size = 3) +
        scale_x_continuous(breaks = unique(long_db_wars$year)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Number of articles per one million deaths") +
        xlab("Year")

# Save to PNG
ggsave("wars_in_medical_journals/articles_per_1m_region.png", width = 10, height = 6, dpi = 300)


# Wars plot

long_db_wars %>%
        ggplot(aes(x = year, y = value/deaths*10000, color = war)) + 
        geom_line(size = 1.5) + 
        geom_point(size = 3) +
        scale_x_continuous(breaks = unique(long_db_wars$year)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Number of articles per ten thousand deaths") +
        xlab("Year")

# Save to PNG
ggsave("wars_in_medical_journals/articles_per_war.png", width = 10, height = 6, dpi = 300)


# Log10 scale
long_db_wars %>%
        ggplot(aes(x = year, y = value/deaths*10000, color = war)) + 
        geom_line(size = 1.5) + 
        geom_point(size = 3) +
        scale_y_continuous(trans = "log10") +
        scale_x_continuous(breaks = unique(long_db_wars$year)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Number of articles per ten thousand deaths in log10 scale") +
        xlab("Year")

# Save to PNG
ggsave("wars_in_medical_journals/articles_per_war_log10.png", width = 10, height = 6, dpi = 300)


# Heat table
# Crude
long_db_wars %>%
        ggplot() +
        aes(year, war) +
        geom_tile(aes(fill = value)) + 
        geom_text(aes(fill = long_db_wars$value, label = long_db_wars$value)) +
        scale_fill_gradient(low = "white",
                            high = "red",
                            na.value = NA) +
        theme(panel.grid.major.x=element_blank(), 
              panel.grid.minor.x=element_blank(), 
              panel.grid.major.y=element_blank(), 
              panel.grid.minor.y=element_blank(),
              panel.background=element_rect(fill="white"),
              axis.text.x = element_text(angle=45, hjust = 1,vjust=1,size = 10),
              axis.text.y = element_text(size = 12,face = "bold")) + 
        scale_x_continuous(breaks = unique(long_db_wars$year), name = "") +
        scale_y_discrete(name="") +
        theme(legend.title=element_text(face="bold", size=10)) +
        labs(fill="Articles")

# Save to PNG
ggsave("wars_in_medical_journals/articles_per_war_heat.png", width = 10, height = 6, dpi = 300)

# article per 10K deaths
long_db_wars = long_db_wars %>% mutate(article_per_10k_deaths = value/deaths*10000)

long_db_wars %>%
        ggplot() +
        aes(year, war) +
        geom_tile(aes(fill = article_per_10k_deaths)) + 
        geom_text(aes(fill = long_db_wars$article_per_10k_deaths, label = round(long_db_wars$article_per_10k_deaths, 2))) +
        scale_fill_gradient(low = "white",
                            high = "red",
                            na.value = NA) +
        theme(panel.grid.major.x=element_blank(), 
              panel.grid.minor.x=element_blank(), 
              panel.grid.major.y=element_blank(), 
              panel.grid.minor.y=element_blank(),
              panel.background=element_rect(fill="white"),
              axis.text.x = element_text(angle=45, hjust = 1,vjust=1,size = 10),
              axis.text.y = element_text(size = 12,face = "bold")) + 
        scale_x_continuous(breaks = unique(long_db_wars$year), name = "") +
        scale_y_discrete(name="") +
        theme(legend.title=element_text(face="bold", size=10)) +
        labs(fill="Articles per 10,000 deaths")

# Save to PNG
ggsave("wars_in_medical_journals/articles_per_10k_deaths_heat.png", width = 15, height = 8, dpi = 300)



# Regression

# Scatterplot
db_wars %>% ggplot() + 
        aes(x = deaths, y = articles_sum) + 
        geom_point() + 
        geom_smooth(method = "lm") +
        ylab("Number of articles") +
        xlab("Number of deaths") +
        scale_x_continuous(labels = scales::comma)

# Save to PNG
ggsave("wars_in_medical_journals/articles_vs_deaths.png", width = 10, height = 6, dpi = 300)

# Correlation
cor.test(db_wars$deaths, db_wars$articles_sum)


# Scatterplot in log
db_wars %>% ggplot() + 
        aes(x = log10(deaths), y = log10(articles_sum)) + 
        geom_point() + 
        geom_smooth(method = "lm") +
        ylab("Number of articles (log10)") +
        xlab("Number of deaths (log10)") +
        scale_x_continuous(labels = scales::comma)

# Save to PNG
ggsave("wars_in_medical_journals/articles_vs_deaths_log10.png", width = 10, height = 6, dpi = 300)


# Correlation in log
cor.test(log(db_wars$deaths), log(db_wars$articles_sum))

# Linear regression
lm_wars = lm(log(articles_sum)~log(deaths)+region, data = db_wars)
summary(lm_wars)

# NB2 regression
require(MASS)
nb_wars = glm.nb(articles_sum~deaths+region, data = db_wars)
summary(nb_wars)
