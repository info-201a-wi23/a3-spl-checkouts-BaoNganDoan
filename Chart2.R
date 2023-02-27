library("dplyr")
library("stringr")
library("ggplot2")

spl_df <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_df <- spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

author_df <- spl_df %>% filter(str_detect(spl_df$Creator, "Stephen King"))

CO_per_pubyr <- author_df %>% 
                        group_by(PublicationYear) %>% 
                        summarize(max_co_pubyr = max(Checkouts, na.rm = TRUE))

ggplot(data = CO_per_pubyr) +
  geom_point(mapping = aes(x = PublicationYear, 
                          y = max_co_pubyr)) +
  labs(title = "Most Checkouts of any Stephen King book in Published Years",
       x = "Year Published",
       y = "Checkouts")
