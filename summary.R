library("dplyr")
library("stringr")
library("ggplot2")


# original dataset
spl_df <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# dates reformatted
spl_df <- spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

# Stephen King data frame
author_df <- spl_df %>% filter(str_detect(spl_df$Creator, "Stephen King"))

# total checkouts for each material type
checkouts_per_type <- spl_df %>% 
                      group_by(MaterialType) %>% 
                      summarize(total_checkouts = sum(Checkouts))

# number of different material types
num_types <- nrow(checkouts_per_type)

# Stephen King total checkouts per month
checkouts_per_month <- author_df %>% 
                        group_by(date) %>% 
                        summarize(total_checkouts = sum(Checkouts))

# Stephen King average checkouts per month
avg_checkouts_per_month <- author_df %>% 
                            group_by(date) %>% 
                            summarize(avg_checkouts = mean(Checkouts))

# Stephen King max checkouts per month
max_checkouts_per_month <- author_df %>% 
                            group_by(date) %>% 
                            summarize(max_checkouts = max(Checkouts))


# average, min, max checkout
max_checkout <- max(author_df$Checkouts, na.rm = TRUE)
min_checkout <- min(author_df$Checkouts, na.rm = TRUE)
avg_checkout <- mean(author_df$Checkouts, na.rm = TRUE)

# book with most checkouts
book_most_checkouts <- author_df %>% 
                        filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% 
                        pull(Title)
# publication year of the book with most checkouts
pub_yr_mc <- author_df %>% 
              filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% 
              pull(PublicationYear)

# number of audiobooks & ebooks
audiobooks <- nrow(author_df %>% 
                    filter(MaterialType == "AUDIOBOOK"))
ebooks <- nrow(author_df %>% 
                filter(MaterialType == "EBOOK"))

# number of Stephen King's work in the data frame
num_works <- nrow(author_df)
