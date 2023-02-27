library("dplyr")
library("stringr")
library("ggplot2")

spl_df <-
  read.csv("2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_df <-
  spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

author_df <-
  spl_df %>% filter(str_detect(spl_df$Creator, "Stephen King"))

checkouts_per_month <- author_df %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts))

ggplot(data = checkouts_per_month) +
  
  geom_line(mapping = aes(x = date,
                          y = total_checkouts)) +
  labs(title = "Stephen King Checkouts 2022",
       x = "Month",
       y = "Total Checkouts")
