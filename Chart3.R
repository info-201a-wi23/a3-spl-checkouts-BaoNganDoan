library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

spl_df <-
  read.csv("2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_df <-
  spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")
spl_df <- spl_df %>% 
          mutate(count = 1)

checkouts_per_type <- spl_df %>%
  group_by(MaterialType) %>%
  summarize(num_items = sum(count, na.rm = TRUE)) %>% 
  filter(num_items >= 1000)

ggplot(checkouts_per_type, aes(x = MaterialType, y = num_items, fill = num_items)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Items of each Material Type",
       x = "Type",
       y = "Number of Items",
       fill = "Total Items")+
  scale_y_continuous(labels = label_number_si())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
