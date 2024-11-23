#the file was downloaded directly from the PC local file with name sales2019
#changing name to book_sale
book_sale <- sales2019


#loading the tidyverse library
library(tidyverse)


#examining the dataset
head(book_sale)
glimpse(book_sale)



# What are the unique values in each column?
for (c in colnames(book_sale)) {
  print("Unique values in the column:")
  print(c)
  print(unique(book_sale[[c]]))
  print("")
}


#dealing with the missing values in total_purchased and user_submitted_review columns
#removing all the missing columns in the user_submitted_review column
book_sale <- book_sale %>% 
  filter(!is.na(user_submitted_review))


#calculating the average total_purchased made to estimate a value for the missing rows in the column
average_total_purchase <- book_sale %>% 
  filter(!is.na(total_purchased))
average_total_purchase <- mean(average_total_purchase$total_purchased)


#rounding the average to one decimal place
round_average_total_purchase <- round(average_total_purchase, 1)


#filling the missing columns in the total_purchased with the calculated average above
book_sale <- book_sale %>% 
  mutate(total_purchased = if_else(is.na(total_purchased), 
                                   round_average_total_purchase, total_purchased))


#creating a function that returns if a review is positive or negative
review_result <- book_sale %>%  
  mutate(is_positive = case_when(str_detect(user_submitted_review, "was okay") ~ TRUE, 
                                 str_detect(user_submitted_review, "OK") ~ TRUE,
                                 str_detect(user_submitted_review, "Awesome!") ~ TRUE,
                                 str_detect(user_submitted_review, "other books were better") ~ TRUE,
                                 str_detect(user_submitted_review, "I learnt a lot") ~ TRUE,
                                 TRUE ~ FALSE))



check_result <- function(x = book_sale$user_submitted_review) {
  review_result <- case_when(str_detect(book_sale$user_submitted_review, "was okay") ~ "Positive", 
                             str_detect(book_sale$user_submitted_review, "OK") ~ "Positive",
                             str_detect(book_sale$user_submitted_review, "Awesome!") ~ "Positive",
                             str_detect(book_sale$user_submitted_review, "I learned a lot") ~ "Positive",
                             str_detect(book_sale$user_submitted_review, "Never read a better book") ~ "Positive",
                             str_detect(book_sale$user_submitted_review, "Hated it") ~ "Negative",
                             str_detect(book_sale$user_submitted_review, "was not needed") ~ "Negative",
                             str_detect(book_sale$user_submitted_review, "not recommend") ~ "Negative",
                             str_detect(book_sale$user_submitted_review, "other books were better") ~ "Negative",
                             TRUE ~ "Undefined")
}


book_sale <- book_sale %>% 
  mutate(is_positive = check_result())


#converting the date string to actual date
book_sale <- book_sale %>% 
  mutate(date = mdy(date)) 


#categorizing the date between pre or post date
book_sale <- book_sale %>% 
  mutate(date_status = if_else(date > ydm("2019/1/7"), "post", "pre"))



#summarizing the pre and post program date sale to determine if the program helped increase sale
book_sale_summary <- book_sale %>% 
  group_by(date_status) %>% 
  summarize(period_sales = n())


#subdividing the summary analysis into customer type to see how each customers made sales during the different periods
book_sale_summary <- book_sale %>% 
  group_by(date_status, customer_type) %>% 
  summarize(period_sales = n())


#summarizing to see the difference in review sentiment before and after the sales program
sent_summary <- book_sale %>% 
  group_by(date_status, is_positive) %>% 
  summarize(num_review = n())


#extracting the months from the date column into a new column
book_sale <- book_sale %>% mutate(sale_month = month(date, label = TRUE))


#classifying the sales made in months
month_book_sale <- book_sale %>% group_by(sale_month) %>% summarize(month_total = n())


#the analysis result shows that there is no substantial improvement in sales as a resul of the sale program done on July 1st 2019