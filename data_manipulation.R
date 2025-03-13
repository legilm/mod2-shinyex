listings <- rio::import("data/listings.csv")

# Clean the price column
listings$price <- as.numeric(sub(",", ".", listings$price, fixed = TRUE)) # Replace comma by dot
listings$price <- as.numeric(gsub("[^0-9.,]", "", listings$price)) # Remove non-numeric characters
listings <- listings %>%
  drop_na(price) %>%
  filter(price > 0 & price < quantile(price, 0.99)) # Remove outliers


rio::export(listings, "data/final_dt.csv")
