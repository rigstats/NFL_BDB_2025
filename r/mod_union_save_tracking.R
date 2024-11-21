# libraries ----
library(dplyr)
library(data.table)
library(future.apply)
library(arrow)


# flip and bind angles ----
standardize_o_dir <- function(x) {

   x2 <- x + 180

   if_else(x2 >= 360, x2 - 360, x2)

}


# process individual CSV files ----
process_csv <- function(file_path) {

   # Read the CSV file efficiently
   df <- fread(file_path)

   # Perform modifications
   df[, playDirectionRight := (playDirection=="right")]

   df[, x := if_else(playDirectionRight, x, 120-x)]
   df[, y := if_else(playDirectionRight, y, (160/3)-y)]

   df[, o := if_else(playDirectionRight, o, standardize_o_dir(o))]
   df[, dir := if_else(playDirectionRight, o, standardize_o_dir(dir))]

   df[, frameTypeInt := case_match(frameType,
                                   "BEFORE_SNAP" ~ -1,
                                   "SNAP" ~ 0,
                                   "AFTER_SNAP" ~ 1)]

   df[, nflId := fifelse(is.na(nflId), 0, nflId)]
   df[, jerseyNumber := fifelse(is.na(jerseyNumber), 0, jerseyNumber)]

   # df[, processed_column := some_transformation(original_column)]

   # Example of multiple column modifications
   # df[, `:=`(
   #    new_column1 = calculate_something(column1),
   #    new_column2 = another_calculation(column2),
   #    # Handle missing values
   #    column3 = fifelse(is.na(column3), 0, column3)
   # )]

   # Optional: Select or reorder columns
   # df <- df[, .(important_column1, important_column2, processed_column, new_column1, new_column2)]

   return(df)

}

# Main processing function ----
process_multiple_csvs <- function(directory_path, pattern = "tracking_week_[1-9].csv$") {

   # Get list of CSV files
   file_paths <- list.files(
      path = directory_path,
      pattern = pattern,
      full.names = TRUE
   )

   # Use future_lapply for parallel processing
   plan(multisession)  # Use multiple cores

   # Process files in parallel
   processed_list <- future_lapply(
      file_paths,
      process_csv,
      future.packages = c("data.table")
   )

   # Combine all processed dataframes
   combined_data <- rbindlist(processed_list, use.names = TRUE, fill = TRUE)

   return(combined_data)

}

# Usage ----

# check if already ran and saved dataset
tracking_union_mod_datasets <- list.files(path = "./input",
                                          pattern = "tracking_union_mod.parquet",
                                          full.names = TRUE)



if (length(tracking_union_mod_datasets) > 0) {

   tracking <- arrow::read_parquet(tracking_union_mod_datasets)

} else {

   final_dataset <- process_multiple_csvs(directory_path = "./input")

   arrow::write_parquet(final_dataset, "./input/tracking_union_mod.parquet")

}






# final_dataset <- process_multiple_csvs(directory_path = "./input")


# 52.89845 secs


# save output ----

# rds
# saveRDS(final_dataset, file = "./input/tracking_union_mod.rds", compress = "gzip")

# parquet
# arrow::write_parquet(final_dataset, "./input/tracking_union_mod.parquet")
