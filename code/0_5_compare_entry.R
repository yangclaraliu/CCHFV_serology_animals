require(pacman)
p_load(readxl, tidyverse, openxlsx)

# Read the two original files into the environment
YJ <- read_xlsx("data/CCHF data (Yeo Jin - Original).xlsx")
ZW <- read_xlsx("data/CCHF data (Zhiwei - Original).xlsx")

# Display dimensions
cat("YJ dimensions:", dim(YJ)[1], "rows,", dim(YJ)[2], "columns\n")
cat("ZW dimensions:", dim(ZW)[1], "rows,", dim(ZW)[2], "columns\n")

# Function to expand comma-separated values in ZW
# Helper function to extract number and label from a string like "255 (Total)"
extract_number_and_label <- function(x) {
  if(is.na(x) || x == "" || x == "\\" || x == "\"") {
    return(list(value = NA_character_, label = ""))
  }
  
  x_trimmed <- str_trim(x)
  
  # Extract label from parentheses
  label_match <- str_extract(x_trimmed, "\\(([^)]+)\\)")
  label <- if(!is.na(label_match)) {
    # Remove parentheses
    str_replace_all(label_match, c("\\(" = "", "\\)" = ""))
  } else {
    ""
  }
  
  # Extract numeric value (first number in the string)
  numbers <- str_extract_all(x_trimmed, "\\d+")[[1]]
  value <- if(length(numbers) > 0) {
    numbers[1]
  } else {
    NA_character_
  }
  
  return(list(value = value, label = label))
}

# Function to uncollapse rows with comma-separated values
uncollapse_rows <- function(df) {
  # Convert Numerator and Denominator to character to preserve values
  df <- df %>%
    mutate(
      Numerator = as.character(Numerator),
      Denominator = as.character(Denominator),
      `Target species` = as.character(`Target species`)
    )
  
  # Identify rows with comma-separated values
  rows_to_expand <- df %>%
    rowwise() %>%
    mutate(
      has_comma_num = grepl(",", Numerator),
      has_comma_den = grepl(",", Denominator),
      has_comma_species = grepl(",", `Target species`)
    ) %>%
    ungroup() %>%
    filter(has_comma_num | has_comma_den | has_comma_species)
  
  # Rows that don't need expansion
  rows_unchanged <- df %>%
    rowwise() %>%
    mutate(
      has_comma_num = grepl(",", Numerator),
      has_comma_den = grepl(",", Denominator),
      has_comma_species = grepl(",", `Target species`)
    ) %>%
    ungroup() %>%
    filter(!has_comma_num & !has_comma_den & !has_comma_species) %>%
    select(-has_comma_num, -has_comma_den, -has_comma_species)
  
  # Expand rows with comma-separated values
  expanded_rows <- rows_to_expand %>%
    rowwise() %>%
    mutate(
      # Split by comma and trim whitespace
      species_list = list(str_trim(str_split(`Target species`, ",")[[1]])),
      numerator_list = list(str_trim(str_split(Numerator, ",")[[1]])),
      denominator_list = list(str_trim(str_split(Denominator, ",")[[1]]))
    ) %>%
    mutate(
      n_species = length(species_list),
      n_num = length(numerator_list),
      n_den = length(denominator_list)
    ) %>%
    ungroup()
  
  # Separate rows with matched counts from mismatched ones
  # Cases that are considered "matched":
  # 1. n_species == n_num == n_den (perfect match)
  # 2. n_species == 1 but n_num == n_den > 1 (single species with labels - expand by num/den count)
  matched_rows <- expanded_rows %>%
    filter((n_species == n_num & n_species == n_den) | 
           (n_species == 1 & n_num == n_den & n_num > 1))
  
  mismatched_rows <- expanded_rows %>%
    filter(!((n_species == n_num & n_species == n_den) | 
             (n_species == 1 & n_num == n_den & n_num > 1)))
  
  if(nrow(mismatched_rows) > 0) {
    cat("\n⚠️  WARNING: Found", nrow(mismatched_rows), "rows with mismatched counts.\n")
    cat("These rows will be SKIPPED and saved to a separate file for manual review.\n")
    cat("Row IDs with mismatches:", paste(mismatched_rows$ID, collapse = ", "), "\n\n")
  }
  
  # Expand rows with matched counts
  result_list <- list()
  
  for(i in 1:nrow(matched_rows)) {
    row <- matched_rows[i, ]
    
    species_vec <- row$species_list[[1]]
    num_vec <- row$numerator_list[[1]]
    den_vec <- row$denominator_list[[1]]
    
    n_species <- length(species_vec)
    n_num <- length(num_vec)
    n_den <- length(den_vec)
    
    # Determine number of items to create
    # If n_species == 1 but n_num == n_den > 1, expand by num/den count (labels case)
    # Otherwise, expand by species count
    if(n_species == 1 && n_num == n_den && n_num > 1) {
      n_items <- n_num
      # Repeat the single species for each numerator/denominator
      species_vec <- rep(species_vec, n_items)
    } else {
      n_items <- n_species
    }
    
    # Create one row for each item
    for(j in 1:n_items) {
      # Extract number and label from numerator
      num_extracted <- extract_number_and_label(num_vec[j])
      # Extract number and label from denominator
      den_extracted <- extract_number_and_label(den_vec[j])
      
      # Determine the target species name
      # If there's a label in numerator or denominator, append it to species name
      base_species <- species_vec[j]
      labels <- c(num_extracted$label, den_extracted$label)
      labels <- labels[labels != "" & !is.na(labels)]
      
      # If we have labels, append the first non-empty label to species
      # If both numerator and denominator have labels, prefer numerator's label
      if(length(labels) > 0) {
        # Use the first label (usually from numerator)
        label_to_use <- labels[1]
        # Format: "camel (Total)" with space before parentheses
        species_name <- paste0(base_species, " (", label_to_use, ")")
      } else {
        species_name <- base_species
      }
      
      new_row <- row %>%
        select(-species_list, -numerator_list, -denominator_list, 
               -has_comma_num, -has_comma_den, -has_comma_species,
               -n_species, -n_num, -n_den) %>%
        mutate(
          `Target species` = species_name,
          Numerator = num_extracted$value,
          Denominator = den_extracted$value
        )
      result_list[[length(result_list) + 1]] <- new_row
    }
  }
  
  # Combine all expanded rows
  if(length(result_list) > 0) {
    expanded_df <- bind_rows(result_list)
  } else {
    expanded_df <- tibble()
  }
  
  # Combine with unchanged rows - ensure column types match
  rows_unchanged <- rows_unchanged %>%
    mutate(
      Numerator = as.character(Numerator),
      Denominator = as.character(Denominator)
    )
  
  final_df <- bind_rows(rows_unchanged, expanded_df) %>%
    arrange(ID)
  
  # Return both the expanded data and the mismatched rows for review
  return(list(expanded = final_df, mismatched = mismatched_rows))
}

# Uncollapse ZW data
cat("\nUncollapsing ZW data...\n")
result <- uncollapse_rows(ZW)
ZW_uncollapsed <- result$expanded
ZW_mismatched <- result$mismatched

cat("ZW uncollapsed dimensions:", dim(ZW_uncollapsed)[1], "rows,", dim(ZW_uncollapsed)[2], "cols\n")
cat("Original ZW dimensions:", dim(ZW)[1], "rows,", dim(ZW)[2], "cols\n")
cat("Expansion: ", dim(ZW_uncollapsed)[1] - dim(ZW)[1] + nrow(ZW_mismatched), "additional rows created\n")

# Save mismatched rows for manual review
if(nrow(ZW_mismatched) > 0) {
  # Clean up the mismatched rows for display (remove list columns)
  ZW_mismatched_clean <- ZW_mismatched %>%
    select(-species_list, -numerator_list, -denominator_list,
           -has_comma_num, -has_comma_den, -has_comma_species,
           -n_species, -n_num, -n_den)
  
  write.xlsx(ZW_mismatched_clean, "data/CCHF data (Zhiwei - Mismatched_for_review).xlsx")
  cat("⚠️  Mismatched rows saved to: data/CCHF data (Zhiwei - Mismatched_for_review).xlsx\n")
  cat("   Please review these", nrow(ZW_mismatched), "rows manually.\n\n")
  
  # Print details of mismatched rows
  cat("Details of mismatched rows:\n")
  for(i in 1:nrow(ZW_mismatched)) {
    row <- ZW_mismatched[i, ]
    # Handle ID as either numeric or character
    id_str <- as.character(row$ID)
    cat(sprintf("  ID %s: %d species, %d numerators, %d denominators\n",
                id_str, row$n_species, row$n_num, row$n_den))
    cat(sprintf("    Species: %s\n", row$`Target species`))
    cat(sprintf("    Numerator: %s\n", row$Numerator))
    cat(sprintf("    Denominator: %s\n\n", row$Denominator))
  }
}

# Save the uncollapsed version
write.xlsx(ZW_uncollapsed, "data/CCHF data (Zhiwei - Uncollapsed).xlsx")
cat("\n✓ Uncollapsed ZW data saved to: data/CCHF data (Zhiwei - Uncollapsed).xlsx\n")
cat("✓ ZW_uncollapsed object created in environment\n")

# Align column types between YJ and ZW_uncollapsed
cat("\n=== Aligning column types between YJ and ZW_uncollapsed ===\n")

align_column_types <- function(df1, df2, df1_name = "YJ", df2_name = "ZW_uncollapsed") {
  # Get common columns
  common_cols <- intersect(names(df1), names(df2))
  
  cat("Common columns:", length(common_cols), "\n")
  
  # Get column types
  types1 <- sapply(df1[common_cols], class)
  types2 <- sapply(df2[common_cols], class)
  
  # Find columns with different types
  type_mismatches <- which(types1 != types2)
  
  if(length(type_mismatches) > 0) {
    cat("Found", length(type_mismatches), "columns with type mismatches:\n")
    for(col in names(type_mismatches)) {
      cat(sprintf("  %s: %s (%s) vs %s (%s)\n", 
                  col, types1[col], df1_name, types2[col], df2_name))
    }
  } else {
    cat("All columns have matching types!\n")
    return(list(df1 = df1, df2 = df2))
  }
  
  # Create copies to modify
  df1_aligned <- df1
  df2_aligned <- df2
  
  # Align types column by column
  for(col in common_cols) {
    type1 <- types1[col]
    type2 <- types2[col]
    
    if(type1 == type2) {
      # Types already match, skip
      next
    }
    
    # Special handling for certain columns
    # Columns that should be numeric if possible: ID, Year of publication, Numerator, Denominator
    numeric_preferred_cols <- c("ID", "Year of publication", "Numerator", "Denominator")
    prefer_numeric <- col %in% numeric_preferred_cols
    
    # Try to convert to numeric if both can be safely converted
    if(prefer_numeric) {
      # For Numerator/Denominator, try to extract first number from text if needed
      if(col %in% c("Numerator", "Denominator")) {
        # Extract first number from character values
        if(type1 == "character") {
          df1_aligned[[col]] <- sapply(df1_aligned[[col]], function(x) {
            if(is.na(x) || x == "" || x == "\\" || x == "\"") return(NA_character_)
            numbers <- str_extract_all(x, "\\d+")[[1]]
            if(length(numbers) > 0) return(numbers[1])
            return(x)
          })
        }
        if(type2 == "character") {
          df2_aligned[[col]] <- sapply(df2_aligned[[col]], function(x) {
            if(is.na(x) || x == "" || x == "\\" || x == "\"") return(NA_character_)
            numbers <- str_extract_all(x, "\\d+")[[1]]
            if(length(numbers) > 0) return(numbers[1])
            return(x)
          })
        }
      }
      
      # Try converting both to numeric
      converted1 <- suppressWarnings(as.numeric(df1_aligned[[col]]))
      converted2 <- suppressWarnings(as.numeric(df2_aligned[[col]]))
      
      # Check if conversion was successful
      # Count how many non-NA values failed conversion
      failed1 <- sum(!is.na(df1_aligned[[col]]) & is.na(converted1))
      failed2 <- sum(!is.na(df2_aligned[[col]]) & is.na(converted2))
      
      # If most values convert successfully (allow some failures for edge cases),
      # convert to numeric. Otherwise, keep as character.
      # Allow up to 5% of non-NA values to fail conversion
      total1 <- sum(!is.na(df1_aligned[[col]]))
      total2 <- sum(!is.na(df2_aligned[[col]]))
      threshold1 <- max(1, floor(total1 * 0.05))  # 5% threshold, at least 1
      threshold2 <- max(1, floor(total2 * 0.05))
      
      if(failed1 <= threshold1 && failed2 <= threshold2 && (total1 > 0 || total2 > 0)) {
        # Most values convert successfully, use numeric
        df1_aligned[[col]] <- converted1
        df2_aligned[[col]] <- converted2
        if(failed1 > 0 || failed2 > 0) {
          cat(sprintf("    Note: %s converted to numeric with %d (df1) and %d (df2) non-convertible values set to NA\n", 
                      col, failed1, failed2))
        }
        next
      } else if(failed1 > 0 || failed2 > 0) {
        cat(sprintf("    Warning: %s kept as character due to %d (df1) and %d (df2) non-numeric values\n", 
                    col, failed1, failed2))
      }
      # If conversion not safe, fall through to character conversion
    }
    
    # Determine target type based on priority:
    # 1. If one is logical, convert to character
    # 2. If one is character, convert both to character (safest, preserves all info)
    # 3. Otherwise, default to character
    
    if(type1 == "logical" || type2 == "logical") {
      # Convert logical to character
      if(type1 == "logical") {
        df1_aligned[[col]] <- as.character(df1_aligned[[col]])
        df1_aligned[[col]][is.na(df1_aligned[[col]])] <- NA_character_
      }
      if(type2 == "logical") {
        df2_aligned[[col]] <- as.character(df2_aligned[[col]])
        df2_aligned[[col]][is.na(df2_aligned[[col]])] <- NA_character_
      }
    } else if(type1 == "character" || type2 == "character") {
      # Convert both to character (safest)
      if(type1 != "character") {
        df1_aligned[[col]] <- as.character(df1_aligned[[col]])
        df1_aligned[[col]][is.na(df1_aligned[[col]])] <- NA_character_
      }
      if(type2 != "character") {
        df2_aligned[[col]] <- as.character(df2_aligned[[col]])
        df2_aligned[[col]][is.na(df2_aligned[[col]])] <- NA_character_
      }
    } else {
      # Default: convert both to character
      df1_aligned[[col]] <- as.character(df1_aligned[[col]])
      df2_aligned[[col]] <- as.character(df2_aligned[[col]])
    }
  }
  
  # Verify alignment
  types1_new <- sapply(df1_aligned[common_cols], class)
  types2_new <- sapply(df2_aligned[common_cols], class)
  remaining_mismatches <- sum(types1_new != types2_new)
  
  if(remaining_mismatches == 0) {
    cat("✓ Successfully aligned all column types!\n")
  } else {
    cat("⚠️  Warning:", remaining_mismatches, "columns still have type mismatches after alignment.\n")
  }
  
  return(list(df1 = df1_aligned, df2 = df2_aligned))
}

# Align column types between YJ and ZW_uncollapsed
aligned_data <- align_column_types(YJ, ZW_uncollapsed, "YJ", "ZW_uncollapsed")
YJ <- aligned_data$df1
ZW_uncollapsed <- aligned_data$df2

# Verify final column types match
common_cols_final <- intersect(names(YJ), names(ZW_uncollapsed))
types_YJ_final <- sapply(YJ[common_cols_final], class)
types_ZW_final <- sapply(ZW_uncollapsed[common_cols_final], class)
if(all(types_YJ_final == types_ZW_final)) {
  cat("\n✓ Verification: All column types now match between YJ and ZW_uncollapsed!\n")
} else {
  mismatches_final <- which(types_YJ_final != types_ZW_final)
  cat("\n⚠️  Warning: Some column types still don't match:\n")
  for(col in names(mismatches_final)) {
    cat(sprintf("  %s: %s (YJ) vs %s (ZW_uncollapsed)\n", 
                col, types_YJ_final[col], types_ZW_final[col]))
  }
}

# Compare entries by study ID and generate HTML report
cat("\n=== Generating comparison report ===\n")

compare_by_study <- function(df1, df2, df1_name = "YJ", df2_name = "ZW") {
  # Ensure ID is numeric
  df1$ID <- as.numeric(df1$ID)
  df2$ID <- as.numeric(df2$ID)
  
  # Get all unique IDs
  all_ids <- sort(unique(c(df1$ID, df2$ID)))
  all_ids <- all_ids[!is.na(all_ids)]
  
  # Normalize species names for comparison (lowercase, trim)
  normalize_species <- function(x) {
    x <- tolower(str_trim(as.character(x)))
    x <- str_replace_all(x, "\\s+", " ")
    return(x)
  }
  
  # Create comparison results for each study
  comparison_results <- list()
  total_ids <- length(all_ids)
  
  for(idx in 1:total_ids) {
    id <- all_ids[idx]
    rows_df1 <- df1[df1$ID == id, ]
    rows_df2 <- df2[df2$ID == id, ]
    
    # Progress indicator (every 10 studies or for first/last)
    if(idx == 1 || idx == total_ids || idx %% 10 == 0) {
      cat(sprintf("Processing study %d (%d/%d)...\n", id, idx, total_ids))
    }
    
    # Filter out empty rows (rows where key fields are all NA or empty)
    # A row is considered empty if Target species is NA/empty AND both Numerator and Denominator are NA/empty
    filter_empty_rows <- function(df) {
      if(nrow(df) == 0) return(df)
      
      # Check each row
      keep_rows <- logical(nrow(df))
      for(i in 1:nrow(df)) {
        species_val <- df$`Target species`[i]
        num_val <- df$Numerator[i]
        den_val <- df$Denominator[i]
        
        # Check if species is empty
        species_empty <- is.na(species_val) || 
                        as.character(species_val) == "" || 
                        trimws(as.character(species_val)) == ""
        
        # Check if numerator is empty
        num_empty <- is.na(num_val) || 
                    as.character(num_val) == "" || 
                    as.character(num_val) == "NA" ||
                    trimws(as.character(num_val)) == ""
        
        # Check if denominator is empty
        den_empty <- is.na(den_val) || 
                    as.character(den_val) == "" || 
                    as.character(den_val) == "NA" ||
                    trimws(as.character(den_val)) == ""
        
        # Keep row if it has at least species OR (numerator AND denominator)
        keep_rows[i] <- !species_empty || (!num_empty && !den_empty)
      }
      
      return(df[keep_rows, , drop = FALSE])
    }
    
    # Filter to only non-empty rows
    rows_df1 <- filter_empty_rows(rows_df1)
    rows_df2 <- filter_empty_rows(rows_df2)
    
    issues <- c()
    
    # Check if ID exists in both datasets (after filtering empty rows)
    if(nrow(rows_df1) == 0 && nrow(rows_df2) == 0) {
      # Both are empty after filtering - no issues
      issues <- c()
    } else if(nrow(rows_df1) == 0) {
      issues <- c(issues, paste0("Study only in ", df2_name))
    } else if(nrow(rows_df2) == 0) {
      issues <- c(issues, paste0("Study only in ", df1_name))
    } else {
      # Compare number of entries (after filtering empty rows)
      if(nrow(rows_df1) != nrow(rows_df2)) {
        issues <- c(issues, paste0("Different number of entries (", df1_name, ": ", nrow(rows_df1), ", ", df2_name, ": ", nrow(rows_df2), ")"))
      }
      
      # Normalize species for comparison
      species_df1 <- normalize_species(rows_df1$`Target species`)
      species_df2 <- normalize_species(rows_df2$`Target species`)
      
      # Remove NA species from comparison
      species_df1 <- species_df1[!is.na(species_df1) & species_df1 != ""]
      species_df2 <- species_df2[!is.na(species_df2) & species_df2 != ""]
      
      # Compare species names
      species_set1 <- sort(unique(species_df1))
      species_set2 <- sort(unique(species_df2))
      
      if(!identical(species_set1, species_set2)) {
        only_in_df1 <- setdiff(species_set1, species_set2)
        only_in_df2 <- setdiff(species_set2, species_set1)
        # Filter out empty strings
        only_in_df1 <- only_in_df1[only_in_df1 != ""]
        only_in_df2 <- only_in_df2[only_in_df2 != ""]
        if(length(only_in_df1) > 0 || length(only_in_df2) > 0) {
          issue_text <- "Different species names"
          if(length(only_in_df1) > 0) {
            issue_text <- paste0(issue_text, " (only in ", df1_name, ": ", paste(only_in_df1, collapse = ", "), ")")
          }
          if(length(only_in_df2) > 0) {
            issue_text <- paste0(issue_text, " (only in ", df2_name, ": ", paste(only_in_df2, collapse = ", "), ")")
          }
          issues <- c(issues, issue_text)
        }
      }
      
      # Compare numerators and denominators by matching species
      # Track mismatches with details
      num_mismatch_details <- list()
      den_mismatch_details <- list()
      
      # Get all unique species (non-empty) that exist in BOTH datasets
      common_species <- intersect(species_set1, species_set2)
      common_species <- common_species[!is.na(common_species) & common_species != ""]
      
      for(species in common_species) {
        # Get rows for this species in both datasets
        rows1_species <- rows_df1[species_df1 == species, ]
        rows2_species <- rows_df2[species_df2 == species, ]
        
        if(nrow(rows1_species) > 0 && nrow(rows2_species) > 0) {
          # Convert to character and handle NA
          num1 <- as.character(rows1_species$Numerator)
          num1[is.na(num1)] <- "NA"
          num1 <- sort(num1)
          
          num2 <- as.character(rows2_species$Numerator)
          num2[is.na(num2)] <- "NA"
          num2 <- sort(num2)
          
          den1 <- as.character(rows1_species$Denominator)
          den1[is.na(den1)] <- "NA"
          den1 <- sort(den1)
          
          den2 <- as.character(rows2_species$Denominator)
          den2[is.na(den2)] <- "NA"
          den2 <- sort(den2)
          
          # Compare numerators - same species but different values
          if(!identical(num1, num2)) {
            num1_str <- paste(num1, collapse = ", ")
            num2_str <- paste(num2, collapse = ", ")
            num_mismatch_details[[species]] <- list(
              species = species,
              df1_values = num1_str,
              df2_values = num2_str
            )
          }
          
          # Compare denominators - same species but different values
          if(!identical(den1, den2)) {
            den1_str <- paste(den1, collapse = ", ")
            den2_str <- paste(den2, collapse = ", ")
            den_mismatch_details[[species]] <- list(
              species = species,
              df1_values = den1_str,
              df2_values = den2_str
            )
          }
        }
      }
      
      # Add issues for numerator mismatches
      if(length(num_mismatch_details) > 0) {
        for(species in names(num_mismatch_details)) {
          detail <- num_mismatch_details[[species]]
          issue_text <- paste0("Same species (", species, "), but different values for numerator (", 
                              df1_name, ": ", detail$df1_values, "; ", 
                              df2_name, ": ", detail$df2_values, ")")
          issues <- c(issues, issue_text)
        }
      }
      
      # Add issues for denominator mismatches
      if(length(den_mismatch_details) > 0) {
        for(species in names(den_mismatch_details)) {
          detail <- den_mismatch_details[[species]]
          issue_text <- paste0("Same species (", species, "), but different values for denominator (", 
                              df1_name, ": ", detail$df1_values, "; ", 
                              df2_name, ": ", detail$df2_values, ")")
          issues <- c(issues, issue_text)
        }
      }
      
      # Compare Year of publication (should be same across all rows for a study)
      if("Year of publication" %in% names(rows_df1) && "Year of publication" %in% names(rows_df2)) {
        year1 <- unique(rows_df1$`Year of publication`)
        year1 <- year1[!is.na(year1)]
        year2 <- unique(rows_df2$`Year of publication`)
        year2 <- year2[!is.na(year2)]
        
        if(length(year1) > 0 && length(year2) > 0) {
          # Extract numeric years from strings (handle cases like "2020" or "2020-2021")
          extract_years <- function(x) {
            x_char <- as.character(x)
            # Extract all 4-digit numbers (years)
            years <- str_extract_all(x_char, "\\b\\d{4}\\b")[[1]]
            if(length(years) > 0) {
              return(as.numeric(years))
            }
            # If no 4-digit year found, try to convert directly
            return(suppressWarnings(as.numeric(x_char)))
          }
          
          years1 <- unique(unlist(lapply(year1, extract_years)))
          years1 <- years1[!is.na(years1)]
          years2 <- unique(unlist(lapply(year2, extract_years)))
          years2 <- years2[!is.na(years2)]
          
          # Fuzzy match: consider matching if there's any overlap in years
          # or if the years are within 1 year of each other
          if(length(years1) > 0 && length(years2) > 0) {
            # Check for overlap
            overlap <- intersect(years1, years2)
            # Check if years are close (within 1 year)
            close_match <- any(sapply(years1, function(y1) any(abs(years2 - y1) <= 1)))
            
            if(length(overlap) == 0 && !close_match) {
              # No overlap and not close - different years
              year1_str <- paste(sort(unique(year1)), collapse = ", ")
              year2_str <- paste(sort(unique(year2)), collapse = ", ")
              issues <- c(issues, paste0("Different year of publication (", 
                                        df1_name, ": ", year1_str, "; ", 
                                        df2_name, ": ", year2_str, ")"))
            }
          } else if(length(years1) == 0 && length(years2) > 0) {
            # df1 has no valid year, df2 does
            issues <- c(issues, paste0("Year of publication missing in ", df1_name, 
                                      " (", df2_name, " has: ", paste(unique(year2), collapse = ", "), ")"))
          } else if(length(years1) > 0 && length(years2) == 0) {
            # df2 has no valid year, df1 does
            issues <- c(issues, paste0("Year of publication missing in ", df2_name, 
                                      " (", df1_name, " has: ", paste(unique(year1), collapse = ", "), ")"))
          }
        } else if(length(year1) > 0 && length(year2) == 0) {
          issues <- c(issues, paste0("Year of publication missing in ", df2_name))
        } else if(length(year1) == 0 && length(year2) > 0) {
          issues <- c(issues, paste0("Year of publication missing in ", df1_name))
        }
      }
      
      # Compare Study period (fuzzy matching)
      if("Study period" %in% names(rows_df1) && "Study period" %in% names(rows_df2)) {
        period1 <- unique(rows_df1$`Study period`)
        period1 <- period1[!is.na(period1) & as.character(period1) != ""]
        period2 <- unique(rows_df2$`Study period`)
        period2 <- period2[!is.na(period2) & as.character(period2) != ""]
        
        if(length(period1) > 0 && length(period2) > 0) {
          # Normalize study periods for comparison (more aggressive normalization)
          normalize_period <- function(x) {
            x <- tolower(str_trim(as.character(x)))
            # Remove all spaces
            x <- str_replace_all(x, "\\s+", "")
            # Normalize dashes and separators (handle both "-" and " - ")
            x <- str_replace_all(x, "\\s*-\\s*", "-")
            # Remove common punctuation that might differ
            x <- str_replace_all(x, "[.,;]", "")
            # Normalize date separators (handle both "/" and "-" in dates)
            # This helps with "09/2023-11/2023" vs "09/2023 - 11/2023"
            return(x)
          }
          
          period1_norm <- normalize_period(period1)
          period2_norm <- normalize_period(period2)
          
          # Fuzzy matching: check if periods are similar
          # Strategy: check if one contains the other, or if they share significant words/years
          match_found <- FALSE
          
          for(p1 in period1_norm) {
            for(p2 in period2_norm) {
              # Exact match after normalization
              if(p1 == p2) {
                match_found <- TRUE
                break
              }
              
              # Extract years from both periods
              years1 <- str_extract_all(p1, "\\b\\d{4}\\b")[[1]]
              years2 <- str_extract_all(p2, "\\b\\d{4}\\b")[[1]]
              
              # If they have the same years, consider them matching (format differences)
              if(length(years1) > 0 && length(years2) > 0) {
                if(setequal(years1, years2)) {
                  match_found <- TRUE
                  break
                }
                # Also check if there's significant overlap in years
                if(length(intersect(years1, years2)) >= min(2, min(length(years1), length(years2)))) {
                  match_found <- TRUE
                  break
                }
              }
              
              # One contains the other (for cases like "2020-2021" vs "2020")
              if(nchar(p1) > 5 && nchar(p2) > 5) {
                if(grepl(p1, p2, fixed = TRUE) || grepl(p2, p1, fixed = TRUE)) {
                  match_found <- TRUE
                  break
                }
              }
              
              # Check if they share significant words (months, etc.)
              words1 <- str_split(p1, "[^a-z0-9]+")[[1]]
              words2 <- str_split(p2, "[^a-z0-9]+")[[1]]
              # Remove very short words and numbers (we already checked years)
              words1 <- words1[nchar(words1) >= 3 & !grepl("^\\d+$", words1)]
              words2 <- words2[nchar(words2) >= 3 & !grepl("^\\d+$", words2)]
              # Check overlap
              if(length(words1) > 0 && length(words2) > 0) {
                if(length(intersect(words1, words2)) >= min(2, min(length(words1), length(words2)) * 0.5)) {
                  match_found <- TRUE
                  break
                }
              }
            }
            if(match_found) break
          }
          
          if(!match_found) {
            period1_str <- paste(sort(unique(period1)), collapse = "; ")
            period2_str <- paste(sort(unique(period2)), collapse = "; ")
            issues <- c(issues, paste0("Different study period (", 
                                      df1_name, ": ", period1_str, "; ", 
                                      df2_name, ": ", period2_str, ")"))
          }
        } else if(length(period1) > 0 && length(period2) == 0) {
          issues <- c(issues, paste0("Study period missing in ", df2_name))
        } else if(length(period1) == 0 && length(period2) > 0) {
          issues <- c(issues, paste0("Study period missing in ", df1_name))
        }
      }
      
      # Compare location information
      # Location fields: UNSD region, UNSD sub-region, Study (country), Study (region), Livestock or wild
      
      # Get unique location values for this study (location should be consistent across rows)
      get_unique_location <- function(df, field) {
        if(!field %in% names(df)) {
          return(character(0))
        }
        vals <- unique(df[[field]])
        vals <- vals[!is.na(vals) & as.character(vals) != "" & as.character(vals) != "NA"]
        return(sort(as.character(vals)))
      }
      
      # Compare UNSD region
      if("UNSD region" %in% names(rows_df1) && "UNSD region" %in% names(rows_df2)) {
        region1 <- get_unique_location(rows_df1, "UNSD region")
        region2 <- get_unique_location(rows_df2, "UNSD region")
        
        if(length(region1) > 0 && length(region2) > 0) {
          if(!identical(region1, region2)) {
            issues <- c(issues, paste0("Different UNSD region (", 
                                      df1_name, ": ", paste(region1, collapse = ", "), "; ", 
                                      df2_name, ": ", paste(region2, collapse = ", "), ")"))
          }
        } else if(length(region1) > 0 && length(region2) == 0) {
          issues <- c(issues, paste0("UNSD region missing in ", df2_name))
        } else if(length(region1) == 0 && length(region2) > 0) {
          issues <- c(issues, paste0("UNSD region missing in ", df1_name))
        }
      }
      
      # Compare UNSD sub-region (with automatic lookup based on country)
      if("UNSD sub-region" %in% names(rows_df1) && "UNSD sub-region" %in% names(rows_df2) &&
         "Study (country)" %in% names(rows_df1) && "Study (country)" %in% names(rows_df2)) {
        subregion1 <- get_unique_location(rows_df1, "UNSD sub-region")
        subregion2 <- get_unique_location(rows_df2, "UNSD sub-region")
        country1 <- get_unique_location(rows_df1, "Study (country)")
        country2 <- get_unique_location(rows_df2, "Study (country)")
        
        # Country to UNSD sub-region lookup table (based on UN official classification)
        # This maps countries to their correct UNSD sub-region
        country_to_subregion <- list(
          # Western Africa
          "benin" = "Western Africa",
          "burkina faso" = "Western Africa",
          "cape verde" = "Western Africa",
          "gambia" = "Western Africa",
          "ghana" = "Western Africa",
          "guinea" = "Western Africa",
          "guinea-bissau" = "Western Africa",
          "ivory coast" = "Western Africa",
          "côte d'ivoire" = "Western Africa",
          "cote d'ivoire" = "Western Africa",
          "liberia" = "Western Africa",
          "mali" = "Western Africa",
          "mauritania" = "Western Africa",
          "niger" = "Western Africa",
          "nigeria" = "Western Africa",
          "senegal" = "Western Africa",
          "sierra leone" = "Western Africa",
          "togo" = "Western Africa",
          # Central Africa
          "angola" = "Central Africa",
          "cameroon" = "Central Africa",
          "central african republic" = "Central Africa",
          "chad" = "Central Africa",
          "congo" = "Central Africa",
          "democratic republic of the congo" = "Central Africa",
          "drc" = "Central Africa",
          "equatorial guinea" = "Central Africa",
          "gabon" = "Central Africa",
          "sao tome and principe" = "Central Africa",
          # Eastern Africa
          "burundi" = "Eastern Africa",
          "comoros" = "Eastern Africa",
          "djibouti" = "Eastern Africa",
          "eritrea" = "Eastern Africa",
          "ethiopia" = "Eastern Africa",
          "kenya" = "Eastern Africa",
          "madagascar" = "Eastern Africa",
          "malawi" = "Eastern Africa",
          "mauritius" = "Eastern Africa",
          "mozambique" = "Eastern Africa",
          "rwanda" = "Eastern Africa",
          "seychelles" = "Eastern Africa",
          "somalia" = "Eastern Africa",
          "south sudan" = "Eastern Africa",
          "uganda" = "Eastern Africa",
          "united republic of tanzania" = "Eastern Africa",
          "tanzania" = "Eastern Africa",
          "zambia" = "Eastern Africa",
          "zimbabwe" = "Eastern Africa",
          # Southern Africa
          "botswana" = "Southern Africa",
          "eswatini" = "Southern Africa",
          "swaziland" = "Southern Africa",
          "lesotho" = "Southern Africa",
          "namibia" = "Southern Africa",
          "south africa" = "Southern Africa",
          # Northern Africa
          "algeria" = "Northern Africa",
          "egypt" = "Northern Africa",
          "libya" = "Northern Africa",
          "morocco" = "Northern Africa",
          "sudan" = "Northern Africa",
          "tunisia" = "Northern Africa",
          "western sahara" = "Northern Africa",
          # Western Asia
          "armenia" = "Western Asia",
          "azerbaijan" = "Western Asia",
          "bahrain" = "Western Asia",
          "cyprus" = "Western Asia",
          "georgia" = "Western Asia",
          "iraq" = "Western Asia",
          "israel" = "Western Asia",
          "jordan" = "Western Asia",
          "kuwait" = "Western Asia",
          "lebanon" = "Western Asia",
          "oman" = "Western Asia",
          "sultanate of oman" = "Western Asia",
          "palestine" = "Western Asia",
          "qatar" = "Western Asia",
          "saudi arabia" = "Western Asia",
          "state of palestine" = "Western Asia",
          "syria" = "Western Asia",
          "syrian arab republic" = "Western Asia",
          "turkey" = "Western Asia",
          "turkiye" = "Western Asia",
          "türkiye" = "Western Asia",
          "united arab emirates" = "Western Asia",
          "uae" = "Western Asia",
          "yemen" = "Western Asia",
          # Central Asia
          "kazakhstan" = "Central Asia",
          "kyrgyzstan" = "Central Asia",
          "tajikistan" = "Central Asia",
          "turkmenistan" = "Central Asia",
          "uzbekistan" = "Central Asia",
          # Eastern Asia
          "china" = "Eastern Asia",
          "hong kong" = "Eastern Asia",
          "japan" = "Eastern Asia",
          "korea" = "Eastern Asia",
          "south korea" = "Eastern Asia",
          "north korea" = "Eastern Asia",
          "macao" = "Eastern Asia",
          "mongolia" = "Eastern Asia",
          "taiwan" = "Eastern Asia",
          # Southern Asia
          "afghanistan" = "Southern Asia",
          "bangladesh" = "Southern Asia",
          "bhutan" = "Southern Asia",
          "india" = "Southern Asia",
          "iran" = "Southern Asia",
          "maldives" = "Southern Asia",
          "nepal" = "Southern Asia",
          "pakistan" = "Southern Asia",
          "sri lanka" = "Southern Asia",
          # South-Eastern Asia
          "brunei" = "South-Eastern Asia",
          "cambodia" = "South-Eastern Asia",
          "indonesia" = "South-Eastern Asia",
          "laos" = "South-Eastern Asia",
          "malaysia" = "South-Eastern Asia",
          "myanmar" = "South-Eastern Asia",
          "philippines" = "South-Eastern Asia",
          "singapore" = "South-Eastern Asia",
          "thailand" = "South-Eastern Asia",
          "timor-leste" = "South-Eastern Asia",
          "vietnam" = "South-Eastern Asia",
          # Eastern Europe
          "belarus" = "Eastern Europe",
          "bulgaria" = "Eastern Europe",
          "czech republic" = "Eastern Europe",
          "czechia" = "Eastern Europe",
          "hungary" = "Eastern Europe",
          "poland" = "Eastern Europe",
          "moldova" = "Eastern Europe",
          "romania" = "Eastern Europe",
          "russia" = "Eastern Europe",
          "russian federation" = "Eastern Europe",
          "slovakia" = "Eastern Europe",
          "ukraine" = "Eastern Europe",
          # Northern Europe
          "denmark" = "Northern Europe",
          "estonia" = "Northern Europe",
          "finland" = "Northern Europe",
          "iceland" = "Northern Europe",
          "ireland" = "Northern Europe",
          "latvia" = "Northern Europe",
          "lithuania" = "Northern Europe",
          "norway" = "Northern Europe",
          "sweden" = "Northern Europe",
          "united kingdom" = "Northern Europe",
          "uk" = "Northern Europe",
          # Southern Europe
          "albania" = "Southern Europe",
          "andorra" = "Southern Europe",
          "bosnia and herzegovina" = "Southern Europe",
          "croatia" = "Southern Europe",
          "greece" = "Southern Europe",
          "italy" = "Southern Europe",
          "malta" = "Southern Europe",
          "montenegro" = "Southern Europe",
          "north macedonia" = "Southern Europe",
          "macedonia" = "Southern Europe",
          "portugal" = "Southern Europe",
          "san marino" = "Southern Europe",
          "serbia" = "Southern Europe",
          "slovenia" = "Southern Europe",
          "spain" = "Southern Europe",
          "kosovo" = "Southern Europe",
          # Western Europe
          "austria" = "Western Europe",
          "belgium" = "Western Europe",
          "france" = "Western Europe",
          "germany" = "Western Europe",
          "liechtenstein" = "Western Europe",
          "luxembourg" = "Western Europe",
          "monaco" = "Western Europe",
          "netherlands" = "Western Europe",
          "switzerland" = "Western Europe"
        )
        
        # Function to lookup correct UNSD sub-region for a country
        lookup_subregion <- function(country_name) {
          if(is.na(country_name) || country_name == "") {
            return(NA_character_)
          }
          
          country_lower <- tolower(str_trim(as.character(country_name)))
          
          # Check if country is in lookup table
          if(country_lower %in% names(country_to_subregion)) {
            return(country_to_subregion[[country_lower]])
          }
          
          # Try to match with country variations (handle "Spain, Portugal" etc.)
          # Extract individual countries and lookup each
          country_lower <- str_replace_all(country_lower, "\\s+and\\s+", ", ")
          countries <- str_split(country_lower, ",")[[1]]
          countries <- str_trim(countries)
          countries <- countries[countries != ""]
          
          subregions <- c()
          for(c in countries) {
            if(c %in% names(country_to_subregion)) {
              subregions <- c(subregions, country_to_subregion[[c]])
            }
          }
          
          if(length(subregions) > 0) {
            # Return unique subregions (might be a vector if multiple countries)
            unique_subregions <- unique(subregions)
            # If all countries have same sub-region, return single value
            if(length(unique_subregions) == 1) {
              return(unique_subregions[1])
            } else {
              # Multiple different sub-regions - return first (or could return all)
              return(unique_subregions[1])
            }
          }
          
          return(NA_character_)
        }
        
        if(length(subregion1) > 0 && length(subregion2) > 0) {
          # Get correct sub-region(s) based on country
          correct_subregion1 <- NA_character_
          correct_subregion2 <- NA_character_
          
          if(length(country1) > 0) {
            correct_subregion1 <- lookup_subregion(country1[1])
          }
          if(length(country2) > 0) {
            correct_subregion2 <- lookup_subregion(country2[1])
          }
          
          # Normalize for comparison
          normalize_text <- function(x) {
            x <- tolower(str_trim(as.character(x)))
            x <- str_replace_all(x, "\\s+", " ")
            return(x)
          }
          
          subregion1_norm <- normalize_text(subregion1)
          subregion2_norm <- normalize_text(subregion2)
          
          # Normalize correct sub-regions for comparison
          # lookup_subregion returns a single character value or NA
          correct_subregion1_norm <- if(!is.na(correct_subregion1) && correct_subregion1 != "") {
            normalize_text(correct_subregion1)
          } else {
            NA_character_
          }
          
          correct_subregion2_norm <- if(!is.na(correct_subregion2) && correct_subregion2 != "") {
            normalize_text(correct_subregion2)
          } else {
            NA_character_
          }
          
          # Check if entries match exactly first
          if(identical(sort(subregion1_norm), sort(subregion2_norm))) {
            # They match exactly - no issue
          } else {
            # They're different - try to auto-resolve using country lookup
            auto_resolved <- FALSE
            
            # Check if one is "Sub-Saharan Africa" (broad category that needs resolution)
            has_sub_saharan_africa1 <- any(grepl("sub.saharan.africa", subregion1_norm, ignore.case = TRUE))
            has_sub_saharan_africa2 <- any(grepl("sub.saharan.africa", subregion2_norm, ignore.case = TRUE))
            
            # Check if countries are the same (for same study, they should be)
            # This will use the same country comparison logic that handles variations
            # For now, use a simple approach: if lookup returns same sub-region for both countries,
            # and the countries are for the same study ID, they're likely the same country
            # We'll rely on the country comparison section to handle country name matching
            
            # Simple check: if both lookups return the same correct sub-region and it's not NA,
            # and the entries match that correct sub-region, they're the same
            countries_match <- FALSE
            
            # For UNSD sub-region auto-resolution, we can use the fact that if the correct
            # sub-regions are the same and both entries match them, they should be considered matching
            # This works even if country names are slightly different (handled by lookup)
            
            # Actually, for the same study ID, countries should match - the country comparison
            # will catch if they don't. So for UNSD sub-region, we can assume if we're comparing
            # the same study, the countries are intended to be the same
            countries_match <- TRUE  # Assume same study = same country (country comparison will flag if different)
            
            # Auto-resolve if countries are the same and we have correct sub-region lookup
            if(countries_match && !is.na(correct_subregion1_norm)) {
              # Countries are the same, use correct sub-region for auto-resolution
              correct_norm <- correct_subregion1_norm
              
              if(length(correct_norm) == 1) {
                # Case 1: One has "Sub-Saharan Africa", other has correct specific sub-region
                if(has_sub_saharan_africa1 && !has_sub_saharan_africa2) {
                  if(any(subregion2_norm == correct_norm)) {
                    auto_resolved <- TRUE
                  }
                } else if(has_sub_saharan_africa2 && !has_sub_saharan_africa1) {
                  if(any(subregion1_norm == correct_norm)) {
                    auto_resolved <- TRUE
                  }
                } else if(has_sub_saharan_africa1 && has_sub_saharan_africa2) {
                  # Both are "Sub-Saharan Africa" - they match
                  auto_resolved <- TRUE
                } else {
                  # Neither is "Sub-Saharan Africa" - check if both match correct
                  if(any(subregion1_norm == correct_norm) && any(subregion2_norm == correct_norm)) {
                    auto_resolved <- TRUE
                  }
                }
              }
            } else if(!is.na(correct_subregion1_norm) && !is.na(correct_subregion2_norm)) {
              # Countries might be different, but if they have same correct sub-region and both match it
              if(identical(correct_subregion1_norm, correct_subregion2_norm) && 
                 length(correct_subregion1_norm) == 1) {
                if(any(subregion1_norm == correct_subregion1_norm) && 
                   any(subregion2_norm == correct_subregion1_norm)) {
                  auto_resolved <- TRUE
                }
              }
            }
            
            # If not auto-resolved, check fuzzy matching
            if(!auto_resolved) {
              match_found <- FALSE
              for(sr1 in subregion1_norm) {
                for(sr2 in subregion2_norm) {
                  if(sr1 == sr2 || grepl(sr1, sr2, fixed = TRUE) || grepl(sr2, sr1, fixed = TRUE)) {
                    match_found <- TRUE
                    break
                  }
                }
                if(match_found) break
              }
              
              if(!match_found) {
                issues <- c(issues, paste0("Different UNSD sub-region (", 
                                          df1_name, ": ", paste(subregion1, collapse = ", "), "; ", 
                                          df2_name, ": ", paste(subregion2, collapse = ", "), ")"))
              }
            }
          }
        } else if(length(subregion1) > 0 && length(subregion2) == 0) {
          issues <- c(issues, paste0("UNSD sub-region missing in ", df2_name))
        } else if(length(subregion1) == 0 && length(subregion2) > 0) {
          issues <- c(issues, paste0("UNSD sub-region missing in ", df1_name))
        }
      }
      
      # Compare Study (country) (fuzzy matching)
      if("Study (country)" %in% names(rows_df1) && "Study (country)" %in% names(rows_df2)) {
        country1 <- get_unique_location(rows_df1, "Study (country)")
        country2 <- get_unique_location(rows_df2, "Study (country)")
        
        if(length(country1) > 0 && length(country2) > 0) {
          # Helper function to extract individual country names from a string
          # Handles both comma-separated and "and"-separated lists
          extract_countries <- function(country_str) {
            # Normalize: replace "and" with comma, then split
            country_str <- tolower(str_trim(as.character(country_str)))
            country_str <- str_replace_all(country_str, "\\s+and\\s+", ", ")
            country_str <- str_replace_all(country_str, "\\s*,\\s*", ",")
            # Split by comma
            countries <- str_split(country_str, ",")[[1]]
            countries <- str_trim(countries)
            countries <- countries[countries != ""]
            # Sort for consistent comparison
            return(sort(countries))
          }
          
          # Extract individual countries from each entry
          # Handle case where country1/country2 might be a vector
          all_countries1 <- c()
          for(c1 in country1) {
            countries <- extract_countries(c1)
            all_countries1 <- c(all_countries1, countries)
          }
          all_countries1 <- unique(all_countries1)
          all_countries1 <- sort(all_countries1)
          
          all_countries2 <- c()
          for(c2 in country2) {
            countries <- extract_countries(c2)
            all_countries2 <- c(all_countries2, countries)
          }
          all_countries2 <- unique(all_countries2)
          all_countries2 <- sort(all_countries2)
          
          # Compare the sets of countries
          # First check if sets are identical (after extracting and sorting)
          if(identical(all_countries1, all_countries2)) {
            # Sets match exactly - no issue
            match_found <- TRUE
          } else {
            # Sets are different - check for fuzzy match
            match_found <- FALSE
            
            # Normalize country names for fuzzy matching
            normalize_country <- function(x) {
              x <- tolower(str_trim(as.character(x)))
              x <- str_replace_all(x, "\\s+", " ")
              return(x)
            }
            
            # Country name variations mapping (common alternative names)
            # Maps variations to a canonical name for comparison
            country_variations <- list(
              "turkey" = c("turkey", "turkiye", "türkiye"),
              "united states" = c("united states", "united states of america", "usa", "u.s.a.", "u.s.", "us"),
              "united kingdom" = c("united kingdom", "uk", "u.k.", "great britain", "britain", "england"),
              "russian federation" = c("russian federation", "russia", "russian"),
              "south korea" = c("south korea", "republic of korea", "korea"),
              "north korea" = c("north korea", "democratic people's republic of korea", "dprk"),
              "czech republic" = c("czech republic", "czechia"),
              "myanmar" = c("myanmar", "burma"),
              "ivory coast" = c("ivory coast", "côte d'ivoire", "cote d'ivoire"),
              "east timor" = c("east timor", "timor-leste"),
              "macedonia" = c("macedonia", "north macedonia"),
              "swaziland" = c("swaziland", "eswatini")
            )
            
            # Function to get canonical country name (or return original if not in variations)
            get_canonical_country <- function(country_name) {
              country_lower <- tolower(str_trim(country_name))
              
              # Check if this country is in our variations list
              for(canonical in names(country_variations)) {
                if(country_lower %in% country_variations[[canonical]]) {
                  return(canonical)
                }
              }
              
              # Not found in variations, return normalized original
              return(country_lower)
            }
            
            # Normalize and convert to canonical names
            countries1_canonical <- sapply(all_countries1, function(c) {
              c_norm <- tolower(str_trim(as.character(c)))
              return(get_canonical_country(c_norm))
            })
            countries2_canonical <- sapply(all_countries2, function(c) {
              c_norm <- tolower(str_trim(as.character(c)))
              return(get_canonical_country(c_norm))
            })
            countries1_canonical <- sort(unique(countries1_canonical))
            countries2_canonical <- sort(unique(countries2_canonical))
            
            # Check if sets match exactly after normalization and canonicalization
            if(identical(countries1_canonical, countries2_canonical)) {
              match_found <- TRUE
            } else if(length(countries1_canonical) == length(countries2_canonical)) {
              # Try to match each country in set1 with a country in set2 using canonical names
              all_match <- TRUE
              matched2_canonical <- logical(length(countries2_canonical))
              
              for(i in 1:length(countries1_canonical)) {
                c1_canonical <- countries1_canonical[i]
                found_match <- FALSE
                for(j in 1:length(countries2_canonical)) {
                  if(!matched2_canonical[j]) {
                    c2_canonical <- countries2_canonical[j]
                    if(c1_canonical == c2_canonical) {
                      matched2_canonical[j] <- TRUE
                      found_match <- TRUE
                      break
                    }
                  }
                }
                if(!found_match) {
                  all_match <- FALSE
                  break
                }
              }
              
              if(all_match && all(matched2_canonical)) {
                match_found <- TRUE
              } else {
                # Fall back to substring matching on normalized names
                all_match <- TRUE
                matched2 <- logical(length(all_countries2))
                for(c1 in all_countries1) {
                  c1_lower <- tolower(str_trim(as.character(c1)))
                  found_match <- FALSE
                  for(i in 1:length(all_countries2)) {
                    if(!matched2[i]) {
                      c2 <- all_countries2[i]
                      c2_lower <- tolower(str_trim(as.character(c2)))
                      # Exact match
                      if(c1_lower == c2_lower) {
                        matched2[i] <- TRUE
                        found_match <- TRUE
                        break
                      }
                      # One contains the other (for variations like "United States" vs "USA")
                      if(nchar(c1_lower) > 3 && nchar(c2_lower) > 3) {
                        if(grepl(c1_lower, c2_lower, fixed = TRUE) || grepl(c2_lower, c1_lower, fixed = TRUE)) {
                          matched2[i] <- TRUE
                          found_match <- TRUE
                          break
                        }
                      }
                    }
                  }
                  if(!found_match) {
                    all_match <- FALSE
                    break
                  }
                }
                if(all_match && all(matched2)) {
                  match_found <- TRUE
                }
              }
            }
            
            if(!match_found) {
              issues <- c(issues, paste0("Different study country (", 
                                        df1_name, ": ", paste(country1, collapse = ", "), "; ", 
                                        df2_name, ": ", paste(country2, collapse = ", "), ")"))
            }
          }
        } else if(length(country1) > 0 && length(country2) == 0) {
          issues <- c(issues, paste0("Study country missing in ", df2_name))
        } else if(length(country1) == 0 && length(country2) > 0) {
          issues <- c(issues, paste0("Study country missing in ", df1_name))
        }
      }
      
      # Compare Study (region) using word-based fuzzy matching
      if("Study (region)" %in% names(rows_df1) && "Study (region)" %in% names(rows_df2)) {
        region1 <- get_unique_location(rows_df1, "Study (region)")
        region2 <- get_unique_location(rows_df2, "Study (region)")
        
        if(length(region1) > 0 && length(region2) > 0) {
          # Take the first region (should be consistent for a study)
          region1_str <- region1[1]
          region2_str <- region2[1]
          
          # Helper function to normalize region text for comparison
          normalize_region_text <- function(x) {
            x <- tolower(str_trim(as.character(x)))
            # Normalize hyphens and punctuation
            x <- str_replace_all(x, "-", " ")
            x <- str_replace_all(x, "[^a-z0-9 ]", " ")
            x <- str_replace_all(x, " +", " ")
            return(str_trim(x))
          }
          
          # Helper function to detect "All X regions of [Country]" pattern
          detect_all_regions_pattern <- function(region_str) {
            # Pattern: "all X regions of [country]" or "all X regions in [country]"
            pattern <- "all\\s+\\d+\\s+regions?\\s+(?:of|in)\\s+(.+)"
            match <- str_extract(tolower(region_str), pattern)
            if(!is.na(match)) {
              # Extract country name
              country_match <- str_replace(region_str, ".*all\\s+\\d+\\s+regions?\\s+(?:of|in)\\s+", "")
              country_match <- str_trim(tolower(country_match))
              return(list(is_pattern = TRUE, country = country_match))
            }
            return(list(is_pattern = FALSE, country = ""))
          }
          
          # Helper function to extract words from region text
          extract_meaningful_words <- function(region_str) {
            # Normalize first
            normalized <- normalize_region_text(region_str)
            # Split into words
            words <- str_split(normalized, " ")[[1]]
            # Remove very short words and common stop words
            words <- words[nchar(words) >= 3]
            # Remove common stop words that don't add meaning
            stop_words <- c("the", "and", "of", "in", "all", "regions", "region", "provinces", "province")
            words <- words[!words %in% stop_words]
            return(words)
          }
          
          # Check for special cases first
          # Case 1: "Not reported" or empty/missing values
          if(tolower(str_trim(region1_str)) %in% c("not reported", "na", "n/a", "") ||
             tolower(str_trim(region2_str)) %in% c("not reported", "na", "n/a", "")) {
            # If one is "Not reported" and the other isn't, they're different
            if(tolower(str_trim(region1_str)) %in% c("not reported", "na", "n/a", "") &&
               !tolower(str_trim(region2_str)) %in% c("not reported", "na", "n/a", "")) {
              issues <- c(issues, paste0("Different study region (", 
                                        df1_name, ": ", region1_str, "; ", 
                                        df2_name, ": ", region2_str, ")"))
            } else if(!tolower(str_trim(region1_str)) %in% c("not reported", "na", "n/a", "") &&
                      tolower(str_trim(region2_str)) %in% c("not reported", "na", "n/a", "")) {
              issues <- c(issues, paste0("Different study region (", 
                                        df1_name, ": ", region1_str, "; ", 
                                        df2_name, ": ", region2_str, ")"))
            }
            # If both are "Not reported", they match (don't add issue)
          } else {
            # Case 2: Check for "All X regions of [Country]" pattern
            pattern1 <- detect_all_regions_pattern(region1_str)
            pattern2 <- detect_all_regions_pattern(region2_str)
            
            match_found <- FALSE
            
            # If one is "All X regions" pattern and the other is a comma-separated list
            # Check if they refer to the same country and similar count
            if(pattern1$is_pattern || pattern2$is_pattern) {
              if(pattern1$is_pattern && !pattern2$is_pattern) {
                # region1 is "All X regions of Country", region2 is a list
                if(grepl(",", region2_str)) {
                  # region2 is a comma-separated list
                  # Check if country matches (using study country if available)
                  country1 <- get_unique_location(rows_df1, "Study (country)")
                  country2 <- get_unique_location(rows_df2, "Study (country)")
                  
                  if(length(country1) > 0 && length(country2) > 0) {
                    country1_norm <- tolower(str_trim(country1[1]))
                    country2_norm <- tolower(str_trim(country2[1]))
                    pattern_country <- pattern1$country
                    
                    # Check if countries match
                    if(country1_norm == country2_norm && 
                       (pattern_country == country1_norm || pattern_country == country2_norm ||
                        grepl(pattern_country, country1_norm) || grepl(pattern_country, country2_norm) ||
                        grepl(country1_norm, pattern_country) || grepl(country2_norm, pattern_country))) {
                      # Count items in the list
                      list_items <- str_split(region2_str, ",")[[1]]
                      list_items <- str_trim(list_items)
                      list_items <- list_items[list_items != ""]
                      
                      # Extract number from pattern
                      num_match <- str_extract(region1_str, "\\d+")
                      if(!is.na(num_match)) {
                        pattern_num <- as.numeric(num_match)
                        # If list count is close to pattern number, consider it a match
                        if(abs(length(list_items) - pattern_num) <= 2) {
                          match_found <- TRUE
                        }
                      }
                    }
                  }
                }
              } else if(!pattern1$is_pattern && pattern2$is_pattern) {
                # region2 is "All X regions of Country", region1 is a list
                if(grepl(",", region1_str)) {
                  # region1 is a comma-separated list
                  country1 <- get_unique_location(rows_df1, "Study (country)")
                  country2 <- get_unique_location(rows_df2, "Study (country)")
                  
                  if(length(country1) > 0 && length(country2) > 0) {
                    country1_norm <- tolower(str_trim(country1[1]))
                    country2_norm <- tolower(str_trim(country2[1]))
                    pattern_country <- pattern2$country
                    
                    if(country1_norm == country2_norm && 
                       (pattern_country == country1_norm || pattern_country == country2_norm ||
                        grepl(pattern_country, country1_norm) || grepl(pattern_country, country2_norm) ||
                        grepl(country1_norm, pattern_country) || grepl(country2_norm, pattern_country))) {
                      list_items <- str_split(region1_str, ",")[[1]]
                      list_items <- str_trim(list_items)
                      list_items <- list_items[list_items != ""]
                      
                      num_match <- str_extract(region2_str, "\\d+")
                      if(!is.na(num_match)) {
                        pattern_num <- as.numeric(num_match)
                        if(abs(length(list_items) - pattern_num) <= 2) {
                          match_found <- TRUE
                        }
                      }
                    }
                  }
                }
              }
            }
            
            # Case 3: Word-based fuzzy matching
            if(!match_found) {
              # Normalize both regions
              region1_norm <- normalize_region_text(region1_str)
              region2_norm <- normalize_region_text(region2_str)
              
              # Exact match after normalization
              if(region1_norm == region2_norm) {
                match_found <- TRUE
              } else {
                # Extract meaningful words
                words1 <- extract_meaningful_words(region1_str)
                words2 <- extract_meaningful_words(region2_str)
                
                if(length(words1) > 0 && length(words2) > 0) {
                  # Remove descriptive/quantitative words that don't represent location names
                  # These are words like: "localities", "villages", "farms", "districts", "provinces", 
                  # "regions", "governorate", "governorates", "division", "divisions", numbers, etc.
                  descriptive_words <- c("localities", "locality", "villages", "village", "farms", "farm",
                                       "districts", "district", "provinces", "province", "regions", "region",
                                       "governorate", "governorates", "division", "divisions", "areas", "area",
                                       "cities", "city", "towns", "town", "counties", "county", "states", "state",
                                       "within", "in", "of", "and", "the", "all", "some", "several", "many",
                                       "north", "south", "east", "west", "central", "northern", "southern",
                                       "eastern", "western", "centre", "center", "southwest", "southeast",
                                       "northwest", "northeast")
                  
                  # Filter out descriptive words to get core location names
                  core_words1 <- words1[!words1 %in% descriptive_words]
                  core_words2 <- words2[!words2 %in% descriptive_words]
                  
                  # Also remove numbers (they're usually counts, not location names)
                  core_words1 <- core_words1[!grepl("^\\d+$", core_words1)]
                  core_words2 <- core_words2[!grepl("^\\d+$", core_words2)]
                  
                  # If we have core location words, use those for matching
                  # Otherwise, fall back to all words
                  if(length(core_words1) > 0 && length(core_words2) > 0) {
                    # Calculate overlap using core location words
                    common_core <- intersect(core_words1, core_words2)
                    overlap_ratio <- length(common_core) / max(length(core_words1), length(core_words2), 1)
                    
                    # Lower threshold for core words (since we've removed descriptive text)
                    # If any core location words match, it's likely the same place
                    if(overlap_ratio > 0) {
                      match_found <- TRUE
                    }
                  }
                  
                  # If core word matching didn't work, try full word matching
                  if(!match_found) {
                    common_words <- intersect(words1, words2)
                    overlap_ratio <- length(common_words) / max(length(words1), length(words2), 1)
                    
                    # Threshold: 50% overlap for shorter lists, 40% for longer lists
                    threshold <- ifelse(max(length(words1), length(words2)) <= 5, 0.5, 0.4)
                    
                    if(overlap_ratio >= threshold) {
                      match_found <- TRUE
                    } else {
                      # Check if shorter list is mostly contained in longer list
                      if(length(words1) >= 2 && length(words2) >= 2) {
                        shorter <- if(length(words1) <= length(words2)) words1 else words2
                        longer <- if(length(words1) > length(words2)) words1 else words2
                        
                        shorter_in_longer <- sum(shorter %in% longer)
                        if(shorter_in_longer / length(shorter) >= 0.7) {
                          match_found <- TRUE
                        }
                      }
                      
                      # Check for multi-word phrase matches (e.g., "al ain city")
                      if(!match_found && length(words1) >= 2 && length(words2) >= 2) {
                        phrases1 <- c()
                        phrases2 <- c()
                        for(i in 1:(length(words1)-1)) {
                          phrases1 <- c(phrases1, paste(words1[i], words1[i+1]))
                        }
                        for(i in 1:(length(words2)-1)) {
                          phrases2 <- c(phrases2, paste(words2[i], words2[i+1]))
                        }
                        
                        if(length(intersect(phrases1, phrases2)) > 0) {
                          match_found <- TRUE
                        }
                      }
                    }
                  }
                } else if(length(words1) == 0 && length(words2) == 0) {
                  # Both are empty after processing - consider them matching
                  match_found <- TRUE
                }
              }
            }
            
            # If no match found, flag as different
            if(!match_found) {
              issues <- c(issues, paste0("Different study region (", 
                                        df1_name, ": ", region1_str, "; ", 
                                        df2_name, ": ", region2_str, ")"))
            }
          }
        } else if(length(region1) > 0 && length(region2) == 0) {
          issues <- c(issues, paste0("Study region missing in ", df2_name))
        } else if(length(region1) == 0 && length(region2) > 0) {
          issues <- c(issues, paste0("Study region missing in ", df1_name))
        }
      }
      
      # Note: We use word-based matching for study regions (fast and interpretable for data entry validation).
      # UNSD sub-region comparison is kept as requested by the user.
      
      # Compare Livestock or wild (exact match required)
      if("Livestock or wild" %in% names(rows_df1) && "Livestock or wild" %in% names(rows_df2)) {
        livestock1 <- get_unique_location(rows_df1, "Livestock or wild")
        livestock2 <- get_unique_location(rows_df2, "Livestock or wild")
        
        if(length(livestock1) > 0 && length(livestock2) > 0) {
          # Normalize for comparison (case-insensitive)
          livestock1_norm <- tolower(str_trim(livestock1))
          livestock2_norm <- tolower(str_trim(livestock2))
          
          if(!identical(sort(livestock1_norm), sort(livestock2_norm))) {
            issues <- c(issues, paste0("Different livestock/wild classification (", 
                                      df1_name, ": ", paste(livestock1, collapse = ", "), "; ", 
                                      df2_name, ": ", paste(livestock2, collapse = ", "), ")"))
          }
        } else if(length(livestock1) > 0 && length(livestock2) == 0) {
          issues <- c(issues, paste0("Livestock/wild classification missing in ", df2_name))
        } else if(length(livestock1) == 0 && length(livestock2) > 0) {
          issues <- c(issues, paste0("Livestock/wild classification missing in ", df1_name))
        }
      }
    }
    
    comparison_results[[as.character(id)]] <- list(
      id = id,
      issues = issues,
      n_rows_df1 = nrow(rows_df1),
      n_rows_df2 = nrow(rows_df2)
    )
  }
  
  return(comparison_results)
}

# Perform comparison
comparison_results <- compare_by_study(YJ, ZW_uncollapsed, "YJ", "ZW")

# Generate HTML report
html_content <- c(
  "<!DOCTYPE html>",
  "<html>",
  "<head>",
  "<title>Entry Comparison Report: YJ vs ZW</title>",
  "<style>",
  "body { font-family: Arial, sans-serif; margin: 20px; line-height: 1.6; }",
  "h1 { color: #333; border-bottom: 2px solid #333; padding-bottom: 10px; }",
  "h2 { color: #666; margin-top: 30px; }",
  ".study { margin: 20px 0; padding: 15px; border-left: 4px solid #4CAF50; background-color: #f9f9f9; }",
  ".study.has-issues { border-left-color: #f44336; background-color: #fff5f5; }",
  ".study-id { font-weight: bold; font-size: 1.2em; color: #333; }",
  ".no-issues { color: #4CAF50; font-style: italic; }",
  ".issues { margin-top: 10px; }",
  ".issue { margin: 5px 0; padding: 5px; background-color: #fff; border-radius: 3px; }",
  ".summary { background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin: 20px 0; }",
  "</style>",
  "</head>",
  "<body>",
  "<h1>Entry Comparison Report: YJ vs ZW</h1>",
  "<div class='summary'>",
  "<h2>Summary</h2>"
)

# Calculate summary statistics
total_studies <- length(comparison_results)
studies_with_issues <- sum(sapply(comparison_results, function(x) length(x$issues) > 0))
studies_no_issues <- total_studies - studies_with_issues

html_content <- c(html_content,
  paste0("<p><strong>Total studies:</strong> ", total_studies, "</p>"),
  paste0("<p><strong>Studies with no issues:</strong> ", studies_no_issues, "</p>"),
  paste0("<p><strong>Studies with issues:</strong> ", studies_with_issues, "</p>"),
  "</div>",
  "<h2>Study-by-Study Comparison</h2>"
)

# Add each study
for(id in sort(as.numeric(names(comparison_results)))) {
  result <- comparison_results[[as.character(id)]]
  has_issues <- length(result$issues) > 0
  
  study_class <- if(has_issues) "study has-issues" else "study"
  
  html_content <- c(html_content,
    paste0("<div class='", study_class, "'>"),
    paste0("<div class='study-id'>Study ", id, ":</div>")
  )
  
  if(!has_issues) {
    html_content <- c(html_content,
      "<div class='no-issues'>No question</div>"
    )
  } else {
    html_content <- c(html_content,
      "<div class='issues'>"
    )
    for(issue in result$issues) {
      html_content <- c(html_content,
        paste0("<div class='issue'>", issue, "</div>")
      )
    }
    html_content <- c(html_content,
      "</div>"
    )
  }
  
  html_content <- c(html_content,
    "</div>"
  )
}

html_content <- c(html_content,
  "</body>",
  "</html>"
)

# Write HTML file
html_file <- "data/comparison_report_YJ_vs_ZW.html"
writeLines(html_content, html_file)

cat("✓ HTML comparison report generated: ", html_file, "\n")
cat("  Total studies: ", total_studies, "\n")
cat("  Studies with no issues: ", studies_no_issues, "\n")
cat("  Studies with issues: ", studies_with_issues, "\n")
cat("\nOpen the file in a web browser to view the report.\n")
