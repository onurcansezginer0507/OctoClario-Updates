library(dplyr)
fmf_panel_v1 <- function(input_dir){
  if (length(list.files(path = input_dir, pattern = "Quantification Cq Results", full.names = TRUE)) < 1) {
    return("Missing File: Quantification Cq Results.")
  } else if (length(list.files(path = input_dir, pattern = "Quantification Cq Results", full.names = TRUE)) > 1) {
    return("Error! Multiple Results Located in Input Directory")
  }
  
  sep <- c()
  dec <- c()
  if(count.fields(textConnection(readLines(list.files(path = input_dir,
                                                      pattern = "Quantification Cq Results",
                                                      full.names = TRUE), n = 1)), sep = ";") == 1 ){
    sep <- ","
    dec <- "."
  }else{
    sep <- ";"
    dec <- ","
  }
  
  well_info <- as.data.frame(read.table(file = list.files(path = input_dir, pattern = "Quantification Cq Results", full.names = TRUE), header = TRUE, sep = ";", dec = ","))
  if (ncol(well_info) < 2) {
    well_info <- as.data.frame(read.table(file = list.files(path = input_dir, pattern = "Quantification Cq Results", full.names = TRUE), header = TRUE, sep = ",", dec = "."))
  }
  well_info <- well_info[,c("Well", "Target", "Fluor", "Sample", "Content")]
  well_info$Well <- gsub("([A-Z])0([1-9])", "\\1\\2", well_info$Well)
  if (length(list.files(path = input_dir, pattern = "Melt Curve Derivative Results_Cy5", full.names = TRUE)) > 0) {
    cy5_data <- as.data.frame(read.table(list.files(path = input_dir, pattern = "Melt Curve Derivative Results_Cy5", full.names = TRUE), header = TRUE, sep = sep, dec = dec))
    
  }else {
    cy5_data <- data.frame(Temperature = seq(35,84.8,0.3), Temperature_1 = seq(35,84.8,0.3),Temperature_2 = seq(35,84.8,0.3))
  }
  
  
  if (length(list.files(path = input_dir, pattern = "Melt Curve Derivative Results_FAM", full.names = TRUE)) > 0) {
    fam_data <- as.data.frame(read.table(file = list.files(path = input_dir, pattern = "Melt Curve Derivative Results_FAM", full.names = TRUE), header = TRUE, sep = sep, dec = dec))
  }else {
    fam_data <- data.frame(Temperature = seq(35,84.8,0.3), Temperature_1 = seq(35,84.8,0.3),Temperature_2 = seq(35,84.8,0.3))
  }
  
  if (length(list.files(path = input_dir, pattern = "Melt Curve Derivative Results_HEX", full.names = TRUE)) > 0) {
    hex_data <- as.data.frame(read.table(file = list.files(path = input_dir, pattern = "Melt Curve Derivative Results_HEX", full.names = TRUE), header = TRUE, sep = sep, dec = dec))
  }else {
    hex_data <- data.frame(Temperature = seq(35,84.8,0.3), Temperature_1 = seq(35,84.8,0.3),Temperature_2 = seq(35,84.8,0.3))
  }
  
  if (length(list.files(path = input_dir, pattern = "Melt Curve Derivative Results_ROX", full.names = TRUE)) > 0 || length(list.files(path = input_dir, pattern = "Melt Curve Derivative Results_Texas Red", full.names = TRUE)) > 0){ 
    if(length(list.files(path = input_dir, pattern = "Melt Curve Derivative Results_ROX", full.names = TRUE)) > 0){
      rox_data <- as.data.frame(read.table(list.files(path = input_dir, pattern = "Melt Curve Derivative Results_ROX", full.names = TRUE), header = TRUE, sep = sep, dec = dec))
    }
  }else {
    rox_data <- data.frame(Temperature = seq(35,84.8,0.3), Temperature_1 = seq(35,84.8,0.3),Temperature_2 = seq(35,84.8,0.3))
  }


  
  #parse fmf parameters
  
  r761h_data <- as.data.frame(cy5_data[,c("Temperature",well_info[grep(pattern = "\\bR761H\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  e148q_data <- as.data.frame(rox_data[,c("Temperature", well_info[grep(pattern = "\\bE148Q\\b", x = well_info$Target), "Well"])])
  p408q_data <- as.data.frame(cy5_data[,c("Temperature", well_info[grep(pattern = "\\bP408Q\\b", x = well_info$Target), "Well"])])
  f479l_data <- as.data.frame(rox_data[,c("Temperature", well_info[grep(pattern = "\\bF479L\\b", x = well_info$Target), "Well"])])
  v726a_data <- as.data.frame(rox_data[,c("Temperature", well_info[grep(pattern = "\\bV726A\\b", x = well_info$Target, ignore.case = TRUE), "Well"])])
  p369s_data <- as.data.frame(cy5_data[,c("Temperature", well_info[grep(pattern = "\\bP369S\\b", well_info$Target, ignore.case = TRUE), "Well"])])
  m694v_data <- as.data.frame(rox_data[,c("Temperature", well_info[grep(pattern = "\\bM694V\\b", x = well_info$Target, ignore.case = TRUE), "Well"])])
  m680i_data <- as.data.frame(cy5_data[,c("Temperature", well_info[grep(pattern = "\\bM680I\\b", well_info$Target, ignore.case = TRUE), "Well"])])
  e167d_data <- as.data.frame(cy5_data[,c("Temperature", well_info[grep(pattern = "\\bE167D\\b", well_info$Target, ignore.case = TRUE), "Well"])])
  a744s_data <- as.data.frame(rox_data[,c("Temperature", well_info[grep(pattern = "\\bA744S\\b", well_info$Target, ignore.case = TRUE), "Well"])])
  
  #data list for all fmf parameters
  
  data_list_all <- list(R761H = r761h_data, E148Q = e148q_data, P408Q = p408q_data, F479L = f479l_data, V726A = v726a_data, P369S = p369s_data, M694V = m694v_data,
                        M680I = m680i_data, E167D = e167d_data, A744S = a744s_data)
  data_list_graph <- list(R761H = r761h_data, E148Q = e148q_data, P408Q = p408q_data, F479L = f479l_data, V726A = v726a_data, P369S = p369s_data, M694V = m694v_data,
                          M680I = m680i_data, E167D = e167d_data, A744S = a744s_data)
  
  #melting temperatures for all fmf parameters
  
  e148q_melt <- c(62,72.6)
  e148v_melt <- c(66.9,72.6)
  r761h_melt <- c(53.7, 64)
  f479l_melt <- c(55,62)
  p408q_melt <- c(60,70.5)
  p369s_melt <- c(57,62.7)
  v726a_melt <- c(59.7, 64.5)
  m694v_melt <- c(58,64.2)
  k695r_melt <- c(55,58)
  m694i_melt <- c(52.5,58)
  m680i_melt <- c(47, 59)
  m680i_ga_melt <- c(48,59)
  e167d_melt <- c(57.6, 65)
  a744s_melt <- c(49.2, 59.4)
  
  melt_for_all <- list(
    E148Q = e148q_melt,
    E148V = e148v_melt,
    R761H = r761h_melt,
    F479L = f479l_melt,
    P408Q = p408q_melt,
    P369S = p369s_melt,
    V726A = v726a_melt,
    M694V = m694v_melt,
    K695R = k695r_melt,
    M694I = m694i_melt,
    M680I = m680i_melt,
    M680I_ga = m680i_ga_melt,
    E167D = e167d_melt,
    A744S = a744s_melt
  )
  
  # Define the list of patterns
  patterns <- c(
    "E148Q", "R761H", "F479L", "P408Q", "P369S", "V726A", "M694V",
      "M680I", "E167D", "A744S"
  )
  
  # Initialize an empty list to store the filtered well info data frames
  well_list_all <- list()
  
  # Loop through each pattern, filter the data, and store it in the list
  for (pattern in patterns) {
    # Construct the full pattern with word boundaries for an exact match
    full_pattern <- paste0("\\b", pattern, "\\b")
    
    # Filter the well_info data frame
    filtered_data <- well_info[grep(pattern = full_pattern, x = well_info$Target, ignore.case = TRUE),]
    
    # Store the result in the list with a descriptive name
    well_list_all[[pattern]] <- filtered_data
  }
  result_tb_all <- list()
  for (i in 1:length(data_list_all)) {
    num_samples <- ncol(data_list_all[[i]]) - 1 # Number of sample columns (excluding Temperature)
    
    # Only create and add result_table if there are actual samples (more than just Temperature column)
    if (num_samples > 0) {
      # Get row names (well names) for the result table
      sample_row_names <- colnames(data_list_all[[i]])[2:ncol(data_list_all[[i]])]
      
      # Initialize columns with NAs of appropriate type and length
      min_dips_col <- rep(NA_real_, num_samples)
      max_dips_col <- rep(NA_real_, num_samples)
      peak_1_col <- rep(NA_real_, num_samples)
      peak_2_col <- rep(NA_real_, num_samples)
      Tm_col <- rep(NA_real_, num_samples)
      Tm_2_col <- rep(NA_real_, num_samples)
      patient_col <- rep(NA_character_, num_samples) # Temporary for initialization
      genotype_col <- rep(NA_character_, num_samples) # Temporary for initialization
      
      result_table <- data.frame(
        Well = sample_row_names, # New column for well names
        min_dips = min_dips_col,
        max_dips = max_dips_col,
        peak_1 = peak_1_col,
        peak_2 = peak_2_col,
        Tm = Tm_col,
        Tm_2 = Tm_2_col,
        `Sample Name` = patient_col, # Renamed from patient
        Genotype = genotype_col,      # Renamed from genotype
        Parameter = rep(names(data_list_all[i]), num_samples), # New column for parameter
        stringsAsFactors = FALSE,
        check.names = FALSE # Important to allow spaces in column names like "Sample Name"
      )
      result_tb_all[[names(data_list_all[i])]] <- result_table
    }
  }
  
  ## --- Melting analysis and genotype determination ---
  # Filter data_list_all to only include parameters that have corresponding entries in result_tb_all
  # This ensures the loop only runs for relevant data, improving efficiency.
  data_list_all <- data_list_all[names(result_tb_all)]
  
  # The 'processed_wells_tracker' has been removed to allow wells to be processed
  # for all relevant parameters they are tested for.
  
  # Only proceed with the main analysis if result_tb_all has any entries
  if(length(result_tb_all) > 0){
    for (i in 1:length(data_list_all)) {
      # The primary parameter for the current data frame (batch of wells)
      primary_analysis_parameter <- names(data_list_all)[i]
      
      # Initialize vectors for the current data frame (i)
      min_dips_current_df <- c()
      max_dips_current_df <- c()
      
      # Loop through each sample column (starting from the second column)
      # Note: j here corresponds to the column index in data_list_all[[i]] (2 to ncol)
      # but the index for result_tb_all[[i]] is (j-1) because result_tb_all[[i]]
      # has rows corresponding to samples, not temperature column.
      for (j in 2:ncol(data_list_all[[i]])) {
        # Vectors to store all detected peaks for this sample, regardless of temperature
        all_peak <- c()
        all_peak_temps <- c()
        
        # Get the well name
        current_well_name <- colnames(data_list_all[[i]])[j]
        
        # --- Get relevant info from well_info for the current well ---
        # This finds all rows in well_info that match the current well name
        well_info_for_current_well <- well_info[well_info$Well == current_well_name, ]
        
        if (nrow(well_info_for_current_well) == 0) {
          warning(paste("Well", current_well_name, "not found in well_info or has no targets. Skipping this sample."))
          # Assign NAs for this specific sample (column j-1 in result_tb_all[[i]])
          result_tb_all[[i]]$peak_1[j-1] <- NA
          result_tb_all[[i]]$peak_2[j-1] <- NA
          result_tb_all[[i]]$Tm[j-1] <- NA
          result_tb_all[[i]]$Tm_2[j-1] <- NA
          result_tb_all[[i]]$Genotype[j-1] <- "No Well Info" # Updated column name
          result_tb_all[[i]]$`Sample Name`[j-1] <- NA # Updated column name
          min_dips_current_df <- c(min_dips_current_df, NA) # Still append NA to these temporary vectors
          max_dips_current_df <- c(max_dips_current_df, NA)
          next # Move to the next sample column
        }
        
        # Get all unique target parameters associated with this well (for debugging/context)
        all_targets_for_current_well <- unique(well_info_for_current_well$Target)
        
        # Get the content type for the current well (assuming 'Content' column exists in well_info)
        current_well_content <- well_info_for_current_well$Content[1] 
        
        # --- Assign patient name directly here ---
        if ("Sample" %in% colnames(well_info_for_current_well)) {
          result_tb_all[[i]]$`Sample Name`[j-1] <- well_info_for_current_well$Sample[1] # Updated column name
        } else {
          result_tb_all[[i]]$`Sample Name`[j-1] <- NA # Updated column name
        }
        # --- END NEW ---
        
        # --- REVISED: Build candidate_parameters_for_peak_search based on primary_analysis_parameter ---
        # This list now only considers the primary parameter of the current data frame
        # and its directly related variants, as per your specified mappings.
        candidate_parameters_for_peak_search <- c(primary_analysis_parameter) # Always include the primary
        
        if (primary_analysis_parameter == "E148Q") {
          candidate_parameters_for_peak_search <- c(candidate_parameters_for_peak_search, "E148V")
        } else if (primary_analysis_parameter == "M680I") {
          candidate_parameters_for_peak_search <- c(candidate_parameters_for_peak_search, "M680I_ga")
        } else if (primary_analysis_parameter == "M694V") {
          candidate_parameters_for_peak_search <- c(candidate_parameters_for_peak_search, "M694I", "K695R")
        }
        # Ensure uniqueness of the final list of candidate parameters (important if primary is also a variant)
        candidate_parameters_for_peak_search <- unique(candidate_parameters_for_peak_search)
        # --- END REVISED SECTION ---
        
        # Min/Max Dips calculation (unchanged)
        if (max(data_list_all[[i]][,j]) < 30) {
          min_dips_current_df <- c(min_dips_current_df, NA)
          max_dips_current_df <- c(max_dips_current_df, NA)
        } else {
          min_dips_current_df <- c(min_dips_current_df, min(data_list_all[[i]][which(data_list_all[[i]][,j] > max(data_list_all[[i]][,j]/2)),1]))
          max_dips_current_df <- c(max_dips_current_df, max(data_list_all[[i]][which(data_list_all[[i]][,j] > max(data_list_all[[i]][,j]/2)),1]))
        }
        
        # --- NEW: Apply smoothing specifically for E148Q data before peak finding ---
        current_data_column <- data_list_all[[i]][,j]
        if (primary_analysis_parameter == "E148Q") {
          # Use a simple moving average filter for smoothing
          filter_size <- 7 # Increased filter size for potentially smoother E148Q data
          smoothed_data_column <- stats::filter(current_data_column, rep(1/filter_size, filter_size), sides = 2)
          
          # Handle NAs introduced by the filter at the ends by copying original values
          if (filter_size > 1) {
            half_filter <- floor(filter_size / 2)
            # Copy original values for the leading NAs
            smoothed_data_column[1:half_filter] <- current_data_column[1:half_filter]
            # Copy original values for the trailing NAs
            smoothed_data_column[(length(smoothed_data_column) - half_filter + 1):length(smoothed_data_column)] <- 
              current_data_column[(length(current_data_column) - half_filter + 1):length(current_data_column)]
          }
          
          # Use smoothed data for peak finding
          data_for_peak_finding <- smoothed_data_column
          print(paste("DEBUG: E148Q data smoothed for well", current_well_name, "with filter size", filter_size))
        } else {
          # For other parameters, use original data
          data_for_peak_finding <- current_data_column
        }
        # --- END NEW ---
        
        
        # Find ALL local peaks (without any temperature range restriction)
        # Apply peak finding on data_for_peak_finding
        for (k in 2:(nrow(data_list_all[[i]])-1)) { # Loop through temperature points
          if(max(data_for_peak_finding, na.rm = TRUE) > 30){ # Use max of smoothed/original data for threshold, handle NA
            # Check for local maximum: current point is higher than both its neighbors
            if (!is.na(data_for_peak_finding[k]) && !is.na(data_for_peak_finding[k+1]) && !is.na(data_for_peak_finding[k-1])) {
              if (data_for_peak_finding[k+1] < data_for_peak_finding[k] && data_for_peak_finding[k-1] < data_for_peak_finding[k]) {
                all_peak <- c(all_peak, data_for_peak_finding[k]) # Peak value from smoothed/original data
                all_peak_temps <- c(all_peak_temps, data_list_all[[i]][k,1]) # Temperature from original data
              }
            }
          }
        }
        # DEBUG: Print all detected peaks and temps before filtering
        print(paste("DEBUG for Well:", current_well_name, "- all_peak_temps (before filtering for ranges):", paste(all_peak_temps, collapse = ", ")))
        
        # Create a data frame of all detected peaks and their original indices
        # This is crucial for applying prominence test based on original data index
        all_peaks_df <- data.frame(
          peak_val = all_peak,
          peak_temp = all_peak_temps,
          original_idx = match(all_peak_temps, data_list_all[[i]][,1]) # Get original index in temperature column
        )
        
        # Initialize combined_valid_peaks and combined_valid_temps
        combined_valid_peaks <- c()
        combined_valid_temps <- c()
        
        # Define E148Q specific prominence threshold and window for lower peak
        # These values can be tuned based on your data characteristics
        e148q_lower_peak_prominence_threshold <- 0.05 # Example: peak must be 0.05 units higher than surrounding points
        e148q_prominence_window_size <- 3 # Check points 3 indices before and 3 indices after
        e148q_min_peak_height <- 10 # NEW: Minimum absolute height for E148Q lower peak to be considered valid
        
        for (param_to_check in candidate_parameters_for_peak_search) { # Use the comprehensive list here
          if (param_to_check %in% names(melt_for_all)) {
            melt_range <- melt_for_all[[param_to_check]]
            
            # --- Define narrow temperature windows around melt_range[1] and melt_range[2] ---
            range1_lower <- melt_range[1] - 1.8
            range1_upper <- melt_range[1] + 1.8
            range2_lower <- melt_range[2] - 1.8
            range2_upper <- melt_range[2] + 1.8
            
            # DEBUG: Print bounds for current candidate parameter
            print(paste("DEBUG for Well:", current_well_name, "- Checking param_to_check:", param_to_check, 
                        "Bounds: [", range1_lower, "-", range1_upper, "] OR [", range2_lower, "-", range2_upper, "]"))
            
            # Filter peaks that fall within the general temperature ranges
            # Ensure peaks_in_range_df remains a data frame using drop = FALSE
            peaks_in_range_df <- all_peaks_df[
              (all_peaks_df$peak_temp >= range1_lower & all_peaks_df$peak_temp <= range1_upper) |
                (all_peaks_df$peak_temp >= range2_lower & all_peaks_df$peak_temp <= range2_upper), , drop = FALSE
            ]
            
            # --- NEW: E148Q specific peak validation logic ---
            if (primary_analysis_parameter == "E148Q" && param_to_check == "E148Q") {
              # Separate peaks into lower and upper melt ranges for E148Q
              # Ensure these also remain data frames using drop = FALSE
              e148q_lower_peaks_candidates <- peaks_in_range_df[
                (peaks_in_range_df$peak_temp >= (melt_for_all[["E148Q"]][1] - 1.8) &
                   peaks_in_range_df$peak_temp <= (melt_for_all[["E148Q"]][1] + 1.8)), , drop = FALSE
              ]
              
              e148q_upper_peaks_candidates <- peaks_in_range_df[
                (peaks_in_range_df$peak_temp >= (melt_for_all[["E148Q"]][2] - 1.8) &
                   peaks_in_range_df$peak_temp <= (melt_for_all[["E148Q"]][2] + 1.8)), , drop = FALSE
              ]
              
              # Validate lower E148Q peaks using the new prominence test AND minimum height
              if (nrow(e148q_lower_peaks_candidates) > 0) {
                true_e148q_lower_peaks <- data.frame(peak_val = numeric(0), peak_temp = numeric(0), original_idx = numeric(0))
                
                for (row_idx in 1:nrow(e148q_lower_peaks_candidates)) {
                  peak_original_idx <- e148q_lower_peaks_candidates$original_idx[row_idx]
                  peak_value <- e148q_lower_peaks_candidates$peak_val[row_idx]
                  
                  # Check for minimum peak height first
                  if (peak_value < e148q_min_peak_height) {
                    print(paste("DEBUG: E148Q lower peak at", e148q_lower_peaks_candidates$peak_temp[row_idx], "FAILED minimum height test for well", current_well_name))
                    next # Skip to next candidate if too small
                  }
                  
                  # Ensure enough points for the window check
                  if (peak_original_idx > e148q_prominence_window_size && 
                      (peak_original_idx + e148q_prominence_window_size) <= length(data_for_peak_finding)) {
                    
                    # Check if peak is significantly higher than points in the window before and after
                    min_val_before <- min(data_for_peak_finding[(peak_original_idx - e148q_prominence_window_size):(peak_original_idx - 1)], na.rm = TRUE)
                    min_val_after <- min(data_for_peak_finding[(peak_original_idx + 1):(peak_original_idx + e148q_prominence_window_size)], na.rm = TRUE)
                    
                    if ( (peak_value - min_val_before) > e148q_lower_peak_prominence_threshold &&
                         (peak_value - min_val_after) > e148q_lower_peak_prominence_threshold ) {
                      true_e148q_lower_peaks <- rbind(true_e148q_lower_peaks, e148q_lower_peaks_candidates[row_idx,])
                      print(paste("DEBUG: E148Q lower peak at", e148q_lower_peaks_candidates$peak_temp[row_idx], "passed prominence test for well", current_well_name))
                    } else {
                      print(paste("DEBUG: E148Q lower peak at", e148q_lower_peaks_candidates$peak_temp[row_idx], "FAILED prominence test for well", current_well_name))
                    }
                  } else {
                    print(paste("DEBUG: E148Q lower peak at", e148q_lower_peaks_candidates$peak_temp[row_idx], "SKIPPED prominence test (insufficient data points) for well", current_well_name))
                  }
                }
                # Add validated lower E148Q peaks
                combined_valid_peaks <- c(combined_valid_peaks, true_e148q_lower_peaks$peak_val)
                combined_valid_temps <- c(combined_valid_temps, true_e148q_lower_peaks$peak_temp)
              }
              
              # Add upper E148Q peaks without special prominence test (they are usually stronger)
              combined_valid_peaks <- c(combined_valid_peaks, e148q_upper_peaks_candidates$peak_val)
              combined_valid_temps <- c(combined_valid_temps, e148q_upper_peaks_candidates$peak_temp)
              
            } else {
              # For all other parameters (and E148Q's upper range if it's also a candidate)
              # Add all peaks that passed the general temperature range filter
              combined_valid_peaks <- c(combined_valid_peaks, peaks_in_range_df$peak_val)
              combined_valid_temps <- c(combined_valid_temps, peaks_in_range_df$peak_temp)
            }
            # --- END NEW E148Q specific peak validation logic ---
          }
        }
        # DEBUG: Print combined valid peaks and temps after all candidate range filtering
        print(paste("DEBUG for Well:", current_well_name, "- combined_valid_temps (after candidate filtering):", paste(combined_valid_temps, collapse = ", ")))
        
        
        # Now sort the combined peaks and their corresponding temperatures
        if (length(combined_valid_peaks) > 0) {
          # Combine peaks and temps into a temporary data frame
          temp_peaks_df <- data.frame(
            peak_val = combined_valid_peaks,
            peak_temp = combined_valid_temps
          )
          
          # Remove duplicate peaks based on both value and temperature
          # This is crucial to prevent the same peak being counted twice if it was
          # detected at slightly different points due to data noise, or if multiple
          # candidate ranges picked up the same physical peak.
          unique_peaks_df <- unique(temp_peaks_df)
          
          # Sort unique peaks by value in decreasing order
          sorted_unique_peaks_df <- unique_peaks_df[order(unique_peaks_df$peak_val, decreasing = TRUE), ]
          
          # DEBUG: Print unique peaks found
          print(paste("DEBUG for Well:", current_well_name, "- unique_peaks_df:"))
          print(sorted_unique_peaks_df)
          
          
          # Assign the highest peak and its Tm
          result_tb_all[[i]]$peak_1[j-1] <- sorted_unique_peaks_df$peak_val[1]
          result_tb_all[[i]]$Tm[j-1] <- sorted_unique_peaks_df$peak_temp[1]
          
          # Assign the second highest peak and its Tm (re-added 1/3 clause with E148Q exception)
          # Ensure there is actually a second unique peak before assigning
          if (nrow(sorted_unique_peaks_df) > 1) {
            if (primary_analysis_parameter == "E148Q") {
              # Removed 1/5 threshold for E148Q's second peak as requested
              result_tb_all[[i]]$peak_2[j-1] <- sorted_unique_peaks_df$peak_val[2]
              result_tb_all[[i]]$Tm_2[j-1] <- sorted_unique_peaks_df$peak_temp[2]
            } else if (sorted_unique_peaks_df$peak_val[2] > sorted_unique_peaks_df$peak_val[1]/3) {
              # Apply 1/3 threshold for other parameters
              result_tb_all[[i]]$peak_2[j-1] <- sorted_unique_peaks_df$peak_val[2]
              result_tb_all[[i]]$Tm_2[j-1] <- sorted_unique_peaks_df$peak_temp[2]
            } else {
              # If threshold not met for other parameters
              result_tb_all[[i]]$peak_2[j-1] <- NA
              result_tb_all[[i]]$Tm_2[j-1] <- NA
            }
          } else {
            result_tb_all[[i]]$peak_2[j-1] <- NA
            result_tb_all[[i]]$Tm_2[j-1] <- NA
          }
        } else {
          # If no valid peaks were found in any candidate range, assign NAs
          result_tb_all[[i]]$peak_1[j-1] <- NA
          result_tb_all[[i]]$Tm[j-1] <- NA
          result_tb_all[[i]]$peak_2[j-1] <- NA
          result_tb_all[[i]]$Tm_2[j-1] <- NA
        }
        # --- END OF UPDATED PEAK ASSIGNMENT LOGIC ---
        
        # --- DEBUGGING PRINT STATEMENTS ADDED HERE ---
        print(paste("--- Debugging Summary for Well:", current_well_name, "---"))
        print(paste("Primary Analysis Parameter (from data_list_all name):", primary_analysis_parameter))
        print(paste("All Targets for this Well (from well_info):", paste(all_targets_for_current_well, collapse = ", ")))
        print(paste("Candidate Parameters for Peak Search:", paste(candidate_parameters_for_peak_search, collapse = ", ")))
        print(paste("Final Assigned Peak 1:", result_tb_all[[i]]$peak_1[j-1], "Tm 1:", result_tb_all[[i]]$Tm[j-1]))
        print(paste("Final Assigned Peak 2:", result_tb_all[[i]]$peak_2[j-1], "Tm 2:", result_tb_all[[i]]$Tm_2[j-1]))
        # --- END DEBUGGING PRINT STATEMENTS ---
        
        # --- NEW: Genotype Assignment Logic (with NTC check and dynamic parameter for homozygous) ---
        # Check for NTC first
        if (!is.null(current_well_content) && toupper(current_well_content) == "NTC") {
          result_tb_all[[i]]$Genotype[j-1] <- "NTC" # Updated column name
        } else if(!is.null(current_well_content) && toupper(current_well_content) == "POS CTRL"){
          result_tb_all[[i]]$Genotype[j-1] <- "Pos Ctrl"
        } else if (!is.na(result_tb_all[[i]]$Tm[j-1]) && !is.na(result_tb_all[[i]]$Tm_2[j-1])) {
          # If both Tm and Tm_2 are present, classify as heterozygous
          result_tb_all[[i]]$Genotype[j-1] <- paste0(primary_analysis_parameter, " Heterozygous") # Updated column name
        } else if (!is.na(result_tb_all[[i]]$Tm[j-1])) {
          # If only Tm is present, classify as Homozygous
          
          # Determine the actual parameter for this homozygous genotype based on Tm
          actual_genotype_parameter <- primary_analysis_parameter # Default to primary
          
          for (param_check_for_genotype in candidate_parameters_for_peak_search) {
            if (param_check_for_genotype %in% names(melt_for_all)) {
              melt_range_for_genotype <- melt_for_all[[param_check_for_genotype]]
              range1_lower_genotype <- melt_range_for_genotype[1] - 1.8
              range1_upper_genotype <- melt_range_for_genotype[1] + 1.8
              range2_lower_genotype <- melt_range_for_genotype[2] - 1.8
              range2_upper_genotype <- melt_range_for_genotype[2] + 1.8
              
              if ( (result_tb_all[[i]]$Tm[j-1] >= range1_lower_genotype && result_tb_all[[i]]$Tm[j-1] <= range1_upper_genotype) ||
                   (result_tb_all[[i]]$Tm[j-1] >= range2_lower_genotype && result_tb_all[[i]]$Tm[j-1] <= range2_upper_genotype) ) {
                actual_genotype_parameter <- param_check_for_genotype
                break # Found the matching parameter, no need to check others
              }
            }
          }
          
          # DEBUG: Print the determined actual_genotype_parameter
          print(paste("DEBUG for Well:", current_well_name, "- Determined actual_genotype_parameter:", actual_genotype_parameter))
          
          # Apply Homozygous Mutant or Wild Type logic based on actual_genotype_parameter
          if (actual_genotype_parameter %in% c("M694V", "E167D", "V726A")) {
            # Reverse logic for M694V and E167D and V726A
            if (actual_genotype_parameter %in% names(melt_for_all)) {
              mean_melt_temp <- mean(melt_for_all[[actual_genotype_parameter]])
              if (result_tb_all[[i]]$Tm[j-1] < mean_melt_temp) {
                result_tb_all[[i]]$Genotype[j-1] <- paste0(actual_genotype_parameter, " Wild Type") # Updated column name
              } else {
                result_tb_all[[i]]$Genotype[j-1] <- paste0(actual_genotype_parameter, " Homozygous Mutant") # Updated column name
              }
            } else {
              result_tb_all[[i]]$Genotype[j-1] <- paste0(actual_genotype_parameter, " Homozygous (Melt Range Missing)") # Updated column name
            }
          } else {
            # Original logic for all other parameters
            if (actual_genotype_parameter %in% names(melt_for_all)) {
              mean_melt_temp <- mean(melt_for_all[[actual_genotype_parameter]])
              if (result_tb_all[[i]]$Tm[j-1] < mean_melt_temp) {
                result_tb_all[[i]]$Genotype[j-1] <- paste0(actual_genotype_parameter, " Homozygous Mutant") # Updated column name
              } else {
                result_tb_all[[i]]$Genotype[j-1] <- paste0(actual_genotype_parameter, " Wild Type") # Updated column name
              }
            } else {
              result_tb_all[[i]]$Genotype[j-1] <- paste0(actual_genotype_parameter, " Homozygous (Melt Range Missing)") # Updated column name
            }
          }
        } else {
          # If neither Tm is present, classify as Negative
          result_tb_all[[i]]$Genotype[j-1] <- "Negative" # Updated column name
        }
        # --- END NEW GENOTYPE ASSIGNMENT ---
        
      } # End of j loop (sample columns)
      
      # Assign the collected min_dips and max_dips for the current data frame
      min_dips_current_df_length <- length(min_dips_current_df)
      max_dips_current_df_length <- length(max_dips_current_df)
      
      # Ensure min_dips and max_dips columns in result_tb_all[[i]] are correctly sized
      # This handles cases where some wells were skipped, leading to shorter temp vectors
      if (min_dips_current_df_length > 0 && min_dips_current_df_length == nrow(result_tb_all[[i]])) {
        result_tb_all[[i]]$min_dips <- min_dips_current_df
        result_tb_all[[i]]$max_dips <- max_dips_current_df
      } else {
        # If lengths don't match (e.g., due to 'next' statements), assign NAs to the whole column
        result_tb_all[[i]]$min_dips <- rep(NA_real_, nrow(result_tb_all[[i]]))
        result_tb_all[[i]]$max_dips <- rep(NA_real_, nrow(result_tb_all[[i]]))
      }
    }
    
  } else {
    # If the initial result_tb_all was empty, ensure it remains an empty list
    result_tb_all <- list()
  }
  
  for (i in 1:length(result_tb_all)) {
    result_tb_all[[i]] <- result_tb_all[[i]][,c(1,8,10,9,4,5)]
    colnames(result_tb_all[[i]]) <- c("Well", "Sample Name", "Parameter", "Genotype", "peak_1", "peak_2")
  }
  
  return(list(result_tb_all, data_list_graph))
}
