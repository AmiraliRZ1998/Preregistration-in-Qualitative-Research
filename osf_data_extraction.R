# Final Code 
########################################
## Step 1: Collect Registration IDs
## (Loops page=1 to 19366)
########################################

# install.packages(c("httr", "jsonlite")) if needed
library(httr)
library(jsonlite)

number_of_pages <- 19366
registration_id <- c()
project_id <- c()
registration_date_created <- c()

max_retries <- 3

for (page_i in seq_len(number_of_pages)) {
  
  url <- paste0(
    "https://api.osf.io/v2/registrations/?sort=date_created&page=", page_i
  )
  cat("\nProcessing page", page_i, " =>", url, "\n")
  
  attempt <- 1
  success <- FALSE
  parsed  <- NULL
  
  # Retry loop
  repeat {
    # 1) Make the GET request (with a longer timeout if you wish)
    res <- tryCatch({
      GET(url, timeout(60))  # 60-second timeout, adjust as needed
    }, error = function(e) {
      cat("GET error on page", page_i, ":", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(res)) {
      # total failure to do GET
      cat("Skipping page", page_i, "due to GET error.\n")
    } else if (res$status_code == 200) {
      # 2) Parse JSON
      parsed <- tryCatch({
        fromJSON(rawToChar(res$content))
      }, error = function(e) {
        cat("JSON parse error on page", page_i, ":", e$message, "\n")
        return(NULL)
      })
      
      if (!is.null(parsed)) {
        success <- TRUE
        break  # break out of 'repeat' if success
      } else {
        cat("Parsing returned NULL, will attempt retry.\n")
      }
    } else {
      cat("HTTP error code:", res$status_code, "on page", page_i, "\n")
    }
    
    attempt <- attempt + 1
    if (attempt > max_retries) {
      cat("Max retries reached for page", page_i, "- skipping.\n")
      break
    }
    cat("Retrying page", page_i, "in 5s...\n")
    Sys.sleep(5)
  }
  
  # If we never succeeded, skip this page
  if (!success || is.null(parsed)) {
    cat("Skipping page", page_i, "after max attempts.\n")
    next  # move on to the next page_i
  }
  
  # If we got here, we have 'parsed'
  if (is.null(parsed$data) || length(parsed$data) == 0) {
    # Possibly no more data
    cat("No more data on page", page_i, "- continuing (or break if you want).\n")
    # If we want to stop altogether when it's empty, do:
    # break
    next
  }
  
  # 3) Temporarily store
  res_temp <- parsed$data
  
  # 4) Extract IDs
  registration_id         <- c(registration_id, res_temp$id)
  registration_date_created <- c(
    registration_date_created, 
    res_temp$attributes$date_created
  )
  project_id <- c(
    project_id, 
    res_temp$relationships$registered_from$data$id
  )
  
  cat("Page", page_i, "done: stored", nrow(res_temp), "registrations.\n")
  Sys.sleep(1)  # optional brief pause between pages
}

# Save final results
save(registration_id, project_id, registration_date_created, 
     file = "registration_data.RData")

cat("\nFinished all pages (1 to", number_of_pages, ")\n")
cat("Saved 'registration_data.RData'!\n")


########################################################
## Step 2: Subset IDs to Only "Qualitative Preregistration"
########################################################


load("registration_data.RData")  
# This file should have, at minimum:
#   registration_id
#   project_id
#   registration_date_created
# or whatever vectors we gathered in step 1.

library(httr)
library(jsonlite)

# Create a data frame to store only the Qual. Prereg records
# We'll capture the ID, date_created, project_id
qp_df <- data.frame(
  reg_id      = character(0),
  date_created= character(0),
  project_id  = character(0),
  stringsAsFactors = FALSE
)

# For robust GET, define max retries
max_retries <- 3

# Track how many we found
qp_count <- 0

cat("\nStarting subset creation for 'Qualitative Preregistration'...\n")
cat("Total IDs to check:", length(registration_id), "\n")

# 1) Loop over each registration ID
for (i in seq_along(registration_id)) {
  id <- registration_id[i]
  
  # If we also want to keep the creation date, project_id from step 1, store them now:
  date_created_val <- registration_date_created[i]
  project_id_val   <- project_id[i]
  
  # Print progress every 500 IDs
  if (i %% 500 == 0) {
    cat("So far processed", i, "IDs; found", qp_count, "Qual. Prereg.\n")
  }
  
  # 2) Robust GET to retrieve full registration data
  attempt <- 1
  success_main <- FALSE
  reg_data <- NULL
  
  repeat {
    single_reg <- tryCatch({
      GET(
        paste0("https://api.osf.io/v2/registrations/?filter[id]=", id),
        timeout(60)
      )
    }, error = function(e) {
      cat("GET error for ID", id, ":", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(single_reg)) {
      cat("Skipping ID", id, "due to GET error.\n")
      break
    }
    
    if (single_reg$status_code != 200) {
      cat("HTTP code:", single_reg$status_code, "for ID", id, "\n")
      attempt <- attempt + 1
      if (attempt > max_retries) {
        cat("Max retries reached - skipping ID", id, "\n")
        break
      }
      cat("Retry in 5s...\n")
      Sys.sleep(5)
      next
    }
    
    parsed_main <- tryCatch({
      fromJSON(rawToChar(single_reg$content))
    }, error = function(e) {
      cat("JSON parse error for ID", id, ":", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(parsed_main) || is.null(parsed_main$data) || length(parsed_main$data) == 0) {
      cat("No data or parse error for ID", id, "\n")
      attempt <- attempt + 1
      if (attempt > max_retries) {
        cat("Max parse retries - skipping ID", id, "\n")
        break
      }
      cat("Retry in 5s...\n")
      Sys.sleep(5)
      next
    }
    
    # If successful, break out of repeat
    reg_data <- parsed_main$data[1, ]
    success_main <- TRUE
    break
  }
  
  # If we failed to retrieve data => skip
  if (!success_main || is.null(reg_data)) {
    next
  }
  
  # 3) Check if 'Qualitative Preregistration'
  supp_val <- reg_data$attributes$registration_supplement
  if (!is.null(supp_val) && supp_val == "Qualitative Preregistration") {
    # If yes, store it in our subset data frame
    qp_count <- qp_count + 1
    
    # We'll create a single row with minimal info
    new_row <- data.frame(
      reg_id       = id,
      date_created = date_created_val,  # from step 1
      project_id   = project_id_val,    # from step 1
      stringsAsFactors = FALSE
    )
    
    qp_df <- rbind(qp_df, new_row)
  }
}

cat("\nFinished scanning all registrations.\n")
cat("Processed", length(registration_id), "IDs total.\n")
cat("Found", qp_count, "qualitative preregistrations.\n")

# 4) Save subset
save(qp_df, file = "qual_prereg_data.RData")
write.csv(qp_df, "qual_prereg_data.csv", row.names = FALSE)

cat("\nSaved 'qual_prereg_data.RData' and 'qual_prereg_data.csv'.\n")

############################################################
## Step 3: Gather Meta Data for QP IDs
############################################################


# 0) Loading QP subset data 
load("qual_prereg_data.RData")  # from QP subset step

library(httr)
library(jsonlite)

# Ensuring `qp_df` has 'reg_id'
n_ids <- nrow(qp_df)
cat("We have", n_ids, "Qual. Prereg IDs in 'qp_df'.\n")

max_retries <- 3

# 1) Prepare final data frame
# We'll put them in this order:
#   Title, Description, Contributors(10), License, date_created, date_modified, date_registered,
#   Field(10), osf_doi, study_design, plus the project info & other attributes.

final_df <- data.frame(
  reg_id       = character(0),
  title        = character(0),
  description  = character(0),
  contrib1     = character(0),
  contrib2     = character(0),
  contrib3     = character(0),
  contrib4     = character(0),
  contrib5     = character(0),
  contrib6     = character(0),
  contrib7     = character(0),
  contrib8     = character(0),
  contrib9     = character(0),
  contrib10    = character(0),
  license_name = character(0),
  date_created = character(0),
  date_modified= character(0),
  date_registered = character(0),
  field1       = character(0),
  field2       = character(0),
  field3       = character(0),
  field4       = character(0),
  field5       = character(0),
  field6       = character(0),
  field7       = character(0),
  field8       = character(0),
  field9       = character(0),
  field10      = character(0),
  osf_doi      = character(0),
  study_design = character(0),
  # Additional columns from Step 2 or prior scripts
  category     = character(0),
  fork         = character(0),
  public       = character(0),
  reg_withdrawn = character(0),
  reg_withdrawal_justification = character(0),
  has_project  = character(0),
  parent_project = character(0),
  project_id   = character(0),
  web_address  = character(0),
  project_created = character(0),
  project_modified= character(0),
  project_title   = character(0),
  project_public  = character(0),
  stringsAsFactors = FALSE
)

# 2) Loop over each QP ID
for (i in seq_len(n_ids)) {
  id <- qp_df$reg_id[i]
  cat("\nNow processing #", i, "of", n_ids, "=>", id, "\n")
  
  ################################################
  ## A) Robust GET for main registration data
  ################################################
  attempt <- 1
  success_main <- FALSE
  reg_data <- NULL
  
  repeat {
    main_res <- tryCatch({
      GET(paste0("https://api.osf.io/v2/registrations/?filter[id]=", id),
          timeout(60))
    }, error = function(e) {
      cat("GET error ID", id, ":", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(main_res)) {
      cat("Skipping ID", id, "=> GET error\n")
      break
    }
    
    if (main_res$status_code != 200) {
      cat("HTTP code:", main_res$status_code, "for ID:", id, "\n")
      attempt <- attempt + 1
      if (attempt > max_retries) {
        cat("Max retries => skipping\n")
        break
      }
      cat("Retry in 5s...\n")
      Sys.sleep(5)
      next
    }
    
    parsed_main <- tryCatch({
      fromJSON(rawToChar(main_res$content))
    }, error = function(e) {
      cat("JSON parse error ID", id, ":", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(parsed_main) || is.null(parsed_main$data) || length(parsed_main$data) == 0) {
      cat("No data => skipping\n")
      attempt <- attempt + 1
      if (attempt > max_retries) {
        cat("Max parse => skipping\n")
        break
      }
      Sys.sleep(5)
      next
    }
    
    reg_data <- parsed_main$data[1, ]
    success_main <- TRUE
    break
  }
  
  if (!success_main || is.null(reg_data)) {
    next
  }
  
  # B) Extract main attributes
  title_val <- ifelse(is.null(reg_data$attributes$title),        "NA", reg_data$attributes$title)
  desc_val  <- ifelse(is.null(reg_data$attributes$description),  "NA", reg_data$attributes$description)
  
  date_created_val   <- ifelse(is.null(reg_data$attributes$date_created),   "NA", reg_data$attributes$date_created)
  date_modified_val  <- ifelse(is.null(reg_data$attributes$date_modified),  "NA", reg_data$attributes$date_modified)
  date_registered_val<- ifelse(is.null(reg_data$attributes$date_registered), "NA", reg_data$attributes$date_registered)
  
  # Category, fork, public, etc.
  cat_val  <- ifelse(is.null(reg_data$attributes$category),  "NA", reg_data$attributes$category)
  fork_val <- ifelse(is.null(reg_data$attributes$fork),      "NA", reg_data$attributes$fork)
  pub_val  <- ifelse(is.null(reg_data$attributes$public),    "NA", reg_data$attributes$public)
  
  withdrawn_val <- ifelse(is.null(reg_data$attributes$withdrawn), "NA", reg_data$attributes$withdrawn)
  withdraw_justif_val <- ifelse(is.null(reg_data$attributes$withdrawal_justification),
                                "NA", reg_data$attributes$withdrawal_justification)
  has_proj_val <- ifelse(is.null(reg_data$attributes$has_project), "NA", reg_data$attributes$has_project)
  
  parent_project_val <- "NA"
  if (!is.null(reg_data$relationships$registered_from$data$id)) {
    parent_project_val <- reg_data$relationships$registered_from$data$id
  }
  
  # (IMPROVED) Study design => from registered_meta$q5$value with defensive check
  study_design_val <- "MISSING_STUDY_DESIGN"
  if (!is.null(reg_data$attributes$registered_meta) &&
      is.list(reg_data$attributes$registered_meta) &&
      "q5" %in% names(reg_data$attributes$registered_meta) &&
      !is.null(reg_data$attributes$registered_meta$q5$value)) {
    study_design_val <- reg_data$attributes$registered_meta$q5$value
  }
  
  # OSF link => reg_data$links$html
  osf_doi_val <- ifelse(is.null(reg_data$links$html), "MISSING", reg_data$links$html)
  
  ################################################
  ## C) Parent project info (Optional)
  ################################################
  project_id_val      <- "NA"
  web_address_val     <- "NA"
  project_created_val <- "NA"
  project_modified_val<- "NA"
  project_title_val   <- "NA"
  project_public_val  <- "NA"
  
  if (!is.null(reg_data$relationships$registered_from$links$related$href)) {
    p_href <- reg_data$relationships$registered_from$links$related$href
    attempt_par <- 1
    repeat {
      par_res <- tryCatch({
        GET(p_href, timeout=60)
      }, error = function(e) {
        cat("GET error parent project ID", id, ":", e$message, "\n")
        return(NULL)
      })
      if (is.null(par_res)) {
        cat("Skipping parent project => GET error\n")
        break
      }
      if (par_res$status_code != 200) {
        cat("HTTP code parent project:", par_res$status_code, "\n")
        attempt_par <- attempt_par + 1
        if (attempt_par > max_retries) {
          cat("Max parent => skip\n")
          break
        }
        Sys.sleep(5)
        next
      }
      
      par_parsed <- tryCatch({
        fromJSON(rawToChar(par_res$content))
      }, error = function(e) {
        cat("Parent parse error:", e$message, "\n")
        return(NULL)
      })
      if (is.null(par_parsed) || is.null(par_parsed$data)) {
        cat("No parent data => skip\n")
        attempt_par <- attempt_par + 1
        if (attempt_par > max_retries) {
          cat("Max parse => skip\n")
          break
        }
        Sys.sleep(5)
        next
      }
      
      # If success:
      project_id_val   <- ifelse(is.null(par_parsed$data$id),            "NA", par_parsed$data$id)
      web_address_val  <- ifelse(is.null(par_parsed$data$links$html),    "NA", par_parsed$data$links$html)
      project_created_val  <- ifelse(is.null(par_parsed$data$attributes$date_created), 
                                     "NA", par_parsed$data$attributes$date_created)
      project_modified_val <- ifelse(is.null(par_parsed$data$attributes$date_modified),
                                     "NA", par_parsed$data$attributes$date_modified)
      project_title_val    <- ifelse(is.null(par_parsed$data$attributes$title),
                                     "NA", par_parsed$data$attributes$title)
      project_public_val   <- ifelse(is.null(par_parsed$data$attributes$public),
                                     "NA", par_parsed$data$attributes$public)
      break
    }
  }
  
  ################################################
  ## D) Contributors (authors) => from Step 2
  ################################################
  contrib_url <- paste0("https://api.osf.io/v2/registrations/", id, "/contributors/")
  authors_vec <- "DELETED"
  
  attempt_contrib <- 1
  repeat {
    c_res <- tryCatch({
      GET(contrib_url, timeout=60)
    }, error = function(e) {
      cat("GET error for contributor of ID", id, ":", e$message, "\n")
      return(NULL)
    })
    if (is.null(c_res)) {
      cat("Skipping contributors => GET error\n")
      break
    }
    if (c_res$status_code != 200) {
      cat("HTTP code:", c_res$status_code, "for contributors of ID", id, "\n")
      attempt_contrib <- attempt_contrib + 1
      if (attempt_contrib > max_retries) {
        cat("Max contributor retries => skip\n")
        break
      }
      Sys.sleep(5)
      next
    }
    
    c_parsed <- tryCatch({
      fromJSON(rawToChar(c_res$content))
    }, error = function(e) {
      cat("JSON parse error for contributors ID", id, ":", e$message, "\n")
      return(NULL)
    })
    if (is.null(c_parsed) || is.null(c_parsed$data)) {
      cat("No contributor data => skip\n")
      attempt_contrib <- attempt_contrib + 1
      if (attempt_contrib > max_retries) {
        cat("Max parse => skip\n")
        break
      }
      Sys.sleep(5)
      next
    }
    
    # EXACT snippet from Step 2:
    authors_vec <- tryCatch({
      c_parsed[["data"]][["embeds"]][["users"]][["data"]][["attributes"]][["full_name"]]
    }, error=function(e) {
      # If subscript fails, set NULL to avoid error
      return(NULL)
    })
    if (is.null(authors_vec)) authors_vec <- "DELETED"
    
    break
  }
  
  # We'll keep up to 10 authors
  if (length(authors_vec) == 1 && authors_vec == "DELETED") {
    # means we couldn't parse
    cont_vec <- rep("DELETED", 10)
  } else {
    cont_vec <- rep("NA", 10)
    limit <- min(10, length(authors_vec))
    if (limit > 0) cont_vec[1:limit] <- authors_vec[1:limit]
  }
  
  ################################################
  ## E) License => relationships$license
  ################################################
  license_name_val <- "MISSING_LICENSE"
  if (!is.null(reg_data$relationships$license$links$related$href)) {
    lic_href <- reg_data$relationships$license$links$related$href
    attempt_lic <- 1
    repeat {
      lic_res <- tryCatch({
        GET(lic_href, timeout=60)
      }, error=function(e) {
        cat("License GET error ID", id, ":", e$message, "\n")
        return(NULL)
      })
      if (is.null(lic_res)) {
        cat("Skipping license => GET error.\n")
        break
      }
      if (lic_res$status_code != 200) {
        cat("License HTTP code:", lic_res$status_code, "\n")
        attempt_lic <- attempt_lic + 1
        if (attempt_lic > max_retries) {
          cat("Max license => skipping.\n")
          break
        }
        Sys.sleep(5)
        next
      }
      lic_parsed <- tryCatch({
        fromJSON(rawToChar(lic_res$content))
      }, error=function(e) {
        cat("License parse error:", e$message, "\n")
        return(NULL)
      })
      if (is.null(lic_parsed) || is.null(lic_parsed$data)) {
        cat("No license data => skip.\n")
        attempt_lic <- attempt_lic + 1
        if (attempt_lic > max_retries) {
          cat("Max parse => skip.\n")
          break
        }
        Sys.sleep(5)
        next
      }
      license_name_val <- lic_parsed$data$attributes$name
      break
    }
  }
  
  ################################################
  ## F) Fields => up to 10
  ################################################
  subs <- reg_data$attributes$subjects
  fields_list <- character(0)
  if (!is.null(subs) && length(subs) > 0) {
    for (outer_list in subs) {
      for (item in outer_list) {
        if (!is.null(item$text)) {
          fields_list <- c(fields_list, item$text)
        }
      }
    }
  }
  field_vec <- rep("NA", 10)
  if (length(fields_list) > 0) {
    limitf <- min(10, length(fields_list))
    field_vec[1:limitf] <- fields_list[1:limitf]
  }
  
  ################################################
  ## G) Build one row
  ################################################
  row_data <- data.frame(
    reg_id       = id,
    title        = title_val,
    description  = desc_val,
    contrib1     = cont_vec[1],
    contrib2     = cont_vec[2],
    contrib3     = cont_vec[3],
    contrib4     = cont_vec[4],
    contrib5     = cont_vec[5],
    contrib6     = cont_vec[6],
    contrib7     = cont_vec[7],
    contrib8     = cont_vec[8],
    contrib9     = cont_vec[9],
    contrib10    = cont_vec[10],
    license_name = license_name_val,
    date_created = date_created_val,
    date_modified= date_modified_val,
    date_registered = date_registered_val,
    field1       = field_vec[1],
    field2       = field_vec[2],
    field3       = field_vec[3],
    field4       = field_vec[4],
    field5       = field_vec[5],
    field6       = field_vec[6],
    field7       = field_vec[7],
    field8       = field_vec[8],
    field9       = field_vec[9],
    field10      = field_vec[10],
    osf_doi      = osf_doi_val,
    study_design = study_design_val,
    category     = cat_val,
    fork         = fork_val,
    public       = pub_val,
    reg_withdrawn = withdrawn_val,
    reg_withdrawal_justification = withdraw_justif_val,
    has_project  = has_proj_val,
    parent_project = parent_project_val,
    project_id   = project_id_val,
    web_address  = web_address_val,
    project_created  = project_created_val,
    project_modified = project_modified_val,
    project_title    = project_title_val,
    project_public   = project_public_val,
    stringsAsFactors = FALSE
  )
  
  final_df <- rbind(final_df, row_data)
}

cat("\nAll done! Processed", n_ids, "IDs.\n")
cat("Final data frame has", nrow(final_df), "rows.\n")

# 3) Save final data
save(final_df, file="final_qual_prereg_data.RData")
write.csv(final_df, "final_qual_prereg_data.csv", row.names=FALSE)
cat("\nData saved => 'final_qual_prereg_data.RData' and 'final_qual_prereg_data.csv'\n")


View(final_df)

##################################################
# Step 4: Extracting Registration Content 
##################################################


load("qual_prereg_data.RData")  

library(httr)
library(jsonlite)

# Check how many QP IDs we have
n_ids <- nrow(qp_df)
cat("We have", n_ids, "QP registrations in 'qp_df'.\n")

max_retries <- 3

# Prepare final data frame
registration_content <- data.frame(
  reg_id         = character(0),
  Research_Aims  = character(0),  # q1
  Type_of_Aim    = character(0),  # q2
  Research_Questions = character(0), # q3
  Anticipated_Duration = character(0), # q4
  Study_Design   = character(0),  # q5
  Sampling_Strategy = character(0), # q6
  Data_Sources   = character(0),  # q7
  Data_Collection_Methods = character(0), # q8
  Data_Collection_Plans   = character(0), # q9
  Download_link_of_protocol = character(0), # q9a
  Stopping_Criteria = character(0), # q10
  Data_analysis_Approach = character(0), # q11
  Data_analysis_Process  = character(0), # q12
  Credibility_Strategies = character(0), # q13
  Credibility_Rationale  = character(0), # q14
  Positionality_Reflection = character(0), # q15
  stringsAsFactors = FALSE
)

# A helper function for safely retrieving a field or "NA"
safe_resp <- function(x) {
  if (!is.null(x)) x else "NA"
}

for (i in seq_len(n_ids)) {
  id <- qp_df$reg_id[i]
  cat("\nNow processing ID #", i, "of", n_ids, "=>", id, "\n")
  
  attempt <- 1
  success_main <- FALSE
  reg_data <- NULL
  
  # Robust GET for the registration
  repeat {
    single_reg <- tryCatch({
      GET(paste0("https://api.osf.io/v2/registrations/?filter[id]=", id),
          timeout(60))
    }, error = function(e) {
      cat("GET error ID", id, ":", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(single_reg)) {
      cat("Skipping ID", id, "=> GET error.\n")
      break
    }
    if (single_reg$status_code != 200) {
      cat("HTTP code:", single_reg$status_code, "=> retry.\n")
      attempt <- attempt + 1
      if (attempt > max_retries) {
        cat("Max retries => skipping ID", id, "\n")
        break
      }
      Sys.sleep(5)
      next
    }
    
    parsed_main <- tryCatch({
      fromJSON(rawToChar(single_reg$content))
    }, error = function(e) {
      cat("JSON parse error ID", id, ":", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(parsed_main) || is.null(parsed_main$data) ||
        length(parsed_main$data) == 0) {
      cat("No data => skipping ID", id, "\n")
      attempt <- attempt + 1
      if (attempt > max_retries) {
        cat("Max parse => skipping.\n")
        break
      }
      Sys.sleep(5)
      next
    }
    
    reg_data <- parsed_main$data[1, ]
    success_main <- TRUE
    break
  }
  
  if (!success_main || is.null(reg_data)) {
    next
  }
  
  # Extracting the 'registration_responses'
  resp <- reg_data$attributes$registration_responses
  
  # **Key fix**: If resp is NULL or not a list => set to an empty list
  if (is.null(resp) || !is.list(resp)) {
    resp <- list()
  }
  
  # Now we can safely do resp$q1, resp$q2, etc.
  q1  <- safe_resp(resp$q1)
  q2  <- safe_resp(resp$q2)
  q3  <- safe_resp(resp$q3)
  q4  <- safe_resp(resp$q4)
  q5  <- safe_resp(resp$q5)
  q6  <- safe_resp(resp$q6)
  q7  <- safe_resp(resp$q7)
  q8  <- safe_resp(resp$q8)
  q9  <- safe_resp(resp$q9)
  
  # q9a => download link only
  q9a_download <- "NA"
  if (!is.null(resp$q9a) && length(resp$q9a) > 0) {
    if (!is.null(resp$q9a[[1]]$file_urls$download)) {
      q9a_download <- resp$q9a[[1]]$file_urls$download
    }
  }
  
  q10 <- safe_resp(resp$q10)
  q11 <- safe_resp(resp$q11)
  q12 <- safe_resp(resp$q12)
  
  # q13 could be a list of strings
  q13_obj <- resp$q13
  if (is.null(q13_obj)) {
    q13_vals <- "NA"
  } else if (is.character(q13_obj)) {
    # single string
    q13_vals <- q13_obj
  } else if (is.vector(q13_obj)) {
    # multiple strings
    q13_vals <- paste(q13_obj, collapse = "; ")
  } else {
    q13_vals <- "NA"
  }
  
  q14 <- safe_resp(resp$q14)
  q15 <- safe_resp(resp$q15)
  
  # Build one row
  row_data <- data.frame(
    reg_id      = id,
    Research_Aims = q1,
    Type_of_Aim   = q2,
    Research_Questions = q3,
    Anticipated_Duration = q4,
    Study_Design  = q5,
    Sampling_Strategy = q6,
    Data_Sources  = q7,
    Data_Collection_Methods = q8,
    Data_Collection_Plans   = q9,
    Download_link_of_protocol = q9a_download,
    Stopping_Criteria  = q10,
    Data_analysis_Approach = q11,
    Data_analysis_Process  = q12,
    Credibility_Strategies = q13_vals,
    Credibility_Rationale  = q14,
    Positionality_Reflection = q15,
    stringsAsFactors = FALSE
  )
  
  registration_content <- rbind(registration_content, row_data)
}

cat("\nAll done! Processed", n_ids, "QP IDs.\n")
cat("Final 'registration_content' has", nrow(registration_content), "rows.\n")

# 3) Save final data
save(registration_content, file="registration_content.RData")
write.csv(registration_content, "registration_content.csv", row.names=FALSE)
cat("\nData saved => 'registration_content.RData' and 'registration_content.csv'\n")

# Optional:
View(registration_content)



#setting working directory 
# Use forward slashes:
setwd("C:/Users/amira/OneDrive/Documents")

# or double backslashes:
setwd("C:\\Users\\amira\\OneDrive\\Documents")

# Verify:
getwd()


######################################
## 1) Load & View final_qual_prereg_data.RData
######################################
load("final_qual_prereg_data.RData")  # This restores the object(s) saved in that file

# Typically, we called the data frame 'final_df'
View(final_df)  # Opens the data frame in the RStudio viewer (or a tab/window)

######################################
## 2) Load & View registration_content.RData
######################################
load("registration_content.RData")  # Restores your 'registration_content' data frame

View(registration_content)  # Inspect it in the viewer
getwd()

#getting rid of the duplicated in registration content data file
# 1) Load your existing file that has duplicates
load("registration_content.RData")  # loads 'registration_content'

# 2) Check how many total rows (and unique IDs)
cat("Rows before deduplication:", nrow(registration_content), "\n")
cat("Unique IDs before deduplication:", length(unique(registration_content$reg_id)), "\n")

# 3) Remove duplicate reg_id rows, keeping the first occurrence
registration_content <- registration_content[!duplicated(registration_content$reg_id), ]

# 4) Now check how many remain
cat("Rows after deduplication:", nrow(registration_content), "\n")
cat("Unique IDs after deduplication:", length(unique(registration_content$reg_id)), "\n")

# 5) Save the cleaned version
save(registration_content, file="registration_content_nodups.RData")
write.csv(registration_content, "registration_content_nodups.csv", row.names=FALSE)

cat("\nDeduplication done! 'registration_content_nodups.RData' and '.csv' saved.\n")
View(registration_content)

#######################################
## Load Both Data Files + Export to XLSX
#######################################

# 1) Install/Load openxlsx if needed
# install.packages("openxlsx")  # if you don't have it yet
library(openxlsx)

# 2) Load your two RData files
load("final_qual_prereg_data.RData")  # Should contain object 'final_df'
load("registration_content.RData")     # Should contain object 'registration_content'

# 3) Export 'final_df' to Excel file "final_qual_prereg_data.xlsx"
wb1 <- createWorkbook()
addWorksheet(wb1, "meta_data")
writeData(wb1, "meta_data", final_df)

# (Optional) Adjust column widths or wrap text
# setColWidths(wb1, "meta_data", cols = 1:ncol(final_df), widths = "auto")
# addStyle(...) to wrap text if desired

saveWorkbook(wb1, "final_qual_prereg_data.xlsx", overwrite=TRUE)
cat("\nSaved 'final_qual_prereg_data.xlsx'!\n")

# 4) Export 'registration_content' to "registration_content.xlsx"
wb2 <- createWorkbook()
addWorksheet(wb2, "registration_content")
writeData(wb2, "registration_content", registration_content)
# Possibly adjust columns here too:
# setColWidths(wb2, "registration_content", cols = 1:ncol(registration_content), widths = "auto")

saveWorkbook(wb2, "registration_content.xlsx", overwrite=TRUE)
cat("\nSaved 'registration_content.xlsx'!\n")


##############################################
## 1) Get 50 random integers from 27..1806
##############################################
set.seed(123)  # For reproducible results (optional)
rand_indices <- sample(27:1806, size=50)  # 50 unique integers in that range
print(rand_indices)  # Check the indices

##############################################
## 2) Load your QP data frame (e.g., 'qp_df')
##############################################
# We'll assume you have something like:
load("qual_prereg_data.RData")  # must have `qp_df` with a column 'reg_id'

# Confirm qp_df has enough rows
cat("qp_df has", nrow(qp_df), "rows.\n")

##############################################
## 3) Extract the 50 random registration IDs
##############################################
# We'll sample 'reg_id' from those row indices
the_ids <- qp_df$reg_id[rand_indices]
cat("Picked these 50 random IDs:\n")
print(the_ids)

##############################################
## 4) Use the same logic as your existing script
##    to robustly get data for each ID
##############################################
library(httr)
library(jsonlite)
library(openxlsx)

max_retries <- 3

# Prepare final data frame
random_50_content <- data.frame(
  reg_id         = character(0),
  Title          = character(0),
  Research_Aims  = character(0),  # q1
  Type_of_Aim    = character(0),  # q2
  Research_Questions = character(0), # q3
  Anticipated_Duration = character(0), # q4
  Study_Design   = character(0),  # q5
  Sampling_Strategy = character(0), # q6
  Data_Sources   = character(0),  # q7
  Data_Collection_Methods = character(0), # q8
  Data_Collection_Plans   = character(0), # q9
  Download_link_of_protocol = character(0), # q9a
  Stopping_Criteria = character(0), # q10
  Data_analysis_Approach = character(0), # q11
  Data_analysis_Process  = character(0), # q12
  Credibility_Strategies = character(0), # q13
  Credibility_Rationale  = character(0), # q14
  Positionality_Reflection = character(0), # q15,
  stringsAsFactors = FALSE
)

# Helper function
safe_resp <- function(x) {
  if (!is.null(x)) x else "NA"
}

# 5) Loop over each ID in 'the_ids'
for (id in the_ids) {
  cat("\nProcessing ID =>", id, "\n")
  
  attempt <- 1
  success_main <- FALSE
  reg_data <- NULL
  
  repeat {
    single_reg <- tryCatch({
      GET(paste0("https://api.osf.io/v2/registrations/?filter[id]=", id),
          timeout(60))
    }, error = function(e) {
      cat("GET error for ID", id, ":", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(single_reg)) {
      cat("Skipping ID", id, "=> GET error.\n")
      break
    }
    
    if (single_reg$status_code != 200) {
      cat("HTTP code:", single_reg$status_code, "=> will retry.\n")
      attempt <- attempt + 1
      if (attempt > max_retries) {
        cat("Max retries => skipping ID", id, "\n")
        break
      }
      Sys.sleep(5)
      next
    }
    
    parsed_main <- tryCatch({
      fromJSON(rawToChar(single_reg$content))
    }, error = function(e) {
      cat("JSON parse error for ID", id, ":", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(parsed_main) || is.null(parsed_main$data) || length(parsed_main$data) == 0) {
      cat("No data => skip.\n")
      attempt <- attempt + 1
      if (attempt > max_retries) {
        cat("Max parse => skipping.\n")
        break
      }
      Sys.sleep(5)
      next
    }
    
    reg_data <- parsed_main$data[1, ]
    success_main <- TRUE
    break
  }
  
  if (!success_main || is.null(reg_data)) {
    next
  }
  
  # Title
  title_val <- ifelse(is.null(reg_data$attributes$title), "NA", reg_data$attributes$title)
  
  # registration_responses
  resp <- reg_data$attributes$registration_responses
  if (is.null(resp) || !is.list(resp)) {
    resp <- list()
  }
  
  q1  <- safe_resp(resp$q1)
  q2  <- safe_resp(resp$q2)
  q3  <- safe_resp(resp$q3)
  q4  <- safe_resp(resp$q4)
  q5  <- safe_resp(resp$q5)
  q6  <- safe_resp(resp$q6)
  q7  <- safe_resp(resp$q7)
  q8  <- safe_resp(resp$q8)
  q9  <- safe_resp(resp$q9)
  
  q9a_download <- "NA"
  if (!is.null(resp$q9a) && length(resp$q9a) > 0) {
    if (!is.null(resp$q9a[[1]]$file_urls$download)) {
      q9a_download <- resp$q9a[[1]]$file_urls$download
    }
  }
  
  q10 <- safe_resp(resp$q10)
  q11 <- safe_resp(resp$q11)
  q12 <- safe_resp(resp$q12)
  
  q13_obj <- resp$q13
  if (is.null(q13_obj)) {
    q13_vals <- "NA"
  } else if (is.character(q13_obj)) {
    q13_vals <- q13_obj
  } else if (is.vector(q13_obj)) {
    q13_vals <- paste(q13_obj, collapse = "; ")
  } else {
    q13_vals <- "NA"
  }
  
  q14 <- safe_resp(resp$q14)
  q15 <- safe_resp(resp$q15)
  
  row_data <- data.frame(
    reg_id      = id,
    Title       = title_val,
    Research_Aims = q1,
    Type_of_Aim   = q2,
    Research_Questions = q3,
    Anticipated_Duration = q4,
    Study_Design  = q5,
    Sampling_Strategy = q6,
    Data_Sources  = q7,
    Data_Collection_Methods = q8,
    Data_Collection_Plans   = q9,
    Download_link_of_protocol = q9a_download,
    Stopping_Criteria  = q10,
    Data_analysis_Approach = q11,
    Data_analysis_Process  = q12,
    Credibility_Strategies = q13_vals,
    Credibility_Rationale  = q14,
    Positionality_Reflection = q15,
    stringsAsFactors = FALSE
  )
  
  random_50_content <- rbind(random_50_content, row_data)
}

cat("\nDone processing these 50 random row-based IDs.\n")
cat("We have", nrow(random_50_content), "rows in 'random_50_content' before dedup.\n")

#########################################
## Remove duplicates by 'reg_id'
#########################################
random_50_content <- random_50_content[!duplicated(random_50_content$reg_id), ]

cat("\nAfter deduplication, we have", nrow(random_50_content), "rows.\n")

#########################################
## Export to XLSX
#########################################
wb <- createWorkbook()
addWorksheet(wb, "random_50")
writeData(wb, "random_50", random_50_content)

saveWorkbook(wb, "50_random_registrations.xlsx", overwrite=TRUE)
cat("\nSaved => '50_random_registrations.xlsx'\n")

# Optional: also save as RData or CSV
save(random_50_content, file="50_random_registrations.RData")
write.csv(random_50_content, "50_random_registrations.csv", row.names=FALSE)

cat("\nSaved => '50_random_registrations.RData' and '50_random_registrations.csv'\n")
View(random_50_content)


##############################################
## 1) Load Both Main Data Sets
##############################################

# Adjust file names if yours differ:
load("final_qual_prereg_data.RData")     # loads 'final_df'
load("registration_content.RData")       # loads 'registration_content'

# Check the objects (optional):
# head(final_df)
# head(registration_content)

##############################################
## 2) Deduplicate each data frame by reg_id
##############################################
final_df_unique <- final_df[!duplicated(final_df$reg_id), ]
registration_content_unique <- registration_content[!duplicated(registration_content$reg_id), ]

cat("final_df had", nrow(final_df), "rows; after dedup =>", nrow(final_df_unique), "\n")
cat("registration_content had", nrow(registration_content), "rows; after dedup =>", nrow(registration_content_unique), "\n")

##############################################
## 3) Merge 'title' into 'registration_content'
##############################################
# We'll keep only reg_id + title from final_df_unique
df_title <- final_df_unique[, c("reg_id", "title")]

# Merge on 'reg_id', keeping all rows from registration_content_unique
registration_content_with_title <- merge(
  registration_content_unique,
  df_title,
  by = "reg_id",
  all.x = TRUE  # we keep all IDs in registration_content_unique
)

##############################################
## 4) Remove duplicates after merge (again, safety)
##############################################
registration_content_with_title <- registration_content_with_title[!duplicated(registration_content_with_title$reg_id), ]

cat("After final merge, we have", nrow(registration_content_with_title), "unique rows.\n")

##############################################
## 5) Reorder columns so 'title' is second
##############################################
col_order <- c(
  "reg_id",
  "title",
  setdiff(names(registration_content_with_title), c("reg_id", "title"))
)
registration_content_with_title <- registration_content_with_title[, col_order]

##############################################
## 6) Save the new data set (RData + CSV)
##############################################
save(registration_content_with_title, file="registration_content_with_title.RData")
write.csv(registration_content_with_title, "registration_content_with_title.csv", row.names=FALSE)

cat("\nSaved 'registration_content_with_title.RData' + '.csv'!\n")

##############################################
## 7) Export to Excel (openxlsx)
##############################################
# install.packages("openxlsx") if needed
library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "with_title")
writeData(wb, "with_title", registration_content_with_title)
# Optionally set column widths, wrap text, etc.
# setColWidths(wb, "with_title", cols=1:ncol(registration_content_with_title), widths="auto")

saveWorkbook(wb, "registration_content_with_title.xlsx", overwrite=TRUE)

cat("\nAll done! 'registration_content_with_title.xlsx' saved.\n")

colnames(final_df)


###########################################################
#Division of the data set into 5 subsets based on the year 
###########################################################

######################################################
## Step 1) Load data sets
######################################################

# Adjust filenames if yours differ
load("final_qual_prereg_data.RData")            # should load a data frame named `final_df`, containing at least: reg_id, date_created, title, ...
load("registration_content_with_title.RData")   # should load a data frame named `registration_content_with_title`, containing reg_id, title, etc. but lacks date_created

# Check they loaded
# names(final_df)
# names(registration_content_with_title)

######################################################
## Step 2) Deduplicate each by reg_id
######################################################
final_df_unique <- final_df[!duplicated(final_df$reg_id), ]
rctwt_unique <- registration_content_with_title[!duplicated(registration_content_with_title$reg_id), ]

cat("final_df had", nrow(final_df), "rows; unique =>", nrow(final_df_unique), "\n")
cat("registration_content_with_title had", nrow(registration_content_with_title),
    "rows; unique =>", nrow(rctwt_unique), "\n")

######################################################
## Step 3) Merge 'date_created' from final_df_unique
##          onto registration content with title
######################################################
# We'll keep reg_id + date_created from final_df
df_dates <- final_df_unique[, c("reg_id","date_created")]
# Merge on reg_id, keep all rows in rctwt_unique
rctwt_with_date <- merge(
  rctwt_unique,
  df_dates,
  by = "reg_id",
  all.x = TRUE  # keep all IDs from rctwt_unique
)

# For safety, deduplicate after merge
rctwt_with_date <- rctwt_with_date[!duplicated(rctwt_with_date$reg_id), ]

cat("After merging date_created, we have", nrow(rctwt_with_date), "unique rows.\n")

######################################################
## Step 4) Split by Year (2020..2024)
##          If date_created is stored like "2020-01-13..."
##          we can do substr(..., 1, 4)
######################################################
rc2020 <- subset(rctwt_with_date, substr(date_created, 1, 4) == "2020")
rc2021 <- subset(rctwt_with_date, substr(date_created, 1, 4) == "2021")
rc2022 <- subset(rctwt_with_date, substr(date_created, 1, 4) == "2022")
rc2023 <- subset(rctwt_with_date, substr(date_created, 1, 4) == "2023")
rc2024 <- subset(rctwt_with_date, substr(date_created, 1, 4) == "2024")

cat("\nCounts:\n")
cat("2020 =>", nrow(rc2020), "rows\n")
cat("2021 =>", nrow(rc2021), "rows\n")
cat("2022 =>", nrow(rc2022), "rows\n")
cat("2023 =>", nrow(rc2023), "rows\n")
cat("2024 =>", nrow(rc2024), "rows\n")

######################################################
## Step 5) Export to Excel
##   Approach: Single workbook with 5 sheets
######################################################
library(openxlsx)

wb_years <- createWorkbook()

addWorksheet(wb_years, "2020"); writeData(wb_years, "2020", rc2020)
addWorksheet(wb_years, "2021"); writeData(wb_years, "2021", rc2021)
addWorksheet(wb_years, "2022"); writeData(wb_years, "2022", rc2022)
addWorksheet(wb_years, "2023"); writeData(wb_years, "2023", rc2023)
addWorksheet(wb_years, "2024"); writeData(wb_years, "2024", rc2024)

saveWorkbook(wb_years, "rctwt_year_splits.xlsx", overwrite=TRUE)
cat("\nExcel 'rctwt_year_splits.xlsx' saved, with 5 sheets for 2020..2024.\n")

######################################################
## (Optional) Save each subset as RData or CSV
######################################################
save(rc2020, file="rc2020.RData"); write.csv(rc2020, "rc2020.csv", row.names=FALSE)
save(rc2021, file="rc2021.RData"); write.csv(rc2021, "rc2021.csv", row.names=FALSE)
save(rc2022, file="rc2022.RData"); write.csv(rc2022, "rc2022.csv", row.names=FALSE)
save(rc2023, file="rc2023.RData"); write.csv(rc2023, "rc2023.csv", row.names=FALSE)
save(rc2024, file="rc2024.RData"); write.csv(rc2024, "rc2024.csv", row.names=FALSE)

cat("\nAll subsets saved as RData + CSV if needed!\n")
getwd()

########################################
## Export each subset to its own Excel
########################################

library(openxlsx)  # install.packages("openxlsx") if not installed

# Assuming you already have the data frames:
#   rc2020, rc2021, rc2022, rc2023, rc2024
# created from splitting by year.

# 2020 -> rc2020.xlsx
wb2020 <- createWorkbook()
addWorksheet(wb2020, "2020")
writeData(wb2020, "2020", rc2020)
saveWorkbook(wb2020, "rc2020.xlsx", overwrite=TRUE)

# 2021 -> rc2021.xlsx
wb2021 <- createWorkbook()
addWorksheet(wb2021, "2021")
writeData(wb2021, "2021", rc2021)
saveWorkbook(wb2021, "rc2021.xlsx", overwrite=TRUE)

# 2022 -> rc2022.xlsx
wb2022 <- createWorkbook()
addWorksheet(wb2022, "2022")
writeData(wb2022, "2022", rc2022)
saveWorkbook(wb2022, "rc2022.xlsx", overwrite=TRUE)

# 2023 -> rc2023.xlsx
wb2023 <- createWorkbook()
addWorksheet(wb2023, "2023")
writeData(wb2023, "2023", rc2023)
saveWorkbook(wb2023, "rc2023.xlsx", overwrite=TRUE)

# 2024 -> rc2024.xlsx
wb2024 <- createWorkbook()
addWorksheet(wb2024, "2024")
writeData(wb2024, "2024", rc2024)
saveWorkbook(wb2024, "rc2024.xlsx", overwrite=TRUE)

cat("\n5 Excel files created (rc2020.xlsx ... rc2024.xlsx), each with one sheet.\n")

