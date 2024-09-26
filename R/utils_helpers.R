# AI GENIE Utils Helpers ----

#' Create Prompts
#'
#' Generates user prompts and a system role prompt for item generation based on provided item types, attributes, scale title, sub-domain, and optional item examples.
#'
#' @param item.attributes A list where each element is a vector of attributes corresponding to each item type.
#' @param item.type.definitions An optional named list or data frame providing definitions for each item type. Each definition should be a character string not exceeding 250 characters. This helps the language model understand the item types better. Definitions are included at the beginning of the prompts for their corresponding item types.
#' @param scale.title An optional character string specifying the name of your inventory.
#' @param sub.domain An optional character string specifying the inventory's sub-domain or specialty.
#' @param item.examples An optional character vector of example item strings.
#' @param system.role An optional character string describing the language model's role.
#' @return A list containing:
#' \describe{
#'   \item{\code{user.prompts}}{A list of user prompts for each item type to instruct the language model.}
#'   \item{\code{system.role}}{A character string for the system role prompt.}
#' }
create.prompts <- function(item.attributes, item.type.definitions, scale.title, sub.domain, item.examples,
                           system.role) {
  item.types <- names(item.attributes)

  system.role <- create.system.role.prompt(system.role, item.types, scale.title,
                                           sub.domain, item.examples)

  user.prompts <- list()

  # Create user prompts
  for (i in seq_along(item.types)) {
    current_type <- item.types[[i]]
    attributes <- item.attributes[[current_type]]

    # Build attributes string
    attr_str <- paste(attributes, collapse = ", ")

    # Retrieve definition if provided
    definition <- ""
    if (!is.null(item.type.definitions) && !is.null(item.type.definitions[[current_type]])) {
      definition <- item.type.definitions[[current_type]]
      definition <- substr(definition, 1, 250)
      definition <- paste0("Definition of '", current_type, "': ", definition, "\n")
    }

    # Construct the prompt
    user.prompts[[current_type]] <- paste0(
      definition,
      "Generate a total of ", length(attributes) * 2, " UNIQUE, psychometrically reliable and valid ",
      ifelse(sub.domain != "Networks Before vs After AI-GENIE", sub.domain, "inventory"),
      " items related to the characteristics of the item type '", current_type, "'. Here are the characteristics of the item type '",
      current_type, "': ", attr_str, ". Generate EXACTLY TWO items PER characteristic." ,
      "\nEACH item should be ONE sentence, CONCISE, and DISTINCTLY worded relative to other items.",
      "\nFOLLOW this format EXACTLY for each item:\n<characteristic>: <item content>",
      "\nThis format is EXTREMELY important. Do NOT number or add ANY other text to your response.",
      "\nUse the characteristics EXACTLY as provided. ONLY output the characteristic and item content—NOTHING else."
    )
  }

  return(list(user.prompts = user.prompts, system.role = system.role))
}




#' Clean Items
#'
#' Cleans and parses the language model's response to extract item statements and their corresponding characteristics. This function ensures the items follow the expected format and removes duplicates.
#'
#' @param response The response object from the language model API call.
#' @param split_content A character vector containing stemmed characteristics to validate against.
#' @param current_items A data frame of the current items collected so far. Defaults to an empty data frame.
#' @return A data frame with columns:
#' \describe{
#'   \item{\code{type}}{The characteristic or attribute associated with each item.}
#'   \item{\code{statement}}{The cleaned item statement.}
#' }
clean_items <- function(response, split_content, current_items = data.frame("type" = NULL, "statement" = NULL)) {

  # Try different formats on the response
  formats <- try_formats(response, split_content)

  # Ensure formats are valid and not empty
  if (all(!is.na(unlist(formats)))) {

    # Create a new data frame with the cleaned items
    new_items <- data.frame(
      type = formats$stemmed_characteristics,
      statement = formats$items
    )

    # Combine with current items
    current_items <- rbind.data.frame(current_items, new_items)

    # Remove punctuation and convert to lowercase for uniqueness check
    statements <- tolower(gsub("[[:punct:]]", "", current_items$statement))

    # Remove duplicates based on cleaned statements
    current_items <- current_items[!duplicated(statements),]
  }

  return(current_items)
}


#' Create System Role Prompt
#'
#' Generates the system role prompt for the language model based on the provided system role, item types, scale title, sub-domain, and optional item examples.
#'
#' @param system.role An optional character string describing the language model's role.
#' @param item.types A character vector of item type labels.
#' @param scale.title An optional character string specifying the name of your inventory.
#' @param sub.domain An optional character string specifying the inventory's sub-domain or specialty.
#' @param item.examples An optional character vector of example item strings.
#' @return A character string containing the system role prompt for the language model.
create.system.role.prompt <- function(system.role, item.types, scale.title, sub.domain, item.examples) {
  # add default system role if none was provided
  if(is.null(system.role)){

    system.role <- paste0(
        "You are an expert psychometrician and test developer",
        ifelse(sub.domain != "Networks Before vs After AI-GENIE",
               paste0(" specializing in ", sub.domain, "."),"."),
        " Your task is to create high-quality, psychometrically robust items",
        ifelse(scale.title != "Networks Before vs After AI-GENIE",
               paste0(" for an inventory called '", scale.title, ".'"), "."))
    }

  if(!is.null(item.examples)){
    # add in examples if the user provided them.
    system.role <- paste0(system.role, "\n\n Here are some examples of high-quality items that may be found on such a scale."
      ,"Emulate these items in terms of QUALITY ONLY-- NOT content:\n", item.examples)
  }

  return(system.role)
}



#' Try Formats
#'
#' Attempts to parse the language model's response into the expected format by extracting characteristics and item statements and then stemming the item statements. Validates the formatting and returns the parsed components if successful.
#'
#' @param response The response object from the language model API call.
#' @param split_content A character vector containing stemmed characteristics to validate against.
#' @param ... Additional arguments (currently not used).
#' @return A list containing:
#' \describe{
#'   \item{\code{stemmed_characteristics}}{A character vector of stemmed characteristics extracted from the response.}
#'   \item{\code{items}}{A character vector of item statements extracted from the response.}
#' }
try_formats <- function(response, split_content, ...) {

  # Clean the response so only the items are retained
  content <- response$choices[[1]]$message$content

  # Split lines and remove empty ones
  items <- strsplit(content, "\n")[[1]]
  items <- trimws(items)
  items <- items[nzchar(items)]
  items <- gsub("\\*", "", items)

  # Initialize lists
  characteristics <- character()
  item_texts <- character()

  # Iterate over items
  for (item in items) {
    # Attempt to split by ":"
    split_item <- strsplit(item, ":", fixed = TRUE)[[1]]
    if (length(split_item) == 2) {
      characteristic <- trimws(split_item[1])
      item_text <- trimws(split_item[2])

      # Append to lists
      characteristics <- c(characteristics, characteristic)
      item_texts <- c(item_texts, item_text)
    } else {
      # Handle items that don't match the format
      next
    }
  }

  # Stem characteristics and items
  stemmed_characteristics <- tm::stemDocument(tolower(gsub("[[:punct:]]", "", characteristics)))
  stemmed_items <- tm::stemDocument(tolower(gsub("[[:punct:]]", "", item_texts)))

  # Check for formatting issues
  formatting_issue <- !all(stemmed_characteristics %in% split_content) ||
    any(stemmed_items %in% split_content) ||
    length(characteristics) != length(item_texts)

  if (!formatting_issue && length(characteristics) > 0) {
    return(list(stemmed_characteristics = stemmed_characteristics, items = item_texts))
  }

  # Return NA if formatting issue
  return(list(stemmed_characteristics = NA, items = NA))
}



' Get Embeddings
#'
#' Generates embeddings for the provided item statements using OpenAI's embedding model.
#'
#' @param items A data frame containing the item statements. Must have a column named \code{statement}.
#' @param dimensions An optional integer specifying the number of dimensions for the embeddings. Defaults to \code{1536}.
#' @param openai.key A character string of your OpenAI API key.
#' @return A matrix of embeddings where each column corresponds to an item statement.
get_embeddings <- function(items, dimensions = NULL, openai.key, ...)
{

  # Set up OpenAI
  openai <- reticulate::import("openai")
  openai$api_key <- openai.key

  # Generate embeddings
  word_embeddings <- openai$Embedding$create(
    model = "text-embedding-3-small",
    input = items$statement,
    dimensions = ifelse(
      is.null(dimensions), 1536L, # default
      as.integer(dimensions)
    )
  )

  # Obtain embeddings
  embeddings <- do.call(
    cbind, lapply(word_embeddings$data, function(x){x$embedding})
  )

  # Name embedding
  colnames(embeddings) <- items$statement

  # Return embeddings
  return(embeddings)

}


#' Remove Redundancies
#'
#' Performs Unique Variable Analysis (UVA) on the embeddings to remove redundant items from the item pool.
#'
#' @param embedding A matrix of embeddings where each column corresponds to an item statement.
#' @param ... Additional arguments passed to the \code{\link[EGAnet]{UVA}} function.
#' @return A reduced data matrix with redundant items removed.
remove_redundancies <- function(embedding, ...)
{

  # Sparsify embedding
  percentiles <- quantile(embedding, probs = c(0.025, 0.975))
  embedding[embedding > percentiles[1] & embedding < percentiles[2]] <- 0

  # Increase count
  count <- 1 # count will go back a step if entering the loop

  # Perform UVA
  uva <- EGAnet::UVA(embedding, ...)
  previous_uva <- uva

  # Start reduction
  while(!is.null(uva$keep_remove)){

    # Store previous
    previous_uva <- uva

    # Perform UVA
    uva <- EGAnet::UVA(uva$reduced_data, ...)

    # Increase count
    count <- count + 1

  }

  # Add count to previous UVA
  if (!is.null(previous_uva$reduced_data)) {
    # Assign the count to the valid reduced_data
    attr(previous_uva$reduced_data, "UVA_count") <- count
  } else {
    previous_uva$reduced_data <- data.frame("type"=character(), "statement"=character())
    attr(previous_uva$reduced_data, "UVA_count") <- 0
  }


  # Return previous UVA result
  return(previous_uva$reduced_data)

}



#' Remove Instabilities
#'
#' Performs Bootstrapped Exploratory Graph Analysis (bootEGA) to remove unstable items from the item pool based on a specified stability cutoff.
#'
#' @param items A data matrix or data frame of items after redundancy removal.
#' @param cut.off Numeric; the stability cutoff value. Defaults to \code{0.75}.
#' @param ... Additional arguments passed to the \code{\link[EGAnet]{bootEGA}} function.
#' @return A data matrix or data frame with unstable items removed.
remove_instabilities <- function(items, cut.off = 0.75, ...)
{

  # Set counter
  count <- 1

  # BootEGA
  bootstrap <- EGAnet::bootEGA(items, clear = TRUE, suppress = TRUE, plot.itemStability = FALSE, ...)

  # Check for instabilities
  while(any(bootstrap$stability$item.stability$item.stability$empirical.dimensions < cut.off)){

    # Update count
    count <- count + 1

    # Update items
    items <- items[,bootstrap$stability$item.stability$item.stability$empirical.dimensions > cut.off]

    # BootEGA
    bootstrap <- EGAnet::bootEGA(items, clear = TRUE, suppress = TRUE, seed = 123,
                                 plot.itemStability = FALSE, ...)

  }

  # Add count to items
  attr(items, "bootEGA_count") <- count

  # Return items
  return(items)

}





# Pipeline ----
#' Get Results
#'
#' Runs the AI-GENIE reduction analysis pipeline, including embeddings generation, redundancy removal using Unique Variable Analysis (UVA), instability removal, and EGA model selection based on Normalized Mutual Information (NMI).
#'
#' @param items A data frame containing the item statements and types.
#' @param EGA_model An optional character string specifying the EGA model to use (\code{"tmfg"} or \code{"glasso"}). If \code{NULL}, both models are evaluated, and the best one is selected.
#' @param openai.key A character string of your OpenAI API key.
#' @param silently Logical; if \code{TRUE}, suppresses console output. Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to underlying functions.
#' @return A list containing the main results, EGA objects, embeddings, NMI values, and other analysis details.
get_results <- function(items, EGA_model, openai.key, silently, ...) {

  # Define the possible models
  possible_models <- c("tmfg", "glasso")

  # Generate embeddings once
  if(!silently){
    cat("\n")
    cat("Staring AI-GENIE Reduction Analysis...")
  }

  embedding <- get_embeddings(items, openai.key = openai.key)

  if(!silently){
    cat(" embeddings obtained...")
  }

  # Initialize a list to store results
  results <- list()

  # If EGA_model is specified, evaluate only that model
  if (!is.null(EGA_model)) {

    results[[EGA_model]] <- compute_EGA(items=items, EGA_model = EGA_model,
                                        embedding = embedding, openai.key = openai.key,
                                        silently = silently)

    chosen_model <- EGA_model
    chosen_result <- results[[EGA_model]]

  } else {
    # If EGA_model is not specified, evaluate both models and select the best
    for (model in possible_models) {
      results[[model]] <- compute_EGA(items=items, EGA_model = model,
                                      embedding = embedding, openai.key = openai.key,
                                      silently = silently)
    }

    # Compare NMIs to select the best model
    nmi_tmfg <- results[["tmfg"]]$nmi
    nmi_glasso <- results[["glasso"]]$nmi

    if (nmi_tmfg > nmi_glasso) {
      chosen_model <- "tmfg"
      chosen_result <- results[["tmfg"]]
    } else {
      chosen_model <- "glasso"
      chosen_result <- results[["glasso"]]
    }
  }

  # Attach the chosen model information to the result attributes
  attr(chosen_result$main_result, "selected_ega_model") <- chosen_model

  # Prepare the final output
  final_output <- list(
    main_result = chosen_result$main_result,
    final_ega_obj = chosen_result$final_ega_obj,
    initial_ega_obj = chosen_result$initial_ega_obj,
    embeddings = chosen_result$embeddings,
    embedding_type = attributes(chosen_result$main_result)$method[["embedding"]],
    selected_model = chosen_model,
    nmi = chosen_result$nmi,
    start_nmi = chosen_result$start_nmi,
    start_N = chosen_result$start_N,
    final_N = chosen_result$final_N
  )

  return(final_output)
}



#' Flatten Text
#'
#' Processes text by converting it to lowercase and removing punctuation.
#'
#' @param text A character string or vector to be flattened.
#' @return A character string or vector with text converted to lowercase and punctuation removed.
flatten_text <- function(text)
{
  tolower(gsub("[[:punct:]]", "", text))
}


#' Print Results
#'
#' Displays a summary of the AI-GENIE analysis results, including the EGA model used, embedding type, starting and final number of items, and NMI values before and after reduction. The summary includes the number of iterations for both UVA (Unique Variable Analysis) and bootstrapped EGA steps.
#'
#' @param obj A list object containing the analysis results returned by \code{get_results}.
#' @return No return value; the function prints the results to the console.
print_results<-function(obj){
  ega_model <- obj[["selected_model"]]
  before_nmi <- obj[["start_nmi"]]
  embedding_type <- obj[["embedding_type"]]
  after_genie <- obj[["nmi"]]
  initial_items <- obj[["start_N"]]
  final_items <- obj[["final_N"]]

  cat("\n")
  cat("\n")
  cat("                          AI-Genie Results")
  cat("\n")
  cat("                          ----------------")
  cat("\n")
  cat(paste("EGA Model:", ega_model,"    Embeddings Used:", embedding_type,
            "    Staring N:", initial_items, "    Final N:", final_items))
  cat("\n")
  cat(paste0("             Initial NMI: ", round(before_nmi,4) * 100, "%",
      "           Final NMI: ", round(after_genie,4) * 100, "%"))
  cat("\n")
  cat("\n")
}



#' Compute EGA
#'
#' Computes the Exploratory Graph Analysis (EGA) steps using the specified EGA model. This function performs initial EGA, removes redundancies using Unique Variable Analysis (UVA), removes instabilities, and computes Normalized Mutual Information (NMI) values before and after AI-GENIE reduction.
#'
#' @param items A data frame containing the item statements and types.
#' @param EGA_model A character string specifying the EGA model to use (\code{"tmfg"} or \code{"glasso"}).
#' @param embedding A matrix of embeddings for the items.
#' @param openai.key A character string of your OpenAI API key.
#' @param silently Logical; if \code{TRUE}, suppresses console output.
#' @param ... Additional arguments passed to underlying functions.
#' @return A list containing the main results, final and initial EGA objects, embeddings, NMI values, and item counts before and after reduction.
compute_EGA <- function(items, EGA_model, embedding, openai.key, silently, ...) {
  if(!silently){
    cat("\n")
    cat(paste0("Computing EGA steps using ", EGA_model, "..."))
  }

  # Assign unique IDs
  items$ID <- as.factor(1:nrow(items))
  items <- items[, c("ID", "statement", "type")]

  ## Get truth
  truth <- as.numeric(factor(tolower(items$type)))
  names(truth) <- items$statement

  # Use the pre-generated embeddings
  # Before AI-GENIE
  temp <- colnames(embedding)
  colnames(embedding) <- items$ID
  before_ega <- EGA.fit(data=embedding, model = EGA_model, plot.EGA = FALSE, verbose = FALSE)$EGA
  colnames(embedding) <- temp

  # Compute NMI before AI-GENIE
  before_nmi <- igraph::compare(comm1=truth, comm2=before_ega$wc, method = "nmi")

  # AI-GENIE

  # Remove redundancies
  unique_items <- remove_redundancies(embedding=embedding, cut.off = 0.20)

  if(!silently){
    cat(" UVA complete...")
  }

  ## Items removed by UVA
  uva_removed <- dim(embedding)[2] - dim(unique_items)[2]

  ## Number of UVA sweeps
  uva_count <- attr(unique_items, "UVA_count") - ifelse(attr(unique_items, "UVA_count") > 1, 1, 0)
  # Removes a count due to going back a previous step if entering the 'while' loop

  ### Sparse embedding
  after_red_sparse <- EGA.fit(data=unique_items, model = EGA_model, plot.EGA = FALSE, verbose = FALSE)$EGA
  after_red_sparse_nmi <- igraph::compare(
    comm1=truth[colnames(unique_items)], comm2=after_red_sparse$wc, method = "nmi"
  )

  ### Full embedding
  unique_items_full <- embedding[, colnames(unique_items)]
  after_red_full <- EGA.fit(data=unique_items_full, model = EGA_model, plot.EGA = FALSE, verbose = FALSE)$EGA
  after_red_full_nmi <- igraph::compare(
    comm1=truth[colnames(unique_items)], comm2=after_red_full$wc, method = "nmi"
  )

  ## Swap embeddings based on higher NMI
  if (after_red_full_nmi > after_red_sparse_nmi) {
    unique_items <- unique_items_full
    chosen_embedding_type <- "full"
    chosen_nmi <- after_red_full_nmi
  } else {
    chosen_embedding_type <- "sparse"
    chosen_nmi <- after_red_sparse_nmi
  }

  ## Remove instabilities
  tryCatch(
  item_set <- remove_instabilities(items=unique_items,
                                   model = EGA_model, EGA.type = "EGA.fit", verbose = FALSE),
  error = function(e) {
    if(grepl("Error in dimnames(data) <- `*vtmp*` :", e$message)) {
      cat(" ...BootEGA failed. Trying new seed...")
      item_set <- remove_instabilities(items=unique_items,
                                       model = EGA_model, EGA.type = "EGA.fit", verbose = FALSE,
                                       seed=sample(1:1000, 1))
    } else {
      stop(e)
    }}
  )

  if(!silently){
    cat(" bootEGA sweeps complete...")
  }

  ## Number of bootEGA sweeps
  bootEGA_count <- attr(item_set, "bootEGA_count")

  ## Final EGA
  temp <- colnames(item_set)
  colnames(item_set) <- items$ID[items$statement %in% temp]
  final_ega <- EGA.fit(data=item_set, model = EGA_model, plot.EGA = FALSE, verbose = FALSE)$EGA
  colnames(item_set) <- temp

  # Compute NMI after AI-GENIE
  after_genie <- igraph::compare(
    comm1=truth[colnames(item_set)], comm2=final_ega$wc, method = "nmi"
  )

  # Ensure matching rows for both columns
  matched_statements <- colnames(item_set)[colnames(item_set) %in% items$statement]
  matched_types <- tolower(items$type[items$statement %in% matched_statements])

  # Construct the data frame
  result <- data.frame(
    ID = items$ID[items$statement %in% temp],
    type = matched_types,
    statement = matched_statements,
    EGA_communities = final_ega$wc[match(matched_statements, colnames(item_set))]
  )

  row.names(result) <- NULL

  # Attach results as attributes
  attr(result, "methods") <- c(
    ega_model = EGA_model,
    before_nmi = before_nmi,
    embedding = chosen_embedding_type,
    after_uva = chosen_nmi,
    after_genie = after_genie,
    uva_count = uva_count,
    bootega_count = bootEGA_count,
    initial_items = dim(embedding)[2],
    uva_removed = uva_removed,
    final_items = dim(final_ega$network)[2]
  )

  # Return result along with NMI
  return(list(
    main_result = result,
    final_ega_obj = final_ega,
    initial_ega_obj = before_ega,
    embeddings = item_set,
    nmi = after_genie,
    start_nmi = before_nmi,
    start_N = dim(embedding)[2],
    final_N = dim(final_ega$network)[2]
  ))
}


#' Handle Error Logic During Item Generation
#'
#' Determines whether the item generation process should continue or stop based on the number of consecutive errors or the lack of new unique items generated.
#'
#' @param error_count Integer; the number of consecutive errors or iterations with no new items generated.
#' @param unique_items_generated Integer; the total number of unique items generated so far.
#' @param error_type Character; the type of error encountered (e.g., "no_new_items", "api_error").
#' @param current_label Character; the label of the current item type being processed. Optional.
#' @param max_consecutive_errors Integer; the maximum number of allowed consecutive errors/no new items before stopping. Defaults to \code{10}.
#' @param min_unique_items Integer; the minimum number of unique items required to continue. Defaults to \code{3}.
#'
#' @return Logical; returns \code{TRUE} to continue the generation process, or \code{FALSE} to stop.
handle_error_logic <- function(error_count, unique_items_generated, error_type, current_label = NULL,
                               max_consecutive_errors = 10, min_unique_items = 3) {

  # Check if the maximum number of consecutive errors has been reached
  if (error_count >= max_consecutive_errors) {
    if (!is.null(current_label)) {
      message(sprintf("...Stopping generation for '%s' after %d consecutive %s.",
                      current_label, max_consecutive_errors, error_type))
    } else {
      message(sprintf("...Stopping generation after %d consecutive %s.",
                      max_consecutive_errors, error_type))
    }
    return(FALSE)
  }

  # Check if the number of unique items is below the minimum threshold
  if (unique_items_generated < min_unique_items && error_count > max_consecutive_errors) {
    if (!is.null(current_label)) {
      message(sprintf("...Insufficient unique items generated (%d) for '%s'. Stopping generation.",
                      unique_items_generated, current_label))
    } else {
      message(sprintf("...Insufficient unique items generated (%d). Stopping generation.",
                      unique_items_generated))
    }
    return(FALSE)
  }

  # Handle specific error types differently if needed
  if (error_type == "api_error") {
    if (error_count >= max_consecutive_errors) {
      message("...Multiple API errors encountered. Please check your API keys or rate limits.")
      return(FALSE)
    }
  }

  # If none of the stopping conditions are met, continue generation
  return(TRUE)
}



