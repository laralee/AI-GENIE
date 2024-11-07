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
#' @param current_label A string of the item type currently being examined
#' @return A data frame with columns:
#' \describe{
#'   \item{\code{type}}{The characteristic or attribute associated with each item.}
#'   \item{\code{statement}}{The cleaned item statement.}
#' }
clean_items <- function(response, split_content,
                        current_items = data.frame("type" = NULL, "attribute"= NULL, "statement" = NULL),
                        current_label) {

  # Try different formats on the response
  formats <- try_formats(response, split_content)

  # Ensure formats are valid and not empty
  if (all(!is.na(unlist(formats)))) {

    # Create a new data frame with the cleaned items
    new_items <- data.frame(
      type = rep(current_label, length(formats$stemmed_characteristics)),
      attribute = formats$stemmed_characteristics,
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
remove_instabilities <- function(items, cut.off = 0.75, verbose, ...)
{
  if (verbose){
    cat("\n")
  }

  # Set counter
  count <- 1

  # BootEGA
  bootstrap <- EGAnet::bootEGA(items, clear = TRUE, suppress = TRUE, plot.itemStability = FALSE,
                               verbose = verbose)
  boot1 <- bootstrap

  current_boot <- NULL

  # Check for instabilities
  while(any(bootstrap$stability$item.stability$item.stability$empirical.dimensions < cut.off)){

    # Update count
    count <- count + 1

    # Update items
    items <- items[,bootstrap$stability$item.stability$item.stability$empirical.dimensions > cut.off]


    # BootEGA
    bootstrap <- EGAnet::bootEGA(items, clear = TRUE, suppress = TRUE, seed = 123,
                                 plot.itemStability = FALSE, verbose = verbose)

    current_boot <- bootstrap
  }

  # Save final bootEGA object
  if (is.null(current_boot)){
    current_boot <- boot1
  }

  boot2 <- current_boot

  # Add count to items
  attr(items, "bootEGA_count") <- count

  # Return items
  return(list(items=items, boot1=boot1, boot2=boot2))

}





# Pipeline ----
#' Get Results
#'
#' Runs the AI-GENIE reduction analysis pipeline, including embeddings generation, redundancy removal using Unique Variable Analysis (UVA), instability removal, and EGA model selection based on Normalized Mutual Information (NMI).
#'
#' @param items A data frame containing the item statements and types.
#' @param EGA.model An optional character string specifying the EGA model to use (\code{"tmfg"} or \code{"glasso"}). If \code{NULL}, both models are evaluated, and the best one is selected.
#' @param openai.key A character string of your OpenAI API key.
#' @param item_type A character string of the current item type undergoing reduction
#' @param keep.org A logical that specifies whether or not the user wants to keep the original items
#' @param silently Logical; if \code{TRUE}, suppresses console output. Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to underlying functions.
#' @return A list containing the main results, EGA objects, bootEGA objects, embeddings, NMI values, and other analysis details.
get_results <- function(items, EGA.model, openai.key, item_type, keep.org, silently, ...) {

  # Define the possible models
  possible_models <- c("tmfg", "glasso")

  # Generate embeddings once
  if(!silently){
    cat("\n")
    cat(paste0("Starting AI-GENIE Reduction Analysis for ", item_type," items..."))
  }

  embedding <- get_embeddings(items, openai.key = openai.key)

  if(!silently){
    cat(" embeddings obtained...")
  }

  # Initialize a list to store results
  results <- list()

  # If EGA.model is specified, evaluate only that model
  if (!is.null(EGA.model)) {

    results[[EGA.model]] <- compute_EGA(items=items, EGA.model = EGA.model,
                                        embedding = embedding, openai.key = openai.key,
                                        silently = silently)

    chosen_model <- EGA.model
    chosen_result <- results[[EGA.model]]

  } else {
    # If EGA.model is not specified, evaluate both models and select the best
    for (model in possible_models) {
      results[[model]] <- compute_EGA(items=items, EGA.model = model,
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
    final_bootega_obj = chosen_result$final_bootega_obj,
    initial_ega_obj = chosen_result$initial_ega_obj,
    initial_bootega_obj = chosen_result$initial_bootega_obj,
    embedding_type = attributes(chosen_result$main_result)$method[["embedding"]],
    selected_model = chosen_model,
    nmi = chosen_result$nmi,
    start_nmi = chosen_result$start_nmi,
    start_N = chosen_result$start_N,
    final_N = chosen_result$final_N
  )

  if(keep.org){
    final_output[["all_item_embeddings"]] <- embedding
  }

  final_output[["embeddings"]] <- chosen_result$embeddings


  return(list(result=final_output, embeddings=embedding))
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
  EGA.model <- obj[["selected_model"]]
  before_nmi <- obj[["start_nmi"]]
  embedding_type <- obj[["embedding_type"]]
  after_genie <- obj[["nmi"]]
  initial_items <- obj[["start_N"]]
  final_items <- obj[["final_N"]]

  cat("\n")
  cat("\n")
  cat(paste("                          AI-Genie Results"))
  cat("\n")
  cat("                          ----------------")
  cat("\n")
  cat(paste("EGA Model:", EGA.model,"    Embeddings Used:", embedding_type,
            "    Staring N:", initial_items, "    Final N:", final_items))
  cat("\n")
  cat(paste0("             Initial NMI: ", round(before_nmi,4) * 100,
             "           Final NMI: ", round(after_genie,4) * 100))
  cat("\n")
  cat("\n")
}



#' Compute EGA
#'
#' Computes the Exploratory Graph Analysis (EGA) steps using the specified EGA model. This function performs initial EGA, removes redundancies using Unique Variable Analysis (UVA), removes instabilities, and computes Normalized Mutual Information (NMI) values before and after AI-GENIE reduction.
#'
#' @param items A data frame containing the item statements and types.
#' @param EGA.model A character string specifying the EGA model to use (\code{"tmfg"} or \code{"glasso"}).
#' @param embedding A matrix of embeddings for the items.
#' @param openai.key A character string of your OpenAI API key.
#' @param silently Logical; if \code{TRUE}, suppresses console output.
#' @param ... Additional arguments passed to underlying functions.
#' @return A list containing the main results, final and initial EGA and bootEGA objects, embeddings, NMI values, and item counts before and after reduction.
compute_EGA <- function(items, EGA.model, embedding, openai.key, silently, ...) {
  if(!silently){
    cat("\n")
    cat(paste0("Computing EGA steps using ", EGA.model, "..."))
  }

  # Assign unique IDs
  items$ID <- as.factor(1:nrow(items))
  items <- items[, c("ID", "statement", "type", "attribute")]

  ## Get truth
  truth <- as.numeric(factor(tolower(items$attribute)))
  names(truth) <- items$statement

  # Use the pre-generated embeddings
  # Before AI-GENIE
  temp <- colnames(embedding)
  colnames(embedding) <- items$ID
  before_ega <- EGA.fit(data=embedding, model = EGA.model, plot.EGA = FALSE, verbose = FALSE)$EGA
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
  after_red_sparse <- EGA.fit(data=unique_items, model = EGA.model, plot.EGA = FALSE, verbose = FALSE)$EGA
  after_red_sparse_nmi <- igraph::compare(
    comm1=truth[colnames(unique_items)], comm2=after_red_sparse$wc, method = "nmi"
  )

  ### Full embedding
  unique_items_full <- embedding[, colnames(unique_items)]
  after_red_full <- EGA.fit(data=unique_items_full, model = EGA.model, plot.EGA = FALSE, verbose = FALSE)$EGA
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
  colnames(unique_items) <- items$ID[items$statement %in% colnames(unique_items)]

  tryCatch(
    boot_res <- remove_instabilities(items=unique_items,
                                     model = EGA.model, EGA.type = "EGA.fit", verbose = !silently),
    error = function(e) {
      if(grepl("Error in dimnames(data) <- `*vtmp*` :", e$message)) {
        cat(" ...BootEGA failed. Trying new seed...")
        boot_res <- remove_instabilities(items=unique_items,
                                         model = EGA.model, EGA.type = "EGA.fit", verbose = !silently,
                                         seed=sample(1:1000, 1))
      } else {
        stop(e)
      }}
  )

  item_set <- boot_res[["items"]]
  colnames(item_set) <- items$statement[items$ID %in% colnames(item_set)]

  if(!silently){
    cat(" bootEGA sweeps complete...")
  }

  ## Number of bootEGA sweeps
  bootEGA_count <- attr(item_set, "bootEGA_count")

  ## Final EGA
  temp <- colnames(item_set)
  colnames(item_set) <- items$ID[items$statement %in% temp]
  final_ega <- EGA.fit(data=item_set, model = EGA.model, plot.EGA = FALSE, verbose = FALSE)$EGA
  colnames(item_set) <- temp

  # Compute NMI after AI-GENIE
  after_genie <- igraph::compare(
    comm1=truth[colnames(item_set)], comm2=final_ega$wc, method = "nmi"
  )

  # Ensure matching rows for both columns
  matched_statements <- colnames(item_set)[colnames(item_set) %in% items$statement]
  matched_attribute <- tolower(items$attribute[items$statement %in% matched_statements])

  # Construct the data frame
  result <- data.frame(
    ID = items$ID[items$statement %in% temp],
    attribute = matched_attribute,
    statement = matched_statements,
    EGA_communities = final_ega$wc[match(matched_statements, colnames(item_set))]
  )

  row.names(result) <- NULL

  # Attach results as attributes
  attr(result, "methods") <- c(
    ega_model = EGA.model,
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
  final_embeddings <- unique_items_full[, colnames(item_set)]

  return(list(
    main_result = result,
    final_ega_obj = final_ega,
    final_bootega_obj = boot_res[["boot2"]],
    initial_ega_obj = before_ega,
    initial_bootega_obj = boot_res[["boot1"]],
    embeddings = final_embeddings,
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

compute_ega_full_sample <- function(embedding, embedding_reduced, items, items_reduced, truth, truth_reduced,
                                    EGA.model, title, calc.final.stability, silently){

  # Assign unique IDs
  full_items <- data.frame("ID"= as.factor(1:nrow(items)),
                           "statement"= items$statement)

  # Get truth for the reduced set
  truth_reduced <- as.numeric(factor(tolower(truth_reduced)))
  names(truth_reduced) <- items_reduced$statement

  # Sparsify reduced embeddings
  embedding_reduced_sparse <- as.matrix(embedding_reduced)
  percentiles <- quantile(embedding_reduced_sparse, probs = c(0.025, 0.975))
  embedding_reduced_sparse[embedding_reduced_sparse > percentiles[1] & embedding_reduced_sparse < percentiles[2]] <- 0

  # Show progress
  if(!silently) {
    cat("\nOptimizing based on the final EGA network...\n")
  }

  best_nmi <- 0
  if(is.null(EGA.model)) {
    for (model_type in c("tmfg", "glasso")) {
      for (use.full in c(TRUE, FALSE)) {
        embedding_use <- if (use.full) embedding_reduced else embedding_reduced_sparse
        temp <- colnames(embedding_use)
        colnames(embedding_use) <- items_reduced$ID

        # Run EGA and calculate NMI
        final_ega <- EGA.fit(data = embedding_use, model = model_type, plot.EGA = FALSE, verbose = FALSE)$EGA
        colnames(embedding_use) <- temp  # Restore original column names
        final_nmi <- igraph::compare(comm1 = truth_reduced, comm2 = final_ega$wc, method = "nmi")

        # Check and update best NMI
        if (final_nmi > best_nmi) {
          embedding_type <- if (use.full) "full" else "sparse"
          model_used <- model_type
          best_nmi <- final_nmi
          best_final_ega <- final_ega
        }
      }
    }
  } else {
    for (use.full in c(TRUE, FALSE)) {
      embedding_use <- if (use.full) embedding_reduced else embedding_reduced_sparse
      temp <- colnames(embedding_use)
      colnames(embedding_use) <- items_reduced$ID

      final_ega <- EGA.fit(data = embedding_use, model = EGA.model, plot.EGA = FALSE, verbose = FALSE)$EGA
      colnames(embedding_use) <- temp
      final_nmi <- igraph::compare(comm1 = truth_reduced, comm2 = final_ega$wc, method = "nmi")

      if (final_nmi > best_nmi) {
        embedding_type <- if (use.full) "full" else "sparse"
        model_used <- EGA.model
        best_nmi <- final_nmi
        best_final_ega <- final_ega
      }
    }
  }

  # Determine initial stability and network structure based on optimal settings
  if(!silently) {
    cat("Final EGA network optimized. Now building initial EGA network based on optimal settings...\n")
  }

  # Prepare full embeddings for initial EGA analysis
  embedding_sparse <- as.matrix(embedding)
  percentiles <- quantile(embedding_sparse, probs = c(0.025, 0.975))
  embedding_sparse[embedding_sparse > percentiles[1] & embedding_sparse < percentiles[2]] <- 0

  embedding_use <- if (embedding_type == "full") embedding else embedding_sparse
  temp <- colnames(embedding_use)
  colnames(embedding_use) <- items$ID

  best_before_ega <- EGA.fit(data = embedding_use, model = model_used, plot.EGA = FALSE, verbose = FALSE)$EGA
  colnames(embedding_use) <- temp
  best_before_nmi <- igraph::compare(comm1 = truth, comm2 = best_before_ega$wc, method = "nmi")

  # Show progress for stability calculation
  if(calc.final.stability && !silently) {
    cat("Done. Now finding final item stability...\n")
  }

  if(calc.final.stability) {
    verbose <- !silently
    bootstrap1 <- EGAnet::bootEGA(embedding_use, clear = TRUE, suppress = TRUE, plot.itemStability = FALSE, seed = 1234, verbose = verbose)

    embedding_use <- if (embedding_type == "full") embedding_reduced else embedding_reduced_sparse
    temp <- colnames(embedding_use)
    colnames(embedding_use) <- items_reduced$ID

    bootstrap2 <- EGAnet::bootEGA(embedding_use, clear = TRUE, suppress = TRUE, plot.itemStability = FALSE, seed = 1234, verbose = verbose)
    colnames(embedding_use) <- temp

    if(!silently) {
      cat("Final stability analysis complete.\n")
    }
  }

  # Network plot based on optimized settings
  network_plot <- plot_networks(p1 = best_before_ega, p2 = best_final_ega, caption1 = "Before AI-GENIE Network",
                                caption2 = "After AI-GENIE Network", nmi2 = best_nmi,
                                nmi1 = best_before_nmi, scale.title = title, ident = FALSE)

  if(calc.final.stability) {
    stability_plot <- plot_networks(p1 = bootstrap1, p2 = bootstrap2, caption1 = "Before AI-GENIE Item Stability",
                                    caption2 = "After AI-GENIE Item Stability", nmi2 = best_nmi,
                                    nmi1 = best_before_nmi, scale.title = title, ident = FALSE)
  }

  # Return result with relevant objects based on stability calculation
  if(calc.final.stability) {
    return_obj <- list(final_ega = best_final_ega, before_ega = best_before_ega, before_nmi = best_before_nmi,
                       final_nmi = best_nmi, items_reduced = items_reduced, initial_bootstrap = bootstrap1,
                       final_bootstrap = bootstrap2, network_plot = network_plot, stability_plot = stability_plot,
                       model_used = model_used, embedding_type = embedding_type)
  } else {
    return_obj <- list(final_ega = best_final_ega, before_ega = best_before_ega, before_nmi = best_before_nmi,
                       final_nmi = best_nmi, items_reduced = items_reduced, network_plot = network_plot,
                       model_used = model_used, embedding_type = embedding_type)
  }

  return(return_obj)
}
