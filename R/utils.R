#' Generate Items Internally
#'
#' This internal function drives the item generation process for a personality inventory.
#' It interacts with a language model API (either OpenAI GPT or a Groq-based model) to produce candidate
#' items based on provided prompts. Depending on the mode, the function operates as follows:
#' \itemize{
#'   \item In default mode (\code{custom = FALSE}), it automatically constructs prompts using the provided
#'         \code{item.attributes} (and, optionally, \code{item.type.definitions} and \code{item.examples}) and a
#'         default system role.
#'   \item In custom mode (\code{custom = TRUE}), it uses user-supplied prompts (\code{user.prompts}) and a custom
#'         cleaning function (\code{cleaner_fun}) to parse the language model's output.
#' }
#'
#' For each item type, the function repeatedly calls the language model API until a target number of unique items,
#' as specified by \code{target.N}, is generated or until a threshold of consecutive errors or iterations with no new
#' items is reached. When \code{adaptive = TRUE}, previously generated items are included in subsequent API calls to
#' help reduce redundancy. The function also cleans and deduplicates the generated items before returning the final
#' output.
#'
#' @param model A character string specifying the language model to use. Internally, certain model names are mapped
#'              to API-specific identifiers (e.g., "gpt3.5" becomes "gpt-3.5-turbo").
#' @param temperature Numeric; controls the randomness of the model's output (range: 0–2).
#' @param top.p Numeric; sets the top-p sampling parameter (range: 0–1).
#' @param groq.API A character string containing the Groq API key (used when a non-GPT model is selected).
#' @param openai.API A character string containing the OpenAI API key.
#' @param target.N An integer or vector of integers specifying the target number of items to generate for each item type.
#' @param item.attributes A named list where each element is a character vector of attributes for an item type.
#'                        The names of the list elements serve as the item type labels.
#' @param scale.title An optional character string specifying the title of the inventory.
#' @param sub.domain An optional character string specifying the inventory's sub-domain or specialty.
#' @param item.examples An optional character vector of example item statements to guide generation.
#' @param system.role An optional character string describing the role the language model should assume
#'                    (e.g., "an expert psychometrician and test developer"). If \code{NULL}, a default is generated.
#' @param user.prompts (Required when \code{custom = TRUE}) A named list of custom prompt strings for each item type.
#' @param item.type.definitions An optional named list or data frame providing brief definitions (up to 250 characters)
#'                              for each item type. In default mode, these definitions are prepended to the generated prompts.
#' @param cleaner_fun (Required when \code{custom = TRUE}) A user-supplied function to clean and parse the language model's output.
#'                    The function must accept a single parameter (the raw output text) and return a data frame with two columns:
#'                    \code{item} and \code{attribute}.
#' @param custom Logical; if \code{TRUE}, user-supplied prompts and cleaning function are used. Defaults to \code{FALSE}.
#' @param adaptive Logical; if \code{TRUE}, previously generated items are incorporated into subsequent API calls to reduce redundancy.
#' @param silently Logical; if \code{TRUE}, progress and status messages are suppressed.
#' @param performance Logical; if \code{TRUE}, the function proceeds in performance mode.
#' @param audience String; only used in performance mode
#' @param level.description Data frame; only used in performance mode
#' @param ... Additional arguments passed to underlying API calls and helper functions.
#'
#' @return A data frame of generated items with at least the following columns:
#' \describe{
#'   \item{\code{type}}{The label of the item type for each generated item.}
#'   \item{\code{statement}}{The cleaned and formatted item statement.}
#' }
#' Duplicate items are removed prior to returning the final data frame.
generate.items.internal <- function(model, temperature, top.p, groq.API, openai.API, target.N, item.attributes,
                                    scale.title, sub.domain, item.examples, system.role, user.prompts,
                                    item.type.definitions, cleaner_fun, custom, adaptive, silently,
                                    performance = FALSE, audience = NULL, level.description = NULL, ...) {

  # Switch model name to the correct name in the API
  model <- switch(
    model,
    "llama3" = "llama3-8b-8192",
    "gemma2" = "gemma2-9b-it",
    "mixtral" = "mixtral-8x7b-32768",
    "gpt3.5" = "gpt-3.5-turbo",
    "gpt4o" = "gpt-4o",
    "deepseek" = "deepseek-r1-distill-llama-70b",
    model # Default to the provided model name if not in the list
  )

  # Extract item types and generate prompts
  if (!custom) {
    item.types <- names(item.attributes)

    # Generate prompts with definitions

    prompts <- create.prompts(item.attributes=item.attributes, item.type.definitions=item.type.definitions,
                              scale.title=scale.title, sub.domain=sub.domain, item.examples=item.examples,
                              system.role=system.role, audience, performance, level.description)
    return(prompts)
    system.role <- prompts[["system.role"]]
    user.prompts <- prompts[["user.prompts"]]
  } else {
    item.types <- names(user.prompts)
    system.role <- create.system.role.prompt(system.role, item.types, scale.title, sub.domain,
                                             item.examples = ifelse(is.data.frame(item.examples), "", item.examples),
                                             audience, performance)
  }

  # Determine which model to use
  if (grepl("gpt", model) || grepl("o1", model) || grepl("o3", model)) {
    openai <- reticulate::import("openai")
    openai$api_key <- openai.API
    generate_FUN <- openai$ChatCompletion$create
    max_tokens_set <- 4096L
  } else {
    groq <- reticulate::import("groq")
    groq_client <- groq$Groq(api_key = groq.API)
    generate_FUN <- groq_client$chat$completions$create
    max_tokens_set <- 7000L
  }

  # For o1 and o3 models, use 'max_completion_tokens' and set it to 20000.
  if (grepl("o1", model) || grepl("o3", model)) {
    completion_param <- "max_completion_tokens"
    completion_value <- 20000L
  } else {
    completion_param <- "max_tokens"
    completion_value <- max_tokens_set
  }

  # Helper function to call the API using the correct parameter
  call_generate_FUN <- function(messages_list) {
    call_params <- list(
      model = model,
      messages = messages_list,
      temperature = temperature,
      top_p = top.p
    )
    call_params[[completion_param]] <- completion_value
    do.call(generate_FUN, call_params)
  }

  items_df <- data.frame("type" = character(), "statement" = character(), stringsAsFactors = FALSE)

  if (!custom) {
    split_content <- tm::stemDocument(unlist(item.attributes))
    split_content <- tolower(gsub("[[:punct:]]", "", split_content))
    # Duplicate attribute check is already handled in validate_item_attributes
  }

  if (is.null(item.examples)) {
    examples.incl <- FALSE
  } else {
    examples.incl <- TRUE
  }


  for (i in seq_along(item.types)) {
    unique_items <- character()
    items_df_medium <- data.frame("type" = character(), "statement" = character(), stringsAsFactors = FALSE)
    current_label <- item.types[[i]]

    # Print "Generating items for..." message
    if(!silently){
      cat(paste("Generating items for", current_label, "...\n"))
      flush.console()  # Ensure message is printed immediately
    }

    error_count <- 0
    consecutive_no_new_items <- 0
    n_empty <- 0
    last_error_message <- NULL

    while (length(unique_items) < target.N[[i]]) {

      response <- ""; class(response) <- "try-error"

      # API Call Loop
      while (inherits(response, "try-error")) {
        response <- tryCatch(
          {
            # Construct the 'content' for the user message
            if (is.null(system.role)) {
              stop("Error: 'system.role' is NULL.")
            }

            if (is.null(user.prompts[[i]])) {
              stop(paste0("Error: 'user.prompts[[", i, "]]' is NULL."))
            }

            if (adaptive && length(unique_items) > 0) {
              previous_items_text <- paste0(unique_items, collapse = "\n")

              constructed_content <- paste0(
                user.prompts[[i]],
                "\nDo NOT repeat or rephrase any items from this list of items you've already generated:\n",
                previous_items_text,
                "\n"
              )
            } else {
              constructed_content <- user.prompts[[i]]
            }

            if (is.null(constructed_content) || constructed_content == "") {
              stop("Error: Constructed 'content' for user message is NULL or empty.")
            }

            # Construct messages_list
            messages_list <- list(
              list("role" = "system", "content" = system.role),
              list("role" = "user", "content" = constructed_content)
            )

            if(is.null(response)){
              stop()
            }

            # API Call with Timeout using the helper function
            # Attempt API call and catch token-limit errors to disable adaptive prompting
            try_resp <- try({
              R.utils::withTimeout({
                call_generate_FUN(messages_list)
              }, timeout = 20, onTimeout = "error")
            }, silent = TRUE)

            if (inherits(try_resp, "try-error") &&
                grepl("token limit|context length|context window|token window", conditionMessage(try_resp), ignore.case = TRUE)) {

              warning(sprintf("Adaptive prompt exceeded token limit for model '%s'. Disabling adaptive mode.", model))
              adaptive <<- FALSE  # Globally disable adaptive prompting

              # Rebuild message without prior items
              messages_list <- list(
                list("role" = "system", "content" = system.role),
                list("role" = "user", "content" = user.prompts[[i]])
              )

              # Retry clean prompt
              try_resp <- try({
                R.utils::withTimeout({
                  call_generate_FUN(messages_list)
                }, timeout = 20, onTimeout = "error")
              }, silent = TRUE)
            }

            # Final fallback check
            if (inherits(try_resp, "try-error")) {
              stop(try_resp)
            } else {
              response <- try_resp
            }
            response
          },
          error = function(e) {
            max_consecutive_errors <- 10
            error_count <<- error_count + 1
            last_error_message <<- conditionMessage(e)

            # Check if the error has happened consecutively enough times to be considered critical
            if (error_count >= max_consecutive_errors) {
              # Now print the full error message, since it has become critical
              cat("\n\n Critical API error encountered: ", last_error_message, "\n")
            }

            # Handle error logic if no new items are generated
            continue_process <- handle_error_logic(
              error_count = error_count,
              unique_items_generated = length(unique_items),
              error_type = "api_error",  # Specify the error type
              current_label = current_label
            )

            if (!continue_process) {
              break  # Exit the loop and proceed to the next item type
            }

            # Reprint progress line after potential messages
            if (!silently) {
              cat(sprintf("\rItems generated for %s: %d", current_label, length(unique_items)))
              flush.console()
            }

            # Return a 'try-error' class to continue the loop
            structure(list(), class = "try-error")
          }
        )

        # Reset error_count after successful API call
        if (!inherits(response, "try-error")) {
          error_count <- 0
        }
      }

      Sys.sleep(runif(1, min = 1, max = 3))

      if (!custom) {
        # Use the clean_items function to process and clean the AI-generated items
        current_items_df <- clean_items(response, split_content, data.frame(), current_label, item.attributes[[current_label]])
      } else {
        # Custom cleaning branch with retry mechanism
        max_cleaning_attempts <- 5
        cleaning_attempt <- 1
        cleaning_success <- FALSE

        while (cleaning_attempt <= max_cleaning_attempts && !cleaning_success) {

          # If this is a retry, re-run the API call for a fresh response
          if (cleaning_attempt > 1) {
            Sys.sleep(runif(1, min = 1, max = 3))
            response <- tryCatch({
              if (adaptive && length(unique_items) > 0) {
                previous_items_text <- paste0(unique_items, collapse = "\n")
                constructed_content <- paste0(
                  user.prompts[[i]],
                  "\nDo NOT repeat or rephrase any items from this list of items you've already generated:\n",
                  previous_items_text,
                  "\n"
                )
              } else {
                constructed_content <- user.prompts[[i]]
              }
              messages_list <- list(
                list("role" = "system", "content" = system.role),
                list("role" = "user", "content" = constructed_content)
              )
              R.utils::withTimeout({
                response <- call_generate_FUN(messages_list)
              }, timeout = 20, onTimeout = "error")
              response
            }, error = function(e) {
              structure(list(), class = "try-error")
            })
          }

          cleaning_result <- tryCatch({
            output <- cleaner_fun(response$choices[[1]]$message$content)
            validate_output <- validate_return_object(output, n_empty, item.attributes)
            list(success = TRUE, validate_output = validate_output)
          }, error = function(e) {
            list(success = FALSE, error = e)
          })

          if (cleaning_result$success) {
            cleaning_success <- TRUE
            validate_output <- cleaning_result$validate_output
            cleaned_items <- validate_output[["items"]]
            n_empty <- validate_output[["n_empty"]]
            cleaned_item_attributes <- validate_output[["item_attributes"]]
          } else {
            cleaning_attempt <- cleaning_attempt + 1
            if (cleaning_attempt > max_cleaning_attempts) {
              stop(cleaning_result$error)
            }
          }
        }

        current_items_df <- data.frame(type = rep(current_label, length(cleaned_items)),
                                       attribute = cleaned_item_attributes,
                                       statement = cleaned_items)
      }

      # Ensure all whitespace is trimmed
      current_items_df$statement <- sapply(current_items_df$statement, trimws)
      current_items_df$statement <- unique(current_items_df$statement)

      # Identify new unique items
      new_unique_items <- unique(setdiff(current_items_df$statement, unique_items))

      # Update unique_items and items_df_medium
      unique_items <- c(unique_items, new_unique_items)
      unique_items <- unique(unique_items)  # Ensure items are unique
      items_df_medium <- rbind(items_df_medium,
                               current_items_df[current_items_df$statement %in% new_unique_items, ])

      items_df_medium <- items_df_medium[!duplicated(items_df_medium$statement),]

      # Update consecutive_no_new_items
      if (length(new_unique_items) == 0) {
        consecutive_no_new_items <- consecutive_no_new_items + 1
      } else {
        consecutive_no_new_items <- 0 # Reset if new items were added
      }

      # Handle error logic if no new items are generated
      continue_process <- handle_error_logic(
        error_count = consecutive_no_new_items,
        unique_items_generated = length(unique_items),
        error_type = "no_new_items"
      )

      if (!continue_process) {
        break # Exit the loop and proceed to the next item type
      }

      # Update progress dynamically
      if (!silently){
        cat(sprintf("\rItems generated for %s: %d", current_label, length(unique_items)))
        flush.console()
      }
    }

    items_df <- rbind(items_df, items_df_medium)
    items_df_nd <- items_df[!duplicated(items_df$statement),]

    if (nrow(items_df) != nrow(items_df_nd)){
      if (!silently){
        cat("\n")
        cat(paste(nrow(items_df) - nrow(items_df_nd), "duplicate items detected and removed."))
        cat("\n")
      }
    }

    if(!silently){
      # Move to the next line after finishing with the current label
      cat("\n")
      cat("\n")
      flush.console()
    }
  }

  items_df$statement <- trimws(gsub('[\"\\\']', "", items_df$statement))
  rownames(items_df) <- NULL

  items_df_nd <- items_df[!duplicated(items_df$statement),]

  if (nrow(items_df) != nrow(items_df_nd)){
    if (!silently){
      cat("\n")
      cat(paste(nrow(items_df) - nrow(items_df_nd), "duplicate item(s) detected and removed."))
      cat("\n")
    }
  }

  if(!silently){
    cat(paste0("All items generated. Final sample size: ", nrow(items_df_nd)))
  }
  return(items_df_nd)
}



#' Run the AI-GENIE Reduction and Analysis Pipeline
#'
#' This function implements the core reduction and analysis pipeline for an item pool. It takes a data frame of items
#' (with at least three columns: \code{statement}, \code{type}, and \code{attribute}) and performs the following steps:
#'
#' \enumerate{
#'   \item \strong{Item-Type Level Analysis:}
#'         \itemize{
#'           \item Splits the item pool into subsets based on the \code{type} column.
#'           \item For each item type, calls \code{get_results} to obtain reduction results that include:
#'                 \describe{
#'                   \item{Embeddings}{A matrix of embeddings for the items of that type.}
#'                   \item{EGA Objects}{Both initial and final Exploratory Graph Analysis (EGA) objects.}
#'                   \item{BootEGA Objects}{Bootstrapped EGA objects for assessing item stability (if calculated).}
#'                   \item{Network/Stability Plots}{Plots comparing the item network and stability before and after reduction.}
#'                 }
#'           \item Collects the embeddings, reduced items, and truth labels (from the \code{attribute} column) from each subset.
#'         }
#'
#'   \item \strong{Overall Sample Analysis:}
#'         \itemize{
#'           \item Aggregates the embeddings and reduced items across all item types.
#'           \item Calls \code{compute_ega_full_sample} to optimize the overall EGA network by evaluating different embedding configurations (full vs. sparse)
#'                 and selecting the model (or combination) that maximizes the Normalized Mutual Information (NMI).
#'           \item Computes overall quality metrics (such as \code{nmi}, \code{start_nmi}, \code{start_N}, and \code{final_N}) and generates
#'                 overall network and (optionally) stability plots.
#'         }
#'
#'   \item \strong{Embedding Augmentation:}
#'         \itemize{
#'           \item Optionally (if \code{keep.org = TRUE}), retains the original item pool and associated embeddings.
#'           \item For each item type, appends the corresponding embedding matrices (both full and sparsified) along with an indicator
#'                 of which type was ultimately used.
#'         }
#' }
#'
#' Additionally, if \code{plot} and/or \code{plot.stability} are \code{TRUE}, the function displays the network and stability plots,
#' and it prints a summary of the overall results via \code{print_results}.
#'
#' @param items A data frame of items with at least three columns: \code{statement} (item text), \code{type} (item type), and \code{attribute} (item attribute).
#'              This data frame represents the complete item pool.
#' @param openai.key A character string containing your OpenAI API key.
#' @param title A character string for the analysis title, used in plot captions (default: "Networks Before and After AI-Genie").
#' @param EGA.model An optional character string specifying the EGA model to use (e.g., \code{"tmfg"} or \code{"glasso"}). If \code{NULL},
#'                 both models are evaluated, and the one with the highest NMI is selected.
#' @param EGA.algorithm A character string specifying the clustering algorithm for EGA (e.g., \code{"walktrap"}, \code{"louvain"}, or \code{"leiden"}).
#' @param embedding.model A character string specifying the OpenAI embedding model to use.
#' @param keep.org Logical; if \code{TRUE}, the original item pool and original embedding matrices are included in the output.
#' @param plot Logical; if \code{TRUE}, network plots comparing the pre- and post-reduction item networks are generated.
#' @param plot.stability Logical; if \code{TRUE}, additional stability plots (from bootEGA) are generated.
#' @param calc.final.stability Logical; if \code{TRUE}, final bootstrapped stability analysis is performed.
#' @param silently Logical; if \code{TRUE}, suppresses console output.
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return A list with two components:
#' \describe{
#'   \item{\code{overall_sample}}{
#'     A list containing overall sample-level analysis results, including:
#'     \itemize{
#'       \item \code{main_result}: A data frame of the refined (reduced) item pool for the entire sample.
#'       \item \code{final_ega_obj}: The final EGA object after reduction.
#'       \item \code{final_bootega_obj}: The final bootEGA object (if stability analysis was performed).
#'       \item \code{initial_ega_obj}: The initial EGA object computed on the full item pool.
#'       \item \code{initial_bootega_obj}: The initial bootEGA object computed on the redundancy-reduced items.
#'       \item \code{selected_model}: The EGA model used (either as specified or chosen based on optimal NMI).
#'       \item \code{nmi}: The final Normalized Mutual Information (NMI) value after reduction.
#'       \item \code{start_nmi}: The NMI computed on the original item pool.
#'       \item \code{start_N}: The total number of items in the original pool.
#'       \item \code{final_N}: The number of items in the final reduced pool.
#'       \item \code{network_plot}: A network plot object comparing pre- and post-reduction item networks.
#'       \item \code{stability_plot}: A stability plot object (if \code{calc.final.stability = TRUE}).
#'       \item \code{embeddings}: A list containing embedding matrices used in the analysis:
#'             \itemize{
#'               \item \code{full}: The full embeddings matrix for the reduced items.
#'               \item \code{sparse}: A sparsified version of the embeddings.
#'               \item \code{embed_type_used}: A string indicating whether "full" or "sparse" embeddings were used.
#'             }
#'       \item Optionally, if \code{keep.org = TRUE}, additional elements are included:
#'             \itemize{
#'               \item \code{original_sample_items}: The original item pool as a data frame.
#'               \item \code{original_sample_full}: The full embeddings matrix for the original items.
#'               \item \code{original_sample_sparse}: A sparsified version of the original embeddings.
#'             }
#'     }
#'   }
#'   \item{\code{item_type_level}}{
#'     A named list with detailed analysis results for each individual item type. Each element is a list
#'     (structured similarly to the overall sample result) representing the analysis performed solely on the items
#'     of that type.
#'   }
#' }
run_pipeline <- function(items, openai.key,
                         title = "Networks Before and After AI-Genie",
                         EGA.model= NULL,
                         EGA.algorithm,
                         embedding.model,
                         keep.org = FALSE,
                         plot = TRUE, plot.stability = FALSE, calc.final.stability,
                         silently = FALSE, ...){




  ## Get results for each trait type
  trait_type_indices <- split(seq_len(nrow(items)), items$type)
  item_type_names <- unique(items$type)
  item_level_results <- setNames(vector("list", length(item_type_names)), item_type_names)

  all_embeds <- data.frame(matrix(NA, nrow = 1536, ncol = 0))
  all_truth <- items$attribute
  embeddings_reduced <- data.frame(matrix(NA, nrow = 1536, ncol = 0))
  items_reduced <- data.frame(matrix(NA, ncol = 3, nrow = 0))
  truth_reduced <- c()

  if(!silently) {
    cat("\n")
  }

  for (i in 1:length(trait_type_indices)){
  item_type <- names(trait_type_indices)[[i]]
  results <- get_results(items=items[trait_type_indices[[i]],], EGA.model=EGA.model,
                         EGA.algorithm=EGA.algorithm,
                         embedding.model = embedding.model,
                         openai.key = openai.key, item_type=item_type,
                         keep.org=keep.org,silently = silently)
  curr_embeds <- results[["embeddings"]]
  results <- results[["result"]]

  curr_items <- data.frame("statement"=results$main_result$statement,
                           "attribute"=results$main_result$attribute,
                           "type"= rep(item_type, length(results$main_result$attribute)))

  all_embeds <- cbind(curr_embeds, all_embeds)


  embeddings_reduced <- cbind(results$embeddings, embeddings_reduced)

  items_reduced <- rbind(curr_items, items_reduced)
  truth_reduced <- c(curr_items$attribute, truth_reduced)


  if (keep.org) {
    results[["original_items"]] <- items[trait_type_indices[[i]],]
  }


  if(!silently){
  flush.console()
  cat(" Done.")
  cat("\n")}



      p1 <- results[["initial_ega_obj"]]
      p2 <- results[["final_ega_obj"]]

      network_plot <- plot_networks(p1=p1, p2=p2, caption1 = "Before AI-GENIE Network",
                                      caption2 = "After AI-GENIE Network",
                                      nmi2 = results[["nmi"]],
                                      nmi1 = results[["start_nmi"]],
                                      scale.title = paste(title, "(For",item_type, "Items)"), ident=FALSE)

    if(plot){
      plot(network_plot)
    }
      results[["network_plot"]] <- network_plot

    p1 <- results[["initial_bootega_obj"]]
    p2 <- results[["final_bootega_obj"]]

    nmi_start <- as.numeric(attributes(results[["main_result"]])$methods[["after_uva"]])

    ident <- ifelse(attributes(results[["main_result"]])$methods[["bootega_count"]] == "1",  TRUE, FALSE)

    stability_plots <- plot_networks(p1 = p1, p2 = p2,
                                    caption1 = "Before BootEGA Step",
                                    caption2 = "After BootEGA Step",
                                    nmi2=results[["nmi"]],
                                    nmi1=nmi_start,
                                    scale.title = paste(title, "(For",item_type, "Items)"),
                                    ident=ident)

    if (plot.stability){
      plot(stability_plots)
    }

    results[["stability_plot"]] <- stability_plots

  item_level_results[[item_type]] <- results
  }


  if(!silently){
  cat("\n")
  cat("Item-level analysis complete.")
  cat("\n")
  cat("\n")
  cat("Checking quality of item reduction on the sample overall...")
  }

  complied_results <- compute_ega_full_sample(embedding_reduced = embeddings_reduced,
                                              embedding = all_embeds,
                                              items_reduced = items_reduced,
                                              items = items,
                                              truth_reduced = truth_reduced,
                                              truth = all_truth,
                                              EGA.model = EGA.model,
                                              EGA.algorithm = EGA.algorithm,
                                              title=title,
                                              calc.final.stability = calc.final.stability,
                                              silently = silently)

  if (calc.final.stability){
  overall_result <- list(
      main_result = complied_results$items_reduced,
      final_ega_obj = complied_results$final_ega,
      final_bootega_obj = complied_results$final_bootstrap,
      initial_ega_obj = complied_results$before_ega,
      initial_bootega_obj = complied_results$initial_bootstrap,
      selected_model = complied_results$model_used,
      nmi = complied_results$final_nmi,
      start_nmi = complied_results$before_nmi,
      start_N = nrow(items),
      final_N = nrow(complied_results$items_reduced),
      network_plot = complied_results$network_plot,
      stability_plot = complied_results$stability_plot
    )
  } else {
    overall_result <- list(
      main_result = complied_results$items_reduced,
      final_ega_obj = complied_results$final_ega,
      initial_ega_obj = complied_results$before_ega,
      selected_model = complied_results$model_used,
      nmi = complied_results$final_nmi,
      start_nmi = complied_results$before_nmi,
      start_N = nrow(items),
      final_N = nrow(complied_results$items_reduced),
      network_plot = complied_results$network_plot
    )
  }

  if(keep.org){
    # include sparse embeddings - all items
    embedding_sparse <- as.matrix(all_embeds)
    percentiles <- quantile(embedding_sparse, probs = c(0.025, 0.975))
    embedding_sparse[embedding_sparse > percentiles[1] & embedding_sparse < percentiles[2]] <- 0


    # include sparse embeddings - selected items
    embedding_sparse_selected <- as.matrix(embeddings_reduced)
    percentiles <- quantile(embedding_sparse_selected, probs = c(0.025, 0.975))
    embedding_sparse_selected[embedding_sparse_selected > percentiles[1] & embedding_sparse_selected < percentiles[2]] <- 0


    # Build return object
    embeddings_list <- list(original_sample_full = as.matrix(all_embeds),
                            original_sample_sparse = as.matrix(embedding_sparse),
                            full = as.matrix(embeddings_reduced),
                            sparse = as.matrix(embedding_sparse_selected),
                            embed_type_used = complied_results$embedding_type)

    # Add the original items and embeds
    overall_result[["original_sample_items"]] <- items
    overall_result[["embeddings"]] <- embeddings_list

  } else{

  # include sparse embeddings
  embedding_sparse <- as.matrix(embeddings_reduced)
  percentiles <- quantile(embedding_sparse, probs = c(0.025, 0.975))
  embedding_sparse[embedding_sparse > percentiles[1] & embedding_sparse < percentiles[2]] <- 0

  # Build return object
  embeddings_list <- list(full = as.matrix(embeddings_reduced),
                          sparse = as.matrix(embedding_sparse),
                          embed_type_used = complied_results$embedding_type)

  # Add embeds return object
  overall_result[["embeddings"]] <- embeddings_list

  }

  if(plot){
    plot(complied_results$network_plot)
  }

  if(plot.stability && calc.final.stability){
    plot(complied_results$stability_plot)
  }

  # add the appropriate embedding vectors to the item-level results
  for (item.type in names(item_level_results)){
    embedding_list <- list()

    IDs_reduced <- items_reduced$type==item.type

    if(keep.org){

      IDs <- items$type==item.type

      # sparsify all embeds
      embedding_sparse <- as.matrix(all_embeds)
      percentiles <- quantile(embedding_sparse, probs = c(0.025, 0.975))
      embedding_sparse[embedding_sparse > percentiles[1] & embedding_sparse < percentiles[2]] <- 0

      embedding_list[["original_sample_full"]] <- as.matrix(all_embeds)[,IDs]
      embedding_list[["original_sample_sparse"]] <- embedding_sparse[,IDs]

      # sparsify selected embeds
      embedding_sparse <- as.matrix(embeddings_reduced)
      percentiles <- quantile(embedding_sparse, probs = c(0.025, 0.975))
      embedding_sparse[embedding_sparse > percentiles[1] & embedding_sparse < percentiles[2]] <- 0

      embedding_list[["full"]] <- as.matrix(embeddings_reduced)[,IDs_reduced]
      embedding_list[["sparse"]] <- embedding_sparse[,IDs_reduced]

    } else {
      # sparsify selected embeds
      embedding_sparse <- as.matrix(embeddings_reduced)
      percentiles <- quantile(embedding_sparse, probs = c(0.025, 0.975))
      embedding_sparse[embedding_sparse > percentiles[1] & embedding_sparse < percentiles[2]] <- 0

      embedding_list[["full"]] <- as.matrix(embeddings_reduced)[,IDs_reduced]
      embedding_list[["sparse"]] <- embedding_sparse[,IDs_reduced]

    }

    # add the embedding type to the list
    embedding_list[["embed_type_used"]] <- item_level_results[[item.type]]$embedding_type

    # drop the embedding type from the overall object
    item_level_results[[item.type]]$embedding_type <- NULL

    item_level_results[[item.type]][["embeddings"]] <- embedding_list
  }

  if(!silently){
  print_results(overall_result, item_level_results)
  }


  return(list(overall_sample = overall_result,
              item_type_level = item_level_results))
}


#' Plot Network Comparisons
#'
#' Generates a comparative plot of two network analysis results, typically representing the item network
#' before and after AI-GENIE reduction. The plot includes provided captions, displays NMI values for each network,
#' and incorporates a scale title to contextualize the comparison. The layout may be adjusted based on the
#' \code{ident} parameter.
#'
#' @param p1 An object representing the first network analysis result (e.g., the initial EGA object before reduction).
#' @param p2 An object representing the second network analysis result (e.g., the final EGA object after reduction).
#' @param caption1 A character string to be used as a caption or title for the first network (e.g., "Before AI-GENIE Network").
#' @param caption2 A character string for the second network (e.g., "After AI-GENIE Network").
#' @param nmi1 A numeric value representing the Normalized Mutual Information (NMI) of the first network.
#' @param nmi2 A numeric value representing the NMI of the second network.
#' @param scale.title A character string specifying the title of the scale or inventory; it is used to annotate the plot.
#' @param ident Logical; if \code{TRUE}, the function adjusts the plot layout or labeling based on item identity details.
#'
#' @return A plot object that visually compares the two network structures. The plot will typically display
#'         the two networks (either side-by-side or in an overlaid manner) with the provided captions and NMI values.
#'         The exact type of the plot object (e.g., a \code{ggplot} object or a base R plot) depends on the implementation.
plot_networks <- function(p1, p2, caption1, caption2, nmi2, nmi1, scale.title, ident=FALSE){

  if(!ident){
  plot1 <- plot(p1) +
    labs(caption = paste0(caption1," (NMI: ", round(nmi1,4) * 100, ")"))

  plot2 <- plot(p2) +
    labs(caption = paste0(caption2," (NMI: ", round(nmi2,4) * 100, ")"))

  combined_plot <- plot1 + plot2 +
    plot_annotation(
      title = scale.title,
      subtitle = paste0("Change in NMI: ", round((nmi2 - nmi1),4) * 100),
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12)
      )
    )


}
  else{
    plot1 <- plot(p1) +
      labs(caption = paste0("No BootEGA Reduction - Stability Plot for All Items (", round(nmi2,4) * 100, ")"))

    combined_plot <- plot1 +
      plot_annotation(
        title = "Item Stability for All Final Items",
        subtitle = scale.title,
        theme = theme(
          plot.title = element_text(hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12)
        ))
  }

  return(combined_plot)
}


#' Generate the Output ONLY for Users Testing a Custom Prompt
#'
#' Generates and returns a list of the text output that was generated by their prompts.
#' This can help them ensure there items are being written as expected. Also, this can
#' help users ensure their cleaning function works as expected.
#'
#' @param openai.API A string containing their OpenAI API key (or \code{NULL} if the user is using a Groq model)
#' @param groq.API A string containing their Groq API key (or \code{NULL} if the user is using an OpenAI model)
#' @param user.prompts A named list specifying the prompts that should be used to generate the output for each item type.
#' @param N.runs The number of times the code should generate output for each prompt.
#' @param model A string containing the LLM model to be used.
#' @param top.p The top-p setting of the model.
#' @param temperature The temperature setting of the model.
#' @param system.role A string containing the desired system role of the model
#'
#' @return A list of lists containing the output of the model for each item type across the runs.
generate_output <- function(openai.API, groq.API,
                            user.prompts, N.runs, model,
                            top.p, temperature, system.role,
                            silently) {

  # Switch model name to the correct name in the API
  model <- switch(
    model,
    "llama3" = "llama3-8b-8192",
    "gemma2" = "gemma2-9b-it",
    "mixtral" = "mixtral-8x7b-32768",
    "gpt3.5" = "gpt-3.5-turbo",
    "gpt4o" = "gpt-4o",
    "deepseek" = "deepseek-r1-distill-llama-70b",
    model # Default to the provided model name if not in the list
  )

  # Determine which API to use
  if (grepl("gpt", model) || grepl("o3", model) || grepl("o1", model)) {
    openai <- reticulate::import("openai")
    openai$api_key <- openai.API
    generate_FUN <- openai$ChatCompletion$create
    max_tokens_set <- 4096L
  } else {
    groq <- reticulate::import("groq")
    groq_client <- groq$Groq(api_key = groq.API)
    generate_FUN <- groq_client$chat$completions$create
    max_tokens_set <- 7000L
  }

  # Default system role if not provided
  if (is.null(system.role)) {
    system.role <- "You are an expert psychometrician and test developer. Your task is to create high-quality, psychometrically robust items."
  }

  # Determine which token parameter to use
  if (grepl("o1", model) || grepl("o3", model)) {
    token_param <- "max_completion_tokens"
    token_value <- 20000L
  } else {
    token_param <- "max_tokens"
    token_value <- max_tokens_set
  }

  item.types <- names(user.prompts)
  responses <- list()

  for (i in seq_along(item.types)) {
    current_label <- item.types[[i]]

    # Print "Generating responses for..." message
    if (!silently) {
      cat(paste("Generating responses for", current_label, "... "))
    }

    messages_list <- list(
      list("role" = "system", "content" = system.role),
      list("role" = "user", "content" = user.prompts[[current_label]])
    )

    # Initialize the list for current_label
    responses[[current_label]] <- list()

    for (j in 1:N.runs) {
      retry_count <- 0
      success <- FALSE

      while (!success && retry_count < 10) {  # Retry up to 10 times
        tryCatch({
          # API Call with Timeout using the appropriate token parameter
          R.utils::withTimeout({
            params <- list(
              model = model,
              messages = messages_list,
              temperature = temperature,
              top_p = top.p
            )
            params[[token_param]] <- token_value
            response <- do.call(generate_FUN, params)
          }, timeout = 20, onTimeout = "error")

          # Store response and break loop on success
          responses[[current_label]][[j]] <- response$choices[[1]]$message$content
          success <- TRUE  # Mark success

        }, error = function(e) {
          retry_count <<- retry_count + 1  # Increment retry count
          if (!silently) {
            cat(paste0("\nError encountered (", retry_count, "/10). Retrying...\n"))
          }
          Sys.sleep(1)  # Wait 1 second before retrying
        })

        if (retry_count == 10) {
          if (!silently) {
            cat("\nFailed 10 times. Skipping this request.\n")
          }
          break
        }
      }
    }

    if (!silently) {
      cat("Done.\n")
    }
  }

  return(responses)
}


