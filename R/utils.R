#' Generate Items Internally
#'
#' Generates items using the specified language model (e.g., GPT-3.5, GPT-4o, or open-source models via Groq API). This internal function constructs prompts, handles API interactions, processes the model's output, and ensures that the generated items meet the required format. It supports both default and custom prompt modes.
#'
#' @param model A character string specifying the model to use for item generation. Options include \code{"gpt3.5"}, \code{"gpt4o"}, \code{"llama3"}, \code{"mixtral"}, or \code{"gemma2"}. The function internally maps these to the correct API model names.
#' @param temperature Numeric; controls the randomness of the model's output. A value between \code{0} and \code{2}, where higher values produce more random results. Defaults to \code{1}.
#' @param top.p Numeric; controls the diversity of the model's output by sampling from the top \code{p} probability mass. A value between \code{0} and \code{1}. Defaults to \code{1}.
#' @param groq.API A character string of your Groq API key. Required if using open-source models.
#' @param openai.API A character string of your OpenAI API key. Required if using GPT models.
#' @param target.N An integer vector specifying the target number of items to generate for each item type.
#' @param item.attributes A named list or data frame where each element is a character vector of attributes corresponding to each item type. Required when \code{custom = FALSE}.
#' @param scale.title An optional character string specifying the name of your inventory.
#' @param sub.domain An optional character string specifying the inventory's sub-domain or specialty.
#' @param item.examples An optional character vector of well-crafted, high-quality example item strings.
#' @param system.role A character string describing the language model's role.
#' @param user.prompts A named list of custom prompts written by the user to be in the model
#' @param item.type.definitions An optional named list or data frame providing definitions for each item type. Each definition should be a character string not exceeding 250 characters. This helps the language model understand the item types better. Definitions are included at the beginning of the prompts for their corresponding item types.
#' @param cleaner_fun A function provided by the user to clean and parse the model's output when \code{custom = TRUE}. The function must accept exactly one parameter (the model's output) and return a list of cleaned items.
#' @param custom Logical; indicates whether custom prompts and a custom cleaning function are used. Defaults to \code{FALSE}.
#' @param adaptive Logical; if \code{TRUE}, uses adaptive prompting to avoid generating redundant items by including previously generated items in the prompt. Defaults to \code{TRUE}.
#' @param silently Logical; if \code{TRUE}, suppresses console output. Defaults to \code{FALSE}.
#' @param ... Additional arguments (currently not used).
#' @return A data frame containing unique, cleaned items with their associated item type labels.
generate.items.internal <- function(model, temperature, top.p, groq.API, openai.API, target.N, item.attributes,
                                    scale.title, sub.domain, item.examples, system.role, user.prompts,
                                    item.type.definitions, cleaner_fun, custom, adaptive, silently, ...) {

  # Switch model name to the correct name in the API
  model <- switch(
    model,
    "llama3" = "llama3-8b-8192",
    "gemma2" = "gemma2-9b-it",
    "mixtral" = "mixtral-8x7b-32768",
    "gpt3.5" = "gpt-3.5-turbo",
    "gpt4o" = "gpt-4o",
    model # Default to the provided model name if not in the list
  )

  # Extract item types and generate prompts
  if (!custom) {
    item.types <- names(item.attributes)

    # Generate prompts with definitions
    prompts <- create.prompts(item.attributes, item.type.definitions, scale.title, sub.domain, item.examples,
                              system.role)
    system.role <- prompts[["system.role"]]
    user.prompts <- prompts[["user.prompts"]]
  } else {
    item.types <- names(user.prompts)
    system.role <- create.system.role.prompt(system.role, item.types, scale.title, sub.domain, item.examples)
  }

  # Determine which model to use
  if (grepl("gpt", model)) {
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

  max_sample_size <- if (examples.incl && grepl("gpt", model)) 50 else if (examples.incl) 150 else if (grepl("gpt", model)) 75 else 200

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
              sampled_items <- sample(unique_items, min(max_sample_size, length(unique_items)))
              previous_items_text <- paste0(sampled_items, collapse = "\n")

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

            #API Call with Timeout
            R.utils::withTimeout({
              response <- generate_FUN(
                model = model,
                messages = messages_list,
                temperature = temperature,
                max_tokens = max_tokens_set,
                top_p = top.p
              )
            }, timeout = 20, onTimeout = "error")
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
        current_items_df <- clean_items(response, split_content, data.frame(), current_label)
      } else {
        tryCatch({
          output <- cleaner_fun(response$choices[[1]]$message$content)
        }, error = function(e) {
          if(grepl("unused argument", e$message)) {
            stop("Your text cleaning function should accept exactly one parameter.")
          } else {
            stop(e)
          }}
        )
        validate_output <- validate_return_object(output, n_empty, item.attributes)
        cleaned_items <- validate_output[["items"]]
        n_empty <- validate_output[["n_empty"]]
        cleaned_item_attributes <- validate_output[["item_attributes"]]
        current_items_df <- data.frame(type=rep(current_label, length(cleaned_items)),
                                       attribute= cleaned_item_attributes,
                                       statement=cleaned_items)
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
      cat("\n")}
  }

  if(!silently){
    cat(paste0("All items generated. Final sample size: ", nrow(items_df_nd)))}
  return(items_df_nd)
}



# Pipeline ----
#' Run AI-GENIE Pipeline
#'
#' Executes the AI-GENIE reduction analysis pipeline on the provided items, including embeddings generation, redundancy removal, instability removal, and network plotting.
#'
#' @param items A data frame containing the item statements and types.
#' @param openai.key A character string of your OpenAI API key.
#' @param title An optional character string specifying the title for the network plots. Defaults to \code{"Networks Before and After AI-Genie"}.
#' @param EGA.model An optional character string specifying the EGA model to use (\code{"tmfg"} or \code{"glasso"}). If \code{NULL}, both models are evaluated, and the best one is selected.
#' @param keep.org Logical; if \code{TRUE}, includes the original items in the returned results. Defaults to \code{FALSE}.
#' @param plot Logical; if \code{TRUE}, displays the network plots. Defaults to \code{TRUE}.
#' @param plot.stability Logical; Specifies whether to display the secondary network stability plots. Defaults to \code{FALSE}.
#' @param calc.final.stability Logical; defaults to `FALSE`. Specifies whether to compute the stability of the item pool before and after item reduction.
#' @param silently Logical; if \code{TRUE}, suppresses console output. Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to underlying functions.
#' @return A list containing:
#' \describe{
#'   \item{\code{main_result}}{A data frame of the item pool after AI-GENIE reduction. The data frame has the columns `ID`, `type`, `statement`, and `EGA_communities`.}
#'   \item{\code{final_ega_obj}}{The final EGA object after reduction.}
#'   \item{\code{final_bootega_obj}}{The final bootEGA object after reduction.}
#'   \item{\code{initial_ega_obj}}{The initial EGA object with the entire item pool.}
#'   \item{\code{initial_bootega_obj}}{The initial bootEGA object generated from redundancy-reduced data.}
#'   \item{\code{embeddings}}{The embeddings generated for the items.}
#'   \item{\code{sparse_embeddings}}{The sparsifies embeddings generated for the items}
#'   \item{\code{embeddings_used}}{The embeddings used to generate the full-sample plots and overall stats (either the full embeddings or the sparse embeddings)}
#'   \item{\code{embedding_type}}{The type of embeddings used ("sparse" or "full").}
#'   \item{\code{selected_model}}{The EGA model used throughout the pipeline.}
#'   \item{\code{nmi}}{The Normalized Mutual Information (NMI) of the final item pool.}
#'   \item{\code{start_nmi}}{The NMI of the original item pool.}
#'   \item{\code{start_N}}{The starting sample size (number of items).}
#'   \item{\code{final_N}}{The final sample size after reduction.}
#'   \item{\code{original_items (optional)}}{(ONLY returns if `keep.org` is `TRUE`) The original sample generated.}
#' }
run_pipeline <- function(items, openai.key,
                         title = "Networks Before and After AI-Genie",
                         EGA.model= NULL,
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

  print_results(overall_result)

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

  return(list(overall_sample = overall_result,
              item_type_level = item_level_results))
}


#' Plot Networks Before and After AI-GENIE Reduction
#'
#' Creates a combined plot of the initial and final network structures before and after AI-GENIE reduction, including NMI values and a change subtitle.
#'
#' @param p1 An EGA object representing the initial network structure (or stability).
#' @param p2 An EGA object representing the final network structure after AI-GENIE reduction (or stability).
#' @param nmi2 Numeric; the Normalized Mutual Information (NMI) of the final item pool.
#' @param nmi1 Numeric; the NMI of the original item pool (or the item pool that was redundancy reduced, in the case of item stability).
#' @param scale.title A character string specifying the title for the combined plot.
#' @param ident Logical; specifies if the two bootEGA objects are the same (i.e., if there was only one round of bootEGA). Defaults to \code{FALSE}
#' @return A combined plot object displaying the initial and final networks with captions and annotations.
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
    model # Default to the provided model name if not in the list
  )


  # Determine which model to use
  if (grepl("gpt", model)) {
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

  if(is.null(system.role)){
    system.role <- "You are an expert psychometrician and test developer. Your task is to create high-quality, psychometrically robust items."
  }

  item.types <- names(user.prompts)


  responses <- list()

  for (i in seq_along(item.types)) {

    current_label <- item.types[[i]]

    # Print "Generating items for..." message
    if(!silently){
      cat(paste("Generating responses for", current_label, "... "))
    }

    messages_list <- list(
      list("role" = "system", "content" = system.role),
      list("role" = "user", "content" = user.prompts[[current_label]])
    )

    for (i in 1:N.runs) {
    #API Call with Timeout
    R.utils::withTimeout({
      response <- generate_FUN( model = model,
                messages = messages_list,
                temperature = temperature,
                max_tokens = max_tokens_set,
                top_p = top.p
              )
            }, timeout = 20, onTimeout = "error")

      responses[[current_label]][[i]] <- response$choices[[1]]$message$content
    }

    if(!silently){
    cat("Done.\n")}

  }

  return(responses)

}
