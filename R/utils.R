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
    system.role <- create.system.role.prompt(system.role, item.types, scale.title, sub.domain, item.attributes)
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
        current_items_df <- clean_items(response, split_content, data.frame())
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
        cleaned_items <- validate_return_object(output)
        current_items_df <- data.frame(type=rep(current_label, length(cleaned_items)),
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

    if(!custom){
      items_df_medium$type <- rep(current_label, nrow(items_df_medium))
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
#' @param EGA_model An optional character string specifying the EGA model to use (\code{"tmfg"} or \code{"glasso"}). If \code{NULL}, both models are evaluated, and the best one is selected.
#' @param keep.org Logical; if \code{TRUE}, includes the original items in the returned results. Defaults to \code{FALSE}.
#' @param plot Logical; if \code{TRUE}, displays the network plots. Defaults to \code{TRUE}.
#' @param silently Logical; if \code{TRUE}, suppresses console output. Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to underlying functions.
#' @return A list containing:
#' \describe{
#'   \item{\code{main_result}}{A data frame of the item pool after AI-GENIE reduction.}
#'   \item{\code{initial_EGA}}{The initial Exploratory Graph Analysis (EGA) object.}
#'   \item{\code{final_EGA}}{The final EGA object after reduction.}
#'   \item{\code{embeddings}}{The embeddings generated for the items.}
#'   \item{\code{embedding_type}}{The type of embeddings used (\code{"sparse"} or \code{"full"}).}
#'   \item{\code{selected_model}}{The EGA model selected based on NMI (\code{"tmfg"} or \code{"glasso"}).}
#'   \item{\code{nmi}}{The Normalized Mutual Information (NMI) of the final item pool.}
#'   \item{\code{start_nmi}}{The NMI of the original item pool.}
#'   \item{\code{start_N}}{The starting sample size (number of items).}
#'   \item{\code{final_N}}{The final sample size after reduction.}
#'   \item{\code{original_items}}{(Optional) The original items provided, included if \code{keep.org = TRUE}.}
#' }
run_pipeline <- function(items, openai.key,
                         title = "Networks Before and After AI-Genie",
                         EGA_model= NULL, keep.org,
                         plot = TRUE, silently = FALSE, ...){

  ## Get results
  results <- get_results(items=items, EGA_model=EGA_model, openai.key = openai.key, silently = silently)

  if(!silently){
  flush.console()
  cat(" Done.")
  cat("\n")}

  p1 <- results[["initial_ega_obj"]]
  p2 <- results[["final_ega_obj"]]



  plot_networks <- plot_networks(p1, p2, results[["nmi"]], results[["start_nmi"]], title)

    if(plot){
    plot(plot_networks)
    }

  if(!silently){
  print_results(results)
  }

 if(!keep.org){return(results)}
  else {
    results <- append(results, items)
    names(results)[length(results)] <- "original_items"
    return(results)}
}


#' Plot Networks Before and After AI-GENIE Reduction
#'
#' Creates a combined plot of the initial and final network structures before and after AI-GENIE reduction, including NMI values and a change subtitle.
#'
#' @param p1 An EGA object representing the initial network structure.
#' @param p2 An EGA object representing the final network structure after AI-GENIE reduction.
#' @param nmi2 Numeric; the Normalized Mutual Information (NMI) of the final item pool.
#' @param nmi1 Numeric; the NMI of the original item pool.
#' @param scale.title A character string specifying the title for the combined plot.
#' @return A combined plot object displaying the initial and final networks with captions and annotations.
plot_networks <- function(p1, p2, nmi2, nmi1, scale.title){
  plot1 <- plot(p1) +
    labs(caption = paste0("Before AI-Genie. NMI: ", round(nmi1,4) * 100, "%"))

  plot2 <- plot(p2) +
    labs(caption = paste0("After AI-Genie. NMI: ", round(nmi2,4) * 100, "%"))

  combined_plot <- plot1 + plot2 +
    plot_annotation(
      title = scale.title,
      subtitle = paste0("Change in NMI: ", round((nmi2 - nmi1),4) * 100, "%"),
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12)
      )
    )


  return(combined_plot)
}






