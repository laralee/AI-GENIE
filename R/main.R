

#### USER FACING FUNCTIONS ####

#' AI-GENIE
#'
#' Generate, validate, and assess your items for quality and redundancy using AI-GENIE (Automatic Item Generation and Validation via Network-Integrated Evaluation). This function generates items using your chosen language model, embeds the items, and performs AI-GENIE item validation and redundancy reduction to refine the item pool. You can either have the function construct prompts for you using provided item attributes and optional high-quality item examples (recommended), or supply your own custom prompts. If providing custom prompts, you must also provide a text cleaning function that can parse and extract item statements from the model output.
#'
#' @param item.attributes A required named list containing item type labels and their corresponding attributes. The list have names or identifiers representing item types. Each element should be a character vector of attributes for that item type.
#' @param user.prompts (Required when `custom = TRUE`) A named list or data frame of custom prompt strings for each item type. Each prompt must be a single character string associated with an item type label.
#' @param item.type.definitions An optional named list or data frame providing definitions for each item type. Each definition should be a character string not exceeding 250 characters. This helps the language model understand the item types better. Definitions are included at the beginning of the prompts for their corresponding item types.
#' @param cleaning.fun (Required when `custom = TRUE`) A text cleaning function that can clean and parse the model's expected text output given your custom prompt. The function must accept exactly one parameter (the model's output) and return a list of cleaned items.
#' @param openai.API A required character string of your OpenAI API key.
#' @param groq.API (Required when using open-source models) A character string of your Groq API key.
#' @param custom Logical; defaults to `FALSE`. Indicates whether you intend to supply your own custom prompts.
#' @param system.role An optional character string describing the language model's role (e.g., "a professional methodologist and scale developer").
#' @param scale.title An optional character string specifying the name of your inventory.
#' @param sub.domain An optional character string specifying the inventory's sub-domain or specialty (e.g., "abnormal psychology").
#' @param model A character string specifying the model to use for item generation. Defaults to `"gpt3.5"`. Options are `"gpt3.5"`, `"gpt4o"`, `"llama3"`, `"mixtral"`, or `"gemma2"`. A Groq API key is required for `"llama3"`, `"mixtral"`, or `"gemma2"` models.
#' @param item.examples An optional character vector of well-crafted, high-quality example item strings. If premium items are not readily available, it is recommended to leave this parameter as `NULL`.
#' @param target.N An integer or integer vector specifying the target number of items to generate. Defaults to `100`. If an integer is provided, the function will generate approximately equal numbers of items per item type summing to this total. If an unequal distribution is desired, provide an integer vector specifying the target number of items for each item type, in the same order as the names of `item.attributes` or `user.prompts`. Each item type must have a target of at least 15 items, and the total must be at least 50 items. This requirement does not apply if only generating items (see `items.only`).
#' @param temperature Numeric; defaults to `1`. A value between `0` and `2` setting the temperature of the language model.
#' @param top.p Numeric; defaults to `1`. A value between `0` and `1` setting the top-p parameter of the language model.
#' @param items.only Logical; defaults to `FALSE`. Set to `TRUE` if you only want the items generated without further processing. When `TRUE`, item pool reduction through AI-GENIE is skipped, and the function returns a data frame of the generated items.
#' @param adaptive Logical; defaults to `TRUE`. Indicates whether to use an adaptive prompting approach (recommended). When `TRUE`, the language model receives a list of previously generated items to avoid redundancy. Set to `FALSE` to skip this step if context length is a concern.
#' @param EGA.model A character string specifying the model to use with Exploratory Graph Analysis (EGA). Options are `"tmfg"` or `"glasso"`. Defaults to `tmfg`. If set to `NULL`, both models are tested, and the one yielding the best Normalized Mutual Information (NMI) is returned.
#' @param EGA.algorithm A character string specifying the clustering algorithm to use with Exploratory Graph Analysis (EGA). Options are `"leiden"`, `"louvain"`, or `"walktrap"`. Defaults to `"walktrap"`.
#' @param embedding.model A character string specifying the OpenAI embedding model that should be used. The options are `"text-embedding-3-small"`, `"text-embedding-3-large"`, or `"text-embedding-ada-002"`. Defaults to `"text-embedding-3-small"`.
#' @param keep.org Logical; defaults to `FALSE`. When `TRUE`, returns a data frame of the original item pool.
#' @param plot Logical; defaults to `TRUE`. Specifies whether to display the main summary network plots.
#' @param plot.stability Logical; defaults to `FALSE`. Specifies whether to display the secondary network stability plots.
#' @param calc.final.stability Logical; defaults to `FALSE`. Specifies whether to compute the stability of the item pool before and after item reduction. Setting this parameter to `TRUE` significantly increases computational expense. Changing this value will NOT change item reduction outcome.
#' @param silently Logical; defaults to `FALSE`. When `TRUE`, suppresses console output.
#' @return A list containing:
#' \describe{
  #'   \item{\code{main_result}}{A data frame of the item pool after AI-GENIE reduction. The data frame has the columns `ID`, `type`, `statement`, and `EGA_communities`.}
  #'   \item{\code{final_ega_obj}}{The final EGA object after reduction.}
  #'   \item{\code{final_bootega_obj}}{The final bootEGA object after reduction.}
  #'   \item{\code{initial_ega_obj}}{The initial EGA object with the entire item pool.}
  #'   \item{\code{initial_bootega_obj}}{The initial bootEGA object generated from redundancy-reduced data.}
  #'   \item{\code{embeddings}}{The embeddings generated for the items.}
  #'   \item{\code{sparse_embeddings}}{The sparsified embeddings generated for the items}
  #'   \item{\code{embeddings_used}}{The embeddings used to generate the full-sample plots and overall stats (either the full embeddings or the sparse embeddings)}
  #'   \item{\code{embedding_type}}{The type of embeddings used ("sparse" or "full").}
  #'   \item{\code{selected_model}}{The EGA model used throughout the pipeline.}
  #'   \item{\code{nmi}}{The Normalized Mutual Information (NMI) of the final item pool.}
  #'   \item{\code{start_nmi}}{The NMI of the original item pool.}
  #'   \item{\code{start_N}}{The starting sample size (number of items).}
  #'   \item{\code{final_N}}{The final sample size after reduction.}
  #'   \item{\code{original_items (optional)}}{(ONLY returns if `keep.org` is `TRUE`) The original sample generated.}
  #' }
#' @export
#' @examples
#' \dontrun{
#'
#' ########################################################
#' #### Example 1: Using AI-GENIE with Default Prompts ####
#' ########################################################
#'
#' # Add an OpenAI API key
#' key <- "INSERT YOUR KEY HERE"
#'
#' # Item type definitions
#' trait.definitions <- list(
#'   neuroticism = "Neuroticism is a personality trait that describes one's tendency to experience negative emotions like anxiety, depression, irritability, anger, and self-consciousness.",
#'   openness = "Openness is a personality trait that describes how open-minded, creative, and imaginative a person is.",
#'   extraversion = "Extraversion is a personality trait that describes people who are more focused on the external world than their internal experience."
#' )
#'
#' # Item attributes
#' aspects.of.personality.traits <- list(
#'   neuroticism = c("anxious", "depressed", "insecure", "emotional"),
#'   openness = c("creative", "perceptual", "curious", "philosophical"),
#'   extraversion = c("friendly", "positive", "assertive", "energetic")
#' )
#'
#' # Example items - these should be plain, unformatted, high-quality examples.
#' # items related to neuroticism
#' personality.items <- c(
#'   "I am someone who would panic should I ever misplace my belongings.",
#'   "I am someone who has a low tolerance for others' incompetence.",
#'
#'   # items related to openness
#'   "I am someone who loves to explore new cultures when I travel.",
#'   "I am someone who enjoys engaging in philosophical debates.",
#'
#'   # items related to extraversion
#'   "I am someone who is almost always the life of the party.",
#'   "I am someone who is rarely intimidated when meeting new people."
#' )
#'
#' # Name the field or specialty
#' sub.domain <- "Personality Measurement"
#'
#' # Name the Inventory being created
#' scale.title <- "Three of 'Big Five:' A Streamlined Personality Inventory"
#'
#' # Run AI-GENIE to generate, validate, and redundancy-check an item pool for your new scale.
#' personality.inventory.results <- AIGENIE(
#'   item.attributes = aspects.of.personality.traits,
#'   openai.API = key,
#'   item.examples = personality.items,
#'   sub.domain = sub.domain,
#'   scale.title = scale.title
#' )
#'
#' # View the final item pool
#' View(personality.inventory.results$main_result)
#'
#'
#' #######################################################
#' #### Example 2: Using AI-GENIE with Custom Prompts ####
#' #######################################################
#'
#'
#' # Define a custom system role
#' system.role <- "You are an expert methodologist who specializes in scale development for personality measurement. You are especially equipped to create novel personality items that mimic the style of popular 'Big Five' assessments."
#'
#' # Define custom prompts for each personality trait
#' custom.personality.prompts <- list(
#'
#'   # Prompt for generating neuroticism traits
#'   neuroticism = paste0(
#'     "Generate NINE unique, psychometrically robust single-statement items designed to assess ",
#'     "the Big Five personality trait neuroticism. Put EACH item on its own line. ",
#'     "Neuroticism has the following characteristics: anxious, depressed, insecure, and emotional. ",
#'     "Generate EXACTLY THREE items that target EACH characteristic. This is VERY important. ",
#'     "Format the items EXACTLY like so: \n<characteristic>||<item statement>\n",
#'     "The formatting is EXTREMELY important, so follow it EXACTLY. Be creative! Avoid repetition."
#'  ),
#'
#'   # Prompt for generating openness traits
#'   openness = paste0(
#'     "Generate TWELVE unique, psychometrically robust single-statement items designed to assess ",
#'     "the Big Five personality trait openness. Put EACH item on its own line. ",
#'     "Openness has the following characteristics: worldly, artistic, philosophical, and curious.",
#'     "Generate EXACTLY THREE items that target EACH characteristic. This is VERY important.",
#'     "Format the items EXACTLY like so: \n<characteristic>||<item statement>\n",
#'     "The formatting is EXTREMELY important, so follow it EXACTLY. Be creative! Avoid repetition."
#'   ),
#'
#'   # Prompt for generating extraversion traits
#'   extraversion = paste0(
#'   "Generate EIGHT unique, psychometrically robust single-statement items designed to assess ",
#'   "the Big Five personality trait extraversion. Put EACH item on its own line. ",
#'   "Extraversion has the following characteristics: friendly, positive, assertive, and energetic. ",
#'   "Generate EXACTLY FOUR items that target EACH characteristic. This is VERY important. ",
#'   "Format the items EXACTLY like so: \n<characteristic>||<item statement>\n",
#'   "The formatting is EXTREMELY important, so follow it EXACTLY. Be creative! Avoid repetition."
#'   )
#'
#' )
#'
#' # Define a custom cleaning function that returns a list of cleaned item statements
#' custom_cleaning <- function(content) {
#'
#'   # Split lines and remove empty ones
#'   items <- strsplit(content, "\n")[[1]]
#'   items <- trimws(items)
#'   items <- items[nzchar(items)]
#'   items <- gsub("\\*", "", items)
#'
#'   # Initialize data frame to store cleaned item statements and attributes
#'   item_texts <- c()
#'   item_attributes <- c()
#'
#'   # Iterate over items
#'   for (item in items) {
#'
#'     # Attempt to split by "||"
#'     split_item <- strsplit(item, "||", fixed = TRUE)[[1]]
#'     if (length(split_item) == 2) {
#'
#'       # Assume that the second element is the item statement
#'       item_text <- trimws(split_item[2])
#'
#'       # Assume that the first element is the item characteristic
#'      item_attribute <- trimws(split_item[1])
#'
#'       # Remove unwanted characters
#'       item_text <- gsub(">", "", item_text)
#'       item_text <- gsub("<", "", item_text)
#'       item_attribute <- gsub(">", "", item_attribute)
#'       item_attribute <- gsub("<", "", item_attribute)
#'
#'       # Append to list
#'       item_texts <- c(item_texts, item_text)
#'       item_attributes <- c(item_attribute, item_attributes)
#'
#'     }
#'   }
#'
#'   # Return a data frame of cleaned item statements
#'   return(data.frame("item"=item_texts, "attribute"=item_attributes))
#' }
#'
#' # Run AI-GENIE to generate, validate, and redundancy-check an item pool for your new scale.
#' personality.inventory.results.custom <- AIGENIE(
#'   item.attributes = aspects.of.personality.traits, # created in example 1
#'   user.prompts = custom.personality.prompts,
#'   openai.API = key, # created in example 1
#'   cleaning.fun = custom_cleaning,
#'   item.examples = personality.items, # created in example 1
#'   scale.title = scale.title, # created in example 1,
#'   EGA.model = "tmfg",
#'   custom = TRUE
#' )
#'
#' # View the final item pool
#' View(personality.inventory.results.custom$main_result)
#' }
AIGENIE <- function(item.attributes, openai.API, groq.API = NULL, custom = FALSE,
                    user.prompts = NULL, item.type.definitions = NULL,
                    cleaning.fun = NULL, system.role = NULL,
                    scale.title = NULL, sub.domain = NULL, model = "gpt3.5", item.examples = NULL,
                    target.N = 100, temperature = 1, top.p = 1, items.only = FALSE, adaptive = TRUE,
                    EGA.model = NULL, EGA.algorithm="walktrap", embedding.model = "text-embedding-3-small", keep.org = FALSE,
                    plot = TRUE, plot.stability = FALSE, calc.final.stability = FALSE, silently = FALSE, ...) {

  # Perform input validation
  validated_params <- AIGENIE_checks(
    item.attributes = item.attributes,
    openai.API = openai.API,
    groq.API = groq.API,
    custom = custom,
    user.prompts = user.prompts,
    item.type.definitions = item.type.definitions,
    cleaning.fun = cleaning.fun,
    system.role = system.role,
    scale.title = scale.title,
    sub.domain = sub.domain,
    model = model,
    item.examples = item.examples,
    target.N = target.N,
    temperature = temperature,
    top.p = top.p,
    items.only = items.only,
    adaptive = adaptive,
    EGA.model = EGA.model,
    EGA.algorithm = EGA.algorithm,
    embedding.model = embedding.model,
    keep.org = keep.org,
    plot = plot,
    plot.stability = plot.stability,
    calc.final.stability = calc.final.stability,
    silently = silently,
    ...
  )

  # Reassign parameters
  item.attributes <- validated_params$item.attributes
  openai.API <- validated_params$openai.API
  groq.API <- validated_params$groq.API
  custom <- validated_params$custom
  user.prompts <- validated_params$user.prompts
  item.type.definitions <- validated_params$item.type.definitions
  cleaning.fun <- validated_params$cleaning.fun
  system.role <- validated_params$system.role
  scale.title <- validated_params$scale.title
  sub.domain <- validated_params$sub.domain
  model <- validated_params$model
  item.examples <- validated_params$item.examples
  target.N <- validated_params$target.N
  temperature <- validated_params$temperature
  top.p <- validated_params$top.p
  items.only <- validated_params$items.only
  adaptive <- validated_params$adaptive
  EGA.model <- validated_params$EGA.model
  EGA.algorithm <- validated_params$EGA.algorithm
  embedding.model <- validated_params$embedding.model
  keep.org <- validated_params$keep.org
  plot <- validated_params$plot
  plot.stability <- validated_params$plot.stability
  calc.final.stability <- validated_params$calc.final.stability
  silently <- validated_params$silently

    generated_items <- generate.items.internal(
      model = model,
      temperature = temperature,
      top.p = top.p,
      groq.API = groq.API,
      openai.API = openai.API,
      target.N = target.N,
      item.attributes = item.attributes,
      scale.title = scale.title,
      sub.domain = sub.domain,
      item.examples = item.examples,
      system.role = system.role,
      user.prompts = user.prompts,
      item.type.definitions = item.type.definitions,
      cleaner_fun = cleaning.fun,
      custom = custom,
      adaptive = adaptive,
      silently = silently,
      ...
    )

  # change the item attributes to the appropriate, un-stemmed labels
  names(item.attributes) <- NULL
  item.attributes <- unlist(item.attributes)
  item.attributes <- trimws(tolower(gsub("[[:punct:]]", "", item.attributes)))
  stemmed_attributes <- tm::stemDocument(item.attributes)

  attribute_map <- setNames(item.attributes, stemmed_attributes)
  generated_items$attribute <- attribute_map[generated_items$attribute]

  if (items.only) {
    return(generated_items)
  }

  # Proceed with AI-GENIE reduction and analysis
  results <- run_pipeline(
    items = generated_items,
    openai.key = validated_params$openai.API,
    title = scale.title,
    EGA.model = EGA.model,
    EGA.algorithm = EGA.algorithm,
    embedding.model = embedding.model,
    keep.org = keep.org,
    plot = plot,
    plot.stability = plot.stability,
    calc.final.stability = calc.final.stability,
    silently = silently,
    ...
  )

  return(results)
}





#--- GENIE
#' GENIE (Item Validation and Reduction without Item Generation)
#'
#' Validate and assess your existing items for quality and redundancy using AI-GENIE (Automatic Item Generation and Validation via Network-Integrated Evaluation). This function embeds user-provided items and performs AI-GENIE item validation and redundancy reduction to refine the item pool.
#'
#' @param items A required data frame containing your item statements and item type labels. The data frame should have two columns: one containing the item statements and one containing the item type labels. The column names do not need to be specific; the function will determine which column contains the item statements and which contains the item type labels. There must be at least two distinct item types which each contain at least 15 items. The total number of unique items should be at least 50.
#' @param openai.API A required character string of your OpenAI API key.
#' @param EGA.model A character string specifying the model to use with Exploratory Graph Analysis (EGA). Options are `"tmfg"` or `"glasso"`. Defaults to `tmfg`. If set to `NULL`, both models are tested, and the one yielding the best Normalized Mutual Information (NMI) is returned.
#' @param plot Logical; defaults to `TRUE`. Specifies whether to display summary network plots.
#' @param plot.stability Logical; defaults to `FALSE`. Specifies whether to display the secondary network stability plots.
#' @param calc.final.stability Logical; defaults to `FALSE`. Specifies whether to compute the stability of the item pool before and after item reduction. Setting this parameter to `TRUE` significantly increases computational expense. Changing this value will NOT change item reduction outcome.
#' @param silently Logical; defaults to `FALSE`. When `TRUE`, suppresses console output.
#' @return A list containing:
#' \describe{
#'   \item{\code{main_result}}{A data frame of the item pool after AI-GENIE reduction. The data frame has the columns `ID`, `type`, `statement`, and `EGA_communities`.}
#'   \item{\code{final_ega_obj}}{The final EGA object after reduction.}
#'   \item{\code{final_bootega_obj}}{The final bootEGA object after reduction.}
#'   \item{\code{initial_ega_obj}}{The initial EGA object with the entire item pool.}
#'   \item{\code{initial_bootega_obj}}{The initial bootEGA object generated from redundancy-reduced data.}
#'   \item{\code{embeddings}}{The embeddings generated for the items.}
#'   \item{\code{embedding_type}}{The type of embeddings used ("sparse" or "full").}
#'   \item{\code{selected_model}}{The EGA model used throughout the pipeline.}
#'   \item{\code{nmi}}{The Normalized Mutual Information (NMI) of the final item pool.}
#'   \item{\code{start_nmi}}{The NMI of the original item pool.}
#'   \item{\code{start_N}}{The starting sample size (number of items).}
#'   \item{\code{final_N}}{The final sample size after reduction.}
#'   \item{\code{original_items (optional)}}{(ONLY returns if `keep.org` is `TRUE`) The original sample generated.}
#' }
#' @export
#' @examples
#' \dontrun{
#' ########################################################################
#' ##### Use AI-GENIE without AI - Validate Existing Items via GENIE ######
#' ########################################################################
#'
#'
#' # Add an OpenAI API key
#' key <- "INSERT YOUR KEY HERE"
#'
#' ## Create a data frame of items to reduce - for this example, we will focus only on Neuroticism and Extraversion
#'
#' ## Neuroticism items
#' # items with the 'anxious' attribute
#' statements <- c("I often worry about things that may never happen.",
#' "The thought of uncertainty makes me feel uneasy.",
#' "I often feel on edge and apprehensive about the future.",
#' "I tend to overthink and dwell on negative possibilities.",
#' "I often find myself anticipating the worst possible outcome.",
#' "I frequently feel a sense of dread without any specific reason.",
#' "I often feel a sense of impending doom for no reason.",
#' "I frequently have irrational fears that something bad will happen.",
#' "I frequently worry about things that may never happen.",
#' "I often feel a tightness in my chest when faced with uncertainty.",
#' "I frequently worry about possible negative outcomes in everyday situations.",
#' "I frequently feel a sense of impending doom for no reason.",
#' "I frequently feel a knot in my stomach over minor uncertainties.",
#' "I am constantly on edge, waiting for something terrible to happen.",
#' "I constantly feel a sense of unease and worry, even in everyday situations.",
#' "I often feel panicky and overwhelmed by minor stressors.")
#'
#' # add the appropriate type and attribute label
#' type <- c(rep("neuroticism", 16))
#' attribute <- c(rep("anxious", 16))
#'
#' # items with the 'depressed' attribute
#' statements <- c(statements, "I frequently feel overwhelmed with sadness.",
#' "Even small setbacks can greatly affect my mood.",
#' "I frequently experience feelings of hopelessness and despair.",
#' "I often struggle to find joy or pleasure in activities I used to enjoy.",
#' "I often feel like there is a dark cloud hanging over me.",
#' "I struggle to find a sense of purpose or meaning in my daily activities.",
#' "I often struggle to see the point in going on each day.",
#' "I find it hard to shake off feelings of sadness and despair.",
#' "I often feel like everything is pointless and bleak.",
#' "I frequently struggle to find motivation to get out of bed in the morning.",
#' "I often feel overwhelmed with sadness.",
#' "I often struggle to find motivation to engage in activities I used to enjoy.",
#' "I frequently feel a heavy weight of sadness pressing down on me.",
#' "I frequently experience a deep sense of sadness that lingers throughout the day.",
#' "I often feel emotionally drained and devoid of energy for no apparent reason.")
#'
#' # add the appropriate type and attribute label
#' type <- c(type, rep("neuroticism", 15))
#' attribute <- c(attribute, rep("depressed", 15))
#'
#'
#' # Items with the 'emotional' attribute
#' statements <- c(statements, "I am easily moved to tears by sentimental moments.",
#' "I often find myself experiencing intense feelings of joy or sorrow.",
#' "I am easily swept away by intense emotions.",
#' "I find it challenging to control my emotional reactions.",
#' "I have a tendency to be deeply affected by other peoples moods.",
#' "I often find myself experiencing mood swings without a clear trigger.",
#' "I am easily overwhelmed by my emotions, even in mundane situations.",
#' "I often feel like my mood can shift drastically without warning.",
#' "I often find myself tearing up over minor incidents.",
#' "I frequently experience intense emotional reactions to everyday events.",
#' "I frequently seek reassurance from others to feel confident.",
#' "I find myself tearing up over seemingly insignificant moments.",
#' "I often experience emotional highs and lows without clear reasons.",
#' "I find myself easily moved to tears by emotional situations.",
#' "I frequently feel overwhelmed by intense feelings that come out of nowhere.")
#'
#' # add the appropriate type and attribute label
#' type <- c(type, rep("neuroticism", 15))
#' attribute <- c(attribute, rep("emotional", 15))
#'
#'
#' # add items with the 'insecure' attribute
#' statements <- c(statements, "I often doubt my own abilities and decisions.",
#' "I often seek reassurance from others to feel confident.",
#' "I often feel inadequate compared to others.",
#' "I am constantly worried about others opinions of me.",
#' "I often feel like Im not good enough no matter how hard I try.",
#' "I frequently worry about being rejected or abandoned by others.",
#' "I frequently doubt my own self-worth and capabilities.",
#' "I often feel like I dont measure up to others standards.",
#' "I often compare myself unfavorably to others around me.",
#' "I frequently doubt whether I am worthy of love and acceptance.",
#' "I frequently doubt my own abilities and decisions.",
#' "I often find it challenging to control my emotional reactions.",
#' "I often doubt whether I am truly deserving of success and happiness.",
#' "I frequently second-guess my social interactions, fearing I said something wrong.",
#' "I frequently doubt my worth and significance in the eyes of others.",
#' "I often feel like I dont belong or fit in with those around me.")
#'
#' # add the appropriate type and attribute label
#' type <- c(type, rep("neuroticism", 16))
#' attribute <- c(attribute, rep("insecure", 16))
#'
#'
#'## Extraversion Items
#'# Items with the 'assertive' attribute
#' statements <- c(statements, "I am not afraid to speak my mind and assert my opinions in group discussions.",
#' "I have no problem taking the lead in group projects or activities.",
#' "I confidently express my thoughts and ideas in professional settings.",
#' "I am not hesitant to take charge and make decisions when needed.",
#' "I am not afraid to speak up and advocate for what I believe in.",
#' "Others see me as someone who confidently asserts my opinions.",
#' "I am not afraid to assert myself and speak up for what I believe in.",
#' "Others see me as someone who confidently takes charge in group settings.",
#' "I confidently voice my opinions and stand up for my beliefs.",
#' "I am not afraid to take charge and lead when necessary.",
#' "I confidently speak my mind and stand up for what I believe in.",
#' "Taking the lead in group settings is something I excel at without hesitation.",
#' "I confidently voice my opinions and assert myself in any situation.",
#' "Others perceive me as someone who confidently takes charge and leads.",
#' "I confidently express my opinions and assert myself when needed.",
#' "Taking the lead in discussions or projects is something I do without hesitation.")
#'
#' # add the appropriate type and attribute label
#' type <- c(type, rep("extraversion", 16))
#' attribute <- c(attribute, rep("assertive", 16))
#'
#' # Items with the 'friendly' attribute
#' statements <- c(statements, "I am someone who easily strikes up conversations with strangers in social settings.",
#' "I tend to make friends wherever I go due to my sociable nature.",
#' "I am known for my warm and welcoming demeanor towards others at social events.",
#' "I never shy away from offering a helping hand or a listening ear to those in need.",
#' "I am quick to offer a smile and strike up conversations with strangers.",
#' "I am often described as someone who radiates warmth and friendliness.",
#' "I am always eager to make new friends and build connections.",
#' "Others often describe me as approachable and easy to talk to.",
#' "I am always the first to greet new people and make them feel welcome.",
#' "Others often describe me as kind-hearted and approachable.",
#' "My warm and approachable nature instantly puts others at ease.",
#' "I always go out of my way to make people feel welcome and comfortable.",
#' "I never fail to greet strangers with a warm smile and welcoming demeanor.",
#' "Making new friends comes naturally to me, thanks to my approachable nature.",
#' "I am always known for my warm and welcoming demeanor towards others.",
#' "I easily make friends wherever I go with my welcoming attitude.",
#' "Others often describe me as radiating kindness and warmth.",
#' "I am always known for my warm and welcoming demeanor towards others at social events.",
#' "I tend to make friends effortlessly due to my warm demeanor.",
#' "My approachable nature makes it easy for others to connect with me.")
#'
#' # add the appropriate type and attribute label
#' type <- c(type, rep("extraversion", 20))
#' attribute <- c(attribute, rep("friendly", 20))
#'
#'
#' # Add items with the 'energetic' attribute
#' statements <- c( statements, "I always bring high levels of energy and enthusiasm to social gatherings.",
#' "I thrive in dynamic and fast-paced environments due to my energized nature.",
#' "I am always on the go, seeking new adventures and experiences.",
#' "My boundless energy and enthusiasm are contagious to those around me.",
#' "My high energy levels and enthusiasm are apparent in all that I do.",
#' "I thrive in fast-paced environments due to my energized nature.",
#' "My boundless energy and enthusiasm are evident in all aspects of my life.",
#' "I thrive in fast-paced environments due to my high energy levels.",
#' "I am always the life of the party, bringing high energy levels wherever I go.",
#' "My boundless energy and enthusiasm are evident in every interaction.",
#' "Thriving in fast-paced environments is second nature to me due to my high energy levels.",
#' "My high energy levels are evident in all aspects of my life.",
#' "I thrive in fast-paced environments due to my boundless enthusiasm.",
#' "My high energy levels infuse enthusiasm into everything I do.",
#' "I am always on the move, seeking new adventures and experiences.")
#'
#' # add the appropriate type and attribute label
#' type <- c(type, rep("extraversion", 15))
#' attribute <- c(attribute, rep("energetic", 15))
#'
#'
#' # Build the data frame with three columns: statement, type, and attribute
#' my.personality.items <- data.frame(statement = statements,
#'                                     type = type,
#'                                     attribute = attribute)
#'
#' # Run Genie to validate and redundancy-check an existing item pool
#' my.personality.inventory.results <- GENIE(items = my.personality.items,
#'                                          openai.API = key,
#'                                          EGA.model = "tmfg")
#'
#' # View the final item pool
#' View(my.personality.inventory.results$main_result)
#' }
GENIE <- function(items, openai.API, EGA.model=NULL, EGA.algorithm = "walktrap", embedding.model="text-embedding-3-small", plot=TRUE, plot.stability = FALSE, calc.final.stability=FALSE, silently=FALSE, ...) {

  # check the user-provided items
  checks <- GENIE_checks(item.data=items, openai.API=openai.API, EGA.model=EGA.model,EGA.algorithm = EGA.algorithm, embedding.model=embedding.model,
                         plot=plot, plot.stability= plot.stability, calc.final.stability= calc.final.stability, silently=silently)
  openai.API <- checks[["openai.API"]]
  items <- checks[["items"]]
  item.attributes <- checks[["item.attributes"]]
  EGA.model <- checks[["EGA.model"]]
  EGA.algorithm <- checks[["EGA.algorithm"]]
  embedding.model <- checks[["embedding.model"]]

  # run the pipeline
  run_pipeline <- run_pipeline(items = items, EGA.model = EGA.model, EGA.algorithm= EGA.algorithm,
                               openai.key=openai.API, embedding.model=embedding.model,
                               labels = items$type, keep.org=FALSE, plot = plot, plot.stability = plot.stability,
                               silently= silently, calc.final.stability = calc.final.stability)

  return(run_pipeline)

}


validate_prompt <- function(openai.API=NULL, groq.API = NULL,
                            user.prompts = NULL, N.runs = 3,
                            model="gpt3.5", top.p=1, temperature=1,
                            system.role=NULL, silently=FALSE){

  validate_promt_inputs <- validate_promt_inputs(openai.API, groq.API,
                                                 user.prompts, N.runs, model,
                                                 top.p, temperature, system.role,
                                                 silently)

  user.prompts <- validate_promt_inputs$user.prompts
  openai.API <- validate_promt_inputs$openai.API
  groq.API <- validate_promt_inputs$groq.API
  model <- validate_promt_inputs$model
  system.role <- validate_promt_inputs$system.role

  return(generate_output(openai.API, groq.API,
                  user.prompts, N.runs, model,
                  top.p, temperature, system.role, silently))
}





