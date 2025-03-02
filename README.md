# Installing AI-GENIE

Note: We reccomend that you first install R and R Studio on your machine. You can find instructions on how to do so here: https://posit.co/download/rstudio-desktop/


## Step 1: Ensure you have access to Reticulate.

AI-GENIE accesses large language models like GPT and Mixtral using an API on python. To run python scripts implicitly in R, you first need to install and load the `reticulate` package. 

```{r Install and load reticulate}
install.packages("reticulate")
library(reticulate)
```

## Step 2: Create a Conda Environment 
Next, it is important that you create a Conda environment, or a are self-contained, isolated space where you can install specific versions of software packages, including dependencies, libraries, and Python versions. You only need to do this step ONCE. After a Conda environment is created, you can load the same environment again and again. 

```{r Create the Conda Environment}
venv_name <- "AIGENIE_python_env"
reticulate::conda_create(envname = venv_name, python_version = 3.11)

reticulate::use_condaenv(venv_name, required = TRUE)
```

## Step 3: Use your Conda Environment and Install the Correct Package Versions
Now that you've created your Conda environment, it's time to load the environment and install the correct versions of the python package dependancies. In AI-GENIE, we use both OpenAI and Groq APIs, so both of these packages will need to be loaded into the Conda environment. The `groq` package does NOT require a specific version; however, the `openai` package MUST be set to version 0.28. 

```{r Load the correct package versions in our Conda environment}
reticulate::py_install("groq", envname = venv_name)
reticulate::py_install("openai == 0.28", envname = venv_name)
```

Great! Now your Conda environment is all set up! You do not need to repeat these steps again. For all future package use, simply load this same pre-made Conda environment.




