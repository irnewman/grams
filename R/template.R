#' Create a GRAMS stimulus selection template
#'
#' Writes an R Markdown file that walks through the full GRAMS stimulus
#' selection workflow: downloading and loading the database, selecting
#' matched solution words, selecting matched anagrams, and (optionally)
#' generating matched unsolvable anagrams. Users edit the example values
#' and run the document chunk by chunk; interactive review windows open at
#' each checkpoint.
#'
#' @param path Path for the .Rmd file (default: "grams_workflow.Rmd").
#' @param overwrite Overwrite an existing file? (default: FALSE)
#'
#' @return The file path, invisibly.
#' @export
create_grams_template <- function(path = "grams_workflow.Rmd",
                                  overwrite = FALSE)
{

  if (file.exists(path) && !overwrite) {
    stop("File already exists. Use overwrite = TRUE to replace it.")
  }

  template <- '---
title: "GRAMS Stimulus Selection"
author: "Your Name"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_float: true
    code_folding: show
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = TRUE, warning = TRUE)
```

# Overview

This document walks through the GRAMS stimulus selection workflow:

1. **Setup**: load the package and the GRAMS anagram database
2. **Stage 1**: select solution words, matched across conditions
3. **Stage 2**: select anagrams of those solutions, matched within each item
4. **Stage 3** (optional): generate unsolvable anagrams matched to the solvable ones
5. **Export**: save the final stimulus sets

Several steps open an interactive review window. The document pauses until
you click **Accept** in that window, then continues. Because of this, run the
document **chunk by chunk** in RStudio rather than knitting it in one go.

Edit the example values in each chunk to suit your design.

# Setup

## Load the package

```{r load-packages}
library(grams)

# Set a seed so random sampling is reproducible
set.seed(123)

# Folder for saved outputs
project_dir <- "my_grams_study"
dir.create(file.path(project_dir, "stage1_solutions"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(project_dir, "stage2_anagrams"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(project_dir, "stage3_unsolvables"), recursive = TRUE, showWarnings = FALSE)
```

## Load the GRAMS database

Stage 1 uses the solution-level index (`GRAMS`) that comes with the package.
Stages 2 and 3 also need the full anagram database (~82MB), which is hosted
on OSF. The first time you run this chunk it downloads the database to a
standard location on your computer; after that it skips the download and
loads your local copy.

```{r load-database}
# Download (skipped automatically if already present)
download_grams_database()

# Load into the session
GRAMS_database <- load_grams_database()

# Where the database lives on your machine:
# grams_database_path()
```

If you keep a copy elsewhere, replace the chunk above with:
`GRAMS_database <- readRDS("path/to/grams_database.rds")`

---

# Stage 1: Select Solution Words

Solution words are the real words your anagrams will be made from. Here you
choose which words are eligible, how they divide into conditions, and which
variables must be matched across conditions.

Stage 1 works with **solution-level** variables: one value per word (e.g.,
`Zipf`, `Length`, `OLD20`, `MLBF`). Anagram-level variables are handled in
Stage 2.

**Three kinds of constraint:**

- `filter`: which words are eligible at all. A two-value vector is a range,
  `c(min, max)`. A longer vector is a set of exact allowed values. Use
  `c(3, 6)` for a range of a continuous variable, not `3:6`.
- `split_by`: your solution-level manipulation, written as a LexOPS split
  string, e.g. `"3:4 ~ 5:6"` for a low bin of 3 to 4 and a high bin of 5 to 6.
  Leave it out if solutions are not manipulated.
- `control_for`: variables matched across conditions within each item, as a
  tolerance `c(lower, upper)`. If you leave this out, Zipf is matched within
  0.2 by default. You cannot control for a variable you split by.

```{r stage1-select}
solutions <- select_solutions(
  data = grams::GRAMS,

  # Eligible words: 5 letters, Zipf between 3 and 6
  filter = list(
    Length = c(5, 5),
    Zipf   = c(3, 6)
  ),

  # Manipulation: low vs. high frequency
  split_by = list(
    Zipf = "3:4 ~ 5:6"
  ),

  # Match across conditions within each item
  control_for = list(
    OLD20 = c(-0.5, 0.5),
    MLBF  = c(-0.2, 0.2)
  ),

  # Number of matched items (each item has one word per condition)
  n = 40,

  # Open the review window when generation finishes
  auto_review = TRUE
)
```

**In the review window** you can deselect poorly matched items, regenerate
them, undo, change which variables are plotted, and export the plot. Click
**Accept** to return the final set.

```{r stage1-plot}
# Static plot of the accepted solutions, for the record
plot_solutions(solutions, vars = c("Zipf", "OLD20", "MLBF"))
```

---

# Stage 2: Select Anagrams

Each solution word has many possible anagrams in the database. Here you
choose which anagrams are eligible and, optionally, split them into LOW and
HIGH conditions while matching the two on other variables.

Stage 2 works with **anagram-level** variables, which are prefixed with `a`
(e.g., `aMLBF`, `aMLBPF`, `aOLD20`), plus properties of the anagram-solution
pair such as `Moves`.

## Step 2.1: Configure constraints

This opens a window with sliders for each constraint. The values you give
here are starting points; adjust them in the window, click **Check Coverage**
to see how many items still have enough anagrams, then click **Accept**.
Items that fail coverage are dropped.

- `split_by`: anagram variable to split into LOW and HIGH bins. Bin ranges
  are set with the sliders.
- `filter`: eligible range for anagram variables, as `c(min, max)`.
- `control_for`: maximum allowed difference between the LOW and HIGH anagram
  of the same solution, as a single tolerance value.

```{r stage2-config}
config <- configure_anagram_constraints(
  solutions = solutions,

  # Manipulation: low vs. high anagram bigram frequency
  split_by = list(
    aMLBF = c("low", "high")
  ),

  # Eligible anagrams
  filter = list(
    Moves = c(2, 4)
  ),

  # Match LOW and HIGH anagrams within each solution
  control_for = list(
    aMLBPF = 0.3
  ),

  database = GRAMS_database
)

# Items removed for insufficient coverage, if any:
config$removed
```

## Step 2.2: Generate a candidate pool

For each solution, draw several candidate anagrams per condition. When
`control_for` is set, candidates are drawn as the best-matched LOW/HIGH
pairs.

```{r stage2-select}
anagram_pool <- select_anagrams(
  solutions  = config$solutions,
  config     = config$config,
  database   = GRAMS_database,
  n_per_cell = 5   # candidates per solution per condition
)
```

## Step 2.3: Choose one anagram per solution and condition

This window shows balance metrics for each item. Click an anagram to swap it
for another candidate, sort items by how well they are balanced, and keep
the best ones.

```{r stage2-filter}
filtered_anagrams <- filter_anagrams(anagram_pool)
```

## Step 2.4: Final review

Plots of the selected anagrams by condition, with lines linking each matched
pair. Deselect any remaining problem items, then click **Accept**.

```{r stage2-review}
final_anagrams <- review_anagrams(filtered_anagrams)
```

```{r stage2-plot}
plot_anagrams(final_anagrams, vars = c("aMLBF", "aMLBPF", "Moves"))
```

---

# Stage 3: Unsolvable Anagrams (Optional)

Unsolvable anagrams are letter strings with no valid solution, used as
filler or catch trials. Each is made by changing one or more letters of a
selected anagram until no dictionary word can be formed, while keeping the
new string matched to the original on the metrics you choose.

Every solvable anagram from Stage 2 gets its own matched unsolvable, so
each cell of your design has the same number of solvable and unsolvable
trials.

Skip this section if your design does not need unsolvables.

## Step 3.1: Generate candidates

- `match_metrics` and `tolerance`: which string metrics must match the
  original anagram, and how closely.
- `n_swaps`: how many letters to change (fewer keeps the string closer to
  the original).
- `swap_type`: which letters may change (`"either"`, `"vowels_only"`,
  `"consonants_only"`, `"both"`).
- `swap_strategy`: how replacements are chosen. `"phonetic"` prefers similar
  sounds, `"random"` does not, `"weighted"` mixes the two.

```{r stage3-generate}
unsolvable_pool <- generate_unsolvable_anagrams(
  anagrams      = final_anagrams,
  match_metrics = c("MLBF"),
  tolerance     = list(MLBF = 0.1),
  n_candidates  = 5,
  n_swaps       = 1,
  swap_type     = "either",
  swap_strategy = "phonetic"
)
```

## Step 3.2: Choose one unsolvable per anagram

Click an unsolvable to see the other candidates and how closely each matches.

```{r stage3-filter}
filtered_unsolvables <- filter_unsolvable_anagrams(unsolvable_pool)
```

## Step 3.3: Final review

Solvable and unsolvable strings plotted side by side, with lines linking
each matched pair.

```{r stage3-review}
final_unsolvables <- review_unsolvable_anagrams(filtered_unsolvables)
```

---

# Export

```{r export}
write.csv(solutions,
          file.path(project_dir, "stage1_solutions", "final_solutions.csv"),
          row.names = FALSE)

write.csv(final_anagrams,
          file.path(project_dir, "stage2_anagrams", "final_anagrams.csv"),
          row.names = FALSE)

if (exists("final_unsolvables")) {
  write.csv(final_unsolvables,
            file.path(project_dir, "stage3_unsolvables", "final_unsolvables.csv"),
            row.names = FALSE)
}
```

# Summary

```{r summary}
data.frame(
  Set   = c("Solution words", "Anagrams", "Unsolvables"),
  Count = c(nrow(solutions),
            nrow(final_anagrams),
            if (exists("final_unsolvables")) nrow(final_unsolvables) else NA)
)

# Solutions per condition
table(solutions$condition)

# Anagrams per condition
table(final_anagrams$anagram_condition)
```

# Session Info

Record package versions so the selection can be reproduced.

```{r session-info}
sessionInfo()
```
'

writeLines(template, path)
message("Template created: ", path)
message("Open it in RStudio and run it chunk by chunk.")

invisible(path)
}
