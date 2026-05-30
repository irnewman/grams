#' Select matched solution words for anagram experiments
#'
#' Uses LexOPS to generate matched sets of solution words across experimental
#' conditions. Solution words are filtered, split into conditions, and matched
#' on control variables. Optionally opens an interactive review interface.
#'
#' @param data Source lexicon (default: grams::GRAMS). Must contain a 'Word' column
#'   and lexical property columns (e.g., Length, Zipf, OLD20).
#' @param filter Named list of filtering criteria. Each element should be a variable
#'   name with either: (1) a 2-element numeric vector for range `c(min, max)`, or
#'   (2) a vector of acceptable values. Example: `list(Length = 5:6, Zipf = 3:6)`
#' @param split_by Named list defining experimental conditions. Each element is a
#'   variable name with a character string specifying the split (LexOPS format).
#'   Example: `list(Zipf = "c(low = 1:3, high = 4:6)")`. Creates crossed conditions
#'   if multiple variables specified.
#' @param control_for Named list of variables to match across conditions. Each element
#'   is a variable name with a 2-element tolerance vector `c(lower, upper)`.
#'   Example: `list(Length = c(-1, 1), OLD20 = c(-0.5, 0.5))`. If NULL and not
#'   splitting by Zipf, defaults to controlling Zipf within ±0.2. Cannot control
#'   for a variable you are splitting by.
#' @param n Number of items to generate per condition cell. Use "all" to generate
#'   the maximum possible matched sets (default: 10).
#' @param auto_review Logical. If TRUE (default), opens the interactive review GUI
#'   immediately after generation. If FALSE, returns the raw matched set.
#' @param seed Optional random seed for reproducibility (default: NULL).
#'
#' @return A data frame in long format with matched solution words. Each row is one
#'   word, with columns: string (word), condition (experimental condition), item_nr
#'   (matched set identifier), plus all lexical properties from the source data.
#'   The selection parameters are stored as an attribute for regeneration.
#'
#' @details
#' The function workflow:
#' 1. Filters the lexicon based on `filter` criteria
#' 2. Splits into experimental conditions using `split_by`
#' 3. Applies matching constraints with `control_for`
#' 4. Generates n matched sets using LexOPS
#' 5. Optionally opens review GUI for visual inspection
#'
#' **Filtering**: Variables can be filtered by range (e.g., `Length = 5:6`) or
#' specific values (e.g., `POS = c("noun", "verb")`).
#'
#' **Splitting**: Creates experimental conditions. Multiple splits create crossed
#' designs. Example: splitting by Zipf (high/low) and OLD20 (sparse/dense) creates
#' a 2×2 design with 4 conditions.
#'
#' **Controlling**: Ensures conditions are matched on nuisance variables. Default
#' controls for word frequency (Zipf) unless you're splitting by it.
#'
#' @examples
#' \dontrun{
#' # Simple: 20 5-6 letter words, high vs low frequency
#' solutions <- select_solutions(
#'   filter = list(Length = 5:6),
#'   split_by = list(Zipf = "c(low = 1:3, high = 4:6)"),
#'   n = 20
#' )
#'
#' # Complex: 2×2 design, custom controls, all possible matches
#' solutions <- select_solutions(
#'   filter = list(Length = 5:6, Zipf = 3:6),
#'   split_by = list(
#'     Zipf = "c(low = 3:4, high = 5:6)",
#'     OLD20 = "c(sparse = 1:2, dense = 3:5)"
#'   ),
#'   control_for = list(Length = c(-0.5, 0.5), Articulability = c(-0.3, 0.3)),
#'   n = "all"
#' )
#'
#' # No automatic review (for scripting)
#' solutions <- select_solutions(
#'   filter = list(Length = 5),
#'   n = 50,
#'   auto_review = FALSE
#' )
#' }
#'
#' @seealso
#' \code{\link{review_solutions}} for the interactive review interface
#' \code{\link{plot_solutions}} for plotting solution distributions
#'
#' @export
select_solutions <- function(data = GRAMS,
                             filter = NULL,
                             split_by = NULL,
                             control_for = NULL,
                             n = 10,
                             auto_review = TRUE,
                             seed = NULL)
{

  if (!is.null(seed)) set.seed(seed)

  # apply filters first
  lex_data <- data

  if (!is.null(filter)) {
    for (var_name in names(filter)) {
      val <- filter[[var_name]]
      if (length(val) == 2) {
        lex_data <- lex_data |>
          dplyr::filter(.data[[var_name]] >= val[1], .data[[var_name]] <= val[2])
      } else {
        lex_data <- lex_data |>
          dplyr::filter(.data[[var_name]] %in% val)
      }
    }
  }

  # start LexOPS pipeline
  lex_data <- lex_data |>
    dplyr::rename(string = Word) |>
    LexOPS::set_options(id_col = "string")

  # apply splits
  if (!is.null(split_by)) {
    for (i in seq_along(split_by)) {
      var_name <- names(split_by)[i]
      spec_string <- split_by[[i]]
      spec_expr <- str2lang(spec_string)

      lex_data <- eval(substitute(
        LexOPS::split_by(lex_data, VAR, SPEC),
        list(VAR = as.name(var_name), SPEC = spec_expr)
      ))
    }
  }

  message("control_for received: ", paste(names(control_for), collapse = ", "))
  message("Length of control_for: ", length(control_for))

  # default control check
  if (is.null(control_for) || length(control_for) == 0) {
    if (!is.null(split_by) && "Zipf" %in% names(split_by)) {
      stop("When splitting by Zipf, you must specify control_for explicitly.\n",
           "Example: control_for = list(Length = c(-1, 1))\n",
           "Cannot control for a variable you are splitting by.")
    }
    message("Adding default control: Zipf ±0.2")
    control_for <- list(Zipf = c(-0.2, 0.2))
  }

  # apply the controls
  if (!is.null(control_for)) {
    for (i in seq_along(control_for)) {
      var_name <- names(control_for)[i]
      tolerance <- control_for[[i]]
      tol_expr <- str2lang(paste0(tolerance[1], ":", tolerance[2]))

      lex_data <- eval(substitute(
        LexOPS::control_for(lex_data, VAR, TOL),
        list(VAR = as.name(var_name), TOL = tol_expr)
      ))
    }
  }

  # generate matched sets
  message("Generating matched sets...")
  if (is.character(n) && n == "all") {
    result <- suppressWarnings({
      lex_data |>
        LexOPS::generate(n = "all") |>
        LexOPS::long_format()
    })
  } else {
    result <- suppressWarnings({
      lex_data |>
        LexOPS::generate(n = n) |>
        LexOPS::long_format()
    })
  }

  message("Result columns: ", paste(names(result), collapse = ", "))
  message("String column class: ", class(result$string))
  message("First few strings: ", paste(head(result$string, 3), collapse = ", "))

  message("Generated ", length(unique(result$item_nr)), " matched sets")

  # store params
  attr(result, "selection_params") <- list(
    data = substitute(data),
    filter = filter,
    split_by = split_by,
    control_for = control_for
  )

  # review
  if (auto_review) {
    result <- review_solutions(result)
  }

  result
}

#' Get plottable variables from a dataset
#'
#' @param data Dataset with variables
#' @param type Either "solution" or "anagram"
#' @export
get_plottable_vars <- function(data, type = c("solution", "anagram"))
{
  type <- match.arg(type)
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  # exclude control columns
  vars <- numeric_vars[!numeric_vars %in% c("item_nr", "match_null")]
  # exclude aggregates
  vars <- vars[!grepl("mean|range|min|max|rank", vars)]
  if (type == "anagram") {
    # only anagram-level variables (start with 'a' followed by uppercase)
    vars <- vars[grepl("^a[A-Z]", vars)]
  } else if (type == "solution") {
    # exclude anagram-level variables (don't start with 'a' followed by uppercase)
    vars <- vars[!grepl("^a[A-Z]", vars)]
  }
  vars
}

#' Plot solution word distributions by condition
#'
#' @param data LexOPS output (long format)
#' @param vars Variables to plot (default: all numeric except item_nr)
#' @param show_means Show mean values (default: FALSE)
#' @param show_labels Show item labels on lines (default: FALSE)
#' @export
plot_solutions <- function(data,
                           vars = NULL,
                           show_means = FALSE,
                           show_labels = FALSE)
{

  # auto-detect numeric variables if not specified
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric)]
    # exclude LexOPS control columns
    vars <- vars[!vars %in% c("item_nr", "match_null")]
  }

  # reshape to long format
  long <- data |>
    dplyr::select(item_nr, condition, string, dplyr::all_of(vars)) |>
    tidyr::pivot_longer(
      cols = -c(item_nr, condition, string),
      names_to = "variable",
      values_to = "value"
    )

  # create base plot
  p <- ggplot2::ggplot(long, ggplot2::aes(x = condition, y = value, fill = condition)) +
    ggplot2::geom_violin(
      alpha = 0.2,
      trim = TRUE,
      color = NA,
      scale = "width"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(group = item_nr),
      alpha = 0.4,
      linewidth = 0.6,
      color = "gray40"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = condition),
      size = 1.5,
      alpha = 0.7
    ) +
    ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Condition",
      y = "Value",
      title = "Solution Words: Condition Connections"
    ) +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # add labels if requested
  if (show_labels) {
    # label top/bottom 2.5% extremes in each condition
    extremes <- long |>
      dplyr::group_by(variable, condition) |>
      dplyr::mutate(
        is_high_extreme = value >= quantile(value, 0.975, na.rm = TRUE),
        is_low_extreme = value <= quantile(value, 0.025, na.rm = TRUE)
      ) |>
      dplyr::filter(is_high_extreme | is_low_extreme) |>
      dplyr::ungroup() |>
      dplyr::distinct(item_nr, variable, condition, value, .keep_all = TRUE)

    # get ordered conditions (matches x-axis order)
    conditions_ordered <- sort(unique(extremes$condition))
    n_conditions <- length(conditions_ordered)

    # add labels for each condition
    for (i in seq_along(conditions_ordered)) {
      cond <- conditions_ordered[i]
      labels_cond <- extremes |> dplyr::filter(condition == cond)

      if (nrow(labels_cond) > 0) {
        # calculate nudge: left (-), right (+)
        if (n_conditions == 1) {
          nudge_val <- 0
        } else {
          nudge_val <- ((i - 1) / (n_conditions - 1) - 0.5) * 0.3
        }

        p <- p +
          ggrepel::geom_text_repel(
            data = labels_cond,
            ggplot2::aes(label = paste0("#", item_nr)),
            size = 3,
            alpha = 0.7,
            max.overlaps = 30,
            nudge_x = nudge_val,
            direction = "y",
            segment.alpha = 0.3,
            segment.size = 0.2
          )
      }
    }
  }

  # add means
  if (show_means) {
    means <- long |>
      dplyr::group_by(condition, variable) |>
      dplyr::summarise(mean = mean(value, na.rm = TRUE), .groups = "drop")

    p <- p +
      ggplot2::geom_point(
        data = means,
        ggplot2::aes(x = condition, y = mean),
        size = 3,
        shape = 23,
        fill = "black",
        color = "white",
        alpha = 0.5,
        stroke = 0.8
      )
  }

  p
}

#' Review and accept solution words
#'
#' @param solutions LexOPS output from select_solutions()
#' @param selection_params Original selection parameters (optional, auto-detected from attribute)
#' @param project_dir Optional project directory for saving
#' @export
review_solutions <- function(solutions,
                             selection_params = NULL,
                             project_dir = NULL)
{

  # handle different column names from LexOPS
  if ("Word" %in% names(solutions) && !"string" %in% names(solutions)) {
    solutions <- solutions |> dplyr::rename(string = Word)
  }

  # join back to GRAMS to get all solution-level variables not already present
  grams_cols <- setdiff(
    names(grams::GRAMS),
    c(names(solutions), "condition", "item_nr")
  )

  # only join if there are missing columns
  if (length(grams_cols) > 0) {
    solutions <- solutions |>
      dplyr::left_join(
        grams::GRAMS |> dplyr::select(Word, dplyr::all_of(grams_cols)),
        by = c("string" = "Word")
      )
  }

  # auto-detect params from attribute
  if (is.null(selection_params)) {
    selection_params <- attr(solutions, "selection_params")
  }

  # available numeric variables for plotting
  plot_vars <- get_plottable_vars(solutions, type = "solution")

  # track state
  unique_items <- unique(solutions$item_nr)

  # create summary table
  summary_table <- solutions |>
    dplyr::group_by(item_nr) |>
    dplyr::summarise(
      Words = paste(string, collapse = " | "),
      Conditions = paste(condition, collapse = " | "),
      .groups = "drop"
    )

  rv <- reactiveValues(
    data = solutions,
    summary_table = summary_table,
    selected_items = unique_items,
    history = list(),
    can_regenerate = !is.null(selection_params),
    params = selection_params,
    proxy_ready = FALSE
  )

  ui <- miniPage(
    gadgetTitleBar("Review Solution Words",
                   right = miniTitleBarButton("done", "Accept", primary = TRUE)),
    miniContentPanel(
      # actions bar
      fillRow(
        height = 70,
        fillCol(
          verbatimTextOutput("summary"),
          helpText("Click rows to deselect items")
        ),
        actionButton("select_all", "Select All", class = "btn-success"),
        actionButton("deselect_all", "Deselect All", class = "btn-secondary"),
        actionButton("export_plot", "Export Plot", class = "btn-info"),
        actionButton("regenerate", "Regenerate Unselected", class = "btn-warning"),
        actionButton("revert", "Undo", class = "btn-secondary"),
        fillCol(
          checkboxInput("show_labels", "Show labels", value = TRUE),
          selectizeInput("plot_vars", "Variables:",
                         choices = plot_vars,
                         selected = head(plot_vars, 3),
                         multiple = TRUE,
                         options = list(plugins = list('remove_button')),
                         width = "300px")
        )
      ),
      # side by side: table (left, larger) and plot (right, smaller)
      fillRow(
        flex = c(1.2, 1),
        fillCol(
          DTOutput("solution_table", height = "100%")
        ),
        fillCol(
          plotOutput("solution_plot", height = "100%")
        )
      )
    )
  )

  server <- function(input, output, session) {

    # render table
    output$solution_table <- renderDT({
      datatable(
        rv$summary_table,
        rownames = FALSE,
        selection = list(mode = 'multiple', selected = 1:nrow(rv$summary_table)),
        options = list(
          pageLength = -1,
          scrollX = FALSE,
          scrollY = "700px",  # add scrolling
          dom = 'ft',
          columnDefs = list(
            list(targets = 0, width = '60px'),
            list(targets = 1, width = '45%'),
            list(targets = 2, width = '25%')
          )
        )
      )
    })

    # create proxy after table renders
    observe({
      if (!rv$proxy_ready && !is.null(input$solution_table_rows_selected)) {
        rv$proxy_ready <- TRUE
      }
    })

    # track row selection
    observeEvent(input$solution_table_rows_selected, {
      if (!rv$proxy_ready) return()

      selected_rows <- input$solution_table_rows_selected

      if (length(selected_rows) > 0) {
        rv$selected_items <- rv$summary_table$item_nr[selected_rows]
      } else {
        rv$selected_items <- integer(0)
      }
    }, ignoreNULL = FALSE)

    # select/deselect all buttons
    observeEvent(input$select_all, {
      proxy <- dataTableProxy("solution_table")
      selectRows(proxy, 1:nrow(rv$summary_table))
    })

    observeEvent(input$deselect_all, {
      proxy <- dataTableProxy("solution_table")
      selectRows(proxy, NULL)
    })

    # render plot
    output$solution_plot <- renderPlot({
      plot_data <- rv$data |>
        dplyr::filter(item_nr %in% rv$selected_items)

      if (nrow(plot_data) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No items selected", cex = 1.5, col = "gray50")
      } else {
        plot_vars_selected <- input$plot_vars
        if (is.null(plot_vars_selected) || length(plot_vars_selected) == 0) {
          plot_vars_selected <- head(plot_vars, 3)
        }

        # only plot vars that exist and are numeric
        plot_vars_selected <- plot_vars_selected[plot_vars_selected %in% names(plot_data)]
        numeric_check <- sapply(plot_vars_selected, function(v) is.numeric(plot_data[[v]]))
        plot_vars_selected <- plot_vars_selected[numeric_check]

        if (length(plot_vars_selected) > 0) {
          plot_solutions(plot_data, vars = plot_vars_selected, show_labels = input$show_labels)
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No valid variables to plot", cex = 1.5, col = "gray50")
        }
      }
    })

    # summary
    output$summary <- renderText({
      n_selected <- length(rv$selected_items)
      n_total <- nrow(rv$summary_table)
      regen <- if (rv$can_regenerate) "✓" else "✗"

      paste0(
        "Selected: ", n_selected, " / ", n_total, "\n",
        "Regen: ", regen
      )
    })

    # export plot
    observeEvent(input$export_plot, {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

      if (!is.null(project_dir)) {
        filepath <- file.path(project_dir, "stage1_solutions",
                              paste0("plot_", timestamp, ".png"))
      } else {
        filepath <- paste0("solution_plot_", timestamp, ".png")
      }

      plot_data <- rv$data |> dplyr::filter(item_nr %in% rv$selected_items)

      plot_vars_selected <- input$plot_vars
      if (is.null(plot_vars_selected) || length(plot_vars_selected) == 0) {
        plot_vars_selected <- head(plot_vars, 3)
      }

      ggplot2::ggsave(filepath,
                      plot_solutions(plot_data, vars = plot_vars_selected,
                                     show_labels = input$show_labels),
                      width = 14, height = 8, dpi = 300)
      showNotification(paste("Saved:", basename(filepath)), type = "message")
    })

    # regenerate
    observeEvent(input$regenerate, {
      if (!rv$can_regenerate || is.null(rv$params)) {
        showNotification("Cannot regenerate", type = "error")
        return()
      }

      all_items <- rv$summary_table$item_nr
      rejected_items <- setdiff(all_items, rv$selected_items)

      if (length(rejected_items) == 0) {
        showNotification("No items deselected", type = "warning")
        return()
      }

      # save history
      rv$history[[length(rv$history) + 1]] <- list(
        data = rv$data,
        summary_table = rv$summary_table,
        selected_items = rv$selected_items
      )

      showNotification(
        paste("Regenerating", length(rejected_items), "items..."),
        id = "regen", duration = NULL
      )

      tryCatch({
        new_solutions <- do.call(select_solutions, c(
          rv$params,
          list(n = length(rejected_items), auto_review = FALSE)
        ))

        attr(new_solutions, "selection_params") <- NULL

        # re-number new items to match rejected item numbers
        new_item_numbers <- sort(rejected_items)
        n_conditions <- length(unique(rv$data$condition))
        new_solutions_renumbered <- new_solutions |>
          dplyr::mutate(
            item_nr = rep(new_item_numbers, each = n_conditions)
          )

        # replace rejected items with renumbered new ones
        rv$data <- rv$data |>
          dplyr::filter(item_nr %in% rv$selected_items) |>
          dplyr::bind_rows(new_solutions_renumbered) |>
          dplyr::arrange(item_nr)

        # rebuild summary table
        rv$summary_table <- rv$data |>
          dplyr::group_by(item_nr) |>
          dplyr::summarise(
            Words = paste(string, collapse = " | "),
            Conditions = paste(condition, collapse = " | "),
            .groups = "drop"
          )

        # keep same items selected (including new ones)
        rv$selected_items <- rv$summary_table$item_nr
        rv$proxy_ready <- FALSE

        removeNotification("regen")
        showNotification("Complete!", type = "message")

      }, error = function(e) {
        removeNotification("regen")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })

    # undo
    observeEvent(input$revert, {
      if (length(rv$history) == 0) {
        showNotification("Nothing to undo", type = "warning")
        return()
      }

      last <- rv$history[[length(rv$history)]]
      rv$data <- last$data
      rv$summary_table <- last$summary_table
      rv$selected_items <- last$selected_items
      rv$history[[length(rv$history)]] <- NULL
      rv$proxy_ready <- FALSE

      showNotification("Reverted", type = "message")
    })

    # accept
    observeEvent(input$done, {
      final <- rv$data |>
        dplyr::filter(item_nr %in% rv$selected_items)

      attr(final, "selection_params") <- rv$params

      if (!is.null(project_dir)) {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        saveRDS(final, file.path(project_dir, "stage1_solutions",
                                 paste0("accepted_", timestamp, ".rds")))
      }

      stopApp(final)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("Review Solutions",
                                              width = 2400, height = 1200))
}

#' Select anagrams for each solution within constraints
#'
#' @param solutions Filtered solutions from configure_anagram_constraints()
#' @param config Config object with bins, filters, and control tolerances
#' @param database GRAMS_database
#' @param n_per_cell Number of anagrams to sample per condition
#' @export
select_anagrams <- function(solutions,
                            config,
                            database = NULL,
                            n_per_cell = 5)
{

  if (is.null(database)) {
    stop("database is required. Pass GRAMS_database explicitly.")
  }

  message("Generating anagrams for ", length(unique(solutions$item_nr)), " items...")

  # get unique solutions
  unique_solutions <- unique(solutions$string)

  # generate anagrams for each solution
  anagrams_list <- lapply(unique_solutions, function(sol) {

    sol_anagrams <- list()

    # for each anagram condition (low/high bins)
    if (!is.null(config$anagram_bins)) {
      for (var in names(config$anagram_bins)) {
        bins <- config$anagram_bins[[var]]

        # get anagrams for each bin
        bin_anagrams <- list()

        for (bin_name in names(bins)) {
          bin_range <- bins[[bin_name]]

          # query database for this solution + bin
          query <- database |>
            dplyr::filter(Word == tolower(sol))

          # apply bin constraint
          query <- query |>
            dplyr::filter(
              .data[[var]] >= bin_range[1],
              .data[[var]] <= bin_range[2]
            )

          # apply filters
          if (!is.null(config$filter_ranges)) {
            for (fvar in names(config$filter_ranges)) {
              frange <- config$filter_ranges[[fvar]]
              query <- query |>
                dplyr::filter(
                  .data[[fvar]] >= frange[1],
                  .data[[fvar]] <= frange[2]
                )
            }
          }

          bin_anagrams[[bin_name]] <- query
        }

        # if control tolerances specified, select matched pairs
        if (!is.null(config$anagram_control_tolerances) &&
            length(bin_anagrams) == 2) {

          low_anagrams <- bin_anagrams[["low"]]
          high_anagrams <- bin_anagrams[["high"]]
          # update: options other than low/high

          if (nrow(low_anagrams) > 0 && nrow(high_anagrams) > 0) {

            # find matched pairs
            matched_pairs <- list()

            for (j in 1:nrow(low_anagrams)) {
              low_row <- low_anagrams[j, ]

              # find high anagrams that match within tolerance
              match_mask <- rep(TRUE, nrow(high_anagrams))

              for (ctrl_var in names(config$anagram_control_tolerances)) {
                tol <- config$anagram_control_tolerances[[ctrl_var]]
                low_val <- low_row[[ctrl_var]]
                high_vals <- high_anagrams[[ctrl_var]]

                match_mask <- match_mask & (abs(high_vals - low_val) <= tol)
              }

              # if matches found, store the pair info
              if (any(match_mask, na.rm = TRUE)) {
                matching_high <- high_anagrams[match_mask, ]

                # calculate match quality (sum of absolute differences)
                match_quality <- sapply(1:nrow(matching_high), function(k) {
                  sum(sapply(names(config$anagram_control_tolerances), function(ctrl_var) {
                    abs(low_row[[ctrl_var]] - matching_high[k, ctrl_var])
                  }), na.rm = TRUE)
                })

                # store all potential matches for this low anagram
                for (k in 1:nrow(matching_high)) {
                  matched_pairs[[length(matched_pairs) + 1]] <- list(
                    low = low_row,
                    high = matching_high[k, ],
                    quality = match_quality[k]
                  )
                }
              }
            }

            # select best n_per_cell pairs
            if (length(matched_pairs) > 0) {
              # sort by quality (lower = better match)
              qualities <- sapply(matched_pairs, function(p) p$quality)
              sorted_indices <- order(qualities)

              # select top n_per_cell (or fewer if not enough pairs)
              n_select <- min(n_per_cell, length(matched_pairs))
              selected_indices <- sorted_indices[1:n_select]

              # extract low anagrams
              low_selected <- do.call(rbind, lapply(matched_pairs[selected_indices], function(p) {
                as.data.frame(p$low)
              }))
              low_selected$Solution <- sol
              low_selected$anagram_condition <- "LOW"

              # extract high anagrams
              high_selected <- do.call(rbind, lapply(matched_pairs[selected_indices], function(p) {
                as.data.frame(p$high)
              }))
              high_selected$Solution <- sol
              high_selected$anagram_condition <- "HIGH"

              sol_anagrams[[length(sol_anagrams) + 1]] <- rbind(low_selected, high_selected)
            }

          }

        } else {

          for (bin_name in names(bin_anagrams)) {
            query <- bin_anagrams[[bin_name]]

            if (nrow(query) > 0) {
              sampled <- query |>
                dplyr::slice_sample(n = min(n_per_cell, nrow(query)))

              sampled$Solution <- sol
              sampled$anagram_condition <- toupper(bin_name)

              sol_anagrams[[length(sol_anagrams) + 1]] <- sampled
            }
          }
        }
      }
    }

    if (length(sol_anagrams) > 0) {
      dplyr::bind_rows(sol_anagrams)
    } else {
      NULL
    }

  }) |>
    purrr::compact() |>
    dplyr::bind_rows()

  # join back to get item_nr and condition from solutions
  solution_info <- solutions |>
    dplyr::select(string, condition, item_nr) |>
    dplyr::distinct()

  anagrams <- anagrams_list |>
    dplyr::left_join(solution_info, by = c("Solution" = "string"))

  # store params
  attr(anagrams, "selection_params") <- list(
    solutions = solutions,
    config = config,
    database = substitute(database)
  )

  message("Generated ", nrow(anagrams), " anagrams")

  anagrams
}

#' Configure anagram constraints and validate coverage
#'
#' @param solutions Solutions from Stage 1
#' @param split_by Anagram-level splits (e.g., list(aSBF = c("low", "high")))
#' @param filter Anagram-level filters (e.g., list(Moves = c(2, 4)))
#' @param control_for Variables to match between conditions (e.g., list(aArticulability = 0.5))
#' @param database GRAMS_database
#' @export
configure_anagram_constraints <- function(solutions,
                                          split_by = NULL,
                                          filter = NULL,
                                          control_for = NULL,
                                          database = NULL)
{

  # load database
  if (is.null(database)) {
    database <- load_grams_database()
  }

  # helper to get variable ranges
  get_range <- function(var, round_to = 3) {
    vals <- database[[var]]
    range_vals <- c(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE))
    round(range_vals, round_to)
  }

  get_step <- function(var) {
    var_range <- range(database[[var]], na.rm = TRUE)
    span <- var_range[2] - var_range[1]

    if (span < 1) return(0.001)
    if (span < 10) return(0.1)
    if (span < 100) return(1)
    return(10)
  }

  # initialize slider values
  initial_values <- list()

  # anagram split bins (use quantiles for initial suggestion)
  if (!is.null(split_by)) {
    for (var in names(split_by)) {
      vals <- split_by[[var]]
      if (all(vals %in% c("low", "high"))) {
        var_range <- get_range(var, 4)
        q <- quantile(database[[var]], c(0.33, 0.67), na.rm = TRUE)

        initial_values[[paste0(var, "_low")]] <- c(var_range[1], round(q[1], 4))
        initial_values[[paste0(var, "_high")]] <- c(round(q[2], 4), var_range[2])
      }
    }
  }

  # anagram filters (use provided values or defaults)
  if (!is.null(filter)) {
    for (var in names(filter)) {
      initial_values[[paste0(var, "_filter")]] <- filter[[var]]
    }
  }

  # anagram controls (use provided values or defaults)
  if (!is.null(control_for)) {
    for (var in names(control_for)) {
      initial_values[[paste0(var, "_control")]] <- control_for[[var]]
    }
  }

  # build UI - side by side layout
  ui <- miniPage(
    gadgetTitleBar("Configure Anagram Constraints",
                   right = miniTitleBarButton("done", "Accept", primary = TRUE)),
    miniContentPanel(
      fillRow(
        flex = c(1, 1),

        # left: sliders
        fillCol(
          div(
            style = "overflow-y: auto; padding: 20px;",
            h3("Anagram Constraints"),

            # anagram bins
            if (!is.null(split_by)) {
              tagList(
                h4("Anagram Split Bins"),
                lapply(names(split_by), function(var) {
                  vals <- split_by[[var]]
                  if (all(vals %in% c("low", "high"))) {
                    var_range <- get_range(var, 4)
                    step_size <- get_step(var)
                    tagList(
                      sliderInput(
                        paste0(var, "_low"),
                        paste0(var, " LOW bin:"),
                        min = var_range[1],
                        max = var_range[2],
                        value = initial_values[[paste0(var, "_low")]],
                        step = step_size,
                        width = "90%"
                      ),
                      sliderInput(
                        paste0(var, "_high"),
                        paste0(var, " HIGH bin:"),
                        min = var_range[1],
                        max = var_range[2],
                        value = initial_values[[paste0(var, "_high")]],
                        step = step_size,
                        width = "90%"
                      )
                    )
                  }
                })
              )
            },

            # anagram filters
            if (!is.null(filter)) {
              tagList(
                h4("Anagram Filters"),
                lapply(names(filter), function(var) {
                  var_range <- get_range(var, 2)
                  step_size <- get_step(var)
                  sliderInput(
                    paste0(var, "_filter"),
                    paste0(var, " range:"),
                    min = var_range[1],
                    max = var_range[2],
                    value = initial_values[[paste0(var, "_filter")]],
                    step = step_size,
                    width = "90%"
                  )
                })
              )
            },

            # anagram controls
            if (!is.null(control_for)) {
              tagList(
                h4("Match Between Conditions"),
                helpText("Tolerance for matching LOW and HIGH anagrams within each item"),
                lapply(names(control_for), function(var) {
                  # default tolerance range: 0 to 2x the initial value
                  default_val <- initial_values[[paste0(var, "_control")]]
                  max_tol <- max(default_val * 3, 1)  # At least 1
                  step_size <- if (max_tol < 1) 0.01 else if (max_tol < 10) 0.1 else 1

                  sliderInput(
                    paste0(var, "_control"),
                    paste0(var, " tolerance (±):"),
                    min = 0,
                    max = max_tol,
                    value = default_val,
                    step = step_size,
                    width = "90%"
                  )
                })
              )
            },

            actionButton("recheck", "Check Coverage",
                         class = "btn-warning", width = "90%"),
            br(),
            helpText("Adjust sliders and click Check Coverage to validate")
          )
        ),

        # right: coverage results
        fillCol(
          div(
            style = "padding: 20px;",
            h3("Coverage Check"),
            verbatimTextOutput("coverage_summary"),
            verbatimTextOutput("progress_text"),
            DTOutput("coverage_table")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    rv <- reactiveValues(
      coverage_done = FALSE,
      filtered_solutions = NULL,
      removed_items = NULL,
      config = NULL
    )

    # initial message
    output$coverage_summary <- renderText({
      "Adjust sliders above, then click 'Check Coverage' to validate."
    })

    # reactive config
    current_config <- reactive({
      config <- list(
        split_by = split_by,
        filter = filter,
        control_for = control_for
      )

      # add bin values
      if (!is.null(split_by)) {
        config$anagram_bins <- list()
        for (var in names(split_by)) {
          vals <- split_by[[var]]
          if (all(vals %in% c("low", "high"))) {
            config$anagram_bins[[var]] <- list(
              low = input[[paste0(var, "_low")]],
              high = input[[paste0(var, "_high")]]
            )
          }
        }
      }

      # add filter values
      if (!is.null(filter)) {
        config$filter_ranges <- list()
        for (var in names(filter)) {
          config$filter_ranges[[var]] <- input[[paste0(var, "_filter")]]
        }
      }

      # add control tolerances
      if (!is.null(control_for)) {
        config$anagram_control_tolerances <- list()
        for (var in names(control_for)) {
          config$anagram_control_tolerances[[var]] <- input[[paste0(var, "_control")]]
        }
      }

      config
    })

    # function to check coverage
    check_coverage <- function() {
      config <- current_config()

      # get unique solutions
      unique_solutions <- unique(solutions$string)
      n_solutions <- length(unique_solutions)

      # create a progress object
      progress <- shiny::Progress$new()
      on.exit(progress$close())

      progress$set(message = "Checking coverage", value = 0)

      # count available anagrams per solution per condition
      coverage_results <- lapply(seq_along(unique_solutions), function(i) {
        sol <- unique_solutions[i]

        # update progress every 50 solutions
        if (i %% 50 == 0) {
          progress$set(value = i / n_solutions,
                       detail = paste(i, "/", n_solutions, "solutions"))
        }

        tryCatch({

          counts <- list()
          matched_pairs <- 0  # count matched pairs

          # for each anagram condition (low/high bins)
          if (!is.null(config$anagram_bins)) {
            for (var in names(config$anagram_bins)) {
              bins <- config$anagram_bins[[var]]

              # get anagrams for each bin
              bin_anagrams <- list()

              for (bin_name in names(bins)) {
                bin_range <- bins[[bin_name]]

                # start with solution match
                query <- database |>
                  dplyr::filter(Word == tolower(sol))

                # apply bin constraint
                query <- query |>
                  dplyr::filter(
                    .data[[var]] >= bin_range[1],
                    .data[[var]] <= bin_range[2]
                  )

                # apply filters
                if (!is.null(config$filter_ranges)) {
                  for (fvar in names(config$filter_ranges)) {
                    frange <- config$filter_ranges[[fvar]]
                    query <- query |>
                      dplyr::filter(
                        .data[[fvar]] >= frange[1],
                        .data[[fvar]] <= frange[2]
                      )
                  }
                }

                counts[[bin_name]] <- nrow(query)
                bin_anagrams[[bin_name]] <- query
              }

              # if control variables specified, count matched pairs
              if (!is.null(config$anagram_control_tolerances) &&
                  length(bin_anagrams) == 2) {

                low_anagrams <- bin_anagrams[["low"]]
                high_anagrams <- bin_anagrams[["high"]]

                if (nrow(low_anagrams) > 0 && nrow(high_anagrams) > 0) {
                  # for each low anagram, count matching high anagrams
                  for (j in 1:nrow(low_anagrams)) {
                    low_row <- low_anagrams[j, ]

                    # check if any high anagrams match within tolerance
                    matches <- TRUE
                    for (ctrl_var in names(config$anagram_control_tolerances)) {
                      tol <- config$anagram_control_tolerances[[ctrl_var]]
                      low_val <- low_row[[ctrl_var]]

                      high_vals <- high_anagrams[[ctrl_var]]
                      in_range <- abs(high_vals - low_val) <= tol

                      if (!any(in_range, na.rm = TRUE)) {
                        matches <- FALSE
                        break
                      }
                    }

                    if (matches) {
                      matched_pairs <- matched_pairs + 1
                    }
                  }
                }
              }
            }
          }

          # create result row
          result <- data.frame(Solution = sol, stringsAsFactors = FALSE)
          for (bin_name in names(counts)) {
            result[[paste0(toupper(bin_name), "_available")]] <- counts[[bin_name]]
          }

          # add matched pairs column if controls specified
          if (!is.null(config$anagram_control_tolerances)) {
            result$Matched_pairs <- matched_pairs
            # status: need at least 1 matched pair (or more depending on n_per_cell)
            result$Status <- ifelse(matched_pairs > 0 && all(unlist(counts) > 0), "✓", "✗")
          } else {
            # status: all bins must have > 0
            result$Status <- ifelse(length(counts) > 0 && all(unlist(counts) > 0), "✓", "✗")
          }

          result

        }, error = function(e) {
          message("ERROR on solution ", i, " (", sol, "): ", e$message)
          return(NULL)
        })

      }) |>
        purrr::compact() |>
        dplyr::bind_rows()

      progress$set(value = 1, message = "Complete!")

      # join back to solutions to get item_nr
      coverage_with_items <- solutions |>
        dplyr::select(item_nr, string) |>
        dplyr::distinct() |>
        dplyr::left_join(coverage_results, by = c("string" = "Solution"))

      # group by item and check if ALL solutions in item pass
      item_status <- coverage_with_items |>
        dplyr::group_by(item_nr) |>
        dplyr::summarise(
          all_pass = all(Status == "✓"),
          n_solutions = dplyr::n(),
          .groups = "drop"
        )

      # filter solutions to keep only complete items
      passing_items <- item_status |>
        dplyr::filter(all_pass) |>
        dplyr::pull(item_nr)

      failing_items <- item_status |>
        dplyr::filter(!all_pass) |>
        dplyr::pull(item_nr)

      rv$filtered_solutions <- solutions |>
        dplyr::filter(item_nr %in% passing_items)

      rv$removed_items <- solutions |>
        dplyr::filter(item_nr %in% failing_items)

      rv$config <- config
      rv$coverage_done <- TRUE

      # summary
      n_items_kept <- length(passing_items)
      n_items_total <- length(unique(solutions$item_nr))
      n_items_removed <- n_items_total - n_items_kept

      output$coverage_summary <- renderText({
        paste0(
          "Items: ", n_items_kept, " / ", n_items_total, " kept (",
          round(100 * n_items_kept / n_items_total, 1), "%)\n",
          n_items_removed, " items removed (failed anagram coverage)\n\n",
          "Solutions: ", nrow(rv$filtered_solutions), " / ", nrow(solutions), " kept"
        )
      })

      # show detailed coverage table
      output$coverage_table <- renderDT({
        datatable(
          coverage_with_items |>
            dplyr::arrange(item_nr, string),
          rownames = FALSE,
          options = list(
            pageLength = 20,
            dom = 'ftp'
          )
        )
      })
    }

    # re-check button
    observeEvent(input$recheck, {
      check_coverage()
    })

    # accept button
    observeEvent(input$done, {
      if (!rv$coverage_done) {
        showNotification("Run coverage check first", type = "warning")
        return()
      }

      result <- list(
        solutions = rv$filtered_solutions,
        removed = rv$removed_items,
        config = rv$config
      )

      stopApp(result)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("Configure Anagram Constraints",
                                              width = 2400, height = 1200))
}

#' Filter anagrams based on balance metrics
#'
#' @param anagrams Anagram pool from select_anagrams()
#' @export
filter_anagrams <- function(anagrams)
{

  # get parameters
  params <- attr(anagrams, "selection_params")

  # get unique anagram conditions
  anagram_conditions <- sort(unique(anagrams$anagram_condition))

  # build summary at solution level
  summary_table <- anagrams |>
    dplyr::select(item_nr, condition, Solution, anagram_condition) |>
    dplyr::distinct() |>
    dplyr::group_by(item_nr, condition, Solution) |>
    dplyr::summarise(
      conditions_available = list(anagram_condition),
      .groups = "drop"
    ) |>
    dplyr::arrange(item_nr, condition)

  # get unique items
  unique_items <- sort(unique(summary_table$item_nr))

  # initial selections - pick first anagram for each condition
  initial_selections <- list()
  for (i in 1:nrow(summary_table)) {
    sol <- summary_table$Solution[i]
    available_conditions <- summary_table$conditions_available[[i]]

    for (cond in available_conditions) {
      sol_anagrams <- anagrams |>
        dplyr::filter(Solution == sol, anagram_condition == cond)
      if (nrow(sol_anagrams) > 0) {
        key <- paste(sol, cond, sep = "_")
        initial_selections[[key]] <- sol_anagrams$Anagram[1]
      }
    }
  }

  # available numeric variables for balance metrics
  anagram_vars <- get_plottable_vars(anagrams, type = "anagram")
  solution_vars <- get_plottable_vars(anagrams, type = "solution")

  # track state
  rv <- reactiveValues(
    data = anagrams,
    summary = summary_table,
    selected_items = unique_items,
    selected_anagrams = initial_selections,
    modal_solution = NULL,
    modal_condition = NULL,
    balance_data = NULL
  )

  ui <- miniPage(
    gadgetTitleBar("Filter Anagrams",
                   right = miniTitleBarButton("done", "Accept Selection", primary = TRUE)),
    miniContentPanel(
      # top controls
      fillRow(
        height = 180,
        # left column
        fillCol(
          flex = c(1, 1, 1),
          # row 1: balance metrics
          div(style = "padding: 5px;",
              strong("Balance metrics to show:"),
              selectizeInput("balance_vars", NULL,
                             choices = list(
                               "Anagram properties" = anagram_vars,
                               "Solution properties" = solution_vars
                             ),
                             selected = if (!is.null(params$config$split_by)) {
                               names(params$config$split_by)
                             } else {
                               c("aSBF", "Moves")
                             },
                             multiple = TRUE,
                             options = list(plugins = list('remove_button')),
                             width = "100%")
          ),
          # row 2: sorting
          fillRow(
            selectInput("sort_by", "Sort by:",
                        choices = c("Item number" = "item_nr"),
                        width = "200px"),
            selectInput("sort_order", "Order:",
                        choices = c("Best first" = "desc", "Worst first" = "asc"),
                        width = "140px"),
            numericInput("keep_n", "Keep top:", value = 80, min = 1, width = "110px"),
            actionButton("apply_sort", "Apply Sort & Select", class = "btn-primary")
          ),
          # row 3: selection
          fillRow(
            actionButton("select_all", "Select All", class = "btn-success"),
            actionButton("deselect_all", "Deselect All", class = "btn-secondary"),
            actionButton("invert_selection", "Invert Selection", class = "btn-secondary")
          )
        ),
        # right column - summary
        fillCol(
          div(style = "padding: 20px;",
              h4("Selection Summary"),
              verbatimTextOutput("summary_text"),
              helpText("Click anagrams in the table to change them. Balance metrics update automatically.")
          )
        )
      ),
      # main table
      fillCol(
        DTOutput("filter_table", height = "100%")
      )
    )
  )

  server <- function(input, output, session) {

    # compute balance metrics
    compute_balance <- reactive({
      rv$selected_anagrams

      req(input$balance_vars)
      balance_vars <- input$balance_vars

      results <- lapply(unique_items, function(item) {
        item_solutions <- rv$summary |>
          dplyr::filter(item_nr == item) |>
          dplyr::pull(Solution)

        row_data <- list(
          item_nr = item,
          Solutions = paste(item_solutions, collapse = " | ")
        )

        # add anagram columns
        for (cond in anagram_conditions) {
          anagrams_for_cond <- sapply(item_solutions, function(sol) {
            key <- paste(sol, cond, sep = "_")
            rv$selected_anagrams[[key]] %||% NA_character_
          })
          row_data[[paste0(cond, "_anagrams")]] <- paste(anagrams_for_cond, collapse = " | ")
        }

        # get anagram data for metrics
        selected_keys <- paste(rep(item_solutions, each = length(anagram_conditions)),
                               rep(anagram_conditions, length(item_solutions)),
                               sep = "_")
        selected_anagram_strings <- unlist(rv$selected_anagrams[selected_keys])

        item_anagrams <- rv$data |>
          dplyr::filter(
            Solution %in% item_solutions,
            Anagram %in% selected_anagram_strings
          )

        # compute balance metrics
        for (var in balance_vars) {
          if (!var %in% names(item_anagrams) || nrow(item_anagrams) == 0) next

          # check if solution-level variable
          is_solution_var <- !grepl("^a[A-Z]", var)

          if (is_solution_var) {
            # for solution variables: check variance across solutions
            solution_values <- item_anagrams |>
              dplyr::select(Solution, all_of(var)) |>
              dplyr::distinct() |>
              dplyr::pull(var)

            if (length(solution_values) > 1) {
              row_data[[paste0(var, "_range")]] <- round(max(solution_values) - min(solution_values), 3)
            }

          } else {
            # for anagram variables: compare low vs high
            cond_means <- item_anagrams |>
              dplyr::group_by(anagram_condition) |>
              dplyr::summarise(mean_val = mean(.data[[var]], na.rm = TRUE), .groups = "drop")

            if (nrow(cond_means) >= 2) {
              is_manipulated <- !is.null(params$config$split_by) &&
                var %in% names(params$config$split_by)

              if (is_manipulated) {
                high_val <- cond_means$mean_val[cond_means$anagram_condition == "HIGH"]
                low_val <- cond_means$mean_val[cond_means$anagram_condition == "LOW"]
                if (length(high_val) > 0 && length(low_val) > 0) {
                  row_data[[paste0(var, "_sep")]] <- round(high_val - low_val, 3)
                }
              } else {
                row_data[[paste0(var, "_bal")]] <- round(abs(diff(cond_means$mean_val)), 3)
              }
            }
          }
        }

        # overall status
        metric_cols <- names(row_data)[grepl("_sep$|_bal$|_range$", names(row_data))]
        if (length(metric_cols) > 0) {
          metric_values <- unlist(row_data[metric_cols])
          sep_bad <- any(metric_values[grepl("_sep$", metric_cols)] < 0.05, na.rm = TRUE)
          bal_bad <- any(metric_values[grepl("_bal$", metric_cols)] > 0.5, na.rm = TRUE)
          range_bad <- any(metric_values[grepl("_range$", metric_cols)] > 1.0, na.rm = TRUE)
          row_data$Status <- if (sep_bad || bal_bad || range_bad) "⚠" else "✓"
        } else {
          row_data$Status <- "—"
        }

        as.data.frame(row_data, stringsAsFactors = FALSE)
      })

      dplyr::bind_rows(results)
    })

    # update sort choices
    observe({
      balance_data <- compute_balance()
      rv$balance_data <- balance_data

      if (!is.null(balance_data)) {
        metric_cols <- names(balance_data)[grepl("_sep$|_bal$|_range$", names(balance_data))]
        all_choices <- c("Item number" = "item_nr", setNames(metric_cols, metric_cols))
        updateSelectInput(session, "sort_by", choices = all_choices)
      }
    })

    # build table
    output$filter_table <- renderDT({
      balance_data <- rv$balance_data

      if (is.null(balance_data) || nrow(balance_data) == 0) {
        return(datatable(data.frame(Message = "Loading...")))
      }

      # make anagram columns clickable
      display_data <- balance_data
      for (cond in anagram_conditions) {
        col_name <- paste0(cond, "_anagrams")
        if (col_name %in% names(display_data)) {
          display_data[[col_name]] <- sapply(1:nrow(display_data), function(i) {
            sols <- strsplit(display_data$Solutions[i], " \\| ")[[1]]
            anagrams <- strsplit(balance_data[[col_name]][i], " \\| ")[[1]]

            links <- sapply(seq_along(sols), function(j) {
              if (!is.na(anagrams[j])) {
                sprintf('<a href="#" class="anagram-link" data-solution="%s" data-condition="%s">%s</a>',
                        sols[j], cond, anagrams[j])
              } else {
                "<span style='color:gray;'>[none]</span>"
              }
            })
            paste(links, collapse = " | ")
          })
        }
      }

      # create datatable
      dt <- datatable(
        display_data,
        rownames = FALSE,
        escape = FALSE,
        selection = 'none',  # disable automatic selection
        options = list(
          pageLength = -1,  # show all rows
          scrollX = TRUE,
          scrollY = "550px",
          dom = 'ft',  # f = filter/search, t = table
          ordering = TRUE
        ),
        callback = JS("
          var selectedRows = [];

          table.on('click', '.anagram-link', function(e) {
            e.preventDefault();
            e.stopPropagation();
            var solution = $(this).data('solution');
            var condition = $(this).data('condition');
            Shiny.setInputValue('anagram_clicked', {
              solution: solution,
              condition: condition,
              random: Math.random()
            });
          });

          // Manual row selection - only when NOT clicking anagram
          table.on('click', 'tbody tr', function(e) {
            if ($(e.target).hasClass('anagram-link')) {
              return;
            }

            var idx = table.row(this).index() + 1;  // R uses 1-based indexing

            if ($(this).hasClass('selected')) {
              $(this).removeClass('selected');
              // Send removal signal
              Shiny.setInputValue('row_deselected', {index: idx, random: Math.random()});
            } else {
              $(this).addClass('selected');
              // Send addition signal
              Shiny.setInputValue('row_selected', {index: idx, random: Math.random()});
            }
          });
        ")
      ) |>
        formatStyle(
          columns = names(display_data)[grepl("_sep$|_bal$|_range$", names(display_data))],
          backgroundColor = styleInterval(
            cuts = c(0.05, 0.1),
            values = c('#ffcccc', '#ffffcc', '#ccffcc')
          )
        )

      # highlight selected rows
      if (length(rv$selected_items) > 0) {
        dt <- dt |> formatStyle(
          'item_nr',
          target = 'row',
          backgroundColor = styleEqual(
            rv$selected_items,
            rep('#d9edf7', length(rv$selected_items))
          )
        )
      }

      dt
    })

    # handle anagram click
    observeEvent(input$anagram_clicked, {
      sol <- input$anagram_clicked$solution
      cond <- input$anagram_clicked$condition

      available <- rv$data |>
        dplyr::filter(Solution == sol, anagram_condition == cond)

      if (nrow(available) == 0) {
        showNotification("No anagrams available", type = "warning")
        return()
      }

      showModal(modalDialog(
        title = paste("Select anagram for", sol, "-", cond),
        size = "m",

        radioButtons(
          "modal_anagram_choice",
          "Choose anagram:",
          choices = setNames(available$Anagram,
                             paste0(available$Anagram,
                                    " (aSBF: ", round(available$aSBF, 3),
                                    ", Moves: ", available$Moves, ")")),
          selected = rv$selected_anagrams[[paste(sol, cond, sep = "_")]]
        ),

        footer = tagList(
          modalButton("Cancel"),
          actionButton("modal_accept", "Accept", class = "btn-primary")
        )
      ))

      rv$modal_solution <- sol
      rv$modal_condition <- cond
    })

    # accept modal
    observeEvent(input$modal_accept, {
      key <- paste(rv$modal_solution, rv$modal_condition, sep = "_")
      rv$selected_anagrams[[key]] <- input$modal_anagram_choice
      removeModal()
    })

    # apply sort and selection
    observeEvent(input$apply_sort, {
      req(rv$balance_data)

      sort_col <- input$sort_by
      balance_data <- rv$balance_data

      if (!sort_col %in% names(balance_data)) {
        showNotification("Sort column not available", type = "warning")
        return()
      }

      # sort the data
      if (input$sort_order == "desc") {
        sorted_data <- balance_data |> dplyr::arrange(dplyr::desc(.data[[sort_col]]))
      } else {
        sorted_data <- balance_data |> dplyr::arrange(.data[[sort_col]])
      }

      # update stored data
      rv$balance_data <- sorted_data

      # select top N
      n_to_keep <- min(input$keep_n, nrow(sorted_data))
      rv$selected_items <- sorted_data$item_nr[1:n_to_keep]

      showNotification(paste("Sorted by", sort_col, "and selected top", n_to_keep, "items"),
                       type = "message", duration = 3)
    })

    # track manual selection
    observeEvent(input$filter_table_rows_selected, {
      balance_data <- rv$balance_data
      if (!is.null(balance_data)) {
        selected_rows <- input$filter_table_rows_selected
        rv$selected_items <- if (length(selected_rows) > 0) {
          balance_data$item_nr[selected_rows]
        } else {
          integer(0)
        }
      }
    }, ignoreNULL = FALSE)

    # handle individual row selection
    observeEvent(input$row_selected, {
      balance_data <- rv$balance_data
      if (!is.null(balance_data) && input$row_selected$index <= nrow(balance_data)) {
        item_to_add <- balance_data$item_nr[input$row_selected$index]
        rv$selected_items <- unique(c(rv$selected_items, item_to_add))
      }
    })

    # handle individual row deselection
    observeEvent(input$row_deselected, {
      balance_data <- rv$balance_data
      if (!is.null(balance_data) && input$row_deselected$index <= nrow(balance_data)) {
        item_to_remove <- balance_data$item_nr[input$row_deselected$index]
        rv$selected_items <- setdiff(rv$selected_items, item_to_remove)
      }
    })

    observeEvent(input$select_all, {
      balance_data <- rv$balance_data
      if (!is.null(balance_data)) {
        rv$selected_items <- balance_data$item_nr
      }
    })

    observeEvent(input$deselect_all, {
      rv$selected_items <- integer(0)
    })

    observeEvent(input$invert_selection, {
      balance_data <- rv$balance_data
      if (!is.null(balance_data)) {
        all_items <- balance_data$item_nr
        rv$selected_items <- setdiff(all_items, rv$selected_items)
      }
    })

    # summary
    output$summary_text <- renderText({
      n_selected <- length(rv$selected_items)
      n_total <- length(unique_items)
      paste0("Selected: ", n_selected, " / ", n_total, " items")
    })

    # accept
    observeEvent(input$done, {
      selected_anagram_strings <- unlist(rv$selected_anagrams)

      final <- rv$data |>
        dplyr::filter(
          item_nr %in% rv$selected_items,
          Anagram %in% selected_anagram_strings
        ) |>
        # deduplicate
        dplyr::group_by(item_nr, Solution, anagram_condition) |>
        dplyr::slice(1) |>
        dplyr::ungroup()

      if (nrow(final) == 0) {
        showNotification("No items selected", type = "warning")
        return()
      }

      attr(final, "selection_params") <- params
      stopApp(final)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("Filter Anagrams",
                                              width = 2400, height = 1200))
}

#' Plot anagram properties
#'
#' @param anagrams Anagram data with anagram_condition
#' @param vars Variables to plot (default: aSBF, aOLD20, Moves)
#' @param show_labels Show item labels on lines (default: FALSE)
#' @export
plot_anagrams <- function(anagrams, vars = c("aSBF", "aOLD20", "Moves"),
                          show_labels = FALSE)
{

  # reshape to long
  long <- anagrams |>
    dplyr::select(Solution, Anagram, anagram_condition,
                  dplyr::any_of(c("condition", "item_nr")),
                  dplyr::all_of(vars)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to = "variable",
      values_to = "value"
    )

  # base plot
  p <- ggplot2::ggplot(long, ggplot2::aes(x = anagram_condition, y = value,
                                          fill = anagram_condition)) +
    ggplot2::geom_violin(
      alpha = 0.2,
      trim = TRUE,
      color = NA,
      scale = "width"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(group = interaction(item_nr, Solution)),
      alpha = 0.4,
      linewidth = 0.6,
      color = "gray40"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = anagram_condition),
      size = 1.5,
      alpha = 0.7
    ) +
    ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Anagram Condition",
      y = "Value",
      title = "Anagram Properties: Condition Connections"
    ) +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # add labels if requested
  if (show_labels) {
    # label top/bottom 2.5% extremes in each condition
    extremes <- long |>
      dplyr::group_by(variable, anagram_condition) |>
      dplyr::mutate(
        is_high_extreme = value >= quantile(value, 0.975, na.rm = TRUE),
        is_low_extreme = value <= quantile(value, 0.025, na.rm = TRUE)
      ) |>
      dplyr::filter(is_high_extreme | is_low_extreme) |>
      dplyr::ungroup() |>
      dplyr::distinct(item_nr, variable, Solution, anagram_condition, value, .keep_all = TRUE)

    # get ordered conditions (matches x-axis order)
    conditions_ordered <- sort(unique(extremes$anagram_condition))
    n_conditions <- length(conditions_ordered)

    # add labels for each condition with appropriate nudge direction
    for (i in seq_along(conditions_ordered)) {
      cond <- conditions_ordered[i]
      labels_cond <- extremes |> dplyr::filter(anagram_condition == cond)

      if (nrow(labels_cond) > 0) {
        # calculate nudge: left (-), right (+)
        # For 2 conditions: -0.15, +0.15
        # For 3 conditions: -0.15, 0, +0.15
        # For n conditions: evenly spaced from -0.15 to +0.15
        if (n_conditions == 1) {
          nudge_val <- 0
        } else {
          nudge_val <- ((i - 1) / (n_conditions - 1) - 0.5) * 0.3
        }

        p <- p +
          ggrepel::geom_text_repel(
            data = labels_cond,
            ggplot2::aes(label = paste0("#", item_nr)),
            size = 3,
            alpha = 0.7,
            max.overlaps = 30,
            nudge_x = nudge_val,
            direction = "y",
            segment.alpha = 0.3,
            segment.size = 0.2
          )
      }
    }
  }

  p
}

#' Review and finalize anagram selection with plots
#'
#' @param anagrams Filtered anagram pool from filter_anagrams()
#' @export
review_anagrams <- function(anagrams)
{

  # get parameters
  params <- attr(anagrams, "selection_params")

  # get unique items
  unique_items <- sort(unique(anagrams$item_nr))

  # available numeric variables for plotting
  anagram_vars <- get_plottable_vars(anagrams, type = "anagram")
  solution_vars <- get_plottable_vars(anagrams, type = "solution")

  # create summary table
  summary_table <- anagrams |>
    group_by(item_nr) |>
    summarise(
      Solutions = paste(unique(Solution), collapse = " | "),
      n_anagrams = n(),
      .groups = "drop"
    )

  # track state
  rv <- reactiveValues(
    data = anagrams,
    summary_table = summary_table,
    selected_items = unique_items,
    proxy_ready = FALSE
  )

  ui <- miniPage(
    gadgetTitleBar("Review Anagrams - Final Selection",
                   right = miniTitleBarButton("done", "Accept", primary = TRUE)),
    miniContentPanel(
      # actions bar
      fillRow(
        height = 70,
        fillCol(
          verbatimTextOutput("summary_text"),
          helpText("Click rows to deselect items")
        ),
        actionButton("select_all", "Select All", class = "btn-success"),
        actionButton("deselect_all", "Deselect All", class = "btn-secondary"),
        actionButton("invert_selection", "Invert Selection", class = "btn-secondary"),
        actionButton("export_plot", "Export Plot", class = "btn-info"),
        fillCol(
          checkboxInput("show_labels", "Show labels", value = TRUE),
          selectizeInput("plot_vars", "Variables to plot:",
                         choices = list(
                           "Anagram properties" = anagram_vars,
                           "Solution properties" = solution_vars
                         ),
                         selected = if (!is.null(params$config$split_by)) {
                           names(params$config$split_by)
                         } else {
                           c("aSBF", "Moves")
                         },
                         multiple = TRUE,
                         options = list(plugins = list('remove_button')),
                         width = "300px")
        )
      ),
      # side by side: table (left, larger) and plot (right, smaller)
      fillRow(
        flex = c(1.2, 1),
        fillCol(
          DTOutput("anagram_table", height = "100%")
        ),
        fillCol(
          plotOutput("anagram_plot", height = "100%")
        )
      )
    )
  )

  server <- function(input, output, session) {

    # render table
    output$anagram_table <- renderDT({
      datatable(
        rv$summary_table,
        rownames = FALSE,
        selection = list(mode = 'multiple', selected = 1:nrow(rv$summary_table)),
        options = list(
          pageLength = -1,
          scrollX = FALSE,
          scrollY = "700px",
          dom = 'ft',
          columnDefs = list(
            list(targets = 0, width = '60px'),
            list(targets = 1, width = '60%'),
            list(targets = 2, width = '20%')
          )
        )
      )
    })

    # create proxy after table renders
    observe({
      if (!rv$proxy_ready && !is.null(input$anagram_table_rows_selected)) {
        rv$proxy_ready <- TRUE
      }
    })

    # track row selection
    observeEvent(input$anagram_table_rows_selected, {
      if (!rv$proxy_ready) return()

      selected_rows <- input$anagram_table_rows_selected

      if (length(selected_rows) > 0) {
        rv$selected_items <- rv$summary_table$item_nr[selected_rows]
      } else {
        rv$selected_items <- integer(0)
      }
    }, ignoreNULL = FALSE)

    # select/deselect all buttons
    observeEvent(input$select_all, {
      proxy <- dataTableProxy("anagram_table")
      selectRows(proxy, 1:nrow(rv$summary_table))
    })

    observeEvent(input$deselect_all, {
      proxy <- dataTableProxy("anagram_table")
      selectRows(proxy, NULL)
    })

    observeEvent(input$invert_selection, {
      current <- input$anagram_table_rows_selected
      all_rows <- 1:nrow(rv$summary_table)
      new_selection <- setdiff(all_rows, current)

      proxy <- dataTableProxy("anagram_table")
      selectRows(proxy, new_selection)
    })

    # render plot
    output$anagram_plot <- renderPlot({
      plot_data <- rv$data |>
        filter(item_nr %in% rv$selected_items)

      if (nrow(plot_data) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No items selected", cex = 1.5, col = "gray50")
      } else {
        plot_vars <- input$plot_vars
        if (is.null(plot_vars) || length(plot_vars) == 0) {
          plot_vars <- c("aSBF", "Moves")
        }

        # Only plot vars that exist and are numeric
        plot_vars <- plot_vars[plot_vars %in% names(plot_data)]
        numeric_check <- sapply(plot_vars, function(v) is.numeric(plot_data[[v]]))
        plot_vars <- plot_vars[numeric_check]

        if (length(plot_vars) > 0) {
          plot_anagrams(plot_data, vars = plot_vars, show_labels = input$show_labels)
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No valid numeric variables", cex = 1.5, col = "gray50")
        }
      }
    })

    # summary
    output$summary_text <- renderText({
      n_selected <- length(rv$selected_items)
      n_total <- nrow(rv$summary_table)
      paste0("Selected: ", n_selected, " / ", n_total, " items")
    })

    # export plot
    observeEvent(input$export_plot, {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      filepath <- paste0("anagram_plot_", timestamp, ".png")

      plot_data <- rv$data |> filter(item_nr %in% rv$selected_items)

      plot_vars_selected <- input$plot_vars
      if (is.null(plot_vars_selected) || length(plot_vars_selected) == 0) {
        plot_vars_selected <- c("aSBF", "Moves")
      }

      # only plot vars that exist and are numeric
      plot_vars_selected <- plot_vars_selected[plot_vars_selected %in% names(plot_data)]
      numeric_check <- sapply(plot_vars_selected, function(v) is.numeric(plot_data[[v]]))
      plot_vars_selected <- plot_vars_selected[numeric_check]

      if (length(plot_vars_selected) > 0) {
        ggplot2::ggsave(filepath,
                        plot_anagrams(plot_data, vars = plot_vars_selected, show_labels = input$show_labels),
                        width = 14, height = 8, dpi = 300)
        showNotification(paste("Saved:", basename(filepath)), type = "message")
      } else {
        showNotification("No valid variables to export", type = "warning")
      }
    })

    # accept
    observeEvent(input$done, {
      final <- rv$data |>
        filter(item_nr %in% rv$selected_items)

      if (nrow(final) == 0) {
        showNotification("No items selected", type = "warning")
        return()
      }

      # reorder columns: solution, anagram, anagram metrics (no aggregates), aggregates, then everything else
      anagram_cols <- grep("^a[A-Z]", names(final), value = TRUE)

      # split anagram columns into base metrics and aggregates
      anagram_base <- anagram_cols[!grepl("mean|range|min|max|rank", anagram_cols)]
      anagram_agg <- anagram_cols[grepl("mean|range|min|max|rank", anagram_cols)]

      other_cols <- setdiff(names(final), c("Solution", "Anagram", anagram_cols))

      final <- final |>
        select(Solution, Anagram, all_of(anagram_base), all_of(anagram_agg), all_of(other_cols))

      attr(final, "selection_params") <- params
      stopApp(final)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("Review Anagrams",
                                              width = 2400, height = 1200))
}

#' Generate unsolvable anagram(s) matched to a target
#'
#' @param target_anagram Character string of the anagram to match
#' @param match_metrics Character vector of metrics to match (e.g., c("SBF", "Articulability"))
#' @param tolerance Named list of tolerances for each metric (e.g., list(SBF = 0.05, Articulability = 0.2))
#' @param n_swaps Number of letters to swap
#' @param swap_type Type of swaps: "vowels_only", "consonants_only", "either", "both"
#' @param swap_strategy Swap strategy: "phonetic", "random", "weighted"
#' @param max_position_overlap Maximum proportion of letters in same positions (NULL = no constraint)
#' @param n_candidates Number of unique candidates to return (default 1)
#' @param max_attempts Maximum number of attempts before giving up
#' @param sig_index Signature index for dictionary lookup used to verify
#'   candidates are not existing words. Defaults to \code{grams::sig_index}.
#'
#' @return Data frame with candidates and their properties, or NULL if none found
#' @export
find_matched_unsolvable <- function(target_anagram,
                                    match_metrics = c("SBF"),
                                    tolerance = list(SBF = 0.05),
                                    n_swaps = 1,
                                    swap_type = c("either",
                                                  "vowels_only",
                                                  "consonants_only",
                                                  "both"),
                                    swap_strategy = c("phonetic",
                                                      "random",
                                                      "weighted"),
                                    max_position_overlap = NULL,
                                    n_candidates = 1,
                                    max_attempts = 100,
                                    sig_index = grams::sig_index)
{
  swap_type <- match.arg(swap_type)
  swap_strategy <- match.arg(swap_strategy)

  # use default dictionary if not provided
  if (is.null(sig_index)) {
    sig_index <- grams::sig_index
  }

  # validate and adjust n_swaps
  n_letters <- nchar(target_anagram)
  if (n_swaps > n_letters) {
    message(paste("n_swaps (", n_swaps, ") exceeds string length (", n_letters, "). Setting n_swaps to ", n_letters, sep = ""))
    n_swaps <- n_letters
  }

  # compute target metrics
  target_metrics <- compute_string_indices(target_anagram)

  # store found candidates
  candidates_list <- list()
  candidates_found <- character(0)

  # check if candidate meets all criteria
  check_candidate <- function(candidate) {
    # must not have any dictionary anagrams (truly unsolvable)
    anagrams <- find_dictionary_anagrams(candidate, sig_index = sig_index)

    if (length(anagrams) > 0) {
      return(FALSE)
    }

    # check distinct letter count matches
    target_distinct <- length(unique(strsplit(tolower(target_anagram), "")[[1]]))
    candidate_distinct <- length(unique(strsplit(tolower(candidate), "")[[1]]))

    if (target_distinct != candidate_distinct) {
      return(FALSE)
    }

    # compute candidate metrics
    candidate_metrics <- compute_string_indices(candidate)

    # check metric tolerances
    for (metric in match_metrics) {
      if (!metric %in% names(target_metrics)) {
        warning(paste("Metric", metric, "not available"))
        return(FALSE)
      }

      target_val <- target_metrics[[metric]]
      candidate_val <- candidate_metrics[[metric]]
      tol <- tolerance[[metric]]

      if (is.na(candidate_val) || abs(candidate_val - target_val) > tol) {
        return(FALSE)
      }
    }

    # check position overlap if specified
    if (!is.null(max_position_overlap)) {
      target_letters <- strsplit(tolower(target_anagram), "")[[1]]
      candidate_letters <- strsplit(tolower(candidate), "")[[1]]

      if (length(target_letters) == length(candidate_letters)) {
        matches <- sum(target_letters == candidate_letters)
        overlap <- matches / length(target_letters)

        if (overlap > max_position_overlap) {
          return(FALSE)
        }
      }
    }

    TRUE
  }

  # define letter classes
  vowels <- c("a", "e", "i", "o", "u", "y")
  all_consonants <- letters[!letters %in% vowels]

  # phonetic substitution classes for consonants
  consonant_subs_phonetic <- list(
    # voiced/voiceless pairs (same place of articulation)
    p = c("b", "f"),
    b = c("p", "v"),
    t = c("d", "s"),
    d = c("t", "z"),
    k = c("g", "c"),
    g = c("k"),
    f = c("v", "p"),
    v = c("f", "b"),
    s = c("z", "c"),
    z = c("s"),
    # nasals
    m = c("n"),
    n = c("m"),
    # liquids
    l = c("r"),
    r = c("l"),
    # glides and others
    w = c("v"),
    h = c("k"),
    c = c("k", "s"),
    j = c("g"),
    q = c("k"),
    x = c("z")
  )

  letters_vec <- strsplit(tolower(target_anagram), "")[[1]]

  # identify vowels and consonants
  is_vowel <- letters_vec %in% vowels
  vowel_positions <- which(is_vowel)
  consonant_positions <- which(!is_vowel)

  for (attempt in 1:max_attempts) {
    # stop if we've found enough candidates
    if (length(candidates_list) >= n_candidates) break

    candidate_letters <- letters_vec

    # determine which positions to swap
    if (swap_type == "vowels_only") {
      if (length(vowel_positions) < n_swaps) next
      swap_positions <- sample(vowel_positions, min(n_swaps, length(vowel_positions)))
    } else if (swap_type == "consonants_only") {
      if (length(consonant_positions) < n_swaps) next
      swap_positions <- sample(consonant_positions, min(n_swaps, length(consonant_positions)))
    } else if (swap_type == "either") {
      swap_positions <- sample(1:n_letters, min(n_swaps, n_letters))
    } else if (swap_type == "both") {
      if (length(vowel_positions) < 1 || length(consonant_positions) < 1) next
      n_vowel_swaps <- max(1, floor(n_swaps / 2))
      n_consonant_swaps <- n_swaps - n_vowel_swaps
      swap_positions <- c(
        sample(vowel_positions, min(n_vowel_swaps, length(vowel_positions))),
        sample(consonant_positions, min(n_consonant_swaps, length(consonant_positions)))
      )
    }

    # apply swaps
    for (pos in swap_positions) {
      original <- candidate_letters[pos]

      if (original %in% vowels) {
        options <- setdiff(vowels, original)
      } else {
        if (swap_strategy == "phonetic") {
          if (original %in% names(consonant_subs_phonetic)) {
            options <- consonant_subs_phonetic[[original]]
          } else {
            options <- setdiff(all_consonants, original)
          }
        } else if (swap_strategy == "random") {
          options <- setdiff(all_consonants, original)
        } else if (swap_strategy == "weighted") {
          if (original %in% names(consonant_subs_phonetic)) {
            phonetic_options <- consonant_subs_phonetic[[original]]
            other_options <- setdiff(all_consonants, c(original, phonetic_options))
            options <- c(rep(phonetic_options, 3), other_options)
          } else {
            options <- setdiff(all_consonants, original)
          }
        }
      }

      if (length(options) > 0) {
        candidate_letters[pos] <- sample(options, 1)
      }
    }

    candidate <- paste(candidate_letters, collapse = "")

    # skip if we've already found this candidate
    if (candidate %in% candidates_found) next

    # check if valid
    if (check_candidate(candidate)) {
      target_letters <- strsplit(tolower(target_anagram), "")[[1]]
      candidate_letters_split <- strsplit(tolower(candidate), "")[[1]]
      changed_positions <- which(target_letters != candidate_letters_split)

      candidate_metrics <- compute_string_indices(candidate)

      # build row for this candidate
      row_data <- list(
        original = target_anagram,
        candidate = candidate,
        n_swaps = n_swaps,
        n_changed = length(changed_positions),
        positions_changed = paste(changed_positions, collapse = ","),
        changes = paste(paste0(target_letters[changed_positions], "→",
                               candidate_letters_split[changed_positions]),
                        collapse = ", ")
      )

      # add metric differences
      for (metric in match_metrics) {
        row_data[[paste0(metric, "_diff")]] <-
          candidate_metrics[[metric]] - target_metrics[[metric]]
      }

      candidates_list[[length(candidates_list) + 1]] <- row_data
      candidates_found <- c(candidates_found, candidate)
    }
  }

  # return results
  if (length(candidates_list) == 0) {
    return(NULL)
  }

  # convert to data frame
  result_df <- do.call(rbind, lapply(candidates_list, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))

  # add full metrics as attributes
  attr(result_df, "target_metrics") <- target_metrics
  attr(result_df, "generation_params") <- list(
    match_metrics = match_metrics,
    tolerance = tolerance,
    n_swaps = n_swaps,
    swap_type = swap_type,
    swap_strategy = swap_strategy,
    max_position_overlap = max_position_overlap
  )

  result_df
}

#' Generate unsolvable anagrams for all experimental anagrams
#'
#' @param anagrams Data frame of experimental anagrams from review_anagrams()
#' @param match_metrics Character vector of metrics to match
#' @param tolerance Named list of tolerances for each metric
#' @param n_candidates Number of unsolvable candidates per anagram
#' @param n_swaps Number of letters to swap
#' @param swap_type Type of swaps: "vowels_only", "consonants_only", "either", "both"
#' @param swap_strategy Swap strategy: "phonetic", "random", "weighted"
#' @param max_position_overlap Maximum proportion of letters in same positions
#' @param max_attempts Maximum attempts per anagram
#' @param sig_index Dictionary to check against (default: grams::sig_index)
#' @return Data frame with unsolvable candidates and their metrics
#' @export
generate_unsolvable_anagrams <- function(anagrams,
                                         match_metrics = c("SBF"),
                                         tolerance = list(SBF = 0.05),
                                         n_candidates = 5,
                                         n_swaps = 1,
                                         swap_type = c("either",
                                                       "vowels_only",
                                                       "consonants_only",
                                                       "both"),
                                         swap_strategy = c("phonetic",
                                                           "random",
                                                           "weighted"),
                                         max_position_overlap = NULL,
                                         max_attempts = 100,
                                         sig_index = grams::sig_index)
{

  swap_type <- match.arg(swap_type)
  swap_strategy <- match.arg(swap_strategy)

  # check required columns
  if (!"Anagram" %in% names(anagrams)) {
    stop("anagrams must have an 'Anagram' column")
  }

  if (!"item_nr" %in% names(anagrams)) {
    stop("anagrams must have an 'item_nr' column")
  }

  # get unique anagrams (one per item)
  unique_anagrams <- anagrams %>%
    dplyr::group_by(item_nr) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  message(paste("Generating", n_candidates, "unsolvable candidates for",
                nrow(unique_anagrams), "anagrams..."))

  # generate unsolvables for each anagram
  all_unsolvables <- list()

  for (i in 1:nrow(unique_anagrams)) {
    target_anagram <- unique_anagrams$Anagram[i]
    item_nr <- unique_anagrams$item_nr[i]

    unsolvables <- find_matched_unsolvable(
      target_anagram = target_anagram,
      match_metrics = match_metrics,
      tolerance = tolerance,
      n_swaps = n_swaps,
      swap_type = swap_type,
      swap_strategy = swap_strategy,
      max_position_overlap = max_position_overlap,
      n_candidates = n_candidates,
      max_attempts = max_attempts,
      sig_index = sig_index
    )

    if (!is.null(unsolvables)) {
      unsolvables$item_nr <- item_nr
      unsolvables$Anagram <- target_anagram
      unsolvables$original <- NULL

      # add solution info from original anagrams
      solution_info <- unique_anagrams %>%
        dplyr::filter(item_nr == !!item_nr) %>%
        dplyr::select(Solution, condition) %>%
        dplyr::slice(1)

      unsolvables$Solution <- solution_info$Solution
      unsolvables$condition <- solution_info$condition

      all_unsolvables[[i]] <- unsolvables
    } else {
      warning(paste("Failed to generate unsolvables for item", item_nr,
                    "anagram:", target_anagram))
    }

    if (i %% 10 == 0) {
      message(paste("  Processed", i, "of", nrow(unique_anagrams)))
    }
  }

  # combine all results
  if (length(all_unsolvables) == 0) {
    stop("Failed to generate any unsolvable anagrams")
  }

  result <- dplyr::bind_rows(all_unsolvables)

  # get anagram metrics from input (already computed in Stage 2)
  message("Extracting anagram metrics from input data...")
  anagram_metrics_lookup <- anagrams %>%
    select(item_nr, Anagram, starts_with("a")) %>%
    distinct()

  # join anagram metrics and rename columns
  for (metric in match_metrics) {
    anagram_col <- paste0("a", metric)

    if (anagram_col %in% names(anagram_metrics_lookup)) {
      temp <- anagram_metrics_lookup %>%
        select(item_nr, Anagram, !!anagram_col) %>%
        rename(!!paste0("Anagram_", metric) := !!anagram_col)

      result <- result %>%
        left_join(temp, by = c("item_nr", "Anagram"))
    }
  }

  # compute metrics for unsolvables (new strings, must compute)
  message("Computing metrics for unsolvable candidates...")

  for (metric in match_metrics) {
    if (metric == "SBF") {
      result$Unsolvable_SBF <- sapply(result$candidate, sbf)
    } else if (metric == "MLBF") {
      result$Unsolvable_MLBF <- sapply(result$candidate, mlbf)
    } else if (metric == "SLF") {
      result$Unsolvable_SLF <- sapply(result$candidate, slf)
    } else if (metric == "STF") {
      result$Unsolvable_STF <- sapply(result$candidate, stf)
    } else if (metric == "MLLF") {
      result$Unsolvable_MLLF <- sapply(result$candidate, mllf)
    } else if (metric == "MLTF") {
      result$Unsolvable_MLTF <- sapply(result$candidate, mltf)
    } else if (metric == "OLD20") {
      result$Unsolvable_OLD20 <- sapply(result$candidate, old20)
    } else if (metric == "ED1") {
      result$Unsolvable_ED1 <- sapply(result$candidate, ed1)
    } else if (metric == "Articulability") {
      result$Unsolvable_Articulability <- sapply(result$candidate, articulability)
    } else if (metric == "Nsyllables") {
      result$Unsolvable_Nsyllables <- sapply(result$candidate, function(s) {
        m <- compute_string_indices(s)
        m$Nsyllables
      })
    }
  }

  # sort candidates by matching quality (best first)
  metric_diff_cols <- grep("_diff$", names(result), value = TRUE)

  result <- result %>%
    mutate(
      match_score = rowSums(across(all_of(metric_diff_cols), ~abs(.x)), na.rm = TRUE)
    ) %>%
    arrange(item_nr, match_score) %>%
    select(-match_score)

  # reorder columns for consistency
  col_order <- c("item_nr", "Solution", "condition", "Anagram", "candidate",
                 "n_swaps", "n_changed", "positions_changed", "changes")
  other_cols <- setdiff(names(result), col_order)
  result <- result[, c(col_order[col_order %in% names(result)], other_cols)]

  # add attributes
  attr(result, "generation_params") <- list(
    match_metrics = match_metrics,
    tolerance = tolerance,
    n_candidates = n_candidates,
    n_swaps = n_swaps,
    swap_type = swap_type,
    swap_strategy = swap_strategy,
    max_position_overlap = max_position_overlap
  )

  attr(result, "anagram_source") <- anagrams

  message(paste("Generated", nrow(result), "total unsolvable candidates"))

  result
}

#' Filter and select unsolvable anagrams
#'
#' @param unsolvables Data frame from generate_unsolvable_anagrams()
#' @export
filter_unsolvable_anagrams <- function(unsolvables)
{

  # get parameters
  params <- attr(unsolvables, "generation_params")

  # get matched metrics from generation params
  matched_metrics <- params$match_metrics
  if (is.null(matched_metrics)) {
    matched_metrics <- "SBF"  # default fallback
  }

  # get the diff columns (e.g., SBF_diff, Articulability_diff)
  diff_cols <- paste0(matched_metrics, "_diff")
  diff_cols <- diff_cols[diff_cols %in% names(unsolvables)]

  # compute initial balance metrics
  compute_balance_metrics <- function(summary_df, unsolvables_df) {
    result <- summary_df

    # add the metrics from unsolvables
    for (i in 1:nrow(result)) {
      item <- result$item_nr[i]
      selected <- result$selected_unsolvable[i]
      anagram <- result$Anagram[i]

      # find the matching unsolvable
      match <- unsolvables_df %>%
        filter(item_nr == item,
               Anagram == anagram,
               candidate == selected) %>%
        slice(1)

      if (nrow(match) > 0) {
        # add all diff columns dynamically
        for (dcol in diff_cols) {
          result[[dcol]][i] <- abs(match[[dcol]])
        }
        result$n_changed[i] <- match$n_changed
      } else {
        # set to NA if not found
        for (dcol in diff_cols) {
          result[[dcol]][i] <- NA_real_
        }
        result$n_changed[i] <- NA_integer_
      }
    }

    result
  }

  # create summary table: one row per item
  summary_table <- unsolvables %>%
    group_by(item_nr) %>%
    summarise(
      Solution = first(Solution),
      Anagram = first(Anagram),
      n_candidates = n(),
      selected_unsolvable = first(candidate),
      n_changed = NA_integer_,
      .groups = "drop"
    )

  # initialize diff columns
  for (dcol in diff_cols) {
    summary_table[[dcol]] <- NA_real_
  }

  summary_table <- compute_balance_metrics(summary_table, unsolvables)

  # track state
  rv <- reactiveValues(
    data = unsolvables,
    summary_table = summary_table,
    selected_items = summary_table$item_nr,
    proxy_ready = FALSE,
    modal_item = NULL
  )

  # modal for selecting unsolvable
  show_unsolvable_modal <- function(item_number) {
    candidates <- rv$data %>%
      filter(item_nr == item_number)

    current_selection <- rv$summary_table %>%
      filter(item_nr == item_number) %>%
      pull(selected_unsolvable)

    rv$modal_item <- item_number

    # build modal table with dynamic diff columns
    modal_table <- candidates %>%
      select(candidate, n_changed, changes, all_of(diff_cols))

    # round diff columns
    for (dcol in diff_cols) {
      if (dcol %in% names(modal_table)) {
        modal_table[[dcol]] <- round(modal_table[[dcol]], 4)
      }
    }

    showModal(modalDialog(
      title = paste("Select Unsolvable for Item", item_number),
      size = "l",
      renderTable(modal_table),
      radioButtons(
        "modal_candidate_choice",
        "Select candidate:",
        choices = setNames(candidates$candidate, candidates$candidate),
        selected = current_selection
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("modal_confirm", "Confirm", class = "btn-primary")
      )
    ))
  }

  ui <- miniPage(
    gadgetTitleBar("Filter Unsolvable Anagrams",
                   right = miniTitleBarButton("done", "Accept Selection", primary = TRUE)),
    miniContentPanel(
      fillRow(
        height = 120,
        fillCol(
          h4("Selection Summary"),
          verbatimTextOutput("summary_text"),
          helpText("Click unsolvable in the table to change selection.")
        ),
        fillCol(
          fillRow(
            actionButton("select_all", "Select All", class = "btn-success"),
            actionButton("deselect_all", "Deselect All", class = "btn-secondary"),
            actionButton("invert_selection", "Invert Selection", class = "btn-secondary")
          )
        )
      ),
      fillCol(
        DTOutput("filter_table", height = "100%")
      )
    )
  )

  server <- function(input, output, session) {

    # render table with clickable unsolvables
    output$filter_table <- renderDT({

      display_table <- rv$summary_table %>%
        mutate(
          Unsolvable = sapply(1:n(), function(i) {
            paste0('<a href="#" onclick="Shiny.setInputValue(\'unsolvable_click\', ',
                   item_nr[i], ', {priority: \'event\'}); return false;">',
                   selected_unsolvable[i], '</a>')
          })
        )

      # round diff columns
      for (dcol in diff_cols) {
        if (dcol %in% names(display_table)) {
          display_table[[dcol]] <- round(display_table[[dcol]], 4)
        }
      }

      # select columns to display
      display_cols <- c("item_nr", "Solution", "Anagram", "Unsolvable",
                        "n_candidates", "n_changed", diff_cols)
      display_cols <- display_cols[display_cols %in% names(display_table)]

      display_table <- display_table %>% select(all_of(display_cols))

      datatable(
        display_table,
        rownames = FALSE,
        escape = FALSE,
        selection = list(mode = 'multiple', selected = 1:nrow(display_table)),
        options = list(
          pageLength = -1,
          scrollY = "600px",
          dom = 'ft',
          columnDefs = list(
            list(targets = 0, width = '60px'),
            list(targets = 1, width = '15%'),
            list(targets = 2, width = '15%'),
            list(targets = 3, width = '15%'),
            list(targets = 4, width = '80px'),
            list(targets = 5, width = '80px')
          )
        )
      )
    })

    # create proxy after table renders
    observe({
      if (!rv$proxy_ready && !is.null(input$filter_table_rows_selected)) {
        rv$proxy_ready <- TRUE
      }
    })

    # track row selection
    observeEvent(input$filter_table_rows_selected, {
      if (!rv$proxy_ready) return()

      selected_rows <- input$filter_table_rows_selected

      if (length(selected_rows) > 0) {
        rv$selected_items <- rv$summary_table$item_nr[selected_rows]
      } else {
        rv$selected_items <- integer(0)
      }
    }, ignoreNULL = FALSE)

    # handle unsolvable clicks - show modal
    observeEvent(input$unsolvable_click, {
      show_unsolvable_modal(input$unsolvable_click)
    })

    # handle modal confirmation
    observeEvent(input$modal_confirm, {
      if (!is.null(rv$modal_item) && !is.null(input$modal_candidate_choice)) {
        new_candidate <- input$modal_candidate_choice

        # update summary table
        rv$summary_table <- rv$summary_table %>%
          mutate(
            selected_unsolvable = ifelse(item_nr == rv$modal_item,
                                         new_candidate,
                                         selected_unsolvable)
          )

        # recompute balance metrics
        rv$summary_table <- compute_balance_metrics(rv$summary_table, rv$data)

        rv$modal_item <- NULL
        removeModal()
      }
    })

    # select/deselect buttons
    observeEvent(input$select_all, {
      proxy <- dataTableProxy("filter_table")
      selectRows(proxy, 1:nrow(rv$summary_table))
    })

    observeEvent(input$deselect_all, {
      proxy <- dataTableProxy("filter_table")
      selectRows(proxy, NULL)
    })

    observeEvent(input$invert_selection, {
      current <- input$filter_table_rows_selected
      all_rows <- 1:nrow(rv$summary_table)
      new_selection <- setdiff(all_rows, current)

      proxy <- dataTableProxy("filter_table")
      selectRows(proxy, new_selection)
    })

    # summary text
    output$summary_text <- renderText({
      n_selected <- length(rv$selected_items)
      n_total <- nrow(rv$summary_table)
      paste0("Selected: ", n_selected, " / ", n_total, " items")
    })

    # accept
    observeEvent(input$done, {
      # get selected items with their chosen unsolvable
      final <- rv$summary_table %>%
        filter(item_nr %in% rv$selected_items) %>%
        select(item_nr, Solution, Anagram, selected_unsolvable) %>%
        rename(Unsolvable = selected_unsolvable)

      # join back to full unsolvable data for the selected candidates
      final_full <- final %>%
        left_join(
          rv$data %>% select(-Solution, -Anagram),  # Remove these to avoid duplicates
          by = c("item_nr", "Unsolvable" = "candidate")
        )

      if (nrow(final_full) == 0) {
        showNotification("No items selected", type = "warning")
        return()
      }

      attr(final_full, "generation_params") <- params
      attr(final_full, "anagram_source") <- attr(rv$data, "anagram_source")
      stopApp(final_full)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("Filter Unsolvable Anagrams",
                                              width = 2400, height = 1200))
}

#' Plot unsolvable anagram matching
#'
#' @param unsolvables Data frame with Anagram and Unsolvable columns plus metrics
#' @param vars Character vector of variables to plot
#' @param show_labels Logical, whether to show extreme value labels
#' @export
plot_unsolvables <- function(unsolvables, vars = c("SBF"), show_labels = FALSE)
{

  # build long format data from a* and u* columns
  plot_data <- unsolvables %>%
    select(item_nr, starts_with("a"), starts_with("u")) %>%
    distinct()

  # extract the variable names (without prefix)
  a_cols <- grep("^a[A-Z]", names(plot_data), value = TRUE)
  u_cols <- grep("^u[A-Z]", names(plot_data), value = TRUE)

  # for each variable in vars, create long format
  plot_list <- lapply(vars, function(v) {
    a_col <- paste0("a", v)
    u_col <- paste0("u", v)

    if (a_col %in% names(plot_data) && u_col %in% names(plot_data)) {
      plot_data %>%
        select(item_nr, all_of(c(a_col, u_col))) %>%
        pivot_longer(
          cols = c(a_col, u_col),
          names_to = "type",
          values_to = "value"
        ) %>%
        mutate(
          type = ifelse(startsWith(type, "a"), "Solvable", "Unsolvable"),
          type = factor(type, levels = c("Solvable", "Unsolvable")),
          variable = v
        )
    } else {
      NULL
    }
  })

  plot_data <- bind_rows(compact(plot_list))

  # get conditions for coloring, use default ggplot2 palette
  conditions <- unique(plot_data$type)
  n_conditions <- length(conditions)

  # default ggplot2 palette
  color_values <- scales::hue_pal()(n_conditions)
  names(color_values) <- conditions

  # base plot
  p <- ggplot(plot_data, aes(x = type, y = value, fill = type)) +
    geom_violin(
      alpha = 0.2,
      trim = TRUE,
      color = NA,
      scale = "width"
    ) +
    geom_line(
      aes(group = item_nr),
      alpha = 0.4,
      linewidth = 0.6,
      color = "gray40"
    ) +
    geom_point(
      aes(color = type),
      size = 1.5,
      alpha = 0.7
    ) +
    scale_color_manual(values = color_values) +
    scale_fill_manual(values = color_values) +
    facet_wrap(~variable, scales = "free_y", ncol = 3) +
    theme_minimal() +
    labs(
      x = "Type",
      y = "Value",
      title = "Unsolvable Anagrams: Matching Quality"
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 11),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  # add labels if requested
  if (show_labels) {
    # label top/bottom 2.5% extremes in each type
    extremes <- plot_data %>%
      group_by(variable, type) %>%
      mutate(
        is_high_extreme = value >= quantile(value, 0.975, na.rm = TRUE),
        is_low_extreme = value <= quantile(value, 0.025, na.rm = TRUE)
      ) %>%
      filter(is_high_extreme | is_low_extreme) %>%
      ungroup() %>%
      distinct(item_nr, variable, type, value, .keep_all = TRUE)

    # get ordered types (matches x-axis order)
    types_ordered <- sort(unique(extremes$type))
    n_types <- length(types_ordered)

    # add labels for each type with appropriate nudge direction
    for (i in seq_along(types_ordered)) {
      typ <- types_ordered[i]
      labels_type <- extremes %>% filter(type == typ)

      if (nrow(labels_type) > 0) {
        # calculate nudge: left (-), right (+)
        if (n_types == 1) {
          nudge_val <- 0
        } else {
          nudge_val <- ((i - 1) / (n_types - 1) - 0.5) * 0.3
        }

        p <- p +
          geom_text_repel(
            data = labels_type,
            aes(label = paste0("#", item_nr)),
            size = 3,
            alpha = 0.7,
            max.overlaps = 30,
            nudge_x = nudge_val,
            direction = "y",
            segment.alpha = 0.3,
            segment.size = 0.2
          )
      }
    }
  }

  p
}

#' Review and finalize unsolvable anagram selection with plots
#'
#' @param unsolvables Filtered unsolvable anagrams from filter_unsolvable_anagrams()
#' @export
review_unsolvable_anagrams <- function(unsolvables)
{

  # get parameters
  params <- attr(unsolvables, "generation_params")

  # get the metrics that were matched on
  matched_metrics <- params$match_metrics
  if (is.null(matched_metrics)) {
    matched_metrics <- c("SBF")
  }

  # get unique items
  unique_items <- sort(unique(unsolvables$item_nr))

  # create summary table
  summary_table <- unsolvables %>%
    group_by(item_nr) %>%
    summarise(
      Solution = first(Solution),
      Anagram = first(Anagram),
      Unsolvable = first(Unsolvable),
      .groups = "drop"
    )

  # build combined metrics from the pre-computed columns
  combined_metrics <- unsolvables %>%
    select(item_nr, starts_with("Anagram_"), starts_with("Unsolvable_")) %>%
    distinct() %>%
    pivot_longer(
      cols = c(starts_with("Anagram_"), starts_with("Unsolvable_")),
      names_to = c("type", "variable"),
      names_pattern = "(.*)_(.*)",
      values_to = "value"
    ) %>%
    mutate(type = ifelse(type == "Anagram", "Solvable", "Unsolvable"))

  # track state
  rv <- reactiveValues(
    data = unsolvables,
    summary_table = summary_table,
    combined_metrics = combined_metrics,
    selected_items = unique_items,
    proxy_ready = FALSE
  )

  ui <- miniPage(
    gadgetTitleBar("Review Unsolvable Anagrams - Final Selection",
                   right = miniTitleBarButton("done", "Accept", primary = TRUE)),
    miniContentPanel(
      # actions bar
      fillRow(
        height = 70,
        fillCol(
          verbatimTextOutput("summary_text"),
          helpText("Click rows to deselect items")
        ),
        actionButton("select_all", "Select All", class = "btn-success"),
        actionButton("deselect_all", "Deselect All", class = "btn-secondary"),
        actionButton("invert_selection", "Invert Selection", class = "btn-secondary"),
        actionButton("export_plot", "Export Plot", class = "btn-info"),
        fillCol(
          checkboxInput("show_labels", "Show labels", value = TRUE),
          selectizeInput("plot_vars", "Variables to plot:",
                         choices = matched_metrics,
                         selected = matched_metrics,
                         multiple = TRUE,
                         options = list(plugins = list('remove_button')),
                         width = "300px")
        )
      ),
      # side by side: table (left, larger) and plot (right, smaller)
      fillRow(
        flex = c(1.2, 1),
        fillCol(
          DTOutput("unsolvable_table", height = "100%")
        ),
        fillCol(
          plotOutput("unsolvable_plot", height = "100%")
        )
      )
    )
  )

  server <- function(input, output, session) {

    # render table
    output$unsolvable_table <- renderDT({
      datatable(
        rv$summary_table,
        rownames = FALSE,
        selection = list(mode = 'multiple', selected = 1:nrow(rv$summary_table)),
        options = list(
          pageLength = -1,
          scrollX = FALSE,
          scrollY = "700px",
          dom = 'ft',
          columnDefs = list(
            list(targets = 0, width = '60px'),
            list(targets = 1, width = '25%'),
            list(targets = 2, width = '25%'),
            list(targets = 3, width = '25%')
          )
        )
      )
    })

    # create proxy after table renders
    observe({
      if (!rv$proxy_ready && !is.null(input$unsolvable_table_rows_selected)) {
        rv$proxy_ready <- TRUE
      }
    })

    # track row selection
    observeEvent(input$unsolvable_table_rows_selected, {
      if (!rv$proxy_ready) return()

      selected_rows <- input$unsolvable_table_rows_selected

      if (length(selected_rows) > 0) {
        rv$selected_items <- rv$summary_table$item_nr[selected_rows]
      } else {
        rv$selected_items <- integer(0)
      }
    }, ignoreNULL = FALSE)

    # select/deselect all buttons
    observeEvent(input$select_all, {
      proxy <- dataTableProxy("unsolvable_table")
      selectRows(proxy, 1:nrow(rv$summary_table))
    })

    observeEvent(input$deselect_all, {
      proxy <- dataTableProxy("unsolvable_table")
      selectRows(proxy, NULL)
    })

    observeEvent(input$invert_selection, {
      current <- input$unsolvable_table_rows_selected
      all_rows <- 1:nrow(rv$summary_table)
      new_selection <- setdiff(all_rows, current)

      proxy <- dataTableProxy("unsolvable_table")
      selectRows(proxy, new_selection)
    })

    # render plot
    output$unsolvable_plot <- renderPlot({
      plot_data <- rv$data %>%
        filter(item_nr %in% rv$selected_items)

      if (nrow(plot_data) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No items selected", cex = 1.5, col = "gray50")
      } else {
        plot_vars <- input$plot_vars
        if (is.null(plot_vars) || length(plot_vars) == 0) {
          plot_vars <- matched_metrics
        }

        plot_unsolvables(plot_data, vars = plot_vars, show_labels = input$show_labels)
      }
    })

    # summary
    output$summary_text <- renderText({
      n_selected <- length(rv$selected_items)
      n_total <- nrow(rv$summary_table)
      paste0("Selected: ", n_selected, " / ", n_total, " items")
    })

    # export plot
    observeEvent(input$export_plot, {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      filepath <- paste0("unsolvable_plot_", timestamp, ".png")

      plot_data <- rv$data %>%
        filter(item_nr %in% rv$selected_items)

      plot_vars_selected <- input$plot_vars
      if (is.null(plot_vars_selected) || length(plot_vars_selected) == 0) {
        plot_vars_selected <- matched_metrics
      }

      ggplot2::ggsave(
        filepath,
        plot_unsolvables(plot_data, vars = plot_vars_selected, show_labels = input$show_labels),
        width = 14, height = 8, dpi = 300
      )

      showNotification(paste("Saved:", basename(filepath)), type = "message")
    })

    # accept
    observeEvent(input$done, {
      final <- rv$data %>%
        filter(item_nr %in% rv$selected_items)

      if (nrow(final) == 0) {
        showNotification("No items selected", type = "warning")
        return()
      }

      # join back to the full anagram data to get all metrics
      anagram_source <- attr(rv$data, "anagram_source")

      if (!is.null(anagram_source)) {
        # get one row per item (all the same anagram per item)
        anagram_data <- anagram_source %>%
          distinct(item_nr, Anagram, .keep_all = TRUE)

        # remove overlapping columns except join keys
        overlap_cols <- intersect(names(final), names(anagram_data))
        overlap_cols <- setdiff(overlap_cols, c("item_nr", "Anagram"))

        if (length(overlap_cols) > 0) {
          anagram_data <- anagram_data %>%
            select(-all_of(overlap_cols))
        }

        final <- final %>%
          left_join(anagram_data, by = c("item_nr", "Anagram"))
      }

      # remove Anagram_* columns (duplicates of a* columns)
      anagram_dup_cols <- grep("^Anagram_", names(final), value = TRUE)
      if (length(anagram_dup_cols) > 0) {
        final <- final %>% select(-all_of(anagram_dup_cols))
      }

      # rename Unsolvable_* columns to u*
      unsolvable_metric_cols <- grep("^Unsolvable_", names(final), value = TRUE)
      if (length(unsolvable_metric_cols) > 0) {
        new_names <- gsub("^Unsolvable_", "u", unsolvable_metric_cols)
        final <- final %>%
          rename_with(~ gsub("^Unsolvable_", "u", .x), starts_with("Unsolvable_"))
      }

      # reorder columns: Solution, Anagram, Unsolvable, anagram metrics, unsolvable metrics, other info, everything else
      anagram_cols <- grep("^a[A-Z]", names(final), value = TRUE)
      anagram_base <- anagram_cols[!grepl("mean|range|min|max|rank", anagram_cols)]
      anagram_agg <- anagram_cols[grepl("mean|range|min|max|rank", anagram_cols)]

      unsolvable_metric_cols <- grep("^u[A-Z]", names(final), value = TRUE)

      unsolvable_info_cols <- c("n_swaps", "n_changed", "positions_changed", "changes")
      unsolvable_info_cols <- unsolvable_info_cols[unsolvable_info_cols %in% names(final)]

      other_cols <- setdiff(
        names(final),
        c("Solution", "Anagram", "Unsolvable", anagram_cols, unsolvable_metric_cols, unsolvable_info_cols)
      )

      final <- final %>%
        select(
          Solution, Anagram, Unsolvable,
          all_of(unsolvable_metric_cols),
          all_of(unsolvable_info_cols),
          all_of(anagram_base),
          all_of(anagram_agg),
          all_of(other_cols)
        )

      attr(final, "generation_params") <- params
      stopApp(final)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("Review Unsolvable Anagrams",
                                              width = 2400, height = 1200))
}
