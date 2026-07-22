
# Note: temporary solution for the GUI functions
utils::globalVariables(c(
  # shiny / miniUI / DT
  "runGadget", "dialogViewer", "miniPage", "gadgetTitleBar",
  "miniTitleBarButton", "miniContentPanel", "fillRow", "fillCol",
  "reactiveValues", "reactive", "observe", "observeEvent",
  "renderText", "renderDT", "renderPlot", "renderTable",
  "showNotification", "showModal", "removeModal", "stopApp",
  "modalDialog", "modalButton", "radioButtons", "actionButton",
  "sliderInput", "selectInput", "selectizeInput", "numericInput",
  "checkboxInput", "verbatimTextOutput", "plotOutput", "DTOutput",
  "helpText", "tagList", "div", "h3", "h4", "br", "strong",
  "dataTableProxy", "selectRows", "datatable", "formatStyle",
  "styleInterval", "styleEqual", "JS", "req", "updateSelectInput",
  "removeNotification",
  # dplyr / tidyr column names
  "Solution", "Anagram", "Word", "item_nr", "string", "condition",
  "anagram_condition", "Status", "all_pass", "Unsolvable",
  "match_score", "selected_unsolvable", "candidate", "changes",
  "n_changed", "is_high_extreme", "is_low_extreme", "type",
  "value", "variable", ".data",
  # internal data objects
  "internal_dict", "morpheme_list", "subtlex_uk", "cmu", "GRAMS",
  # dplyr/tidyr functions used unqualified
  "filter", "select", "mutate", "arrange", "distinct", "rename",
  "rename_with", "left_join", "group_by", "summarise", "ungroup",
  "pivot_longer", "bind_rows", "compact", "starts_with", "all_of",
  "across", "first", "slice", "pull", "n",
  # ggplot2
  "ggplot", "aes", "geom_violin", "geom_line", "geom_point",
  "geom_text_repel", "facet_wrap", "theme_minimal", "theme",
  "element_text", "element_blank", "scale_color_manual",
  "scale_fill_manual", "labs",
  # base functions flagged
  "setNames", "head", "tail", "read.csv", "write.csv",
  "download.file", "mahalanobis", "cov", "quantile", "runif",
  # phonetics variables
  "phone", "syll", "sonority", "part", "nucleus_sonority",
  "nucleus_is_peak", "onset_increasing", "coda_decreasing",
  "violation_type",
  # leftover references
  "penalty_illegal", "penalty_rare", "find_dictionary_anagrams",
  "word", ":=",
  "%>%", "text"
))

#' @importFrom DT datatable
#' @importFrom miniUI miniPage
#' @importFrom tibble tibble
NULL
