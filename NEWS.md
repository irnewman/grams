# grams 0.1.0.9000

## Initial development release

First public release of {grams} on GitHub. The package is functional but under active development.

### Core features available

* `compute_string_indices()` — full suite of psycholinguistic indices for any string
* `compute_anagram_indices()` — source-word-relative measures for anagram candidates
* `classify_string()` — Mahalanobis distance classifier (word / pseudoword / nonword)
* `solve_anagram()` — fast dictionary anagram lookup via signature index
* `build_signature_index()` — build a custom signature index from any dictionary
* `generate_anagram_candidates()` — generate and score anagram candidate sets
* `articulability()` — sonority-based pronounceability scoring
* `create_classifier()` — build a custom Mahalanobis classifier from a reference corpus
* `download_grams_database()` / `load_grams_database()` — access the GRAMS database
* `build_freq_tables()`, `tally_ngram_frequencies()`, `build_ngram_cache()` — frequency table pipeline

### Known limitations

* Sonority penalty weights are provisional and have not yet been empirically calibrated
* Mahalanobis classifier threshold requires external validation against pronounceability ratings
* Stimulus selection functions and GUI are not fully implemented
* Unsolvable anagram generator not yet implemented
