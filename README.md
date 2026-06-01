# {grams} 

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/irnewman/grams/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/irnewman/grams/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> **[A Generative Reposity of Anagrams and Metrics, built from SUBTLEX]**

{grams} is an R package for computing psycholinguistic metrics and
generating anagram stimuli. Developed with metareasoning experiments in
mind, the package provides tools applicable across psycholinguistic domains. 
It provides a pipeline for generating anagram candidates, computing 
psycholinguistic indices, and classifying strings. Stimulus selection 
graphical interface is in active development.


## Status

{grams} is functional but under active development. Package documentation and this README are also in progress.

## Installation

Install the development version of {grams} from GitHub:

```r
# install.packages("devtools")
devtools::install_github("irnewman/grams")
```

Some features require Python (for grapheme-to-phoneme transcription via g2p). If you plan to use the `articulability` or `syllables` functions, you will also need the `reticulate` package and a working Python environment:

```r
install.packages("reticulate")
```

## Overview

{grams} core capabilities:

**Building frequency tables**: compute n-gram frequencies from any corpus specified by the user, as a word vector.

**Solving anagrams.**: given a target string of letters, `solve_anagram` returns all valid dictionary anagrams using an internal lexicon built from multiple lexical sources (CMU, GCIDE, WordNet, SUBTLEX-UK). Users can specify their own dictionary if preferred. Call `build_sig_index` for fast lookups.

**Computing psycholinguistic metrics.**: `compute_string_indices()` computes lexical and sublexical metrics for any string. `compute_anagram_indices()` computes metrics regarding the correspondence between an anagram and its solution.

**Anagram stimuli GUI**: in-progress, a graphical interface to generate and filter a set of stimuli, matched on user-specified metrics. 

**Database tools**: a set of tools to 

**Classifying strings.**: a Mahalanobis distance classifier that assigns strings to one of three categories, based on their similarity to the classifier multivariate distribution: `"word"`, `"pseudoword"`, or `"nonword"`.

## GRAMS Database 

The database is hosted on the Open Science Framework: https://osf.io/j37sa

### List of Database Columns 

## Database columns

### Source word properties

| Column | Description |
|---|---|
| `Word` | Source word |
| `Zipf` | Zipf-scaled frequency (SUBTLEX-UK) |
| `Length` | Number of letters |
| `SBF` | Summed bigram frequency (positional) |
| `MLBF` | Mean log bigram frequency (positional) |
| `SBFnp` | Summed bigram frequency (non-positional) |
| `MLBFnp` | Mean log bigram frequency (non-positional) |
| `SBFt` | Summed bigram frequency (token-weighted) |
| `MLBFt` | Mean log bigram frequency (token-weighted) |
| `SLF` | Summed letter frequency |
| `STF` | Summed trigram frequency |
| `MLLF` | Mean log letter frequency |
| `MLTF` | Mean log trigram frequency |
| `OLD20` | Orthographic Levenshtein distance (20 neighbours) |
| `ED1` | Number of orthographic neighbours at edit distance 1 |
| `GTzero` | Proportion of bigrams with non-zero frequency |
| `Articulability` | Sonority-based pronounceability score |
| `Nsyllables` | Estimated syllable count |
| `SyllableSource` | Source of syllabification (dictionary or algorithm) |
| `Nvowels` | Number of vowels |
| `Nconsonants` | Number of consonants |
| `Vratio` | Vowel ratio (vowels / length) |
| `Nuniqueletters` | Number of distinct letters |
| `ULratio` | Unique letter ratio (distinct letters / length) |
| `FirstLetter` | First letter of the word |
| `InfreqLetter` | Whether the word contains an infrequent letter |
| `Morphemes` | Morpheme string |
| `Nmorphemes` | Number of morphemes |
| `HasSpellingVariant` | Whether the word has a known spelling variant |
| `IsCompound` | Whether the word is a compound |
| `HomophonicEntry` | Whether the word has a homophonic dictionary entry |
| `DoubleWordEntry` | Whether the word appears as two separate words |

### Source word anagram set summary

| Column | Description |
|---|---|
| `Nwords` | Number of dictionary anagrams |
| `Npseudowords` | Number of pseudoword anagrams |
| `Nnonwords` | Number of nonword anagrams |
| `sSBFrank` | Rank of source word SBF within its anagram set |
| `sMLBFrank` | Rank of source word MLBF within its anagram set |
| `aSBFmean` | Mean SBF across anagram set |
| `aSBFrange` | Range of SBF across anagram set |
| `aSBFmin` | Minimum SBF in anagram set |
| `aSBFmax` | Maximum SBF in anagram set |
| `aMLBFmean` | Mean MLBF across anagram set |
| `aMLBFrange` | Range of MLBF across anagram set |
| `aMLBFmin` | Minimum MLBF in anagram set |
| `aMLBFmax` | Maximum MLBF in anagram set |
| `aOLD20mean` | Mean OLD20 across anagram set |
| `aOLD20range` | Range of OLD20 across anagram set |
| `aOLD20min` | Minimum OLD20 in anagram set |
| `aOLD20max` | Maximum OLD20 in anagram set |
| `aArticulabilitymean` | Mean articulability across anagram set |
| `aArticulabilityrange` | Range of articulability across anagram set |
| `aArticulabilitymin` | Minimum articulability in anagram set |
| `aArticulabilitymax` | Maximum articulability in anagram set |
| `Movesmin` | Minimum letter moves across anagram set |
| `Movesmax` | Maximum letter moves across anagram set |
| `Nintactmin` | Minimum intact bigrams across anagram set |
| `Nintactmax` | Maximum intact bigrams across anagram set |
| `NpreservedBGmin` | Minimum preserved bigrams across anagram set |
| `NpreservedBGmax` | Maximum preserved bigrams across anagram set |
| `aNmorphemesmin` | Minimum morpheme count across anagram set |
| `aNmorphemesmax` | Maximum morpheme count across anagram set |

### Anagram properties

| Column | Description |
|---|---|
| `Anagram` | Anagram string |
| `aSBF` | Summed bigram frequency of anagram (positional) |
| `aMLBF` | Mean log bigram frequency of anagram (positional) |
| `aOLD20` | Orthographic Levenshtein distance of anagram |
| `aED1` | Edit distance 1 neighbours of anagram |
| `aArticulability` | Articulability score of anagram |
| `aNsyllables` | Estimated syllable count of anagram |
| `aSyllableSource` | Source of syllabification for anagram |
| `aFirstLetter` | First letter of anagram |
| `aMorphemes` | Morpheme string of anagram |
| `aNmorphemes` | Number of morphemes of anagram |
| `aProvisionalClass` | Provisional classification (word/pseudoword/nonword) |
| `SBFrank` | Rank of anagram SBF within its anagram set |
| `MLBFrank` | Rank of anagram MLBF within its anagram set |
| `Moves` | Number of letter position changes from source word |
| `Intact` | Number of intact bigrams shared with source word |
| `Nintact` | Count of intact bigrams |
| `PreservedBG` | Preserved bigram string |
| `NpreservedBG` | Number of preserved bigrams |
| `SameFirstLetter` | Whether anagram shares first letter with source word |

## Frequency Table Generator

## String Indices Computed

`compute_string_indices()` returns the following measures in the table below. Use `classify = TRUE` to include string classification and `full = TRUE` for non-default n-gram indices.

| Index | Description |
|---|---|
| `SBF` | Summed bigram frequency (default: positional, type-based) |
| `MLBF` | Mean log bigram frequency (default: positional, type-based) |
| `OLD20` | Orthographic Levenshtein distance (20 neighbours) |
| `ED1` | Number of orthographic neighbours at edit distance 1 |
| `GTzero` | Proportion of possible bigrams with non-zero frequency |
| `Articulability` | Sonority- and phonotactic-based pronounceability score |
| `Nsyllables` | Estimated syllable count |
| `SyllableSource` | Source of syllable count (CMUdict, `gruut`, `g2p`) |
| `Nvowels`, `Nconsonants`, `Vratio` | Vowel/consonants counts and vowel:consonant ratio|
| `FirstLetter` | First letter of string (vowel, consonant) |
| `InfreqLetter` | Presence of an infrequent letter (J, K, Q, V, W, X, Z) |
| `NuniqueLetters`, `ULratio` | Number of non-repeated letters and unique:total ratio |
| `Morphemes` | English morphemes found in the string |
| `Nmorphemes` | Number of morphemes found |
| `Classification` | Provisional string classifier (word, psuedoword, nonword) |
| `SBFnp`, `SBFt` | Summed bigram frequency (positional and token-based) |
| `MLBFnp`, `MLBFt` | Mean log bigram frequency (positional and token-based) |
| `SLF`, `STF`, `MLLF`, `MLTF` | Summed letter/trigram frequency; mean log letter/trigram frequency |

## Anagram Indices Computed

`compute_anagram_indices()` returns the following measures for a solution and its anagram:

| Index | Description |
|---|---|
| `aSBF`, `aMLBF`, etc. | All `compute_string_indices` metrics with a leading "a" denoting anagram (except where redundant) |
| `SBFrank` ||
| `MLBFrank` ||
| `Moves` ||
| `Intact` ||
| `Nintact` ||
| `PreservedBG` ||
| `NpreservedBG` ||
| `SameFirstLetter` ||

## Build Word Summary Indices Computed

For each word, up to 100 anagrams are computed, and a set of summary metrics are computed on those results.

Solution 
WordLength
sSBFrank ,
sMLBFrank 
Nwords               
Npseudowords         
Nnonwords            

aSBFmean            
aSBFrange            
aSBFmin             
aSBFmax             

aMLBFmean            
aMLBFrange           
aMLBFmin            
aMLBFmax             

aOLD20mean           
aOLD20range          
aOLD20min            
aOLD20max            

aArticulabilitymean       
aArticulabilityrange       
aArticulabilitymin         
aArticulabilitymax         

Movesmin             
Movesmax             

Nintactmin           
Nintactmax           

NpreservedBGmin      
NpreservedBGmax      

aNmorphemesmin       
aNmorphemesmax       

## "Articulability" Score

An algorithm for computing an articulatory ease score. This algorithm and function still in development. Based on sonority scoring (plosives through vowels) and phonotactic rule violations.

See the articulability vignette.

## Database Generation Tools

| Index | Description |
|---|---|
generate_word_csv, add_index_to_csv, rename_csv_columns
grams_database_path, download_grams_database, load_grams_database

## Planned Vignettes

- user-specified corpora and dictionaries: building frequency caches a signature index
- articulability
- database generation

## What's coming

- Stimulus selection functions (select matched sets of anagram pairs controlled on specified indices)
- Unsolvable anagram generator
- Empirical calibration of pronounceability scoring and classifier threshold

## Citation

If you use {grams} in published research, please cite it as:

```
[Manuscript in preparation — to be updated. ]
```

## License

MIT © [Ian R. Newman]
