#' Extract safety population
#'
#' Filters input data to `SAFFL == "Y`, the safety-evaluable population
#'
#' @md
#' @param d input `data.frame` of clinical data
#'
#' @import dplyr
#'
#' @return a `data.frame` containing just the `STUDYID` and `USUBJID`
#'         for the safety-evaluable population.
#' @export
get_safety_pop <- function(d) {
  d %>%
    dplyr::filter(SAFFL == "Y") %>%
    dplyr::select(STUDYID, USUBJID)
}

#' Extract endpoint data and prepare for profiling
#'
#' @md
#' @param d input `data.frame` of clinical data
#' @param pop_of_interest input `data.frame` containing identifiers corresponding to the population of interest
#'
#' @importFrom tidyselect one_of
#' @import dplyr
#'
#' @return a `data.frame` containing endpoint data for the population of interest
#' @export
extract_data_for_cm_profiling <- function(d = NULL, pop_of_interest = NULL){

  # selected variables
  vars <- c("STUDYID", "USUBJID", "SAFFL", "ACTARM", "TRTEDT", "TRTSDT", "nested.CM", "nested.EX")

  # filter on population of interest and columns of interest
  data <- dplyr::left_join(pop_of_interest,
                           d,
                           by = c('STUDYID', 'USUBJID')) %>%
    dplyr::select(tidyselect::one_of(vars))

  # extract survival data
  survival_data <- extract_paramcd(d = d,
                                   nested.X = "nested.TTE",
                                   param.code = "OS",
                                   val.col = "AVAL",
                                   col.name = "OS") %>%
    dplyr::left_join(extract_paramcd(d = d,
                                     nested.X = "nested.TTE",
                                     param.code = "OS",
                                     val.col = "CNSR",
                                     col.name = "OS_CNSR"),
                     by = c('STUDYID', 'USUBJID'))

  # merge survival data with full data
  dplyr::left_join(data, survival_data, by = c('STUDYID', 'USUBJID'))

}

#' process_treatment_days
#'
#' @md
#' @param cm_data input `data.frame` of conmeds data
#' @param treatment_lasting_effect how long (in days) the treatment is expected to be
#'                                 impactful following exposure
#'
#' @import dplyr
#'
#' @return a `data.frame` of the input data with treatment days annotated
#' @export
process_treatment_days <- function(cm_data = NULL, treatment_lasting_effect = 21){
  # Create START/END TREATMENT variables from TRTSDT/TRTEDT
  cm_data %>%
    # create START and END of TREATMENT variables
    dplyr::mutate(
      # always assumes that start_treatment is O
      START_TREATMENT = 0,
      # it computes end_treatment based on TRTEDT & TRTSDT
      # it takes into account an hypothetical lasting effect after the last treatment
      END_TREATMENT = as.numeric(difftime(TRTEDT, TRTSDT, units = c("days"))) + treatment_lasting_effect)
}

#' Filter data to concomitant medication records matching a given basket
#'
#' @md
#' @param nested_cm_data input `data.frame` of clinical data with nested conmeds
#' @param drug_basket input `data.frame` of drug basket classifications
#' @param unknownDoseCategoryLabel suffix to use when dose is unknown
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @importFrom stringr str_replace_all
#'
#' @details
#' Performs some cleanup of conmeds data.
#'
#' @return a `data.frame` of concomitant medication data with classes merged
#' @export
extract_basket_from_cm <- function(nested_cm_data = NULL, drug_basket = NULL, unknownDoseCategoryLabel = "UNKDOSE"){

  # extract unique CMDECOD/CLASS pairs
  drug_basket_no_dosage <- unique(drug_basket[, c("CMDECOD", "CLASS")])

  # extract ConMeds records matching Drug Basket based on CMDECOD
  data <- nested_cm_data %>%
    tidyr::unnest(cols = c(nested.CM)) %>%
    dplyr::filter(CMCAT %in% c("CONCOMITANT", "CONCOMITANT MEDICATION", "CONCOMITANT MEDICATIONS", "PREV/CONCOM TREATMENT")) %>%
    dplyr::mutate(CMDECOD = stringr::str_replace_all(CMDECOD, ";", "/")) %>% # some are recoded to ;
    dplyr::inner_join(drug_basket_no_dosage, by = c('CMDECOD'))

  # Warn if extraction was not successful
  if(is.null(data) | dim(data)[1] == 0){
    warning("Empty ConMed record extraction with current basket, check CMDECOD column.")
  }

  # Check if drug basket has also dosage information to match
  if(all(c("CMDOSE", "CMDOSU", "DoseCategoryLabel", "DoseCategoryRank") %in% names(drug_basket))){

    # capture matched CM records with dosage
    matched_cm = dplyr::inner_join(data,
                                   drug_basket,
                                   by = c('CMDECOD', 'CLASS', 'CMDOSE', 'CMDOSU'))

    # capture unmatched CM records with dosage and label them as Unknown Dose 'UNKDOSE'
    unmatched_cm = dplyr::anti_join(data,
                                    drug_basket,
                                    by = c('CMDECOD', 'CLASS', 'CMDOSE', 'CMDOSU')) %>%
                    dplyr::mutate(SIG = NA,
                                  DoseCategoryLabel = unknownDoseCategoryLabel,
                                  DoseCategoryRank = .Machine$integer.max) #last possible rank

    # Merge matched and unmatched list
    extracted_cm <- dplyr::bind_rows(matched_cm, unmatched_cm)
  } else {
    # No dosage information found in drug basket
    extracted_cm = data
  }

  return(extracted_cm)
}


#' Filter out PRIOR concomitant medication records
#'
#' @md
#' @param cm_data input `data.frame` of clinical data with concomitant medications
#'
#' @import dplyr
#' @importFrom methods is
#'
#' @details
#' Removes records corresponding to concomitant medications taken prior to treatment
#'
#' @return a `data.frame` of filtered concomitant medication data
#' @export
remove_prior_cm_records <- function(cm_data = NULL){

  # Input requirements
  stopifnot(!is.null(cm_data), is(cm_data, "data.frame"),
            all(c("CMENDY") %in% names(cm_data)))

  # filter-out records where CMEND is defined with a time < 0
  # these CM records are considered PRIOR ConMeds and can be discarded
  data <- dplyr::filter(cm_data, is.na(CMENDY) | CMENDY >= 0)

  return(data)
}

#' Impute end of concomitant medication exposure
#'
#' @md
#' @param cm_data input `data.frame` of clinical data with concomitant medications
#' @param treatments vector of treatment names as found in the exposure domain (to distinguish from control)
#'
#' @import dplyr
#'
#' @details
#' In some cases, the end day of concomitant medication exposure (`CMEND`) is not avaialble, either
#' due to not being recorded, the administration being ongoing, or exit of the trial.
#'
#' This function attempts to recover a usable end day where possible.
#'
#' @return a `data.frame` of concomitant medication data with imputed values
#' @export
impute_cm_days <- function(cm_data = NULL, treatments = c("trt1")){

  # Input requirements
  stopifnot(!is.null(cm_data), is(cm_data, "data.frame"),
            all(c("CMSTDY", "CMENDY", "CMENRTPT", "CMSTRTPT", "CMSTTPT") %in% names(cm_data)),
            all(c("END_TREATMENT", "OS") %in% names(cm_data)))

  data <- cm_data %>%
    dplyr::mutate(isTreatedPt = USUBJID %in% list_by_treatment(cm_data, "treatment", treatments = treatments)) %>%
    # impute ConMed start day
    dplyr::mutate(
      CMSTART = dplyr::case_when(
        # WARNING: a fair number of records have CMSTDY==NA & CMSTRTPT==NA & CMSTTPT==NA & CMENRTPT==ONGOING
        # Our understanding of CMENRTPT==ONGOING needs to improve to see if we can rescue some more records
        !is.na(CMSTDY) ~ as.integer(CMSTDY),
        is.na(CMSTDY) & CMSTRTPT == "BEFORE" & CMSTTPT == "SCREENING" ~ 0L,
        is.na(CMSTDY) ~ NA_integer_,
        TRUE ~ NA_integer_),
      # we keep track of the imputation
      CMSTART_IMPUTED = ifelse(is.na(CMSTDY) & CMSTRTPT == "BEFORE" & CMSTTPT == "SCREENING", TRUE, FALSE)) %>%
    # impute ConMed end day
    dplyr::mutate(
      CMEND = dplyr::case_when(
        # If CMENDY is missing and treatment is ongoing, we assume that we
        # can impute the end date to be the min of {end_of_study, end_of_treatment, overall_survival}
        !is.na(CMENDY) ~ as.integer(CMENDY),
        is.na(CMENDY) & CMENRTPT == "ONGOING" ~ as.integer(pmin(END_TREATMENT, OS, na.rm = TRUE)),
        is.na(CMENDY) ~ NA_integer_,
        TRUE ~ NA_integer_),
      # we keep track of the imputation
      CMEND_IMPUTED = ifelse(is.na(CMENDY) & CMENRTPT == "ONGOING", TRUE, FALSE))

  return(data)
}

#' Determine if concomitant medication timing is uncertain
#'
#' @md
#' @param CMEND ADaM data for end day (relative to start of trial) of concomitant medication exposure
#' @param CMSTART ADaM data for start day (relative to start of trial) of concomitant medication exposure
#' @param START_TREATMENT ADaM data for start day (relative to start of trial) of treatment exposure
#' @param END_TREATMENT ADaM data for end day (relative to start of trial) of treatment exposure
#'
#' @return a `logical` vector of whether a given record is uncertain in timing
#' @export
is_cm_time_uncertain <- function(CMEND = NULL, CMSTART = NULL,
                                 START_TREATMENT = NULL, END_TREATMENT = NULL){

  # Input requirements
  stopifnot(!is.null(CMEND),
            !is.null(CMSTART),
            !is.null(START_TREATMENT),
            !is.null(END_TREATMENT))

  #uncertain <- anyNA(c(CMEND, CMSTART, START_TREATMENT, END_TREATMENT))
  uncertain <- ifelse(is.na(CMEND) |
                        is.na(CMSTART) |
                        is.na(START_TREATMENT) |
                        is.na(END_TREATMENT), TRUE, FALSE)

  return(uncertain)
}

#' Flag uncertain (timing) concomitant medication records
#'
#' @md
#' @param cm_data input `data.frame` of clinical data with concomitant medications
#'
#' @import dplyr
#'
#' @return a `data.frame` of concomitant medication data with an additional `UNCERTAIN_TIMING` column
#' @export
flag_uncertain_cm_records <- function(cm_data = NULL){

  # Input requirements
  stopifnot(!is.null(cm_data), is(cm_data, "data.frame"),
            all(c("CMSTART", "CMEND") %in% names(cm_data)),
            all(c("START_TREATMENT", "END_TREATMENT", "OS") %in% names(cm_data)))

  # create flag to identify CM records which have not enough
  # time information to resolve in terms of concomittance.
  data <- cm_data %>%
    dplyr::mutate(UNCERTAIN_TIMING = is_cm_time_uncertain(CMSTART = CMSTART,
                                                          CMEND = CMEND,
                                                          START_TREATMENT = START_TREATMENT,
                                                          END_TREATMENT = END_TREATMENT))

  # No NA should have been introduced
  stopifnot(!any(is.na(data$UNCERTAIN_TIMING)))

  return(data)
}

#' Flag uncertain timing and COMBO records
#'
#' @md
#' @param cm_data input `data.frame` of clinical data with concomitant medications
#' @param combo_drug_label_prefix prefix to use for COMBO records
#' @param combo_drug_label_suffix suffix to use for COMBO records
#' @param combo_drug_uncertain_timing_suffix suffix to use for uncertain timing records
#' @param class_uncertain_timing_suffix class to assign uncertain timing records
#' @param separator separator to use bewteen prefix, class, and suffix
#'
#' @import dplyr
#' @importFrom tidyselect one_of
#'
#' @return a `data.frame` of concomitant medication records, with labels processed
#' @export
identify_combo_and_uncertain_cm <- function(cm_data = NULL,
                                            combo_drug_label_prefix = "DRUG",
                                            combo_drug_label_suffix = "COMBO",
                                            combo_drug_uncertain_timing_suffix = "UNCERTAIN_TIMING",
                                            class_uncertain_timing_suffix = "UNCERTAIN_TIMING",
                                            separator = "_"){

  # Input requirements
  stopifnot(!is.null(cm_data), is(cm_data, "data.frame"),
            all(c("STUDYID", "USUBJID", "CLASS", "UNCERTAIN_TIMING") %in% names(cm_data)),
            !is.null(combo_drug_label_prefix), is.character(combo_drug_label_prefix),
            !is.null(combo_drug_label_suffix), is.character(combo_drug_label_suffix),
            !is.null(combo_drug_uncertain_timing_suffix), is.character(combo_drug_uncertain_timing_suffix),
            !is.null(class_uncertain_timing_suffix), is.character(class_uncertain_timing_suffix))

  # If dosage info present, process combo entries due to different dosages within a unique CLASS
  if (all(c("DoseCategoryLabel", "DoseCategoryRank") %in% names(cm_data))) {

    # IMPORTANT: In case of multiple dosages within a given CLASS, the 1st ranked dosage intensity
    # is assumed for the patient, independently of the respective timing information.

    # extract the 1st ranked dosage across combos
    cm_dose_unique_category <- cm_data %>%
      dplyr::select(tidyselect::one_of("STUDYID", "USUBJID", "CLASS", "DoseCategoryLabel", "DoseCategoryRank")) %>%
      dplyr::group_by(STUDYID, USUBJID, CLASS) %>%
      dplyr::arrange(DoseCategoryRank, .by_group = TRUE) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(RANKED_1ST_CLASS = paste0(CLASS, "_", DoseCategoryLabel)) %>%
      dplyr::select(tidyselect::one_of("STUDYID", "USUBJID", "CLASS", "RANKED_1ST_CLASS"))

    # overwrite CLASS column with unique 1st ranked class from combos
    cm_data <- cm_data %>%
      dplyr::left_join(cm_dose_unique_category, by = c("STUDYID", "USUBJID", "CLASS")) %>%
      dplyr::select(-CLASS) %>%
      dplyr::rename("CLASS" = "RANKED_1ST_CLASS")
  }


  # Compute number of distinct drug basket classes per patient
  data <- cm_data %>%
    dplyr::group_by(STUDYID, USUBJID) %>%
    dplyr::summarise(
      NB_CM_CLASSES = dplyr::n_distinct(CLASS),
      NB_CM_RECORDS = dplyr::n(),
      NB_UNCERTAIN_TIMINGS = sum(UNCERTAIN_TIMING),
      # Assign patients to a specific basket, or a "Combo" basket in case of overlapping medications between defined baskets.
      # It also accounts for CM records with uncertain timings. If all CM records, within a given basket (or Combo), are uncertain,
      # then the basket is considered to be XXX_UNCERTAIN. However, if a single CM record is valid, the assigned basket is conserved.
      BASKET_CATEGORY = dplyr::case_when(
        # we found multiple basket categories but at least one CM is not uncertain
        NB_CM_CLASSES > 1 & NB_UNCERTAIN_TIMINGS < NB_CM_RECORDS ~ paste0(combo_drug_label_prefix, separator,
                                                             combo_drug_label_suffix),
        # we found multiple basket categories but ALL records are uncertain
        NB_CM_CLASSES > 1 & NB_UNCERTAIN_TIMINGS == NB_CM_RECORDS ~ paste0(combo_drug_label_prefix, separator,
                                                            combo_drug_label_suffix, separator,
                                                            combo_drug_uncertain_timing_suffix),
        # Note: it complains if we do not insure unicity of CLASS, hence the use of 'collapse'
        # Error message: Column `BASKET_CATEGORY` must be length 1 (a summary value), not 2
        # Don't understand why ...
        # we found a single basket category, and at least one CM is not uncertain
        NB_CM_CLASSES == 1 & NB_UNCERTAIN_TIMINGS < NB_CM_RECORDS ~ paste0(unique(CLASS), collapse = "__"),
        # we found a single basket category, and ALL CMs are uncertain
        NB_CM_CLASSES == 1 & NB_UNCERTAIN_TIMINGS == NB_CM_RECORDS ~ paste0(unique(CLASS), separator,
                                                             class_uncertain_timing_suffix, collapse = "__"),
        TRUE ~ NA_character_))

  # No NA should have been introduced
  stopifnot(!any(is.na(data$BASKET_CATEGORY)))

  # join patient info back to full ConMed records
  cm_data <- cm_data %>% dplyr::left_join(data, by = c('STUDYID', 'USUBJID'))

  return(cm_data)
}

#' Compute overlapping classes
#'
#' @md
#' @param cm_data input `data.frame` of clinical data with concomitant medications
#' @param uncertain_basket_pattern regex to use to identify uncertain basket categories
#'
#' @import dplyr
#' @importFrom tidyselect one_of
#' @importFrom stats na.omit
#'
#' @return a `data.frame` of outputs - classifications of subjects into conmeds profiles and additional data
#' @export
compute_cm_overlaps <- function(cm_data = NULL, uncertain_basket_pattern = NULL){

  # Input requirements
  stopifnot(!is.null(cm_data), is(cm_data, "data.frame"),
            all(c("STUDYID", "USUBJID", "CLASS", "BASKET_CATEGORY") %in% names(cm_data)),
            all(c("CMSTART", "CMSTART_IMPUTED", "CMEND", "CMEND_IMPUTED") %in% names(cm_data)),
            all(c("OS", "START_TREATMENT", "END_TREATMENT") %in% names(cm_data)),
            !is.null(uncertain_basket_pattern), is.character(uncertain_basket_pattern))

  # Extract patients with full uncertain records
  data_cm_uncertain <- cm_data %>%
    dplyr::filter(grepl(pattern = uncertain_basket_pattern, BASKET_CATEGORY))  %>%
    dplyr::select(tidyselect::one_of("STUDYID", "USUBJID",
                                     "BASKET_CATEGORY", "NB_CM_RECORDS")) %>%
    dplyr::distinct()

  # Process non-uncertain patients by removing all uncertain patients
  data_cm_defined <- cm_data %>%
    # remove patients identified with full uncertain categories
    dplyr::filter(!(USUBJID %in% data_cm_uncertain$USUBJID)) %>%
    # IMPORTANT: remove all CM records with uncertain timings to proceed
    dplyr::filter(!is_cm_time_uncertain(CMSTART = CMSTART,
                                 CMEND = CMEND,
                                 START_TREATMENT = START_TREATMENT,
                                 END_TREATMENT = END_TREATMENT)) %>%
    ## for each row,
    dplyr::rowwise() %>%
    ## create a sequence covering the medication span
    dplyr::mutate(medicated = list(pmax(0, CMSTART):CMEND)) %>%
    # process per patient and basket category (included added UNCERTAIN_CLASSES due to timing missingness)
    dplyr::group_by(USUBJID, BASKET_CATEGORY) %>%
    dplyr::summarize(STUDYID = unique(STUDYID),
                     isTreatedPt = unique(isTreatedPt),
                     OS = unique(OS),
                     OS_CNSR = unique(OS_CNSR),
                     START_TREATMENT = unique(START_TREATMENT),
                     END_TREATMENT = unique(END_TREATMENT),
                     # capture number of records aggregated within this basket category
                     NB_CM_RECORDS = unique(NB_CM_RECORDS),
                     # number of records where CMSTART was imputed
                     NB_CMSTART_IMPUTED = sum(CMSTART_IMPUTED),
                     # number of records where CMEND was imputed
                     NB_CMEND_IMPUTED = sum(CMEND_IMPUTED),
                     # merge medicated days from drugs belonging to same basket category
                     multimed = list(unique(sort(unlist(c(medicated))))),
                     # first day of medication from this basket category
                     CMSTART_FIRST = min(unlist(multimed)),
                     # last day of medication from this basket category
                     CMEND_LAST = max(unlist(multimed))) %>%
    ## calculate the overlap between treatment and the combination of medications
    dplyr::mutate(intersection = list(intersect(START_TREATMENT:END_TREATMENT, unlist(multimed)))) %>%
    ## number of days a patient is on a basket drug, between day 0 and OS
    dplyr::mutate(ndays_drug_os = length(na.omit(unique(unlist(multimed))))) %>%
    ## percent of 0:OS range for which a patient is on a basket drug
    ## OS + 1 to include day 0
    dplyr::mutate(percent_drug_os = 100 * ndays_drug_os / (OS + 1)) %>%
    ## number of days a patient is on both a basket drug and treatment, between day 0 and day OS
    dplyr::mutate(ndays_drug_trt = length(na.omit(unlist(intersection)))) %>%
    ## percent of Atezo range for which a patient is on both a basket drug and treatment
    dplyr::mutate(percent_drug_trt = 100 * ndays_drug_trt / (END_TREATMENT - START_TREATMENT + 1)) %>%
    dplyr::select(tidyselect::one_of(c("STUDYID", "USUBJID", "isTreatedPt",
                                       "BASKET_CATEGORY", "NB_CM_RECORDS",
                                       "NB_CMSTART_IMPUTED", "CMSTART_FIRST",
                                       "NB_CMEND_IMPUTED", "CMEND_LAST",
                                       "START_TREATMENT", "OS", "OS_CNSR", "END_TREATMENT",
                                       "ndays_drug_os", "percent_drug_os",
                                       "ndays_drug_trt", "percent_drug_trt")))

  #bind uncertain and non-uncertain
  data <- dplyr::bind_rows(data_cm_defined, data_cm_uncertain)

  #Each processed patient should have a unique row, no USUBJID duplication
  stopifnot(sum(duplicated(data$USUBJID))==0)

  return(data)
}

#' Complement profiles with the DRUG_FREE cohort
#'
#' @md
#' @param cm_profiles concomitant medication profiles
#' @param overall_pop identifiers for the full population data
#' @param drug_free_label label to use for the drug free population
#' @param full_data full population data
#' @param treatments vector of treatment names as found in the exposure domain (to distinguish from control)
#'
#' @import dplyr
#'
#' @return output `data.frame` complemented with subjects who did not take any medications in the drug basket
#' @export
complement_with_drug_free_pop <- function(cm_profiles = NULL, overall_pop = NULL, full_data = NULL, drug_free_label = "DRUG_FREE", treatments = c("trt1")) {

  # Input requirements
  stopifnot(!is.null(cm_profiles), is(cm_profiles, "data.frame"),
            all(c('STUDYID', 'USUBJID','BASKET_CATEGORY') %in% names(cm_profiles)),
            !is.null(overall_pop), is(overall_pop, "data.frame"),
            all(c('STUDYID', 'USUBJID') %in% names(overall_pop)),
            !is.null(drug_free_label), is.character(drug_free_label))

  # merge overall population of interest with ConMed profiles
  data <- overall_pop %>%
    dplyr::left_join(cm_profiles, by = c('STUDYID', 'USUBJID')) %>%
    dplyr::mutate(isTreatedPt = USUBJID %in% list_by_treatment(full_data, "treatment", treatments = treatments)) %>%
    # assign a drug_free_label to patients not medicated by drugs belonging to drug basket
    dplyr::mutate(BASKET_CATEGORY = ifelse(is.na(BASKET_CATEGORY), drug_free_label, BASKET_CATEGORY))

  #Each patient should have a unique row, no USUBJID duplication
  stopifnot(sum(duplicated(data$USUBJID))==0)

  return(data)
}

#' Build conMed profiles for a given study and a given drug basket
#'
#' @md
#' @param d input `data.frame` of clinical data with concomitant medications
#' @param drug_basket_name name of drug basket (will be searched - see details)
#' @param drug_basket_data_object drug basket input data
#' @param combo_drug_label_prefix prefix to use for COMBO records
#' @param combo_drug_label_suffix suffix to use for COMBO records
#' @param drug_free_label label to use for DRUG_FREE records
#' @param treatments vector of treatment names as found in the exposure domain (to distinguish from control)
#'
#' @importFrom utils data
#'
#' @details
#' if `drug_basket_data_object` is provided, an object with that name will be
#' fetched from the workspace / global environment. If this is not provided, but
#' `drug_basket_name` is, a basket with this name will be fetched from the package.
#'
#' This package stores a `basket_demo` object for demonstration.
#'
#' @return output `data.frame` of conmed profiles
#'
#' @export
generate_conmed_profiles <- function(d = NULL,
                                     drug_basket_name = NULL,
                                     drug_basket_data_object = NULL,
                                     combo_drug_label_prefix = NULL,
                                     combo_drug_label_suffix = NULL,
                                     drug_free_label = NULL,
                                     treatments = c("trt1")) {
  if (is.null(drug_basket_data_object)) {
    #load drug basket
    data(list = drug_basket_name, package = "ConMedClassify")
    drug_basket <- get(drug_basket_name, envir = parent.frame())
  } else {
    drug_basket_name <- drug_basket_data_object
    drug_basket <- get(drug_basket_data_object, envir = .GlobalEnv)
  }
  message(paste0("Drug basket loaded: ", drug_basket_name))

  # get safety population
  safety_pop <- get_safety_pop(d = d)

  message(paste0("Starting with ConMed Profiling ... "))

  # full ConMed profiling process on basket drug
  cm_profiles <- extract_data_for_cm_profiling(d = d,
                                               pop_of_interest = safety_pop) %>%
    process_treatment_days() %>%
    extract_basket_from_cm(drug_basket = drug_basket, unknownDoseCategoryLabel = "UNKDOSE") %>%
    remove_prior_cm_records() %>%
    impute_cm_days(treatments = treatments) %>%
    flag_uncertain_cm_records() %>%
    identify_combo_and_uncertain_cm(combo_drug_label_prefix = combo_drug_label_prefix,
                                    combo_drug_label_suffix = combo_drug_label_suffix,
                                    combo_drug_uncertain_timing_suffix = "UNKTIME",
                                    class_uncertain_timing_suffix = "UNKTIME",
                                    separator = "_") %>%
    compute_cm_overlaps(uncertain_basket_pattern = "UNKTIME") %>%
    complement_with_drug_free_pop(overall_pop = safety_pop,
                                  full_data = d,
                                  drug_free_label = drug_free_label,
                                  treatments = treatments)


  message(paste0("Finished with ConMed Profiling."))

  return(cm_profiles)
}

#' Extract all drug baskets profiles for a given study to a TSV file
#'
#' @md
#' @param d input `data.frame` of clinical data with concomitant medications
#' @param drug_baskets a `data.frame` of drug basket parameters, one per row. See details
#' @param drug_basket_data_objects a `list` of drug basket classifications, aligned to `drug_baskets`
#' @param output_dir directory to which results should be written. Should already exist
#' @param output_filename_prefix prefix to use for written files
#' @param treatments vector of treatment names as found in the exposure domain (to distinguish from control)
#'
#' @importFrom utils write.table
#'
#' @details
#' `drug_baskets` should be a `data.frame` containing one row for each basket, with columns
#' "drug_basket_name", "combo_drug_label", "drug_free_label".
#'
#' This package stores a `drug_basket` object for demonstration.
#'
#' @return nothing, used for the side-effect of writing results files
#'
#' @export
extract_conmed_profiles_to_tsv <- function(d = NULL,
                                           drug_baskets = NULL,
                                           drug_basket_data_objects = NULL,
                                           output_dir = "outputs",
                                           output_filename_prefix = "ConMedProfiles_",
                                           treatments = c("trt1")) {

  study_name <- unique(d$STUDYID)

  message("Initiating ConMed Profile extraction for ", study_name)
  for (i in seq_len(nrow(drug_baskets))) {
    tmp_profiles <- generate_conmed_profiles(d = d,
                                             drug_basket_name = drug_baskets$drug_basket_name[i],
                                             drug_basket_data_object = drug_basket_data_objects[[i]],
                                             combo_drug_label_prefix = drug_baskets$combo_drug_label[i],
                                             combo_drug_label_suffix = "COMBO",
                                             drug_free_label = drug_baskets$drug_free_label[i],
                                             treatments = treatments)

    utils::write.table(tmp_profiles,
                       file = paste0(paste(output_dir, output_filename_prefix, sep = "/"),
                                     study_name, "_",
                                     drug_baskets$drug_basket_name[i], ".tsv"),
                       sep = "\t",
                       row.names = FALSE)
  }
}


#' Merge all individual TSV files across studies into a single one for a given DrugBasket
#'
#' @md
#' @param input_tsv_path directory from which stored results should be loaded
#' @param tsv_pattern file pattern identifying files to load
#' @param basket_name basket name to use in aggregated file name
#' @param output_filename_prefix prefix to use in aggregated file name
#'
#' @importFrom utils read.delim2 write.table
#' @importFrom tidyselect one_of
#' @import dplyr
#'
#' @export
aggregate_conmed_profile_tsv <- function(input_tsv_path = NULL,
                                         tsv_pattern = NULL,
                                         basket_name = NULL,
                                         output_filename_prefix = NULL){

  files <- dir(path = input_tsv_path, pattern = tsv_pattern, full.names = TRUE)
  cm_profiles <- lapply(files, utils::read.delim2)
  cm_profiles_total <- do.call(rbind, cm_profiles)
  cm_profiles_total <- cm_profiles_total %>%
    dplyr::mutate(DRUG_BASKET = basket_name) %>%
    dplyr::select(tidyselect::one_of(c("DRUG_BASKET", "STUDYID", "USUBJID", "isTreatedPt", "BASKET_CATEGORY",
                                       "NB_CM_RECORDS", "NB_CMSTART_IMPUTED", "CMSTART_FIRST",
                                       "NB_CMEND_IMPUTED", "CMEND_LAST",
                                       "ndays_drug_os", "percent_drug_os",
                                       "ndays_drug_trt", "percent_drug_trt")))

  utils::write.table(cm_profiles_total,
                     file = paste0(output_filename_prefix, basket_name, paste0("_", format(Sys.Date(), "%Y%m%d"),".tsv")),
                     sep = "\t", row.names = FALSE)
}

#' Identify records by their treatment
#'
#' @md
#' @param d input `data.frame` of clinical data with concomitant medications
#' @param subset which treatment arm to consider - treatment vs control
#' @param treatments vector of treatment names as found in the exposure domain (to distinguish from control)
#'
#' @import dplyr
#'
#' @details
#' This function performs some cleanup - particular EXTRT records are considered as 'not treated'. )
#' Additionally, subject who missed all their doses are considered as 'not treated'.
#'
#' @return a `vector` of the subject IDs who receieved the given subset of treatments
#' @export
list_by_treatment <- function(d, subset = c("treatment", "control"), treatments = c("trt1")) {

  ex_data <- ex_records(d)

  subset <- match.arg(subset)
  all_treatments <- unique(ex_data$EXTRT)

  non_treatments <- c("PLACEBO", "MEAL", "DOSE", "")
  control_treatments <- setdiff(all_treatments, c(treatments, non_treatments))

  d <- ex_data %>%
    dplyr::filter(!USUBJID %in% find_MissedAllDosePatients(d)$USUBJID) %>%
    dplyr::group_by(USUBJID)

  d <- switch(subset,
              treatment = dplyr::filter(d, any(EXTRT %in% treatments)),
              control = dplyr::filter(d, !any(EXTRT %in% treatments) & any(EXTRT %in% control_treatments)))

  d %>%
    dplyr::pull(USUBJID) %>%
    unique()

}

#' Identify subjects who missed all doses of their treatment
#'
#' @md
#' @param d input `data.frame` of clinical data with concomitant medications
#'
#' @import dplyr
#'
#' @return a `data.frame` of the filtered data
#' @export
find_MissedAllDosePatients <- function(d) {

  d %>%
    unnest_domain(nested.EX) %>%
    # some records list the overall dosage
    dplyr::filter(AVISIT != "OVERALL") %>%
    dplyr::filter(!grepl("not treated", ACTARM, ignore.case = TRUE)) %>% # remove NOT TREATED
    # some records indicate the dosage with no date indicating
    # that the exposure did not occur
    dplyr::group_by(USUBJID) %>%
    dplyr::summarise(hadTreatment = any(EXOCCUR == "Y")) %>%
    dplyr::filter(!hadTreatment) %>%
    dplyr::distinct()

}

#' Extract records from nested.CM domain
#'
#' @md
#' @param d input `data.frame` of clinical data with concomitant medications
#'
#' @import dplyr
#' @importFrom tidyselect one_of
#'
#' @details
#' Additionally filters to concomitant medication records according to `CMCAT`
#' and removes 'not treated' records.
#'
#' @return a `data.frame` of concomitant medication records
#' @export
cm_records <- function(d = NULL) {

  cmnames <- c("STUDYID", "USUBJID", "EOSDY",
               "CMDECOD", "CMCAT", "CMCLAS", "CMDOSE", "CMDOSU",
               "CMSTDY", "CMENDY", "CMENRTPT", "CMROUTE", "CMINDC")

  !is.null(d) && length(d) > 0 || stop("Required parameter: d", call. = FALSE)

  d %>%
    unnest_domain(nested.CM) %>%
    dplyr::filter(CMCAT %in% c("CONCOMITANT", "CONCOMITANT MEDICATION", "CONCOMITANT MEDICATIONS", "PREV/CONCOM TREATMENT")) %>%
    dplyr::filter(!grepl("not treated", ACTARM, ignore.case = TRUE)) %>% # remove NOT TREATED
    dplyr::select(tidyselect::one_of(cmnames)) %>%
    dplyr::distinct()
}


#' Extract records from nested.EX domain
#'
#' @md
#' @param d input `data.frame` of clinical data with concomitant medications
#'
#' @import dplyr
#' @importFrom tidyselect one_of
#'
#' @details
#' Additionally, filters out `AVISIT == OVERALL` records and 'not treated' subjects,
#' and records where `EXOCCUR != "Y"`.
#'
#' @return a `data.frame` of exposure records
#' @export
ex_records <- function(d = NULL) {

  cmnames <- c("STUDYID", "USUBJID", "USUBJID",
               "EXTRT", "EXDOSE", "EXDOSU", "EXOCCUR",
               "VISIT", "AVISIT", "EXSTDY", "EXENDY")

  !is.null(d) && length(d) > 0 || stop("Required parameter: d", call. = FALSE)

  d %>%
    unnest_domain(nested.EX) %>%
    # some records list the overall dosage
    dplyr::filter(AVISIT != "OVERALL") %>%
    dplyr::filter(!grepl("not treated", ACTARM, ignore.case = TRUE)) %>% # remove NOT TREATED
    # some records indicate the dosage with no date indicating
    # that the exposure did not occur
    dplyr::filter(EXOCCUR == "Y") %>%
    dplyr::filter(!is.na(EXTRT)) %>%
    dplyr::select(tidyselect::one_of(cmnames)) %>%
    dplyr::mutate(DOSAGE = paste0(EXDOSE, EXDOSU)) %>%
    dplyr::distinct()
}
