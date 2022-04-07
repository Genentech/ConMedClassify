adsl <- data.frame(
  STUDYID = "trial1",
  USUBJID = paste0("trial1-site1-subject", 1:4),
  ACTARM = paste0("arm", c(1, 1, 2, 2)),
  SEX = c("M", "F", "M", "F"),
  AGE = c(30, 50, 54, 71),
  TRTSDT = "2020-01-01",
  TRTEDT = "2020-02-20",
  SAFFL = "Y"
)

## profile: classX
## 20 TABLET should convert to 20mg
cm1 <- data.frame(
  STUDYID = "trial1",
  USUBJID = "trial1-site1-subject1",
  EOSDY = 50,
  CMDECOD = c("drugA", "drugA/miscA", "miscC", "drugA"),
  CMCAT = "CONCOMITANT MEDICATION",
  CMCLAS = "clasw",
  CMDOSE = c(20, 10, 15, 1),
  CMDOSU = c("TABLET", "mg", "mg", "TABLET"),
  CMSTDY = c(5, 6, 10, 12),
  CMENDY = c(10, 8, 15, 20),
  CMENRTPT = "ONGOING",
  CMSTRTPT = "BEFORE",
  CMSTTPT = "SCREENING",
  CMROUTE = "ORAL",
  CMINDC = c("pain", "nausea", "allergy", "pain")
)

## profile: demo_COMBO (drugA + drugB)
cm2 <- data.frame(
  STUDYID = "trial1",
  USUBJID = "trial1-site1-subject2",
  EOSDY = 30,
  CMDECOD = c("drugA", "drugA/miscA", "miscC", "drugB"),
  CMCAT = "CONCOMITANT MEDICATION",
  CMCLAS = "clasw",
  CMDOSE = c(1, 10, 15, 1),
  CMDOSU = c("TABLET", "mg", "mg", "TABLET"),
  CMSTDY = c(5, 6, 10, 12),
  CMENDY = c(10, 8, 15, 20),
  CMENRTPT = "ONGOING",
  CMSTRTPT = "BEFORE",
  CMSTTPT = "SCREENING",
  CMROUTE = "ORAL",
  CMINDC = c("pain", "nausea", "allergy", "pain")
)

## profile: classZ
cm3 <- data.frame(
  STUDYID = "trial1",
  USUBJID = "trial1-site1-subject3",
  EOSDY = 30,
  CMDECOD = c("drugC", "miscA/drugC", "miscC", "drugC"),
  CMCAT = "CONCOMITANT MEDICATION",
  CMCLAS = "clasw",
  CMDOSE = c(1, 10, 15, 1),
  CMDOSU = c("TABLET", "mg", "mg", "TABLET"),
  CMSTDY = c(5, 6, 10, 12),
  CMENDY = c(10, 8, 15, 20),
  CMENRTPT = "ONGOING",
  CMSTRTPT = "BEFORE",
  CMSTTPT = "SCREENING",
  CMROUTE = "ORAL",
  CMINDC = c("pain", "nausea", "allergy", "pain")
)

## profile: basket_free
cm4 <- data.frame(
  STUDYID = "trial1",
  USUBJID = "trial1-site1-subject4",
  EOSDY = 30,
  CMDECOD = c("drugD", "miscA/drugD", "miscC", "drugD"),
  CMCAT = "CONCOMITANT MEDICATION",
  CMCLAS = "clasw",
  CMDOSE = c(1, 10, 15, 1),
  CMDOSU = c("TABLET", "mg", "mg", "TABLET"),
  CMSTDY = c(5, 6, 10, 12),
  CMENDY = c(10, 8, 15, 20),
  CMENRTPT = "ONGOING",
  CMSTRTPT = "BEFORE",
  CMSTTPT = "SCREENING",
  CMROUTE = "ORAL",
  CMINDC = c("pain", "nausea", "allergy", "pain")
)
cm <- dplyr::bind_rows(cm1, cm2, cm3, cm4)

ex <- data.frame(
  STUDYID = "trial1",
  USUBJID = paste0("trial1-site1-subject", 1:4),
  EXTRT = paste0("trt", c(1, 1, 2, 2)),
  EXDOSE = c(100, 100, 200, 200),
  EXDOSU = "units",
  EXOCCUR = "Y",
  VISIT = "BASELINE",
  AVISIT = "BASELINE",
  EXSTDY = 1,
  EXENDY = 100
)

tte <- data.frame(
  STUDYID = "trial1",
  USUBJID = paste0("trial1-site1-subject", 1:4),
  PARAMCD = "OS",
  AVAL = c(150, 200, 250, 100),
  CNSR = c(1, 0, 1, 1)
)

clinical_data <- dplyr::as_tibble(adsl)
clinical_data <- dplyr::left_join(clinical_data, tidyr::nest(cm, nested.CM = setdiff(names(cm), c("STUDYID", "USUBJID"))))
clinical_data <- dplyr::left_join(clinical_data, tidyr::nest(tte, nested.TTE = setdiff(names(tte), c("STUDYID", "USUBJID"))))
clinical_data <- dplyr::left_join(clinical_data, tidyr::nest(ex, nested.EX = setdiff(names(ex), c("STUDYID", "USUBJID"))))

## drugA : classX
## drugB : classY
## drugC : classZ
## miscA : unrelated
## miscB : unrelated
## miscC : unrelated
basket_demo <- data.frame(
  CMDECOD = c("drugA", "drugA/miscA", "drugA/miscB", "drugB", "drugB/miscB/miscC", "drugC"),
  CLASS = c("classX", "classX", "classX", "classY", "classY", "classZ")
)

# # demo drug basket definition
drug_baskets <- tibble::tribble(
  ~drug_basket_name,        ~combo_drug_label,    ~drug_free_label,
  "basket_demo",            "demo",               "demo_FREE"
)
