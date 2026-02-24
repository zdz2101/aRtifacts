.core50_make_domain <- function(name, df) {
  out <- df
  out$.ROWID <- seq_len(nrow(out))
  new_domain_snapshot(name, out, key_vars = ".ROWID")
}

.core50_fixture_artifacts <- function() {
  adsl_prev <- data.frame(
    STUDYID = c("S1", "S1", "S1"),
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    SITEID = c("SITE-01", "SITE-01", "SITE-02"),
    SEX = c("M", "F", "F"),
    PREGNANT = c("N", "Y", "N"),
    AGE = c(34, 52, 47),
    RANDDT = c("2024-01-01", "2024-01-02", "2024-01-03"),
    RANDTM = c("08:00", "09:00", "10:00"),
    TRTSDT = c("2024-01-05", "2024-01-06", "2024-01-07"),
    TRTEDT = c("2024-02-05", "2024-02-06", "2024-02-07"),
    SAFFL = c("Y", "Y", "Y"),
    ADURN = c(31, 31, 31),
    TRT01A = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )

  adsl_cur <- data.frame(
    STUDYID = c("S1", "S1", "S1", "S1", "S1"),
    USUBJID = c("SUBJ-001", "SUBJ-001", "SUBJ-002", "SUBJ-004", ""),
    SITEID = c("SITE-01", "SITE-01", "SITE-01", "SITE-02", "SITE-01"),
    SEX = c("M", "X", "F", "M", "M"),
    PREGNANT = c("N", "Y", "Y", "N", "Y"),
    AGE = c(NA, 999, 52, 29, 40),
    RANDDT = c("2024/01/01", "2024-01-01", "", "2024-01-04", "2024-01-05"),
    RANDTM = c("08:00", "08:30", "09:00", "11:00", "12:00"),
    TRTSDT = c("2024-03-01", "2024-01-05", "", "2024-01-08", "2024-01-09"),
    TRTEDT = c("2024-01-01", "2024-02-05", "2024-02-06", "2024-02-08", "2024-02-09"),
    SAFFL = c("Y", "Y", "Y", "Y", "Y"),
    ADURN = c(-3, 31, 31, 31, 31),
    TRT01A = c("B", "B", "B", "A", "A"),
    stringsAsFactors = FALSE
  )

  dm_prev <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    SITEID = c("SITE-01", "SITE-01", "SITE-02"),
    SEX = c("M", "F", "F"),
    RACE = c("WHITE", "ASIAN", "BLACK"),
    AGE = c(34, 52, 47),
    stringsAsFactors = FALSE
  )

  dm_cur <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    SITEID = c("SITE-01", "SITE-01"),
    SEX = c("F", "F"),
    RACE = c("WHITE", "ASIAN"),
    AGE = c(34, 52),
    stringsAsFactors = FALSE
  )

  ae_prev <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    AESEQ = c(1, 1, 1),
    AESTDTC = c("2024-01-10", "2024-01-11", "2024-01-12"),
    AEENDTC = c("2024-01-12", "2024-01-13", "2024-01-14"),
    AETERM = c("HEADACHE", "NAUSEA", "FATIGUE"),
    AESEV = c("MILD", "MODERATE", "SEVERE"),
    VISIT = c("BASELINE", "WEEK1", "WEEK1"),
    SITEID = c("SITE-01", "SITE-01", "SITE-02"),
    stringsAsFactors = FALSE
  )

  ae_cur <- rbind(
    data.frame(
      USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003", "SUBJ-004"),
      AESEQ = c(99, 1, 1, 1),
      AESTDTC = c("2024-01-10", "2024-01-11", "2024-01-12", "2025-01-15"),
      AEENDTC = c("2024-01-12", "2024-01-13", "2024-01-14", "2025-01-16"),
      AETERM = c("HEADACHE", "NAUSEA", "FATIGUE", "DIZZINESS"),
      AESEV = c("SEVERE", "MODERATE", "SEVERE", "MILD"),
      VISIT = c("BASELINE", "WEEK1", "WEEK1", "WEEK2"),
      SITEID = c("SITE-01", "SITE-01", "SITE-02", "SITE-02"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      USUBJID = rep("SUBJ-001", 25),
      AESEQ = 2:26,
      AESTDTC = rep("2024-01-20", 25),
      AEENDTC = rep("2024-01-21", 25),
      AETERM = rep("HEADACHE", 25),
      AESEV = rep("SEVERE", 25),
      VISIT = rep("WEEK2", 25),
      SITEID = rep("SITE-01", 25),
      stringsAsFactors = FALSE
    )
  )

  ex_prev <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    EXSEQ = c(1, 1, 1),
    EXSTDTC = c("2024-01-05", "2024-01-06", "2024-01-07"),
    EXENDTC = c("2024-01-20", "2024-01-21", "2024-01-22"),
    EXDOSE = c(10, 20, 15),
    VISIT = c("BASELINE", "WEEK1", "WEEK1"),
    SITEID = c("SITE-01", "SITE-01", "SITE-02"),
    stringsAsFactors = FALSE
  )

  ex_cur <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-005"),
    EXSEQ = c(1, 1, 1),
    EXSTDTC = c("2024-01-05", "2024-01-06", "2024-01-08"),
    EXENDTC = c("2024-01-10", "2024-01-21", "2024-01-09"),
    EXDOSE = c(30, 20, 5),
    VISIT = c("BASELINE", "WEEK1", "WEEK2"),
    SITEID = c("SITE-01", "SITE-01", "SITE-02"),
    stringsAsFactors = FALSE
  )

  lb_prev <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    LBSEQ = c(1, 1, 1),
    LBTESTCD = c("ALT", "ALT", "ALT"),
    LBSTRESN = c(22, 40, 35),
    LBDTC = c("2024-01-10", "2024-01-11", "2024-01-12"),
    LBTM = c("08:30", "08:45", "09:00"),
    VISIT = c("BASELINE", "WEEK1", "WEEK1"),
    SITEID = c("SITE-01", "SITE-01", "SITE-02"),
    LBSTAT = c("", "", ""),
    LBOCCUR = c("Y", "Y", "Y"),
    LBDUR = c(0, 0, 0),
    stringsAsFactors = FALSE
  )

  lb_cur <- rbind(
    data.frame(
      USUBJID = c("SUBJ-001", "SUBJ-001", "SUBJ-001", "SUBJ-001", "SUBJ-002", "SUBJ-003", "SUBJ-004"),
      LBSEQ = c(1, 2, 3, 4, 1, 1, 1),
      LBTESTCD = c("ALT", "ALT", "ALT", "ALT", "ALT", "ALT", "ALT"),
      LBSTRESN = c(500, 500, 500, 500, 40, 35, 600),
      LBDTC = c("2024-01-10", "2024-01-10", "2024-01-10", "2024-01-10", "2024-01-11", "2024-01-12", "2024-01-15"),
      LBTM = c("08:30", "08:31", "08:32", "08:33", "08:45", "09:00", "09:15"),
      VISIT = c("BASELINE", "BASELINE", "BASELINE", "BASELINE", "WEEK99", "WEEK1", "WEEK2"),
      SITEID = c("SITE-01", "SITE-01", "SITE-01", "SITE-01", "SITE-01", "SITE-02", "SITE-02"),
      LBSTAT = c("", "", "", "", "", "", ""),
      LBOCCUR = c("Y", "Y", "Y", "Y", "Y", "Y", "Y"),
      LBDUR = c(0, 0, 0, 0, 0, 0, 0),
      stringsAsFactors = FALSE
    )
  )

  vs_prev <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    VISIT = c("BASELINE", "WEEK1", "WEEK1"),
    VSTESTCD = c("SYSBP", "SYSBP", "SYSBP"),
    VSSTRESN = c(120, 132, 128),
    SITEID = c("SITE-01", "SITE-01", "SITE-02"),
    stringsAsFactors = FALSE
  )

  vs_cur <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-004"),
    VISIT = c("BASELINE", "WEEK1", "WEEK2"),
    VSTESTCD = c("SYSBP", "SYSBP", "SYSBP"),
    VSSTRESN = c(120, 132, 140),
    SITEID = c("SITE-01", "SITE-01", "SITE-02"),
    stringsAsFactors = FALSE
  )

  eg_prev <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    EGSEQ = c(1, 1, 1),
    EGTESTCD = c("QT", "QT", "QT"),
    EGSTRESN = c(420, 430, 415),
    VISIT = c("BASELINE", "WEEK1", "WEEK1"),
    SITEID = c("SITE-01", "SITE-01", "SITE-02"),
    stringsAsFactors = FALSE
  )

  eg_cur <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-001", "SUBJ-001", "SUBJ-004"),
    EGSEQ = c(1, 2, 3, 1),
    EGTESTCD = c("QT", "QT", "QT", "QT"),
    EGSTRESN = c(420, 430, 440, 450),
    VISIT = c("BASELINE", "WEEK1", "WEEK2", "WEEK2"),
    SITEID = c("SITE-01", "SITE-01", "SITE-01", "SITE-02"),
    stringsAsFactors = FALSE
  )

  cm_prev <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    CMSEQ = c(1, 1, 1),
    CMSTDTC = c("2024-01-10", "2024-01-11", "2024-01-12"),
    CMENDTC = c("2024-01-12", "2024-01-13", "2024-01-14"),
    VISIT = c("BASELINE", "WEEK1", "WEEK1"),
    stringsAsFactors = FALSE
  )

  cm_cur <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-004"),
    CMSEQ = c(1, 1, 1),
    CMSTDTC = c("2026-01-01", "2024-01-11", "2025-02-01"),
    CMENDTC = c("2026-01-05", "2024-01-13", "2025-02-05"),
    VISIT = c("WEEK2", "WEEK1", "WEEK2"),
    stringsAsFactors = FALSE
  )

  prev <- new_artifact_snapshot(domains = list(
    ADSL = .core50_make_domain("ADSL", adsl_prev),
    DM = .core50_make_domain("DM", dm_prev),
    AE = .core50_make_domain("AE", ae_prev),
    EX = .core50_make_domain("EX", ex_prev),
    LB = .core50_make_domain("LB", lb_prev),
    VS = .core50_make_domain("VS", vs_prev),
    EG = .core50_make_domain("EG", eg_prev),
    CM = .core50_make_domain("CM", cm_prev)
  ))

  current <- new_artifact_snapshot(domains = list(
    ADSL = .core50_make_domain("ADSL", adsl_cur),
    DM = .core50_make_domain("DM", dm_cur),
    AE = .core50_make_domain("AE", ae_cur),
    EX = .core50_make_domain("EX", ex_cur),
    LB = .core50_make_domain("LB", lb_cur),
    VS = .core50_make_domain("VS", vs_cur),
    EG = .core50_make_domain("EG", eg_cur),
    CM = .core50_make_domain("CM", cm_cur)
  ))

  list(current = current, previous = prev)
}
