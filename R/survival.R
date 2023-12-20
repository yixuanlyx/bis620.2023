#' survival analysis
#' @param final an object inherited from list.
#' @return survival analysis plot and summary statistics
#' @importFrom dplyr select
#' @importFrom survival Surv survfit coxph
#' @importFrom survminer ggsurvplot pairwise_survdiff
#' @export

# AGE, SEX, B_WEIGHT, marker, AESEVCD

survATRT_SEX<-function(final){
 dt_SEX<-final |> select(SUBJID, DTHDY, DTH, ATRT, SEX)
 dt_SEX$arm = paste(dt_SEX$SEX, dt_SEX$ATRT, sep = ", ")
 fit_ATRT_SEX=survfit(Surv(DTHDY, DTH)~arm, data= dt_SEX)
 print(fit_ATRT_SEX)
 ggsurvplot(fit_ATRT_SEX, data = dt_SEX)
 fit_sex = coxph(Surv(DTHDY, DTH) ~ arm, data = dt_SEX)
 summary(fit_sex)
}

survATRT_AGE<-function(final){
  dt_AGE<-final |> select(SUBJID, DTHDY, DTH, ATRT, AGE)
  AGE_CAT<-c(1:length(dt_AGE$AGE))
  for (i in 1:length(dt_AGE$AGE)) {
    if (dt_AGE$AGE[i] <40) {
      AGE_CAT[i] <- "age=[25, 40)"
    }
    if (dt_AGE$AGE[i] >=40 & dt_AGE$AGE[i] <60) {
      AGE_CAT[i] <- "age=[40, 60)"
    }
    if (dt_AGE$AGE[i] >=60 & dt_AGE$AGE[i] <= 85) {
      AGE_CAT[i] <- "age=[60, 85]"
    }
  }
  dt_AGE$AGE_CAT<-AGE_CAT
  dt_AGE$arm = paste(dt_AGE$AGE_CAT, dt_AGE$ATRT, sep = ", ")
  fit_ATRT_AGE=survfit(Surv(DTHDY, DTH)~arm, data= dt_AGE)
  print(fit_ATRT_AGE)
  ggsurvplot(fit_ATRT_AGE, data = dt_AGE)
  fit_age = coxph(Surv(DTHDY, DTH) ~ arm, data = dt_AGE)
  summary(fit_age)
}

survATRT_WEIGHT<-function(final){
  dt_WEIGHT<-final |> select(SUBJID, DTHDY, DTH, ATRT,B_WEIGHT)
  WEIGHT_CAT<-c(1:length(dt_WEIGHT$B_WEIGHT))
  for (i in 1:length(dt_WEIGHT$B_WEIGHT)) {
    if (dt_WEIGHT$B_WEIGHT[i] <50) {
      WEIGHT_CAT[i] <- "weight=[35, 50)"
    }
    if (dt_WEIGHT$B_WEIGHT[i] >=50 & dt_WEIGHT$B_WEIGHT[i] <75) {
      WEIGHT_CAT[i] <- "weight=[50, 75)"
    }
    if (dt_WEIGHT$B_WEIGHT[i] >=75 & dt_WEIGHT$B_WEIGHT[i] < 100) {
      WEIGHT_CAT[i] <- "weight=[75, 100)"}
    if (dt_WEIGHT$B_WEIGHT[i] >= 100) {
      WEIGHT_CAT[i] <- "weight=[100, 165)"
    }
  }
  dt_WEIGHT$WEIGHT_CAT<-WEIGHT_CAT
  dt_WEIGHT$arm = paste(dt_WEIGHT$WEIGHT_CAT, dt_WEIGHT$ATRT, sep = ", ")
  fit_ATRT_WEIGHT=survfit(Surv(DTHDY, DTH)~arm, data= dt_WEIGHT)
  print(fit_ATRT_WEIGHT)
  ggsurvplot(fit_ATRT_WEIGHT, data = dt_WEIGHT)
  fit_w = coxph(Surv(DTHDY, DTH) ~ arm, data = dt_WEIGHT)
  summary(fit_w)
}

survATRT_marker<-function(final){
  dt_marker<-final |> select(SUBJID, DTHDY, DTH, ATRT, marker)
  dt_marker$arm = paste(dt_marker$marker, dt_marker$ATRT, sep = ", ")
  fit_ATRT_marker=survfit(Surv(DTHDY, DTH)~arm, data= dt_marker)
  print(fit_ATRT_marker)
  ggsurvplot(fit_ATRT_marker, data = dt_marker)
  fit_m = coxph(Surv(DTHDY, DTH) ~ arm, data = dt_marker)
  summary(fit_m)
}

survATRT_AESEVCD<-function(final){
  dt_AESEVCD<-final |> select(SUBJID, DTHDY, DTH, ATRT, AESEVCD)
  dt_AESEVCD$arm = paste(dt_AESEVCD$AESEVCD, dt_AESEVCD$ATRT, sep = ", ")
  fit_ATRT_AESEVCD=survfit(Surv(DTHDY, DTH)~arm, data= dt_AESEVCD)
  print(fit_ATRT_AESEVCD)
  ggsurvplot(fit_ATRT_AESEVCD, data = dt_AESEVCD)
  fit_ad = coxph(Surv(DTHDY, DTH) ~ arm, data = dt_AESEVCD)
  summary(fit_ad)
}
