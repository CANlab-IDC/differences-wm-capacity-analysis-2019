excluded_subjects_ <- c("102", "108", "112", "116", "125", "127", "239", "241", "242", "244", "222", "250")
digit_span_ <- read.csv("trial_labels_digit_span_incorrect.csv", header = T, col.names = c("TRIAL_ID"))
practice_trials <- factor(c(1, 2, 3, 4, 37, 38, 39, 40))

make_lldb <- function(.file, .name = NULL, .path = ".") {
  .name <- ifelse(is.null(.name), sub("^.*?/?(\\w*).RData$", "\\1", .file, perl = T), .name)
  # convert .RData -> .rdb/.rdx
  e = local({load(file.path(.path, .file)); environment()})
  tools:::makeLazyLoadDB(e, .name)
}

load_project_libs <- function() {
  if (sum(is(try(find.package("knitr"), silent = T)) == "try-error")) install.packages("knitr", "rmarkdown")
  library("knitr")
  if (sum(is(try(find.package("zeallot"), silent = T)) == "try-error")) install.packages("zeallot")
  library("zeallot")
  if (sum(is(try(find.package("codetools"), silent = T)) == "try-error")) install.packages("codetools")
  if (sum(is(try(find.package("quantreg"), silent = T)) == "try-error")) install.packages("quantreg")
  if (sum(is(try(find.package("tidyverse"), silent = T)) == "try-error")) install.packages("tidyverse")
  library("magrittr")
  library("tidyverse")
  
  if (sum(is(try(find.package("zoo"), silent = T)) == "try-error")) install.packages("zoo")
  #library("zoo")
  
  if (sum(is(try(find.package("glue"), silent = T)) == "try-error")) install.packages("glue")
  if (sum(is(try(find.package("styler"), silent = T)) == "try-error")) install.packages("styler")
  if (sum(is(try(find.package("lintr"), silent = T)) == "try-error")) install.packages("lintr")
  if (sum(is(try(find.package("lavaan"), silent = T)) == "try-error")) install.packages("lavaan")
  
  if (sum(is(try(find.package("devtools"), silent = T)) == "try-error")) install.packages("devtools")
  # library("devtools")
  
  if (sum(is(try(find.package("hrbrthemes"), silent = T)) == "try-error")) install.packages("hrbrthemes")
  if (sum(is(try(find.package("numDeriv"), silent = T)) == "try-error")) install.packages("numDeriv")
  
  if (sum(is(try(find.package("viridis"), silent = T)) == "try-error")) install.packages("viridis")
  if (sum(is(try(find.package("ggedit"), silent = T)) == "try-error")) install.packages("ggedit")
  #library("ggedit")
  
  if (sum(is(try(find.package("gridExtra"), silent = T)) == "try-error")) install.packages("gridExtra")
  #library("gridExtra")

  if (sum(is(try(find.package("plotly"), silent = T)) == "try-error")) install.packages("plotly")
  if (sum(is(try(find.package("padr"), silent = T)) == "try-error")) install.packages("padr")
  #library("padr")
  
  if (sum(is(try(find.package("lambda.r"), silent = T)) == "try-error")) install.packages("lambda.r")
  #library("lambda.r")
  
  if (sum(is(try(find.package("VWPre"), silent = T)) == "try-error")) install.packages("VWPre")
  library("VWPre")
  
  if (sum(is(try(find.package("nlme"), silent = T)) == "try-error")) install.packages("nlme")
  
  if (sum(is(try(find.package("glmmADMB"), silent = T)) == "try-error")) install.packages("glmmADMB")
  if (sum(is(try(find.package("glmmTMB"), silent = T)) == "try-error")) install.packages("glmmTMB")
  if (sum(is(try(find.package("glmmBUGS"), silent = T)) == "try-error")) install.packages("glmmBUGS")
  if (sum(is(try(find.package("car"), silent = T)) == "try-error")) install.packages("car")
  if (sum(is(try(find.package("lme4"), silent = T)) == "try-error")) install.packages("lme4")
  library("lme4")
  
  # replace the string below with your desired working directory
  setwd("~/dev/CANlab-IDC/gal-matan-analysis/")
  
  source("./functions.R", local = T, echo = F)
}

############################ DATA PREPARATION FUNCTIONS ############################

add_trial_id <- function(.data) {
  .data %>%
    mutate(TRIAL_ID = paste(Subject, TRIAL_INDEX, sep = ":"))
}

add_time_ranks <- function(.data, time_col = "Time") {
  .data %>%
    mutate(Time.pct_rk = percent_rank(Time), Time.min_rk = min_rank(Time)) %>%
    arrange(Time)
}

find_keybd_selections <- function(.data) {
  .data %>%
    filter(grepl("KEYBD_SELECT", SAMPLE_MESSAGE)) %>%
    select(TRIAL_ID)
}

find_touches <- function(.data) {
  .data %>%
    select(TRIAL_ID, Subject, TRIAL_INDEX, Time, SAMPLE_MESSAGE, condition, load, wmcap, LEFT_INTEREST_AREA_LABEL, RIGHT_INTEREST_AREA_LABEL, IA_LABEL) %>%
    filter(grepl("TOUCH_", SAMPLE_MESSAGE)) %>%
    mutate(Touched = sub(".*TOUCH_(\\w*);?.*", "\\1", SAMPLE_MESSAGE)) %>%
    add_time_ranks()
}

find_selections <- function(.data) {
  .data %>%
    select(TRIAL_ID, Subject, TRIAL_INDEX, Time, SAMPLE_MESSAGE, condition, load, wmcap, LEFT_INTEREST_AREA_LABEL, RIGHT_INTEREST_AREA_LABEL, IA_LABEL) %>%
    filter(grepl("SELECTION", SAMPLE_MESSAGE)) %>%
    mutate(Correct = grepl("SELECTION_CORRECT", SAMPLE_MESSAGE)) %>%
    add_time_ranks()
}

findIncorrectSelections <- function(.data) {
  .data %>%
    find_selections() %>%
    filter(!Correct) %>%
    add_time_ranks()
}

find_correct_selections <- function(.data) {
  .data %>%
    find_selections() %>%
    filter(Correct) %>%
    add_time_ranks()
}

find_correct_crit_selections <- function(.data) {
  .data %>%
    find_correct_selections() %>%
    filter(condition != 0) %>%
    add_time_ranks()
}

add_exclude_columns <- function(.data, digit_span = digit_span_, excluded_subjects = excluded_subjects_) {
  inc_dig_ids <- digit_span %>% pull(TRIAL_ID)
  inc_sel_ids <- .data %>% findIncorrectSelections() %>% pull(TRIAL_ID)
  kbd_sel_ids <- .data %>% find_keybd_selections() %>% pull(TRIAL_ID)
  return(
    .data %>%
      add_trial_id() %>%
      mutate(
        is_practice_trial = factor(TRIAL_INDEX) %in% practice_trials,
        is_incorrect_trial = paste(Subject, TRIAL_INDEX, sep = ":") %in% inc_sel_ids,
        is_incorrect_digit = paste(Subject, TRIAL_INDEX, sep = ":") %in% inc_dig_ids,
        is_keyboard_select = paste(Subject, TRIAL_INDEX, sep = ":") %in% kbd_sel_ids,
        is_exclude_subject = Subject %in% excluded_subjects
      )
  )
}

exclude_filler <- function(.data) {
  .data %>% filter(critical == "y")
}

exclude_practice <- function(.data) {
  .data %>% filter(!is_practice_trial)
}

exclude_inc_sel <- function(.data) {
  .data %>% filter(!is_incorrect_trial)
}

exclude_inc_dig <- function(.data) {
  .data %>% filter(!is_incorrect_digit)
}

exclude_kbd_sel <- function(.data) {
  .data %>% filter(!is_keyboard_select)
}

exclude_bad_sub <- function(.data) {
  .data %>% filter(!is_exclude_subject)
}

exclude_all <- function(.data) {
  .data %>%
    add_exclude_columns() %>%
    exclude_filler() %>%
    exclude_practice() %>%
    exclude_inc_sel() %>%
    exclude_inc_dig() %>%
    exclude_kbd_sel() %>%
    exclude_bad_sub()
}

keep_useful_cols <- function(.data) {
  .data %>%
    select(
      Subject,
      TRIAL_INDEX,
      SAMPLE_INDEX,
      SAMPLE_MESSAGE,
      TIMESTAMP,
      LAST_Position,
      condition,
      Item,
      critical,
      distractor,
      filler_1,
      filler_2,
      load,
      version,
      withha_onset,
      withoutha_onset,
      Event,
      Time,
      EyeRecorded,
      EyeSelected,
      IA_ID,
      IA_LABEL,
      IA_Data
    )
}

collect_trial_stats <- function(.data) {
  .data %>%
    group_by(Subject, TRIAL_INDEX) %>%
    summarize(
      TrialSampleLength = n(),
      TrialLength = max(Time)
    ) %>%
    ungroup() %>%
    merge(y = .data %>% find_selections() %>% add_exclude_columns() %>% rename_at(vars(contains("Time")), function(t) paste("Selection.", t, sep = "")), by = c("Subject", "TRIAL_INDEX")) %>%
    merge(y = .data %>% find_touches() %>% select(Subject, TRIAL_INDEX, contains("Time")) %>% rename_at(vars(contains("Time")), function(t) paste("Touch.", t, sep = "")), by = c("Subject", "TRIAL_INDEX"))
}

trim_to_selection <- function(.data) {
  .data %>%
    filter(Time <= Selection.Time)
}

mutate_rows <- function(.data, .p, ...) {
  .p <- rlang::enquo(.p)
  .p_lgl <- rlang::eval_tidy(.p, .data)
  .data[.p_lgl, ] <- .data[.p_lgl, ] %>% mutate(...)
  .data
}

pad_trials <- function(.data, start_val = -200, end_val = 3500, step = 2, .fill_type = "Target") {
  .data %>%
    group_by(Subject, TRIAL_INDEX) %>%
    arrange(Time) %>%
    pad_int(
      by = "Time",
      start_val = start_val,
      end_val = end_val,
      step = step
    ) %>%
    filter(Time < end_val) %>%
    (switch(
      .fill_type,
      Target = (
        . %>%
          replace_na(IA_ID = 1, IA_LABEL = factor("Target"), IA_Data = factor("Contains_IA_Looks")) %>%
          fill(everything())
      ),
      LOCF = (. %>% fill(everything()))
    )) %>%
    ungroup()
}

############################ MODEL UTILITY FUNCTIONS ############################

scale_dat <- function(.data) {
  .data %>%
    mutate_if(is.numeric, funs(scale(., center = TRUE, scale = max(., rm.na = TRUE) / 100)))
}

# add_ot_cols_trad <- function(.data, .max_deg = 3) {
#   .data$timeBin <- dense_rank(.data$Time)
#   .ot <- poly(unique(.data$timeBin), .max_deg)
#   .data[, paste("ot", 1:.max_deg, sep = ".")] <- t[.data$timeBin, 1:.max_deg]
#   .data
# }

add_ot_cols <- function(.data, .max_deg = 3) {
  .data <- .data %>% mutate(TimeBin = dense_rank(Time))
  .ot <- as_tibble(poly(unique(.data$TimeBin), .max_deg)) %>%
    nest(!!!paste0(1:.max_deg), .key = "ot") %>%
    unnest(.sep = ".") %>%
    rownames_to_column() %>%
    mutate(rowname = as.numeric(rowname))

  .data %>% left_join(.ot, by = c("TimeBin" = "rowname"))
}

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

defaultControl <- list(
  algorithm = "NLOPT_LN_BOBYQA",
  xtol_rel = 1e-6,
  maxeval = 1e5,
  print_level = 1,
  ftol_abs = 1e-6,
  maxfun = 1e5,
  maxit = 1e5,
  epsilon = 1e-8,
  trace = TRUE
)

nloptwrap2 <- function(fn, par, lower, upper, control = list(), ...) {
  for (n in names(defaultControl))
    if (is.null(control[[n]])) control[[n]] <- defaultControl[[n]]
  res <- nloptr::nloptr(x0 = par, eval_f = fn, lb = lower, ub = upper, opts = control, ...)
  with(
    res,
    list(
      par = solution,
      fval = objective,
      feval = iterations,
      conv = if (status > 0) 0 else status,
      message = message
    )
  )
}

lm.beta.lmer <- function(mod) {
  b <- fixef(mod)[-1]                      ## fixed-effect coefs, sans intercept
  sd.x <- apply(getME(mod, "X")[,-1],2,sd) ## pull out model (design) matrix,
                                           ## drop intercept column, calculate
                                           ## sd of remaining columns
  sd.y <- sd(getME(mod, "y"))              ## sd of response
  b*sd.x/sd.y
}

############################ MODEL CHECKING FUNCTIONS ############################

check_model_grad <- function(mod) {
  relgrad <- with(mod@optinfo$derivs, solve(Hessian, gradient))
  return(max(pmin(abs(relgrad), abs(mod@optinfo$derivs$gradient))))
}

check_model_sing <- function(mod) {
  diag.vals <- getME(mod, "theta")[getME(mod, "lower") == 0]
  any(diag.vals < 1e-6)
}

check_model_scgrad <- function(mod) {
  devfun <- update(mod, devFunOnly = TRUE)
  if (isLMM(mod)) {
    pars <- getME(mod, "theta")
  } else {
    ## GLMM: requires both random and fixed parameters
    pars <- getME(mod, c("theta", "fixef"))
  }
  if (require("numDeriv")) {
    cat("hess:\n")
    print(hess <- hessian(devfun, unlist(pars)))
    cat("grad:\n")
    print(grad <- grad(devfun, unlist(pars)))
    cat("scaled gradient:\n")
    print(scgrad <- solve(chol(hess), grad))
  }
}

check_model_allfits <- function(mod) {
  source(system.file("utils", "allFit.R", package = "lme4"))
  mod.fit.all <- allFit(mod)
  ss <- summary(mod.fit.all)
  cat("extract fixed effects:\n")
  print(ss$fixef) ## extract fixed effects
  cat("log-likelihoods:\n")
  print(ss$llik) ## log-likelihoods
  cat("SDs and correlations:\n")
  print(ss$sdcor) ## SDs and correlations
  cat("Cholesky factors:\n")
  print(ss$theta) ## Cholesky factors
  cat("which fits worked:\n")
  print(ss$which.OK) ## which fits worked
  c(fit.all = mod.fit.all, ss = ss)
}

############################ PLOTTING FUNCTIONS ############################

add_scales <- function(value, name) {
  print(value)
  imap(value, function(scale.opts, scale.type)
    do.call(paste("scale", name, scale.type, sep = "_"), scale.opts)
  )
}

get_breaks <- function(lim, nbreaks = 5, label.factor = 1, label.digits = 3) {
  signif(seq(lim[1], lim[2], length.out = nbreaks), digits = label.digits)
}

get_labels <- function(lim, nbreaks = 5, label.factor = 1, label.digits = 3) {
  format(seq(lim[1], lim[2], length.out = nbreaks) * label.factor, digits = label.digits)
}

add_predict <- function(.data, .fit, ..., re.form = NA, pred.type = "response") {
  .data %>%
    filter_(.dots = enquos(...)) %>%
    add_column(pred = predict(.fit, newdata = ., re.form = re.form, type = pred.type))
}

get_thresh <- function(.data,
                       ...,
                       .group_dots = list(),
                       .x = pred,
                       .y = Time,
                       .xout = c(0.25, 0.5, 0.75),
                       .print.label = NA) {
  .x <- ensym(.x)
  .y <- ensym(.y)
  .group_dots <- c(.group_dots, enquos(...))
  thresh <- .data %>%
    group_by(!!!.group_dots) %>%
    do(summarise_(.,
      .dots =
        as_tibble(spline(.[[.y]] ~ .[[.x]], xout = .xout)) %>%
          rename(thresh = "x") %>%
          spread("thresh", "y", sep = ".")
    )) %>%
    ungroup()
  #if (!is.na(.print.label)) print(glimpse(thresh))
  thresh
}

plot_vwp_gca <- function(
  data,
  fit,
  re.form = NA,
  pred.type = "response",
  x.var = Time,
  x.lim = c(0.200, 1.500),
  y.var = IA_1_P,
  y.lim = c(0, 1.00),
  gg.scales = lst(
    color = lst(
      # brewer = lst(type = "qual", palette = 2, aesthetics = c("color", "fill")),
      # manual = list(values = alpha(c(
      #   "#b2df8a",
      #   "#fb9a99",
      #   "#33a02c",
      #   "#e31a1c"
      #   ), c(1, 1, 1, 1)), labels = c("Onset - Low", "Offset - Low", "Onset - High", "Offset - High"))
      # manual = list(values = alpha(c(
      #   "#666666",
      #   "#000000",
      #   "#666666",
      #   "#000000"
      # ), c(1, 1, 1, 1)), labels = c("Onset - Low", "Offset - Low", "Onset - High", "Offset - High"))
      manual = list(values = alpha(c(
        "#000000",
        "#000000"
      ), c(1, 1)), labels = c("Low", "High"))
      
      # manual = list(values = alpha(c("#7570b3", "#d95f02", "#7570b3", "#d95f02"), c(1, 1, 0.03, 0.03)), labels = c("Onset - Low", "Offset - Low", "Onset - High", "Offset - High"))
      # manual = list(values = c("#7570b3", "#d95f02"), labels = c("Onset", "Offset"))
    ),
    linetype = lst(
      manual = lst(values = c("solid", "twodash"), labels = c("Low", "High"))
      # manual = lst(values = c("solid", "solid", "twodash", "twodash"), labels = c("Onset - Low", "Offset - Low", "Onset - High", "Offset - High"))
      # manual = lst(values = c("solid", "dotted", "dashed", "twodash"), labels = c("Onset - Low", "Offset - Low", "Onset - High", "Offset - High"))
    ),
    shape = lst(
      # manual = lst(values = c("L", "H"), labels = c("Low", "High"))
      # manual = lst(values = c(25, 25, 24, 24), labels = c("Onset - Low", "Offset - Low", "Onset - High", "Offset - High"))
      # manual = lst(values = c(6, 6, 2, 2), labels = c("Onset - Low", "Offset - Low", "Onset - High", "Offset - High"))
      manual = lst(values = c(6, 2), labels = c("Low", "High"))
    ),
    fill = lst(
      # brewer = lst(type = "qual", palette = 2, aesthetics = c("color", "fill")),
      # manual = list(values = alpha(c(
      #  "#b2df8a",
      #  "#fb9a99",
      #  "#33a02c",
      #  "#e31a1c"
      # ), c(1, 1, 1, 1)), labels = c("Onset - Low", "Offset - Low", "Onset - High", "Offset - High"))
      # manual = list(values = alpha(c(
      #   "#666666",
      #   "#000000",
      #   "#666666",
      #   "#000000"
      # ), c(1, 1, 1, 1)), labels = c("Onset - Low", "Offset - Low", "Onset - High", "Offset - High"))
      manual = list(values = alpha(c(
        "#000000",
        "#000000"
      ), c(1, 1)), labels = c("Low", "High"))
      # manual = list(values = alpha(c("#7570b3", "#d95f02", "#7570b3", "#d95f02"), c(1, 1, 0.03, 0.03)), labels = c("Onset - Low", "Offset - Low", "Onset - High", "Offset - High"))
    )
  ),
  onset = T,
  offset = T,
  showfill = F,
  showdata = F,
  showdata.binwidth = 0.200,
  showdata.ptsize = 0.2,
  showdata.smooth.method = loess,
  #showdata.dodgewidth = 0.05,
  showdata.fundata = mean_se,
  facet.x = condition,
  facet.y = load,
  aes.lty = wmcap,
  aes.shape = wmcap,
  #aes.lty = condition.wmcap,
  #aes.shape = condition.wmcap,
  #aes.color = condition,
  #aes.color = condition.wmcap,
  aes.color = wmcap,
  #aes.fill = condition.wmcap,
  aes.fill = wmcap,
  show.thresh.geoms = c(
    segment = T,
    point = F,
    label = T
  ),
  .theme = theme_bw,
  theme.text.family = "Times New Roman",
  theme.text.size = 12,
  theme.text.lineheight = 12,
  nbreaks = 5,
  label.factor = 1,
  label.digits = 3,
  grid.major.linetype = 1,
  grid.major.size = 0.25,
  grid.major.color = "#999999",
  grid.minor.linetype = 3,
  grid.minor.size = 0.1,
  grid.minor.color = "#CCCCCC",
  x.scale.units = "s",
  .title = "Effect of low vs. high WM load on target fixation",
  .subtitle = "In onset and offset overlap across subjects of low vs. high WM capacity",
  output = NULL
) {
  resid_full <- resid(fit, type = "response")

    newpred_full <- data %>% add_predict(.fit = fit)
    #newpred_full <- newdata_full  # predict(fit, newdata=newdata_full, re.form=re.form, type=pred.type)
    thresh_df <- newpred_full %>% get_thresh(wmcap, load, condition)
    # newdata_full_with_thresh <- left_join(newpred_full, thresh_df, by = lst(wmcap, load, condition))
    # print(filter(thresh_df, wmcap  == "low", load == 1, condition == "c")$thresh.0.5)
    # .thresh.segments <- if (show.thresh.geoms$segment) c(
    #   geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",   color = "#7570b3", size = 0.5, inherit.aes = FALSE),
    #   geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",   color = "#d95f02", size = 0.5, inherit.aes = FALSE),
    #   geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash", color = "#7570b3", size = 0.5, inherit.aes = FALSE),
    #   geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash", color = "#d95f02", size = 0.5, inherit.aes = FALSE),
    #   geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",   color = "#7570b3", size = 0.5, inherit.aes = FALSE),
    #   geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash", color = "#7570b3", size = 0.5, inherit.aes = FALSE),
    #   geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",   color = "#d95f02", size = 0.5, inherit.aes = FALSE),
    #   geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash", color = "#d95f02", size = 0.5, inherit.aes = FALSE)
    # ) else c()
    # .thresh.points <- if (show.thresh.geoms$point) c(
    #   geom_point(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "c") %>% mutate(y = 0.5), aes(x = thresh.0.5, y = y), shape = 6, color = "#7570b3", size = 0.5, inherit.aes = FALSE),
    #   geom_point(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "r") %>% mutate(y = 0.5), aes(x = thresh.0.5, y = y), shape = 6, color = "#d95f02", size = 0.5, inherit.aes = FALSE),
    #   geom_point(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "c") %>% mutate(y = 0.5), aes(x = thresh.0.5, y = y), shape = 2, color = "#7570b3", size = 0.5, inherit.aes = FALSE),
    #   geom_point(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "r") %>% mutate(y = 0.5), aes(x = thresh.0.5, y = y), shape = 2, color = "#d95f02", size = 0.5, inherit.aes = FALSE),
    #   geom_point(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "c") %>% mutate(y = 0.5), aes(x = thresh.0.5, y = y), shape = 6, color = "#7570b3", size = 0.5, inherit.aes = FALSE),
    #   geom_point(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "r") %>% mutate(y = 0.5), aes(x = thresh.0.5, y = y), shape = 6, color = "#d95f02", size = 0.5, inherit.aes = FALSE),
    #   geom_point(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "c") %>% mutate(y = 0.5), aes(x = thresh.0.5, y = y), shape = 2, color = "#7570b3", size = 0.5, inherit.aes = FALSE),
    #   geom_point(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "r") %>% mutate(y = 0.5), aes(x = thresh.0.5, y = y), shape = 2, color = "#d95f02", size = 0.5, inherit.aes = FALSE)
    # ) else c()
    # .thresh.labels <- if (show.thresh.geoms$label) c(
    #   geom_label(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "c") %>% mutate(y = 0.15), aes(x = thresh.0.5, y = y, label = signif(thresh.0.5, digits = 4)), color = "#7570b3", nudge_x = -0.05, nudge_y =  0, size = 3, angle = 0, family = "Times New Roman", inherit.aes = FALSE),
    #   geom_label(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "r") %>% mutate(y = 0.15), aes(x = thresh.0.5, y = y, label = signif(thresh.0.5, digits = 4)), color = "#d95f02", nudge_x = -0.05, nudge_y =  0, size = 3, angle = 0, family = "Times New Roman", inherit.aes = FALSE),
    #   geom_label(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "c") %>% mutate(y = 0.15), aes(x = thresh.0.5, y = y, label = signif(thresh.0.5, digits = 4)), color = "#7570b3", nudge_x =  0.05, nudge_y = -0, size = 3, angle = 0, family = "Times New Roman", inherit.aes = FALSE),
    #   geom_label(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "r") %>% mutate(y = 0.15), aes(x = thresh.0.5, y = y, label = signif(thresh.0.5, digits = 4)), color = "#d95f02", nudge_x =  0.05, nudge_y = -0, size = 3, angle = 0, family = "Times New Roman", inherit.aes = FALSE),
    #   geom_label(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "c") %>% mutate(y = 0.15), aes(x = thresh.0.5, y = y, label = signif(thresh.0.5, digits = 4)), color = "#7570b3", nudge_x = -0.05, nudge_y =  0, size = 3, angle = 0, family = "Times New Roman", inherit.aes = FALSE),
    #   geom_label(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "r") %>% mutate(y = 0.15), aes(x = thresh.0.5, y = y, label = signif(thresh.0.5, digits = 4)), color = "#d95f02", nudge_x = -0.05, nudge_y =  0, size = 3, angle = 0, family = "Times New Roman", inherit.aes = FALSE),
    #   geom_label(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "c") %>% mutate(y = 0.15), aes(x = thresh.0.5, y = y, label = signif(thresh.0.5, digits = 4)), color = "#7570b3", nudge_x =  0.05, nudge_y = -0, size = 3, angle = 0, family = "Times New Roman", inherit.aes = FALSE),
    #   geom_label(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "r") %>% mutate(y = 0.15), aes(x = thresh.0.5, y = y, label = signif(thresh.0.5, digits = 4)), color = "#d95f02", nudge_x =  0.05, nudge_y = -0, size = 3, angle = 0, family = "Times New Roman", inherit.aes = FALSE)
    # ) else c()
    
    # .thresh.layers <- imap(show.thresh.geoms, function(show, names) {
    #   thresh_df %>%
    #     # filter(wmcap  == "low", load == 1, condition == "c") %>%
    #     mutate(y = 0.5, yend = -Inf) %>%
    #     rowwise() %>%
    #     do(geom = do.call)
    # })
    condition_labels <- lst(
      "c" = "Onset",
      "r" = "Offset"
    )
    load_labels <- lst(
      "1" = "Low WM load",
      "4" = "High WM load"
    )
    wmcap_labels <- lst(
      "low" = "Low WM capacity",
      "high" = "High WM capacity"
    )
    #facet_labeller <- function(variable, value){
    #  variable <- as.character(variable)
    #  value <- as.character(value)
    #  print(paste("LABELLER VARIABLE", variable))
    #  print(paste("LABELLER VALUE", value))
    #  if (variable == "load") {
    #    return(load_labels[value])
    #  } else if (variable == "wmcap") {
    #    return(wmcap_labels[value])
    #  } else {
    #    return(condition_labels[value])
    #  }
    #}

    quick_labeller <- labeller(
      condition = as_labeller(c("c" = "Onset", "r" = "Offset")),
      load = as_labeller(c(`1` = "Low WM load", `4` = "High WM load")),
      wmcap = as_labeller(c("low" = "Low WM capacity", "high" = "High WM capacity"))
    )

    .layers <- lst(
      (!!.theme)(),
      facet_grid(!!enquo(facet.y) ~ !!enquo(facet.x), labeller = quick_labeller),
      theme(
        text = element_text(
          family = theme.text.family,
          size = theme.text.size,
          lineheight = theme.text.lineheight,
          margin = margin(
            t = theme.text.size/2,
            b = theme.text.size/2,
            unit = "pt"
          )),
        panel.grid.major = element_line(
          linetype = grid.major.linetype,
          size = grid.major.size,
          color = grid.major.color
        ),
        panel.grid.minor = element_line(
          linetype = grid.minor.linetype,
          size = grid.minor.size,
          color = grid.minor.color
        )
      ),
      #labs(x = paste0("Time since word onset (", x.scale.units, ")"), y = "Probability of fixation"),
      labs(x = "", y = ""),
      labs(
        margin = margin(
          t = theme.text.size/2,
          b = theme.text.size/2,
          unit = "pt"
        ),
        fill = "WM Capacity", color = "WM Capacity", lty = "WM Capacity", shape = "WM Capacity"),
      #fill = "Overlap - WM Capacity", color = "Overlap - WM Capacity", lty = "Overlap - WM Capacity", shape = "Overlap - WM Capacity"),
      #labs(x = "Time since word onset (s)", y = "Probability of fixation", fill = "Overlap", color = "Overlap", lty = "WM Capacity", shape = "WM Capacity"),
      #labs(x = "Time since word onset (s)", y = "Probability of fixation", fill = "Overlap", color = "Overlap", lty = "WM Load", shape = "WM Load"),
      stat_summary(aes(y = pred), fun.y = mean, geom = "line"),
      # eval(as.call(c(list(scale_color_manual), gg.scales$color))),
      # eval(as.call(c(list(scale_linetype_manual), gg.scales$linetype))),
      # eval(as.call(c(list(scale_fill_manual), gg.scales$fill))),
      # eval(as.call(c(list(scale_shape_manual), gg.scales$shape))),
      !!!imap(gg.scales, add_scales),

     # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#666666", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#000000", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#666666", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#000000", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#666666", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#000000", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#666666", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#000000", size = 0.5, inherit.aes = FALSE),
     # 
     # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#666666", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#000000", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#666666", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#000000", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#666666", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#000000", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#666666", size = 0.5, inherit.aes = FALSE),
     # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#000000", size = 0.5, inherit.aes = FALSE),
      
      geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#000000", size = 0.5, inherit.aes = FALSE),

      geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#000000", size = 0.5, inherit.aes = FALSE),
      geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#000000", size = 0.5, inherit.aes = FALSE),
      
      #geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#b2df8a", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#fb9a99", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#b2df8a", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#fb9a99", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#33a02c", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#e31a1c", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#33a02c", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#e31a1c", size = 0.5, inherit.aes = FALSE),

      #geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#b2df8a", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#fb9a99", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#b2df8a", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#fb9a99", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#33a02c", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#e31a1c", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#33a02c", size = 0.5, inherit.aes = FALSE),
      #geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#e31a1c", size = 0.5, inherit.aes = FALSE),
      
      # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#7570b3", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#d95f02", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#7570b3", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "solid",      color = "#d95f02", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#7570b3", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#d95f02", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "c") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#7570b3", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "r") %>% mutate(y = 0.5, yend = -Inf), aes(x = thresh.0.5, y = y, yend = yend, xend = thresh.0.5), linetype = "twodash",    color = "#d95f02", size = 0.5, inherit.aes = FALSE),
    
      # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#7570b3", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 1, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#d95f02", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#7570b3", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap  == "low", load == 4, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "solid",   color = "#d95f02", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#7570b3", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 1, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#d95f02", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "c") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#7570b3", size = 0.5, inherit.aes = FALSE),
      # geom_segment(data = thresh_df %>% filter(wmcap == "high", load == 4, condition == "r") %>% mutate(y = 0.75, yend = -Inf), aes(x = thresh.0.75, y = y, yend = yend, xend = thresh.0.75), linetype = "twodash", color = "#d95f02", size = 0.5, inherit.aes = FALSE),
      
      #.thresh.segments,
      #.thresh.points,
      #.thresh.labels,
    
      #scale_x_continuous(limits = x.lim, breaks = waiver(), labels = waiver()),
      #scale_x_continuous(limits = x.lim, breaks = c(0.400, 0.800, thresh_df %>% arrange(condition, load) %>% pull(thresh.0.5), 1.200), labels = c("0.4", "0.8", round(thresh_df %>% arrange(condition, load) %>% pull(thresh.0.5), 1), "1.2")),
      # scale_x_continuous(limits = x.lim, breaks = c(0.400, 0.600, thresh50_RH, thresh50_RL, thresh50_CL, thresh50_CH, 1.000, 1.200), labels = c("400", "600", round(thresh50_RH), round(thresh50_RL), round(thresh50_CL), round(thresh50_CH), "1000", "1200")),
      #scale_y_continuous(limits = y.lim),
      #ylim(y.lim),
      # theme(axis.text.x = element_text(color = waiver(), angle = waiver(), hjust = waiver(), size =waiver()), axis.ticks.x = element_line(color = waiver(), size = waiver())),
      # theme(axis.text.x = element_text(color = c("black", "black", "#d95f02", "#d95f02","#d95f02", "#d95f02", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "black"), hjust = c(.5, .5, 1, 1, 1, 1, 1, 1, 1, 1, .5), size = c(12, 12, 10, 10, 10, 10, 10, 10, 10, 10, 12)), axis.ticks.x = element_line(color = c("black", "black", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#7570b3", "#7570b3",  "#7570b3", "#7570b3", "black"), size = c(.5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5))),
      ggtitle(.title, subtitle = .subtitle)
    )
  # print(.layers)
  output <- newpred_full %>%
    unite(condition.wmcap, condition, wmcap, remove = FALSE) %>%
    mutate(condition.wmcap = as_factor(condition.wmcap)) %>%
    # ggplot(aes(Time, IA_1_C / Obs, color=condition, lty=load ,fill=condition,shape=load))
    ggplot(aes(
      x = !!enquo(x.var),
      y = !!enquo(y.var),
      color = factor(!!enquo(aes.color)),
      lty = factor(!!enquo(aes.lty)),
      fill = factor(!!enquo(aes.fill)),
      shape = factor(!!enquo(aes.shape)),
      lineheight = theme.text.lineheight
    )) %+%
    # stat_summary_bin(mapping = aes(x = Time, y = IA_1_P), data = newpred_full, fun.data = mean_se, geom = "pointrange", size = 0.2, binwidth = 0.100) +
    #ggplot(aes(Time, IA_1_C / Obs, color = condition, lty = factor(load), fill = condition, shape = factor(load))) +
    .layers
  if (offset && showfill) {
    output <- output +
      geom_ribbon(aes(ymin = rep.int(mean_RH, 565), ymax = rep.int(mean_RL, 565)), color = "#d95f02", fill = alpha("#d95f02", .03), lty = "blank")
  }
  if (onset && showfill) {
    output <- output +
      geom_ribbon(aes(ymin = rep.int(mean_CH, 565), ymax = rep.int(mean_CL, 565)), color = "#7570b3", fill = alpha("#7570b3", .03), lty = "blank")
  }
  if (showdata) {
    dodgewidth <- 0.1 * showdata.ptsize * (x.lim[2] - x.lim[1])
    output <- output +
      #geom_point() +
      #geom_smooth(method = showdata.smooth.method) +
      stat_summary_bin(
        fun.data = showdata.fundata,
        geom = "pointrange",
        position = position_dodge(width = dodgewidth),
        size = showdata.ptsize,
        binwidth = showdata.binwidth
        )
  }
  if (x.scale.units == "ms") {
    label.factor <- 1000
  }
  .x.breaks <- get_breaks(c(0,3.2), nbreaks, label.factor, label.digits)
  .x.minor_breaks <- get_breaks(c(0,3.2), nbreaks * 2, label.factor, label.digits)
  .x.labels <- get_labels(c(0,3.2), nbreaks, label.factor, label.digits)
  return(output +
           scale_x_continuous(limits = c(-0.2,3.4), breaks = .x.breaks, minor_breaks = .x.minor_breaks, labels = .x.labels) +
           scale_y_continuous(limits = c(-0.1,1.1)) +
           coord_cartesian(ylim = y.lim, xlim = x.lim)
         )
}