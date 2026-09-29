
#' Stepwise covariate modelling for exposure-response models
#'
#' Automates the search for which covariates belong in an
#' exposure-response model: `erglm_scm_forward()` greedily adds candidate
#' terms, `erglm_scm_backward()` greedily removes them, and
#' `erglm_scm_history()` retrieves the audit log of every model considered
#' along the way.
#'
#' @param mod An erglm model object
#' @param candidates Character vector with list of candidate terms
#' @param threshold Threshold to test against. Used only when
#' `criterion = "p-value"` (the default); ignored otherwise. Defaults to
#' `0.01` for `erglm_scm_forward()` and `0.001` for `erglm_scm_backward()`.
#' @param criterion Model selection criterion. One of `"p-value"`
#' (default), `"aic"`, or `"bic"`.
#' @param test Which significance test to use when comparing nested
#' models. Only used when `criterion = "p-value"`. `"auto"` (the default)
#' picks a likelihood-ratio chi-squared test (`"Chisq"`) for families
#' with known dispersion (binomial, poisson) and an F-test (`"F"`) for
#' families with an estimated dispersion parameter (gaussian, gamma,
#' inverse.gaussian, quasi*), matching `stats::anova()`'s own `test`
#' argument. Set explicitly to override.
#' @param seed Optional seed controlling the order candidate terms are
#' tested in within a step. Defaults to `NULL`, in which case one is
#' chosen automatically and used silently -- unlike
#' [simulate.erglm_model()]'s auto-picked seed, it is not reported, since
#' (per Details below) it essentially never changes the result.
#'
#' @returns For `erglm_scm_forward()` and `erglm_scm_backward()`, the
#' updated erglm model is returned, with the SCM history log updated
#' internally. For `erglm_scm_history()`, a data frame is returned
#' containing the SCM history log
#'
#' @section Reproducibility and the seed argument:
#' `seed` exists as a safety measure against two hypothetical
#' sources of run-to-run variation: (a) the order in which candidate
#' terms are tested within a step, and (b) some part of the model-fitting
#' machinery secretly depending on `.Random.seed`. As currently
#' implemented, only (a) is real, and even then its effect is usually
#' invisible. Concretely: each step of `erglm_scm_forward()`/
#' `erglm_scm_backward()` shuffles the candidate terms (`sample()`)
#' before testing them one at a time, and the shuffled order is the
#' *only* thing `seed` (via a seeded-then-restored RNG block) controls. Term p-values
#' come from `stats::anova()` on models fitted with `stats::glm()`, which
#' is a deterministic algorithm (iteratively reweighted least squares,
#' no random starting values) -- so which candidate is *found* to be
#' best does not depend on the seed. The seed can only change which
#' candidate is *selected* in the (rare, essentially measure-zero for
#' continuous predictors) case of an exact tie in p-values within a
#' step, since ties are broken by encounter order (`p_val < lowest_p`/
#' `p_val > highest_p` are strict inequalities in the internal
#' `.erglm_once_forward()`/`.erglm_once_backward()` helpers). In short:
#' for typical data, `seed` is redundant for reproducibility of the
#' *result* (though it still affects the row order of the intermediate
#' attempts recorded in [erglm_scm_history()]) -- it's retained mainly
#' as a guard against future refactors reintroducing genuine
#' seed-sensitivity (e.g. if candidate order were ever used as an
#' early-stopping rule rather than exhaustively tested every step).
#'
#' @section Aliased or collinear candidates:
#' If a candidate term is aliased (perfectly collinear) with a term
#' already in the model, `stats::anova()` reports zero additional
#' degrees of freedom and an `NA` p-value for it. That candidate is
#' skipped for the step (with a warning) rather than being selected or
#' crashing the search -- comparisons against `NA` aren't meaningful,
#' and the candidate can never improve the fit anyway once it's
#' aliased.
#'
#' @section Selection criteria:
#' Three model selection criteria are available via the `criterion`
#' argument:
#'
#' - `"p-value"` (default): Models are compared with the significance
#'   test named by `test`. A term is added if its p-value falls below
#'   `threshold` (forward) or removed if its p-value exceeds `threshold`
#'   (backward). When multiple candidates satisfy the threshold within a
#'   step, the one with the most extreme p-value is chosen.
#' - `"aic"`: A term is added (forward) or removed (backward) if doing
#'   so strictly decreases AIC relative to the current model. When
#'   multiple candidates improve AIC, the one yielding the lowest AIC is
#'   chosen.
#' - `"bic"`: Same as `"aic"`, but using BIC as the criterion.
#'
#' When `criterion` is `"aic"` or `"bic"`, the `threshold` and `test`
#' arguments have no effect, and `term_p_value` is left `NA` in the
#' history for every candidate tested that step (the significance test
#' isn't computed, since it plays no role in selection). The `model_aic`
#' and `model_bic` columns are always recorded regardless of which
#' criterion drove selection, and the history's `criterion` column
#' records which one was used for each forward/backward step.
#'
#' @section Candidate validation:
#' `candidates` is validated up front: every element must be parseable
#' as a formula and name exactly one covariate term (e.g. `"sex"`, not
#' `"sex + dose"` or `"not a formula"`). This errors immediately, before
#' any model fitting, naming the offending element -- rather than
#' surfacing only once the search happens to test that candidate, many
#' steps into what might be a long, expensive search.
#'
#' @name erglm_scm
#' @examples
#' mod0 <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
#' mod1 <- erglm_scm_forward(mod0, candidates = c("sex", "dose"))
#' erglm_scm_history(mod1)
#' 
#' mod2 <- erglm_model(ae1 ~ aucss + sex + dose, erglm_data, family = binomial())
#' mod3 <- erglm_scm_backward(mod2, candidates = c("sex", "dose"))
#' erglm_scm_history(mod3)
#'
#' # AIC-based forward addition/backward elimination instead of p-value
#' mod4 <- erglm_scm_forward(mod0, candidates = c("sex", "dose"), criterion = "aic")
#' mod5 <- erglm_scm_backward(mod4, candidates = c("sex", "dose"), criterion = "bic")
#' erglm_scm_history(mod5)
NULL

#' @rdname erglm_scm
#' @export
erglm_scm_forward <- function(mod, candidates, threshold = 0.01, criterion = "p-value", test = c("auto", "Chisq", "F"), seed = NULL) {
  test <- match.arg(test)
  .erglm_check_candidates(candidates)
  .erglm_check_criterion(criterion)
  if (is.null(seed)) {
    seed <- .pick_seed()
  }
  .seed_with_seed(
    seed = seed,
    code = {
      mod_out <- .erglm_scm_forward(
        mod = mod,
        candidates = candidates,
        threshold = threshold,
        criterion = criterion,
        test = test
      )
    }
  )
  return(mod_out)
}

.erglm_scm_forward <- function(mod, candidates, threshold, criterion = "p-value", test) {
  history <- erglm_scm_history(mod)
  last_iter <- max(history$iteration)
  while (TRUE) {
    mod_new <- .erglm_once_forward(mod, candidates, threshold, criterion, test)
    history_new <- erglm_scm_history(mod_new)
    this_iter <- max(history_new$iteration)
    if (this_iter == last_iter) return(mod)
    history <- history_new
    last_iter <- this_iter
    mod <- mod_new
    updates <- history$model_updated[history$iteration == last_iter]
    if (all(updates == 0L)) return(mod)
  }
}

#' @rdname erglm_scm
#' @export
erglm_scm_backward <- function(mod, candidates, threshold = 0.001, criterion = "p-value", test = c("auto", "Chisq", "F"), seed = NULL) {
  test <- match.arg(test)
  .erglm_check_candidates(candidates)
  .erglm_check_criterion(criterion)
  if (is.null(seed)) {
    seed <- .pick_seed()
  }
  .seed_with_seed(
    seed = seed,
    code = {
      mod_out <- .erglm_scm_backward(
        mod = mod,
        candidates = candidates,
        threshold = threshold,
        criterion = criterion,
        test = test
      )
    }
  )
  return(mod_out)
}

.erglm_scm_backward <- function(mod, candidates, threshold, criterion = "p-value", test) {
  history <- erglm_scm_history(mod)
  last_iter <- max(history$iteration)
  while (TRUE) {
    mod_new <- .erglm_once_backward(mod, candidates, threshold, criterion, test)
    history_new <- erglm_scm_history(mod_new)
    this_iter <- max(history_new$iteration)
    if (this_iter == last_iter) return(mod)
    history <- history_new
    last_iter <- this_iter
    mod <- mod_new
    updates <- history$model_updated[history$iteration == last_iter]
    if (all(updates == 0L)) return(mod)
  }
}

#' @rdname erglm_scm
#' @export
erglm_scm_history <- function(mod) {
  history <- mod$erglm$history
  if (!is.null(history)) return(history)
  history_row <- data.frame(
    check.names = FALSE,
    iteration = 0L,
    attempt = 0L,
    step = "base model",
    criterion = NA_character_,
    action = NA_character_,
    term_tested = NA_character_, 
    model_tested = deparse(mod$formula),
    model_converged = mod$converged,
    term_p_value = NA_real_,
    model_aic = stats::AIC(mod),
    model_bic = stats::BIC(mod),
    model_updated = NA_integer_
  )
  return(history_row)
}

# `use_ic`/`ic_fn` implement the "aic"/"bic" branch of `criterion`: a
# candidate is compared against `best_metric` (the *current* model's IC,
# updated as better candidates are found within the step) rather than
# against `threshold`, which only applies to `criterion = "p-value"`. The
# significance test isn't computed at all in IC mode -- it plays no role
# in selection there -- so `term_p_value` is left `NA` in the history for
# every row tested under "aic"/"bic".
.erglm_once_forward <- function(mod, candidates, threshold, criterion = "p-value", test) {
  candidates <- sample(candidates)
  history <- erglm_scm_history(mod)
  iter <- max(history$iteration) + 1L
  attm <- max(history$attempt)
  use_ic <- criterion %in% c("aic", "bic")
  ic_fn <- if (criterion == "bic") stats::BIC else stats::AIC
  best_metric <- if (use_ic) as.numeric(ic_fn(mod)) else threshold
  update_ind <- NA_integer_
  best_mod <- mod
  for (cc in candidates) {    
    add <- stats::as.formula(paste("~", cc))
    attm <- attm + 1L
    if (!.erglm_term_in_model(mod, add)) {
      mod_new <- erglm_add_term(mod, add, quiet = TRUE)
      p_val <- if (use_ic) NA_real_ else .erglm_anova_p(mod, mod_new, test)
      history_row <- data.frame(
        check.names = FALSE,
        iteration = iter,
        attempt = attm,
        step = "forward",
        criterion = criterion,
        action = "add",
        term_tested = deparse(add), 
        model_tested = deparse(mod_new$formula),
        model_converged = mod_new$converged,
        term_p_value = p_val,
        model_aic = stats::AIC(mod_new),
        model_bic = stats::BIC(mod_new),
        model_updated = NA_integer_
      )
      history <- rbind(history, history_row)
      if (use_ic) {
        candidate_ic <- as.numeric(ic_fn(mod_new))
        if (candidate_ic < best_metric) {
          update_ind <- attm
          best_metric <- candidate_ic
          best_mod <- mod_new
        }
      } else if (is.na(p_val)) {
        .cond_warn(paste0(
          "Skipping candidate term `", deparse(add), "` in forward step ",
          iter, ": comparison p-value is NA (often caused by a candidate ",
          "that's aliased/collinear with a term already in the model, ",
          "giving zero additional degrees of freedom)."
        ))
      } else if (p_val < best_metric) {
        update_ind <- attm
        best_metric <- p_val
        best_mod <- mod_new
      }
    }
  }
  history <- history |> 
    .verb_mutate(
      model_updated = .case_when(
        iteration != iter ~ model_updated,
        attempt == update_ind ~ 1L,
        TRUE ~ 0L
      )
    )
  best_mod$erglm$history <- history
  return(best_mod)
}

.erglm_once_backward <- function(mod, candidates, threshold, criterion = "p-value", test) {
  trm_mod <- stats::terms(mod)
  trm_lab <- attr(trm_mod, "term.labels")
  candidates <- intersect(trm_lab, candidates)
  if (length(candidates) == 0L) return(mod)
  candidates <- sample(candidates)
  history <- erglm_scm_history(mod)
  iter <- max(history$iteration) + 1L
  attm <- max(history$attempt)
  use_ic <- criterion %in% c("aic", "bic")
  ic_fn <- if (criterion == "bic") stats::BIC else stats::AIC
  best_metric <- if (use_ic) as.numeric(ic_fn(mod)) else threshold
  update_ind <- NA_integer_
  best_mod <- mod
  for (cc in candidates) {    
    del <- stats::as.formula(paste("~", cc))
    attm <- attm + 1L
    if (.erglm_term_in_model(mod, del)) {
      mod_new <- erglm_remove_term(mod, del, quiet = TRUE)
      p_val <- if (use_ic) NA_real_ else .erglm_anova_p(mod, mod_new, test)
      history_row <- data.frame(
        check.names = FALSE,
        iteration = iter,
        attempt = attm,
        step = "backward",
        criterion = criterion,
        action = "remove",
        term_tested = deparse(del), 
        model_tested = deparse(mod_new$formula),
        model_converged = mod_new$converged,
        term_p_value = p_val,
        model_aic = stats::AIC(mod_new),
        model_bic = stats::BIC(mod_new),
        model_updated = NA_integer_
      )
      history <- rbind(history, history_row)
      if (use_ic) {
        candidate_ic <- as.numeric(ic_fn(mod_new))
        if (candidate_ic < best_metric) {
          update_ind <- attm
          best_metric <- candidate_ic
          best_mod <- mod_new
        }
      } else if (is.na(p_val)) {
        .cond_warn(paste0(
          "Skipping candidate term `", deparse(del), "` in backward step ",
          iter, ": comparison p-value is NA (often caused by a candidate ",
          "that's aliased/collinear with another term in the model, ",
          "giving zero degrees of freedom for the comparison)."
        ))
      } else if (p_val > best_metric) {
        update_ind <- attm
        best_metric <- p_val
        best_mod <- mod_new
      }
    }
  }
  history <- history |> 
    .verb_mutate(
      model_updated = .case_when(
        iteration != iter ~ model_updated,
        attempt == update_ind ~ 1L,
        TRUE ~ 0L
      )
    )
  best_mod$erglm$history <- history
  return(best_mod)
}

.erglm_anova_p <- function(mod1, mod2, test) {
  family_name <- stats::family(mod1)$family
  test <- .erglm_resolve_test(test, family_name)
  smm <- stats::anova(mod1, mod2, test = test)
  p_col <- grep("^Pr\\(", colnames(smm))[1]
  return(smm[[p_col]][2])
}

.erglm_term_in_model <- function(mod, term) {
  trm_mod <- stats::terms(mod)
  trm_tst <- stats::terms(term)
  trm_mod_lab <- attr(trm_mod, "term.labels")
  trm_tst_lab <- attr(trm_tst, "term.labels")
  ind <- which(trm_mod_lab == trm_tst_lab)
  return(length(ind) != 0)
}

#' Add or remove a covariate term from an exposure-response model
#'
#' Add or remove a single covariate term from an existing erglm model,
#' returning a new fitted model object.
#'
#' @param mod An erglm model object, as returned by [erglm_model()]
#' @param term A one-sided formula naming the term to add/remove, e.g.
#' `~ sex`
#' @param quiet If `TRUE`, suppress the warning issued when the term
#' can't be added/removed (because it's already in the model / isn't in
#' the model, respectively). Defaults to `FALSE`.
#'
#' @details These functions are not typically called directly; they
#' underpin [erglm_scm_forward()] and [erglm_scm_backward()]. Named and
#' shaped to match the companion `emaxnls` package's
#' [emaxnls::emax_add_term()]/[emaxnls::emax_remove_term()], which serve the same purpose
#' for `emaxnls`/`emaxlogistic` models -- with one structural
#' difference: `emaxnls`'s terms are two-sided formulas naming a
#' structural parameter (e.g. `E0 ~ AGE`), since covariates there attach
#' to a specific Emax parameter, whereas erglm's terms are plain
#' one-sided `glm()` formula terms (e.g. `~ sex`), since erglm has no
#' equivalent parameter-level structure to attach covariates to. `term`
#' must be a one-sided formula naming exactly one covariate; passing
#' `NULL`, a non-formula, a two-sided formula, or a multi-term formula
#' (e.g. `~ weight + age`) errors informatively rather than failing with
#' a low-level error (`NULL`/non-formula) or being silently
#' misinterpreted (two-sided formulas; multi-term formulas, which used
#' to add/attempt every term at once with no warning).
#'
#' @returns An erglm model object. If the term can't be added/removed
#' (see `quiet`), the original `mod` is returned unchanged.
#'
#' @name erglm_term
#' @examples
#' mod <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
#' mod2 <- erglm_add_term(mod, ~ sex)
#' mod3 <- erglm_remove_term(mod2, ~ sex)
NULL

#' @rdname erglm_term
#' @export
erglm_add_term <- function(mod, term, quiet = FALSE) {
  .erglm_check_term(term)
  trm_mod <- stats::terms(mod)
  trm_add <- stats::terms(term)
  trm_mod_lab <- attr(trm_mod, "term.labels")
  trm_add_lab <- attr(trm_add, "term.labels")
  ind <- which(trm_mod_lab == trm_add_lab)
  if (length(ind) != 0L) {
    if (!quiet) .cond_warn("cannot add a term that already exists in the model")
    return(mod)
  }
  trm_add_var <- all.vars(attr(trm_add, "variables"))
  dat <- mod$data
  vars_ok <- trm_add_var %in% names(dat)
  if (!all(vars_ok)) {
    if (!quiet) .cond_warn("cannot add a term that uses variables not in the data")
    return(mod)
  }
  fml <- stats::as.formula(
    paste(deparse(mod$formula), deparse(term[[2]]), sep = " + ")
  )
  erglm_model(formula = fml, data = dat, family = stats::family(mod))
}

#' @rdname erglm_term
#' @export
erglm_remove_term <- function(mod, term, quiet = FALSE) {
  .erglm_check_term(term)
  trm_mod <- stats::terms(mod)
  trm_del <- stats::terms(term)
  trm_mod_lab <- attr(trm_mod, "term.labels")
  trm_del_lab <- attr(trm_del, "term.labels")
  ind <- which(trm_mod_lab == trm_del_lab)
  if (length(ind) == 0L) {
    if (!quiet) .cond_warn("cannot remove a term that does not exist in the model")
    return(mod)
  }
  dat <- mod$data
  trm_new <- stats::drop.terms(trm_mod, ind, keep.response = TRUE)
  erglm_model(formula = trm_new, data = dat, family = stats::family(mod))
}

