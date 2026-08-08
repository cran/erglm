
#' Stepwise covariate modelling for exposure-response models
#'
#' @param mod An erglm model object
#' @param candidates Character vector with list of candidate terms
#' @param threshold Threshold to test against
#' @param test Which significance test to use when comparing nested
#' models. `"auto"` (the default) picks a likelihood-ratio chi-squared
#' test (`"Chisq"`) for families with known dispersion (binomial,
#' poisson) and an F-test (`"F"`) for families with an estimated
#' dispersion parameter (gaussian, gamma, inverse.gaussian, quasi*),
#' matching `stats::anova()`'s own `test` argument. Set explicitly to
#' override.
#' @param seed Optional seed to control order of term tests
#'
#' @returns For `erglm_scm_forward()` and `erglm_scm_backward()`, the
#' updated erglm model is returned, with the SCM history log updated
#' internally. For `erglm_scm_history()`, a data frame is returned
#' containing the SCM history log
#'
#' @details `seed` exists as a safety measure against two hypothetical
#' sources of run-to-run variation: (a) the order in which candidate
#' terms are tested within a step, and (b) some part of the model-fitting
#' machinery secretly depending on `.Random.seed`. As currently
#' implemented, only (a) is real, and even then its effect is usually
#' invisible. Concretely: each step of `erglm_scm_forward()`/
#' `erglm_scm_backward()` shuffles the candidate terms (`sample()`)
#' before testing them one at a time, and the shuffled order is the
#' *only* thing `seed` (via `withr::with_seed()`) controls. Term p-values
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
#' If a candidate term is aliased (perfectly collinear) with a term
#' already in the model, `stats::anova()` reports zero additional
#' degrees of freedom and an `NA` p-value for it. That candidate is
#' skipped for the step (with a warning) rather than being selected or
#' crashing the search -- comparisons against `NA` aren't meaningful,
#' and the candidate can never improve the fit anyway once it's
#' aliased.
#'
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
NULL

#' @rdname erglm_scm
#' @export
erglm_scm_forward <- function(mod, candidates, threshold = 0.01, test = c("auto", "Chisq", "F"), seed = NULL) {
  test <- match.arg(test)
  .erglm_check_candidates(candidates)
  if (is.null(seed)) {
    seed <- .pick_seed()
  }
  withr::with_seed(
    seed = seed,
    code = {
      mod_out <- .erglm_scm_forward(
        mod = mod,
        candidates = candidates,
        threshold = threshold,
        test = test
      )
    }
  )
  return(mod_out)
}

.erglm_scm_forward <- function(mod, candidates, threshold, test) {
  history <- erglm_scm_history(mod)
  last_iter <- max(history$iteration)
  while (TRUE) {
    mod_new <- .erglm_once_forward(mod, candidates, threshold, test)
    history_new <- erglm_scm_history(mod_new)
    this_iter <- max(history_new$iteration)
    if (this_iter == last_iter) return(mod)
    history <- history_new
    last_iter <- this_iter
    mod <- mod_new
    updates <- history |> 
      dplyr::filter(iteration == last_iter) |> 
      dplyr::pull(model_updated)
    if (all(updates == 0L)) return(mod)
  }
}

#' @rdname erglm_scm
#' @export
erglm_scm_backward <- function(mod, candidates, threshold = 0.001, test = c("auto", "Chisq", "F"), seed = NULL) {
  test <- match.arg(test)
  .erglm_check_candidates(candidates)
  if (is.null(seed)) {
    seed <- .pick_seed()
  }
  withr::with_seed(
    seed = seed,
    code = {
      mod_out <- .erglm_scm_backward(
        mod = mod,
        candidates = candidates,
        threshold = threshold,
        test = test
      )
    }
  )
  return(mod_out)
}

.erglm_scm_backward <- function(mod, candidates, threshold, test) {
  history <- erglm_scm_history(mod)
  last_iter <- max(history$iteration)
  while (TRUE) {
    mod_new <- .erglm_once_backward(mod, candidates, threshold, test)
    history_new <- erglm_scm_history(mod_new)
    this_iter <- max(history_new$iteration)
    if (this_iter == last_iter) return(mod)
    history <- history_new
    last_iter <- this_iter
    mod <- mod_new
    updates <- history |> 
      dplyr::filter(iteration == last_iter) |> 
      dplyr::pull(model_updated)
    if (all(updates == 0L)) return(mod)
  }
}

#' @rdname erglm_scm
#' @export
erglm_scm_history <- function(mod) {
  history <- mod$erglm$history
  if (!is.null(history)) return(history)
  history_row <- tibble::tibble(
    iteration = 0L,
    attempt = 0L,
    step = "base model",
    action = NA_character_,
    term_tested = NA_character_, 
    model_tested = deparse(mod$formula),
    model_converged = mod$converged,
    term_p_value = NA_real_,
    model_aic = stats::AIC(mod),
    model_bic = stats::BIC(mod),
    model_updated = NA
  )
  return(history_row)
}

.erglm_once_forward <- function(mod, candidates, threshold, test) {
  candidates <- sample(candidates)
  history <- erglm_scm_history(mod)
  iter <- max(history$iteration) + 1L
  attm <- max(history$attempt)
  lowest_p <- threshold
  update_ind <- NA_integer_
  best_mod <- mod
  for (cc in candidates) {    
    add <- stats::as.formula(paste("~", cc))
    attm <- attm + 1L
    if (!.erglm_term_in_model(mod, add)) {
      mod_new <- erglm_add_term(mod, add, quiet = TRUE)
      p_val <- .erglm_anova_p(mod, mod_new, test)
      history_row <- tibble::tibble(
        iteration = iter,
        attempt = attm,
        step = "forward",
        action = "add",
        term_tested = deparse(add), 
        model_tested = deparse(mod_new$formula),
        model_converged = mod_new$converged,
        term_p_value = p_val,
        model_aic = stats::AIC(mod_new),
        model_bic = stats::BIC(mod_new),
        model_updated = NA
      )
      history <- tibble::add_row(history, history_row)
      if (is.na(p_val)) {
        rlang::warn(paste0(
          "Skipping candidate term `", deparse(add), "` in forward step ",
          iter, ": comparison p-value is NA (often caused by a candidate ",
          "that's aliased/collinear with a term already in the model, ",
          "giving zero additional degrees of freedom)."
        ))
      } else if (p_val < lowest_p) {
        update_ind <- attm
        lowest_p <- p_val
        best_mod <- mod_new
      }
    }
  }
  history <- history |> 
    dplyr::mutate(
      model_updated = dplyr::case_when(
        iteration != iter ~ model_updated,
        attempt == update_ind ~ 1L,
        TRUE ~ 0L
      )
    )
  best_mod$erglm$history <- history
  return(best_mod)
}

.erglm_once_backward <- function(mod, candidates, threshold, test) {
  trm_mod <- stats::terms(mod)
  trm_lab <- attr(trm_mod, "term.labels")
  candidates <- intersect(trm_lab, candidates)
  if (length(candidates) == 0L) return(mod)
  candidates <- sample(candidates)
  history <- erglm_scm_history(mod)
  iter <- max(history$iteration) + 1L
  attm <- max(history$attempt)
  highest_p <- threshold
  update_ind <- NA_integer_
  best_mod <- mod
  for (cc in candidates) {    
    del <- stats::as.formula(paste("~", cc))
    attm <- attm + 1L
    if (.erglm_term_in_model(mod, del)) {
      mod_new <- erglm_remove_term(mod, del, quiet = TRUE)
      p_val <- .erglm_anova_p(mod, mod_new, test)
      history_row <- tibble::tibble(
        iteration = iter,
        attempt = attm,
        step = "backward",
        action = "remove",
        term_tested = deparse(del), 
        model_tested = deparse(mod_new$formula),
        model_converged = mod_new$converged,
        term_p_value = p_val,
        model_aic = stats::AIC(mod_new),
        model_bic = stats::BIC(mod_new),
        model_updated = NA
      )
      history <- tibble::add_row(history, history_row)
      if (is.na(p_val)) {
        rlang::warn(paste0(
          "Skipping candidate term `", deparse(del), "` in backward step ",
          iter, ": comparison p-value is NA (often caused by a candidate ",
          "that's aliased/collinear with another term in the model, ",
          "giving zero degrees of freedom for the comparison)."
        ))
      } else if (p_val > highest_p) {
        update_ind <- attm
        highest_p <- p_val
        best_mod <- mod_new
      }
    }
  }
  history <- history |> 
    dplyr::mutate(
      model_updated = dplyr::case_when(
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
#' the model, respectively)
#'
#' @details These functions are not typically called directly; they
#' underpin [erglm_scm_forward()] and [erglm_scm_backward()]. Named and
#' shaped to match the companion `emaxnls` package's
#' `emax_add_term()`/`emax_remove_term()`, which serve the same purpose
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
    if (!quiet) rlang::warn("cannot add a term that already exists in the model")
    return(mod)
  }
  trm_add_var <- all.vars(attr(trm_add, "variables"))
  dat <- mod$data
  vars_ok <- trm_add_var %in% names(dat)
  if (!all(vars_ok)) {
    if (!quiet) rlang::warn("cannot add a term that uses variables not in the data")
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
    if (!quiet) rlang::warn("cannot remove a term that does not exist in the model")
    return(mod)
  }
  dat <- mod$data
  trm_new <- stats::drop.terms(trm_mod, ind, keep.response = TRUE)
  erglm_model(formula = trm_new, data = dat, family = stats::family(mod))
}

