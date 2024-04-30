fun <- function(x) { x }

hook <- quote({cat('before x=', x, '\n'); x <- x + 1 })

trace(fun, hook, print = FALSE)

hook <- quote({
  if (!missing(package)) {
    if (!character.only)
      pkg <- as.character(substitute(package))
    else
      pkg <- package
    print(pkg)
    srcpkgs::pkg_load(pkg)
    # if (enabled) FUN('qbdev_loader')(pkg, attach = TRUE, 'library')
  }
})

trace(library, hook)
untrace(library)



# hookr: a custom trace-like system for hooking code in functions
#
# Author: karl
###############################################################################


#' add a hook to a function in place
#'
#' @inheritParams replace_binding
#' @inheritParams hook_function
#' @param fun_name	the name of the function to hook and replace
#'
#' @keywords internal
#' @seealso hook_r_loaders
hook_and_replace_function <- function(ns, fun_name, hook,
  hook_name = 'fun_name')
{
  '!DEBUG hook_and_replace_function(fun_name="`fun_name`")'

  ns <- asNamespace(ns)
  fun <- get(fun_name, ns)
  if (is_hooked(fun)) return(invisible()) # avoid adding multiple hooks

  fun2 <- hook_function(fun, hook, hook_name)

  replace_binding(ns, fun_name, fun2)
}

#' restore a hooked function
#'
#' @inheritParams hook_and_replace_function
#' @keywords internal
#' @seealso hook_r_loaders
restore_hooked_function <- function(ns, fun_name)
{
  '!DEBUG restore_hooked_function(fun_name="`fun_name`")'

  ns <- asNamespace(ns)
  fun <- get(fun_name, ns)
  if (!is_hooked(fun)) return(invisible())

  fun2 <- unhook_function(fun)

  replace_binding(ns, fun_name, fun2)
}

#' add a hook to a function
#'
#' the specified code will be executed just before the function
#'
#' the hooks can be globally disabled using the option qbdev.hookr.disable
#' set to FALSE.
#' The hooks can be called with in a verbose way if the option qbdev.hookr.verbose
#' is set.
#'
#' @param fun	the function to hook. must be a non-one liner function,
#' 	i.e. the body of the function must be a block, inside braces "{}",
#' and must not be a primitive function
#' @inheritParams hook_runner
#'
#' @return the hooked function
#' @keywords internal
#' @seealso hook_r_loaders
hook_function <- function(fun, code, hook_name = 'unknown') {
  '!DEBUG hook_function()'

  stop_if(is.primitive(fun), "fun can not be a primitive function")
  stop_unless(length(as.list(body(fun))) > 1,
    "the body of function fun must be a block")

  qcode <- enquote(code)
  hook <- bquote(getFromNamespace('hook_runner', 'qbdev')(.(qcode), .(hook_name)))

  body(fun) <- as.call(append(after = 1, as.list(body(fun)), hook))

  fun
}

#' actually run the hook
#'
#' the hooks can be globally disabled using the option qbdev.hookr.disable
#' set to FALSE.
#' The hooks can be called with in a verbose way if the option qbdev.hookr.verbose
#' is set.
#'
#' @param code	the actual code to execute as an expression/language object
#' @inheritParams base::eval
#' @param hook_name	for debug purposes, to be included in log
#' @return the evaluated code, or NULL if disabled
#' @keywords internal
#' @seealso hook_r_loaders
hook_runner <- function(code, hook_name = '', envir = parent.frame(),
  verbose = getOption('qbdev.hookr.verbose', FALSE),
  disable = getOption('qbdev.hookr.disable', FALSE))
{
  '!DEBUG hook_runner(hook_name="`hook_name`")'

  verbose <- isTRUE(verbose)
  disable <- isTRUE(disable)

  if (verbose) message('executing hook', hook_name, '\n')

  if (disable) {
    if (verbose)
      message('hook_runner: hook is disabled ', hook_name)
    return(invisible())
  }

  if (verbose) {
    message('hook_runner: running hook ', hook_name)
  }

  force(envir)
  invisible(eval(code, envir))
}

#' test if a function had been hooked
#'
#' @inheritParams base::eval
#' @return TRUe iff hooked
#' @keywords internal
#' @seealso hook_r_loaders
is_hooked <- function(fun) {
  lst <- as.list(body(fun))
  if (length(lst) < 2) return(FALSE)

  grepl('hook_runner', deparse(lst[[2]])[1])
}

#' remove a hook on function if any
#'
#' @inheritParams hook_function
#' @return TRUe iff hooked
#' @keywords internal
#' @seealso hook_r_loaders
unhook_function <- function(fun) {
  '!DEBUG unhook_function()'
  if (!is_hooked(fun)) return(fun)

  body(fun) <- as.call(as.list(body(fun)[-2]))

  fun
}


#' replace a binding in a namespace
#'
#' @param ns			 string or namespace environment.
#' @param name		the name of the binding/variable
#' @param value		the value of the bindingvariable
#'
#' @keywords internal
#' @seealso hook_r_loaders
replace_binding <- function(ns, name, value) {
  '!DEBUG replace_binding(name="`name`")'
  ns <- asNamespace(ns)
  unlock <- get('unlockBinding') # to fool r CMD check
  lock <-  get('lockBinding')
  islocked <- get('bindingIsLocked')


  was_locked <- islocked(name, ns)

  if (was_locked) unlock(name, ns)
  assign(name, value, ns)
  if (was_locked) lock(name, ns)

  invisible()
}

dummy_function_for_replace_binding_test <- function(x) { x + 1 }



