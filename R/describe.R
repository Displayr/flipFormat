
#' Formats a model as an equation (string), for
#' description purposes. It shows the linear expectation of the mean, and assumes you have not manipulated
#' default assumptions (e.g., via offsets and link functions).
#'
#' @param object The model.
#' @export
setGeneric("Equation", function(object)
{
    coefs <- coef(object)
    parameter.names <- names(coefs)
    dependent.name <- outcomeName(formula(object$call))
    parameter.names[1] <- "" #Intercept.
    signs <- sign(coefs)
    operator <- rep(" + ", length(signs))
    if (sum(signs == -1) > 0)
        operator[signs == -1] <- " - "
    operator[1] <- ifelse(signs[1] == 1, "", " -")
    coefs <- FormatAsReal(abs(coefs))
    equation <- paste0(dependent.name, " = ",
                       paste0(operator, coefs, parameter.names, collapse = ""))
    if (object$type == "Poisson" || object$type == "Quasi-Poisson" )
        equation <- paste0("exp(", equation, ")")
    strwrap(equation)
})
