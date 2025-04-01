package framework.components

import framework.localization.LocalizedAppliedValidate
import yantl.Validate

/** Validator that always fails if autocomplete field has a needle and not a selected value. */
val AutocompleteValidateValueRequired: Validate[Option[String], AutocompleteValidateValueRequiredError.type] =
  AutocompleteValidateValueRequiredWith(AutocompleteValidateValueRequiredError)

/** Validator that always fails if autocomplete field has a needle and not a selected value. */
def AutocompleteValidateValueRequiredWith[TError](error: TError): Validate[Option[String], TError] =
  Validate.of((opt: Option[String]) => opt.fold2(Vector.empty, _ => Vector(error)))

case object AutocompleteValidateValueRequiredError
