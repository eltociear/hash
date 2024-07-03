pub(crate) mod error;
pub(in crate::ontology) mod raw;

use crate::ValidationError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OneOfSchema<T> {
    pub possibilities: Vec<T>,
}

impl<T> OneOfSchema<T> {
    /// Creates a new `OneOf` without validating.
    pub fn new_unchecked<U: Into<Vec<T>>>(possibilities: U) -> Self {
        Self {
            possibilities: possibilities.into(),
        }
    }

    /// Creates a new `OneOf` from the given vector.
    ///
    /// # Errors
    ///
    /// - [`ValidationError`] if the object is not in a valid state.
    pub fn new<U: Into<Vec<T>>>(one_of: U) -> Result<Self, ValidationError> {
        let one_of = Self::new_unchecked(one_of);
        one_of.validate()?;
        Ok(one_of)
    }

    #[must_use]
    pub fn possibilities(&self) -> &[T] {
        &self.possibilities
    }

    fn validate(&self) -> Result<(), ValidationError> {
        if self.possibilities.is_empty() {
            return Err(ValidationError::EmptyOneOf);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use crate::{
        raw, utils::tests::ensure_failed_validation, ParseOneOfError, PropertyValues,
        ValidationError,
    };

    type OneOf = super::OneOfSchema<PropertyValues>;
    type OneOfRepr = raw::OneOfSchema<raw::PropertyValues>;

    #[test]
    fn empty() {
        ensure_failed_validation::<OneOfRepr, OneOf>(
            &json!({
                "oneOf": []
            }),
            &ParseOneOfError::ValidationError(ValidationError::EmptyOneOf),
        );
    }
}
