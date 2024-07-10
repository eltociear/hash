use std::collections::HashSet;

use error_stack::Report;
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use crate::schema::{
    data_type::{
        constraint::{extend_report, StringFormat},
        DataTypeExtensionError,
    },
    DataType, DataTypeLabel, JsonSchemaValueType,
};
use crate::schema::data_type::ExtensionBehavior;

#[expect(
    clippy::trivially_copy_pass_by_ref,
    reason = "Only used in serde skip_serializing_if"
)]
const fn is_false(value: &bool) -> bool {
    !*value
}

#[derive(Debug, Serialize, Deserialize)]
#[cfg_attr(target_arch = "wasm32", derive(tsify::Tsify))]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ClosedDataType {
    #[serde(default, skip_serializing_if = "DataTypeLabel::is_empty")]
    pub label: DataTypeLabel,

    // constraints for any types
    #[serde(rename = "type")]
    #[cfg_attr(target_arch = "wasm32", tsify(type = "[JsonSchemaValueType]"))]
    pub json_type: HashSet<JsonSchemaValueType>,
    #[serde(rename = "enum", default, skip_serializing_if = "Vec::is_empty")]
    #[cfg_attr(target_arch = "wasm32", tsify(type = "[JsonValue, ...JsonValue[]]"))]
    pub enum_values: Vec<JsonValue>,

    // constraints for number types
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[cfg_attr(target_arch = "wasm32", tsify(type = "[number, ...number[]]"))]
    pub multiple_of: Vec<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub maximum: Option<f64>,
    #[serde(default, skip_serializing_if = "is_false")]
    pub exclusive_maximum: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub minimum: Option<f64>,
    #[serde(default, skip_serializing_if = "is_false")]
    pub exclusive_minimum: bool,

    // constraints for string types
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub min_length: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_length: Option<usize>,
    #[serde(
        default,
        skip_serializing_if = "Vec::is_empty",
        with = "codec::serde::regex::iter"
    )]
    #[cfg_attr(target_arch = "wasm32", tsify(type = "[string, ...string[]]"))]
    pub pattern: Vec<Regex>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub format: Option<StringFormat>,
}

impl ClosedDataType {
    #[must_use]
    pub fn new(data_type: DataType) -> Self {
        Self {
            label: data_type.label,
            json_type: vec![data_type.json_type],
            // We don't need to check if `enum` matches `const` as this is done in the validation
            // step
            enum_values: data_type
                .const_value
                .map_or(data_type.enum_values, |const_value| vec![const_value]),
            multiple_of: data_type.multiple_of.map(|x| vec![x]).unwrap_or_default(),
            maximum: data_type.maximum,
            exclusive_maximum: data_type.exclusive_maximum,
            minimum: data_type.minimum,
            exclusive_minimum: data_type.exclusive_minimum,
            min_length: data_type.min_length,
            max_length: data_type.max_length,
            pattern: data_type.pattern.map(|x| vec![x]).unwrap_or_default(),
            format: data_type.format,
        }
    }

    /// Extends the current data type with the given data type.
    ///
    ///  
    pub fn extend(&mut self, mut other: DataType) -> Result<(), Report<DataTypeExtensionError>> {
        let mut result = Ok::<_, Report<DataTypeExtensionError>>(());

        match (self.json_type, other.json_type) {
            (child, parent) if child == parent => {}
            (JsonSchemaValueType::Integer, JsonSchemaValueType::Number)
            | (JsonSchemaValueType::Number, JsonSchemaValueType::Integer) => {}
            (child, parent) => {
                extend_report!(
                    result,
                    DataTypeExtensionError::TypeMismatch { child, parent }
                );
            }
        }

        let mut new_const_value = None;
        match (&self.const_value, other.const_value) {
            (None, Some(parent)) => {
                new_const_value = Some(parent);
            }
            (Some(child), Some(parent)) if *child != parent => {
                extend_report!(
                    result,
                    DataTypeExtensionError::ConstMismatch {
                        child: child.clone(),
                        parent
                    }
                );
            }
            _ => {}
        }

        let mut new_enum_values = Vec::new();
        if !other.enum_values.is_empty() {
            if self.enum_values.is_empty() {
                new_enum_values = other.enum_values;
            } else {
                for value in &self.enum_values {
                    if let Some(index) = other.enum_values.iter().position(|v| v == value) {
                        // In validation we ensure that no duplicates are present, so we can
                        // safely remove the value from the other enum values
                        new_enum_values.push(other.enum_values.swap_remove(index));
                    }
                }
            }
        }

        let mut new_multiple_of = None;
        match (self.multiple_of, other.multiple_of) {
            (None, Some(parent)) => {
                new_multiple_of = Some(parent);
            }
            (Some(child), Some(parent))
                if (child % parent > f64::EPSILON) && (parent % child > f64::EPSILON) =>
            {
                extend_report!(
                    result,
                    DataTypeExtensionError::MultipleOfMismatch { child, parent }
                );
            }
            _ => {}
        }

        let mut new_maximum = None;
        match (self.maximum, other.maximum) {
            (None, Some(parent)) => {
                new_maximum = Some(parent);
            }
            (Some(child), Some(parent)) if parent < child => {
                new_maximum = Some(parent);
            }
            _ => {}
        }

        let mut new_minimum = None;
        match (self.minimum, other.minimum) {
            (None, Some(parent)) => {
                new_minimum = Some(parent);
            }
            (Some(child), Some(parent)) if parent > child => {
                new_minimum = Some(parent);
            }
            _ => {}
        }

        let mut new_max_length = None;
        match (self.max_length, other.max_length) {
            (None, Some(parent)) => {
                new_max_length = Some(parent);
            }
            (Some(child), Some(parent)) if parent < child => {
                new_max_length = Some(parent);
            }
            _ => {}
        }

        let mut new_min_length = None;
        match (self.min_length, other.min_length) {
            (None, Some(parent)) => {
                new_min_length = Some(parent);
            }
            (Some(child), Some(parent)) if parent > child => {
                new_min_length = Some(parent);
            }
            _ => {}
        }

        let mut new_pattern = None;
        match (&self.pattern, other.pattern) {
            (None, Some(parent)) => {
                new_pattern = Some(parent);
            }
            (Some(child), Some(parent)) if child.as_str() != parent.as_str() => {
                extend_report!(
                    result,
                    DataTypeExtensionError::PatternMismatch {
                        child: child.clone(),
                        parent
                    }
                );
            }
            _ => {}
        }

        let mut new_format = None;
        match (&self.format, other.format) {
            (None, Some(parent)) => {
                new_format = Some(parent);
            }
            (Some(child), Some(parent)) if *child != parent => {
                extend_report!(
                    result,
                    DataTypeExtensionError::FormatMismatch {
                        child: *child,
                        parent
                    }
                );
            }
            _ => {}
        }

        // Make sure we don't overwrite the result if we have an error
        result?;

        if let Some(const_value) = new_const_value {
            self.const_value = Some(const_value);
        }
        if !new_enum_values.is_empty() {
            self.enum_values = new_enum_values;
        }
        if let Some(multiple_of) = new_multiple_of {
            self.multiple_of = Some(multiple_of);
        }
        if let Some(maximum) = new_maximum {
            self.maximum = Some(maximum);
        }
        self.exclusive_maximum = self.exclusive_maximum || other.exclusive_maximum;
        if let Some(minimum) = new_minimum {
            self.minimum = Some(minimum);
        }
        self.exclusive_minimum = self.exclusive_minimum || other.exclusive_minimum;
        if let Some(max_length) = new_max_length {
            self.max_length = Some(max_length);
        }
        if let Some(min_length) = new_min_length {
            self.min_length = Some(min_length);
        }
        if let Some(pattern) = new_pattern {
            self.pattern = Some(pattern);
        }
        if let Some(format) = new_format {
            self.format = Some(format);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use regex::Regex;
    use serde_json::{json, Value as JsonValue};

    use crate::{
        schema::{ClosedDataType, DataType, DataTypeValidator},
        utils::tests::{ensure_validation, ensure_validation_from_str, JsonEqualityCheck},
    };

    #[tokio::test]
    async fn empty_list() {
        let empty_list = ensure_validation_from_str::<DataType, _>(
            graph_test_data::data_type::EMPTY_LIST_V1,
            DataTypeValidator,
            JsonEqualityCheck::Yes,
        )
        .await;

        let closed_schema = ClosedDataType::new(empty_list.into_inner());
        assert_eq!(closed_schema.enum_values, [JsonValue::Array(Vec::new())]);
    }

    #[tokio::test]
    async fn zip_code() {
        let zip_code_pattern = "^[0-9]{5}(?:-[0-9]{4})?$";
        let zip_code = ensure_validation::<ClosedDataType, _>(
            json!({
                "type": ["string"],
                "pattern": [zip_code_pattern],
            }),
            DataTypeValidator,
            JsonEqualityCheck::Yes,
        )
        .await;

        assert_eq!(
            zip_code
                .pattern
                .iter()
                .map(Regex::to_string)
                .collect::<Vec<_>>(),
            [zip_code_pattern]
        );
    }
}
