use serde::Deserialize;
use type_system::url::VersionedUrl;

use crate::knowledge::{
    property::provenance::PropertyProvenance, Confidence, Property, PropertyPath,
};

#[derive(Debug, Deserialize)]
#[cfg_attr(feature = "utoipa", derive(utoipa::ToSchema))]
#[serde(rename_all = "camelCase", tag = "op")]
pub enum PropertyPatchOperation {
    Add {
        path: PropertyPath<'static>,
        value: Property,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[cfg_attr(feature = "utoipa", schema(nullable = false))]
        confidence: Option<Confidence>,
        #[serde(default, skip_serializing_if = "PropertyProvenance::is_empty")]
        #[cfg_attr(feature = "utoipa", schema(nullable = false))]
        provenance: PropertyProvenance,
        #[serde(default, skip_serializing_if = "PropertyProvenance::is_empty")]
        #[cfg_attr(feature = "utoipa", schema(nullable = false))]
        data_type_id: Option<VersionedUrl>,
    },
    Remove {
        path: PropertyPath<'static>,
    },
    Replace {
        path: PropertyPath<'static>,
        value: Property,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[cfg_attr(feature = "utoipa", schema(nullable = false))]
        confidence: Option<Confidence>,
        #[serde(default, skip_serializing_if = "PropertyProvenance::is_empty")]
        #[cfg_attr(feature = "utoipa", schema(nullable = false))]
        provenance: PropertyProvenance,
        #[serde(default, skip_serializing_if = "PropertyProvenance::is_empty")]
        #[cfg_attr(feature = "utoipa", schema(nullable = false))]
        data_type_id: Option<VersionedUrl>,
    },
}
