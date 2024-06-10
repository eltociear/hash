use core::borrow::Borrow;
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    num::NonZero,
};

use error_stack::{bail, Report, ResultExt};
use futures::{stream, StreamExt, TryStreamExt};
use graph_types::knowledge::{
    entity::{Entity, EntityId},
    link::LinkData,
    Property, PropertyMetadata, PropertyMetadataMap, PropertyMetadataMapElement,
    PropertyMetadataMapValue, PropertyObject, PropertyPath,
};
use thiserror::Error;
use type_system::{
    url::{BaseUrl, OntologyTypeVersion, VersionedUrl},
    Array, ClosedEntityType, DataType, JsonSchemaValueType, Object, PropertyType,
    PropertyTypeReference, PropertyValues, ValueOrArray,
};

use crate::{
    error::{Actual, Expected},
    EntityProvider, EntityTypeProvider, OntologyTypeProvider, PropertyValidationError, Schema,
    Validate, ValidateEntityComponents,
};

macro_rules! extend_report {
    ($status:ident, $error:expr $(,)?) => {
        if let Err(ref mut report) = $status {
            report.extend_one(error_stack::report!($error))
        } else {
            $status = Err(error_stack::report!($error))
        }
    };
}

#[derive(Debug, Error)]
pub enum EntityValidationError {
    #[error("The properties of the entity do not match the schema")]
    InvalidProperties,
    #[error("The entity is not a link but contains link data")]
    UnexpectedLinkData,
    #[error("The entity is a link but does not contain link data")]
    MissingLinkData,
    #[error("Entities without a type are not allowed")]
    EmptyEntityTypes,
    #[error("the validator was unable to read the entity type `{ids:?}`")]
    EntityTypeRetrieval { ids: Vec<VersionedUrl> },
    #[error("the validator was unable to read the entity `{id}`")]
    EntityRetrieval { id: EntityId },
    #[error("The link type `{link_types:?}` is not allowed")]
    InvalidLinkTypeId { link_types: Vec<VersionedUrl> },
    #[error("The link target `{target_types:?}` is not allowed")]
    InvalidLinkTargetId { target_types: Vec<VersionedUrl> },
    #[error("The property path is invalid: `{path:?}`")]
    InvalidPropertyPath { path: PropertyPath<'static> },
}

#[derive(Debug, Copy, Clone)]
pub struct EntitySchema<'a> {
    pub entity_type: &'a ClosedEntityType,
    pub metadata: &'a PropertyMetadataMap,
}

impl<P> Validate<EntitySchema<'_>, P> for PropertyObject {
    type Error = EntityValidationError;

    async fn validate(
        &self,
        schema: &EntitySchema<'_>,
        components: ValidateEntityComponents,
        context: &P,
    ) -> Result<(), Report<Self::Error>> {
        self.validate(
            &PropertyObjectSchema {
                properties: &schema.entity_type.properties,
                required: &schema.entity_type.required,
                metadata: schema.metadata.as_ref(),
            },
            components,
            context,
        )
        .await
        .change_context(EntityValidationError::InvalidProperties)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PropertyObjectSchema<'a> {
    pub properties: &'a HashMap<BaseUrl, ValueOrArray<PropertyTypeReference>>,
    pub metadata: &'a HashMap<BaseUrl, PropertyMetadataMapElement>,
    pub required: &'a HashSet<BaseUrl>,
}

impl<P> Validate<PropertyObjectSchema<'_>, P> for PropertyObject {
    type Error = PropertyValidationError;

    async fn validate(
        &self,
        schema: &PropertyObjectSchema<'_>,
        components: ValidateEntityComponents,
        context: &P,
    ) -> Result<(), Report<Self::Error>> {
        let mut status: Result<(), Report<PropertyValidationError>> = Ok(());

        for (key, property) in self.iter() {
            let Some(property_schema) = schema.properties.get(key) else {
                extend_report!(
                    status,
                    PropertyValidationError::UnexpectedProperty { key: key.clone() }
                );
                continue;
            };

            let (metadata_map, metadata) = schema
                .metadata
                .get(key)
                .map(|element| (element.nested.as_ref(), element.metadata.as_ref()))
                .unwrap_or_default();

            match (property, property_schema) {
                (value, ValueOrArray::Value(schema)) => {
                    let result = value
                        .validate(
                            &PropertySchema {
                                property: PropertyRef::Reference(schema.url()),
                                metadata_map,
                                metadata,
                            },
                            components,
                            context,
                        )
                        .await;
                    if let Err(report) = result {
                        extend_report!(
                            status,
                            report.change_context(PropertyValidationError::InvalidProperty {
                                key: key.clone(),
                            })
                        );
                    }
                }
                (Property::Array(array), ValueOrArray::Array(schema)) => {
                    if let Some(metadata) = metadata {
                        extend_report!(
                            status,
                            PropertyValidationError::UnexpectedMetadata {
                                key: metadata.key.clone()
                            }
                        );
                    }
                    array
                        .validate_value(
                            &PropertyArraySchema {
                                items: &schema.items,
                                min_items: schema.min_items,
                                max_items: schema.max_items,
                                metadata: &Default::default(),
                            },
                            components,
                            provider,
                        )
                        .await
                }
                (_, ValueOrArray::Array(_)) => {
                    bail!(PropertyValidationError::InvalidType {
                        actual: property.json_type(),
                        expected: JsonSchemaValueType::Array,
                    })
                }
            }

            if let Err(report) = property
                .validate(
                    &PropertySchema {
                        property,
                        metadata_map,
                        metadata,
                    },
                    components,
                    context,
                )
                .await
            {
                extend_report!(
                    status,
                    report.change_context(PropertyValidationError::InvalidProperty {
                        key: key.clone(),
                    })
                );
            }
        }

        for metadata_key in schema.metadata.keys() {
            if !self.properties().contains_key(metadata_key) {
                extend_report!(
                    status,
                    PropertyValidationError::UnexpectedMetadata {
                        key: metadata_key.clone()
                    }
                );
            }
        }

        if components.required_properties {
            for required_property in schema.required {
                if !self.properties().contains_key(required_property) {
                    extend_report!(
                        status,
                        PropertyValidationError::MissingRequiredProperty {
                            key: required_property.clone(),
                        }
                    );
                }
            }
        }

        status
    }
}

impl<S, P> Validate<ValueOrArray<PropertyTypeReference>, P> for Property
where
    Property: Validate<S, P>,
{
    type Error = PropertyValidationError;

    async fn validate(
        &self,
        schema: &ValueOrArray<PropertyMetadataMapElement>,
        components: ValidateEntityComponents,
        context: &P,
    ) -> Result<(), Report<Self::Error>> {
        match (self, schema) {
            (value, ValueOrArray::Value(schema)) => {
                value.validate(schema, components, context).await
            }
            (Property::Array(array), ValueOrArray::Array(schema)) => {
                array.validate(array, components, context).await
            }
            (_, ValueOrArray::Array(_)) => {
                bail!(PropertyValidationError::InvalidType {
                    actual: self.json_type(),
                    expected: JsonSchemaValueType::Array,
                })
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ValueOrArraySchema<'a> {
    pub property_type: PropertyRef<'a>,
    pub metadata: &'a PropertyMetadataMapElement,
}

pub struct PropertyArraySchema<'a> {
    pub items: &'a PropertyTypeReference,
    pub min_items: Option<usize>,
    pub max_items: Option<NonZero<usize>>,
    pub metadata: &'a HashMap<usize, PropertyMetadataMapElement>,
}

impl<P> Validate<PropertyArraySchema<'_>, P> for &[Property] {
    type Error = PropertyValidationError;

    async fn validate(
        &self,
        schema: &PropertyArraySchema<'_>,
        components: ValidateEntityComponents,
        context: &P,
    ) -> Result<(), Report<Self::Error>> {
        let mut status: Result<(), Report<PropertyValidationError>> = Ok(());

        if components.num_items {
            if let Some(min) = schema.min_items {
                if self.len() < min {
                    extend_report!(
                        status,
                        PropertyValidationError::TooFewItems {
                            actual: self.len(),
                            min,
                        },
                    );
                }
            }

            if let Some(max) = schema.max_items.map(NonZero::get) {
                if self.len() > max {
                    extend_report!(
                        status,
                        PropertyValidationError::TooManyItems {
                            actual: self.len(),
                            max,
                        },
                    );
                }
            }
        }

        for (idx, property) in self.iter().enumerate() {
            let (metadata_map, metadata) = schema
                .metadata
                .get(&idx)
                .map(|element| (element.nested.as_ref(), element.metadata.as_ref()))
                .unwrap_or_default();

            if let Err(report) = property
                .validate(
                    &PropertySchema {
                        property: PropertyRef::Reference(schema.items.url()),
                        metadata,
                        metadata_map,
                    },
                    components,
                    context,
                )
                .await
            {
                extend_report!(status, report);
            }
        }

        status
    }
}

#[derive(Debug, Copy, Clone)]
pub enum PropertyRef<'a> {
    Reference(&'a VersionedUrl),
    Inline(&'a PropertyType),
}

#[derive(Debug, Copy, Clone)]
pub struct PropertySchema<'a> {
    pub property: PropertyRef<'a>,
    pub metadata: Option<&'a PropertyMetadata>,
    pub metadata_map: Option<&'a PropertyMetadataMapValue>,
}

impl<P> Schema<PropertyObject, P> for ClosedEntityType
where
    P: OntologyTypeProvider<PropertyType> + OntologyTypeProvider<DataType> + Sync,
{
    type Error = EntityValidationError;

    async fn validate_value<'a>(
        &'a self,
        value: &'a PropertyObject,
        components: ValidateEntityComponents,
        provider: &'a P,
    ) -> Result<(), Report<EntityValidationError>> {
        // TODO: Distinguish between format validation and content validation so it's possible
        //       to directly use the correct type.
        //   see https://linear.app/hash/issue/BP-33
        Object::<_, 0>::new(self.properties.clone(), self.required.clone())
            .expect("`Object` was already validated")
            .validate_value(value.properties(), components, provider)
            .await
            .change_context(EntityValidationError::InvalidProperties)
            .attach_lazy(|| Expected::EntityType(self.clone()))
            .attach_lazy(|| Actual::Properties(value.clone()))
    }
}

impl<P> Validate<ClosedEntityType, P> for PropertyObject
where
    P: OntologyTypeProvider<PropertyType> + OntologyTypeProvider<DataType> + Sync,
{
    type Error = EntityValidationError;

    async fn validate(
        &self,
        schema: &ClosedEntityType,
        components: ValidateEntityComponents,
        context: &P,
    ) -> Result<(), Report<Self::Error>> {
        schema.validate_value(self, components, context).await
    }
}

impl<P> Validate<ClosedEntityType, P> for Option<&LinkData>
where
    P: EntityProvider
        + EntityTypeProvider
        + OntologyTypeProvider<PropertyType>
        + OntologyTypeProvider<DataType>
        + Sync,
{
    type Error = EntityValidationError;

    async fn validate(
        &self,
        schema: &ClosedEntityType,
        components: ValidateEntityComponents,
        context: &P,
    ) -> Result<(), Report<Self::Error>> {
        if !components.link_data {
            return Ok(());
        }

        let mut status: Result<(), Report<EntityValidationError>> = Ok(());

        // TODO: The link type should be a const but the type system crate does not allow
        //       to make this a `const` variable.
        //   see https://linear.app/hash/issue/BP-57
        let link_type_id = VersionedUrl {
            base_url: BaseUrl::new(
                "https://blockprotocol.org/@blockprotocol/types/entity-type/link/".to_owned(),
            )
            .expect("Not a valid URL"),
            version: OntologyTypeVersion::new(1),
        };
        let is_link = schema.schemas.contains_key(&link_type_id);

        if let Some(link_data) = self {
            if !is_link {
                extend_report!(status, EntityValidationError::UnexpectedLinkData);
            }

            if let Err(error) = schema.validate_value(*link_data, components, context).await {
                extend_report!(status, error);
            }
        } else if is_link {
            extend_report!(status, EntityValidationError::MissingLinkData);
        }

        status
    }
}

impl<P> Validate<ClosedEntityType, P> for Entity
where
    P: EntityProvider
        + EntityTypeProvider
        + OntologyTypeProvider<PropertyType>
        + OntologyTypeProvider<DataType>
        + Sync,
{
    type Error = EntityValidationError;

    async fn validate(
        &self,
        schema: &ClosedEntityType,
        components: ValidateEntityComponents,
        context: &P,
    ) -> Result<(), Report<Self::Error>> {
        let mut status: Result<(), Report<EntityValidationError>> = Ok(());

        if self.metadata.entity_type_ids.is_empty() {
            extend_report!(status, EntityValidationError::EmptyEntityTypes);
        }
        if let Err(error) = self.properties.validate(schema, components, context).await {
            extend_report!(status, error);
        }
        if let Err(error) = self
            .link_data
            .as_ref()
            .validate(schema, components, context)
            .await
        {
            extend_report!(status, error);
        }
        if let Err(error) = self
            .metadata
            .properties
            .validate(&self.properties, components, context)
            .await
        {
            extend_report!(status, error);
        }

        status
    }
}

impl<P> Schema<LinkData, P> for ClosedEntityType
where
    P: EntityProvider + EntityTypeProvider + Sync,
{
    type Error = EntityValidationError;

    // TODO: validate link data
    //   see https://linear.app/hash/issue/H-972
    async fn validate_value<'a>(
        &'a self,
        value: &'a LinkData,
        _: ValidateEntityComponents,
        provider: &'a P,
    ) -> Result<(), Report<EntityValidationError>> {
        let mut status: Result<(), Report<EntityValidationError>> = Ok(());

        let left_entity = provider
            .provide_entity(value.left_entity_id)
            .await
            .change_context_lazy(|| EntityValidationError::EntityRetrieval {
                id: value.left_entity_id,
            })?;

        let left_entity_type = stream::iter(&left_entity.borrow().metadata.entity_type_ids)
            .then(|entity_type| async {
                Ok::<_, Report<EntityValidationError>>(
                    provider
                        .provide_type(entity_type)
                        .await
                        .change_context_lazy(|| EntityValidationError::EntityTypeRetrieval {
                            ids: left_entity.borrow().metadata.entity_type_ids.clone(),
                        })?
                        .borrow()
                        .clone(),
                )
            })
            .try_collect::<Self>()
            .await?;

        let right_entity = provider
            .provide_entity(value.right_entity_id)
            .await
            .change_context_lazy(|| EntityValidationError::EntityRetrieval {
                id: value.right_entity_id,
            })?;

        let right_entity_type = stream::iter(&right_entity.borrow().metadata.entity_type_ids)
            .then(|entity_type| async {
                Ok::<_, Report<EntityValidationError>>(
                    provider
                        .provide_type(entity_type)
                        .await
                        .change_context_lazy(|| EntityValidationError::EntityTypeRetrieval {
                            ids: right_entity.borrow().metadata.entity_type_ids.clone(),
                        })?
                        .borrow()
                        .clone(),
                )
            })
            .try_collect::<Self>()
            .await?;

        // We track that at least one link type was found to avoid reporting an error if no
        // link type was found.
        let mut found_link_target = false;
        for link_type_id in self.schemas.keys() {
            let Some(maybe_allowed_targets) = left_entity_type.links.links().get(link_type_id)
            else {
                continue;
            };

            // At least one link type was found
            found_link_target = true;

            let Some(allowed_targets) = maybe_allowed_targets.array().items() else {
                continue;
            };

            // Link destinations are constrained, search for the right entity's type
            let mut found_match = false;
            for allowed_target in allowed_targets.one_of() {
                if right_entity_type
                    .schemas
                    .keys()
                    .any(|right_type| right_type.base_url == allowed_target.url().base_url)
                {
                    found_match = true;
                    break;
                }
            }

            if !found_match {
                extend_report!(
                    status,
                    EntityValidationError::InvalidLinkTargetId {
                        target_types: right_entity_type.schemas.keys().cloned().collect(),
                    }
                );
            }
        }

        if !found_link_target {
            extend_report!(
                status,
                EntityValidationError::InvalidLinkTypeId {
                    link_types: self.schemas.keys().cloned().collect(),
                }
            );
        }

        status
    }
}

#[cfg(test)]
mod tests {
    use crate::{tests::validate_entity, ValidateEntityComponents};

    #[tokio::test]
    async fn address() {
        let entities = [];
        let entity_types = [];
        let property_types = [
            graph_test_data::property_type::ADDRESS_LINE_1_V1,
            graph_test_data::property_type::POSTCODE_NUMBER_V1,
            graph_test_data::property_type::CITY_V1,
        ];
        let data_types = [graph_test_data::data_type::TEXT_V1];

        validate_entity(
            graph_test_data::entity::ADDRESS_V1,
            graph_test_data::entity_type::UK_ADDRESS_V1,
            entities,
            entity_types,
            property_types,
            data_types,
            ValidateEntityComponents::full(),
        )
        .await
        .expect("validation failed");
    }

    #[tokio::test]
    async fn block() {
        let entities = [];
        let entity_types = [];
        let property_types = [graph_test_data::property_type::NAME_V1];
        let data_types = [graph_test_data::data_type::TEXT_V1];

        validate_entity(
            graph_test_data::entity::BLOCK_V1,
            graph_test_data::entity_type::BLOCK_V1,
            entities,
            entity_types,
            property_types,
            data_types,
            ValidateEntityComponents::full(),
        )
        .await
        .expect("validation failed");
    }

    #[tokio::test]
    async fn book() {
        let entities = [];
        let entity_types = [];
        let property_types = [
            graph_test_data::property_type::NAME_V1,
            graph_test_data::property_type::BLURB_V1,
            graph_test_data::property_type::PUBLISHED_ON_V1,
        ];
        let data_types = [graph_test_data::data_type::TEXT_V1];

        validate_entity(
            graph_test_data::entity::BOOK_V1,
            graph_test_data::entity_type::BOOK_V1,
            entities,
            entity_types,
            property_types,
            data_types,
            ValidateEntityComponents::full(),
        )
        .await
        .expect("validation failed");
    }

    #[tokio::test]
    async fn building() {
        let entities = [];
        let entity_types = [];
        let property_types = [];
        let data_types = [];

        validate_entity(
            graph_test_data::entity::BUILDING_V1,
            graph_test_data::entity_type::BUILDING_V1,
            entities,
            entity_types,
            property_types,
            data_types,
            ValidateEntityComponents::full(),
        )
        .await
        .expect("validation failed");
    }

    #[tokio::test]
    async fn organization() {
        let entities = [];
        let entity_types = [];
        let property_types = [graph_test_data::property_type::NAME_V1];
        let data_types = [graph_test_data::data_type::TEXT_V1];

        validate_entity(
            graph_test_data::entity::ORGANIZATION_V1,
            graph_test_data::entity_type::ORGANIZATION_V1,
            entities,
            entity_types,
            property_types,
            data_types,
            ValidateEntityComponents::full(),
        )
        .await
        .expect("validation failed");
    }

    #[tokio::test]
    async fn page() {
        let entities = [];
        let entity_types = [];
        let property_types = [graph_test_data::property_type::TEXT_V1];
        let data_types = [graph_test_data::data_type::TEXT_V1];

        validate_entity(
            graph_test_data::entity::PAGE_V1,
            graph_test_data::entity_type::PAGE_V1,
            entities.to_vec(),
            entity_types,
            property_types,
            data_types,
            ValidateEntityComponents::full(),
        )
        .await
        .expect("validation failed");

        validate_entity(
            graph_test_data::entity::PAGE_V2,
            graph_test_data::entity_type::PAGE_V2,
            entities,
            entity_types,
            property_types,
            data_types,
            ValidateEntityComponents::full(),
        )
        .await
        .expect("validation failed");
    }

    #[tokio::test]
    async fn person() {
        let entities = [];
        let entity_types = [];
        let property_types = [
            graph_test_data::property_type::NAME_V1,
            graph_test_data::property_type::AGE_V1,
        ];
        let data_types = [
            graph_test_data::data_type::TEXT_V1,
            graph_test_data::data_type::NUMBER_V1,
        ];

        validate_entity(
            graph_test_data::entity::PERSON_ALICE_V1,
            graph_test_data::entity_type::PERSON_V1,
            entities.to_vec(),
            entity_types,
            property_types,
            data_types,
            ValidateEntityComponents::full(),
        )
        .await
        .expect("validation failed");

        validate_entity(
            graph_test_data::entity::PERSON_BOB_V1,
            graph_test_data::entity_type::PERSON_V1,
            entities.to_vec(),
            entity_types,
            property_types,
            data_types,
            ValidateEntityComponents::full(),
        )
        .await
        .expect("validation failed");

        validate_entity(
            graph_test_data::entity::PERSON_CHARLES_V1,
            graph_test_data::entity_type::PERSON_V1,
            entities,
            entity_types,
            property_types,
            data_types,
            ValidateEntityComponents::full(),
        )
        .await
        .expect("validation failed");
    }

    #[tokio::test]
    async fn playlist() {
        let entities = [];
        let entity_types = [];
        let property_types = [graph_test_data::property_type::NAME_V1];
        let data_types = [graph_test_data::data_type::TEXT_V1];

        validate_entity(
            graph_test_data::entity::PLAYLIST_V1,
            graph_test_data::entity_type::PLAYLIST_V1,
            entities,
            entity_types,
            property_types,
            data_types,
            ValidateEntityComponents::full(),
        )
        .await
        .expect("validation failed");
    }

    #[tokio::test]
    async fn song() {
        let entities = [];
        let entity_types = [];
        let property_types = [graph_test_data::property_type::NAME_V1];
        let data_types = [graph_test_data::data_type::TEXT_V1];

        validate_entity(
            graph_test_data::entity::SONG_V1,
            graph_test_data::entity_type::SONG_V1,
            entities,
            entity_types,
            property_types,
            data_types,
            ValidateEntityComponents::full(),
        )
        .await
        .expect("validation failed");
    }
}
