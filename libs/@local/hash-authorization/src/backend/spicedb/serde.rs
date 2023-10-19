use serde::{de::IntoDeserializer, Deserialize, Deserializer, Serialize};

use crate::zanzibar::types::Resource;

pub(crate) mod resource {
    use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

    use crate::zanzibar::types::Resource;

    #[derive(Serialize, Deserialize)]
    #[serde(rename_all = "camelCase")]
    struct SerializedResource<N, I> {
        object_type: N,
        object_id: I,
    }

    pub(crate) fn serialize<T, S>(resource: &T, serializer: S) -> Result<S::Ok, S::Error>
    where
        T: Resource<Namespace: Serialize, Id: Serialize>,
        S: Serializer,
    {
        let (resource_type, resource_id) = resource.to_parts();
        SerializedResource {
            object_type: resource_type,
            object_id: resource_id,
        }
        .serialize(serializer)
    }

    pub(crate) fn deserialize<'de, T, D>(deserializer: D) -> Result<T, D::Error>
    where
        T: Resource<Namespace: Deserialize<'de>, Id: Deserialize<'de>>,
        D: Deserializer<'de>,
    {
        let resource = SerializedResource::<T::Namespace, T::Id>::deserialize(deserializer)?;
        T::from_parts(resource.object_type, resource.object_id).map_err(de::Error::custom)
    }
}

pub(crate) mod resource_ref {
    use serde::{Serialize, Serializer};

    use crate::zanzibar::types::Resource;

    #[expect(clippy::trivially_copy_pass_by_ref, reason = "Used in generic context")]
    pub(crate) fn serialize<T, S>(resource: &&T, serializer: S) -> Result<S::Ok, S::Error>
    where
        T: Resource<Namespace: Serialize, Id: Serialize>,
        S: Serializer,
    {
        super::resource::serialize(*resource, serializer)
    }
}

#[derive(Serialize, Deserialize)]
#[serde(
    rename_all = "camelCase",
    bound(
        serialize = "O: Resource<Namespace: Serialize, Id: Serialize>, R: Serialize",
        deserialize = "O: Resource<Namespace: Deserialize<'de>, Id: Deserialize<'de>>, R: \
                       Deserialize<'de>"
    )
)]
struct SerializedSubject<O, R> {
    #[serde(with = "resource")]
    object: O,
    #[serde(deserialize_with = "empty_string_as_none")]
    optional_relation: Option<R>,
}

pub(crate) mod subject_ref {
    use serde::{Serialize, Serializer};

    use crate::{
        backend::spicedb::serde::SerializedSubject,
        zanzibar::types::{Resource, Subject},
    };

    #[expect(clippy::trivially_copy_pass_by_ref, reason = "Used in generic context")]
    pub(crate) fn serialize<T, S>(subject: &&T, serializer: S) -> Result<S::Ok, S::Error>
    where
        T: Subject<Resource: Resource<Namespace: Serialize, Id: Serialize>, Relation: Serialize>,
        S: Serializer,
    {
        let (resource, optional_relation) = subject.to_parts();
        SerializedSubject {
            object: resource,
            optional_relation,
        }
        .serialize(serializer)
    }
}

fn empty_string_as_none<'de, D, T>(de: D) -> Result<Option<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    let opt = Option::<String>::deserialize(de)?;
    match opt.as_deref() {
        None | Some("") => Ok(None),
        Some(s) => T::deserialize(s.into_deserializer()).map(Some),
    }
}

pub(crate) mod relationship {
    use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

    use crate::{
        backend::spicedb::serde::SerializedSubject,
        zanzibar::{
            types::{Caveat, Relationship, Resource},
            Relation,
        },
    };

    #[derive(Serialize, Deserialize)]
    #[serde(
        rename_all = "camelCase",
        bound(
            serialize = "O: Resource<Namespace: Serialize, Id: Serialize>, R: Serialize, S: \
                         Resource<Namespace: Serialize, Id: Serialize>, SR: Serialize, C: Caveat \
                         + Serialize",
            deserialize = "O: Resource<Namespace: Deserialize<'de>, Id: Deserialize<'de>>, R: \
                           Deserialize<'de>, S: Resource<Namespace: Deserialize<'de>, Id: \
                           Deserialize<'de>>, SR: Deserialize<'de>, C: Caveat + Deserialize<'de>"
        )
    )]
    struct SerializedRelationship<O, R, S, SR, C> {
        #[serde(with = "super::resource")]
        resource: O,
        relation: R,
        subject: SerializedSubject<S, SR>,
        #[serde(
            with = "super::caveat",
            default,
            skip_serializing_if = "Option::is_none"
        )]
        optional_caveat: Option<C>,
    }

    pub(crate) fn serialize<T, S>(relationship: &T, serializer: S) -> Result<S::Ok, S::Error>
    where
        T: Relationship<
                Resource: Resource<Namespace: Serialize, Id: Serialize>,
                Relation: Relation<T::Resource> + Serialize,
                Subject: Resource<Namespace: Serialize, Id: Serialize>,
                SubjectSet: Relation<T::Subject> + Serialize,
                Caveat: Serialize,
            >,
        S: Serializer,
    {
        let (resource, relation, subject, subject_set, optional_caveat) = relationship.to_parts();
        SerializedRelationship {
            resource,
            relation,
            subject: SerializedSubject {
                object: subject,
                optional_relation: subject_set,
            },
            optional_caveat,
        }
        .serialize(serializer)
    }

    pub(crate) fn deserialize<'de, T, D>(deserializer: D) -> Result<T, D::Error>
    where
        T: Relationship<
                Resource: Resource<Namespace: Deserialize<'de>, Id: Deserialize<'de>>,
                Relation: Relation<T::Resource> + Deserialize<'de>,
                Subject: Resource<Namespace: Deserialize<'de>, Id: Deserialize<'de>>,
                SubjectSet: Relation<T::Subject> + Deserialize<'de>,
                Caveat: Deserialize<'de>,
            >,
        D: Deserializer<'de>,
    {
        let relationship = SerializedRelationship::deserialize(deserializer)?;
        T::from_parts(
            relationship.resource,
            relationship.relation,
            relationship.subject.object,
            relationship.subject.optional_relation,
            relationship.optional_caveat,
        )
        .map_err(de::Error::custom)
    }
}

pub(crate) mod caveat {
    use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

    use crate::zanzibar::types::Caveat;

    #[derive(Serialize, Deserialize)]
    #[serde(
        rename_all = "camelCase",
        bound(
            serialize = "C: Caveat + Serialize",
            deserialize = "C: Caveat + Deserialize<'de>"
        )
    )]
    struct SerializedCaveat<'n, C> {
        caveat_name: &'n str,
        context: C,
    }

    pub(crate) fn serialize<C, S>(caveat: &Option<C>, serializer: S) -> Result<S::Ok, S::Error>
    where
        C: Caveat + Serialize,
        S: Serializer,
    {
        caveat.as_ref().map_or_else(
            || unreachable!("Caveat::serialize should not be called with None"),
            |caveat| {
                SerializedCaveat {
                    caveat_name: caveat.name(),
                    context: caveat,
                }
                .serialize(serializer)
            },
        )
    }

    pub(crate) fn deserialize<'de, C, D>(deserializer: D) -> Result<Option<C>, D::Error>
    where
        C: Caveat + Deserialize<'de>,
        D: Deserializer<'de>,
    {
        let caveat = SerializedCaveat::<C>::deserialize(deserializer)?;
        if caveat.caveat_name == caveat.context.name() {
            Ok(Some(caveat.context))
        } else {
            Err(de::Error::custom(format!(
                "Expected caveat name {}, got {}",
                caveat.context.name(),
                caveat.caveat_name
            )))
        }
    }
}

pub(crate) mod relationship_filter {
    use serde::{Serialize, Serializer};

    use crate::zanzibar::types::RelationshipFilter;

    #[derive(Serialize)]
    #[serde(rename_all = "camelCase")]
    struct SubjectFilterRelationFilter<'a, R> {
        relation: &'a R,
    }

    #[derive(Serialize)]
    #[serde(rename_all = "camelCase")]
    struct SerializedSubjectFilter<'a, N, I, R> {
        subject_type: &'a N,
        #[serde(skip_serializing_if = "Option::is_none")]
        optional_subject_id: Option<&'a I>,
        #[serde(skip_serializing_if = "Option::is_none")]
        optional_relation: Option<SubjectFilterRelationFilter<'a, R>>,
    }

    #[derive(Serialize)]
    #[serde(rename_all = "camelCase")]
    struct SerializedRelationshipFilter<'a, ON, OI, R, SN, SI, SR> {
        resource_type: &'a ON,
        #[serde(skip_serializing_if = "Option::is_none")]
        optional_resource_id: Option<&'a OI>,
        #[serde(skip_serializing_if = "Option::is_none")]
        optional_relation: Option<&'a R>,
        #[serde(skip_serializing_if = "Option::is_none")]
        optional_subject_filter: Option<SerializedSubjectFilter<'a, SN, SI, SR>>,
    }

    pub(crate) fn serialize<ON, OI, R, SN, SI, SR, S>(
        relationship: &RelationshipFilter<ON, OI, R, SN, SI, SR>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        ON: Serialize,
        OI: Serialize,
        R: Serialize,
        SN: Serialize,
        SI: Serialize,
        SR: Serialize,
        S: Serializer,
    {
        SerializedRelationshipFilter {
            resource_type: &relationship.resource.namespace,
            optional_resource_id: relationship.resource.id.as_ref(),
            optional_relation: relationship.relation.as_ref(),
            optional_subject_filter: relationship.subject.as_ref().map(|subject| {
                SerializedSubjectFilter {
                    subject_type: &subject.resource.namespace,
                    optional_subject_id: subject.resource.id.as_ref(),
                    optional_relation: subject
                        .relation
                        .as_ref()
                        .map(|relation| SubjectFilterRelationFilter { relation }),
                }
            }),
        }
        .serialize(serializer)
    }
}
