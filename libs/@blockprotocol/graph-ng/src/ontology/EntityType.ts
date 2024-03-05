import * as S from "@effect/schema/Schema";

import * as VersionedUrl from "../VersionedUrl";
import * as EntityTypeReference from "./EntityTypeReference";
import * as EntityTypeUrl from "./EntityTypeUrl";
import * as PropertyTypeReference from "./PropertyTypeReference";
import * as PropertyTypeUrl from "./PropertyTypeUrl";

const TypeId: unique symbol = Symbol.for(
  "@blockprotocol/graph/ontology/EntityType",
);
export type TypeId = typeof TypeId;

export const EntityType = S.struct({
  $schema: S.literal(
    "https://blockprotocol.org/types/modules/graph/0.3/schema/entity-type",
  ),
  kind: S.literal("entityType"),

  $id: EntityTypeUrl.EntityTypeUrl,
  title: S.string.pipe(S.nonEmpty()),
  description: S.optional(S.string.pipe(S.nonEmpty())),
  examples: S.record(S.string, S.any),

  allOf: S.optional(S.array(EntityTypeReference.EntityTypeReference), {
    default: () => [],
  }),

  type: S.literal("object"),
  // TODO: PropertyArray :peepo:
  properties: S.record(PropertyTypeUrl.PropertyTypeUrl, S.any),
  required: S.readonlySet(PropertyTypeUrl.PropertyTypeUrl),
}).pipe(
  S.filter((value) => {
    const propertyKeys = Object.keys(value.properties);
    for (const required of value.required) {
      if (!propertyKeys.includes(required)) {
        return false;
      }
    }

    return true;
  }),
  S.brand(TypeId),
);

export type EntityType = S.Schema.To<typeof EntityType>;

// TODO: generate an appropriate schema (w/ transformation functions) for EntityType!
