import {
  Either,
  Equal,
  Hash,
  HashSet,
  Inspectable,
  Option,
  pipe,
  Pipeable,
  Predicate,
} from "effect";
import * as S from "@effect/schema/Schema";
import * as DataTypeUrl from "./DataTypeUrl";
import * as Json from "../internal/Json";
import { EncodeError } from "./DataType/errors";
import { encodeSchema } from "./DataType/encode";
import { DataTypeSchema } from "./DataType/schema";

const TypeId: unique symbol = Symbol.for(
  "@blockprotocol/graph/ontology/DataType",
);
export type TypeId = typeof TypeId;

/** @internal */
export const AnnotationId: unique symbol = Symbol.for(
  "@blockprotocol/graph/ontology/DataType/Annotation",
);
/** @internal */
export type AnnotationId = typeof AnnotationId;

interface Annotations {}

export interface DataType<T>
  extends Equal.Equal,
    Pipeable.Pipeable,
    Inspectable.Inspectable {
  [TypeId]: TypeId;

  readonly id: DataTypeUrl.DataTypeUrl;
  readonly schema: S.Schema<T, Json.Value>;

  readonly annotations: Annotations;
}

interface DataTypeImpl<T> extends DataType<T> {}

const DataTypeProto: Omit<DataTypeImpl<unknown>, "id" | "schema"> = {
  [TypeId]: TypeId,
  annotations: {},

  toJSON(this: DataTypeImpl<unknown>): unknown {
    return {
      _id: "DataType",
      id: this.id,
      schema: this.schema.ast.toJSON(),
    };
  },
  toString(this: DataTypeImpl<unknown>): string {
    return Inspectable.format(this.toJSON());
  },
  [Inspectable.NodeInspectSymbol]() {
    return this.toJSON();
  },
  pipe() {
    Pipeable.pipeArguments(this, arguments);
  },

  [Hash.symbol](this: DataTypeImpl<unknown>) {
    const hash = pipe(Hash.hash(TypeId), Hash.combine(Hash.hash(this.id)));

    return Hash.cached(this, hash);
  },
  [Equal.symbol]<T>(this: DataType<T>, that: unknown): boolean {
    if (!isDataType(that)) {
      return false;
    }

    return this.id === that.id;
  },
};

export function isDataType(value: unknown): value is DataType<unknown> {
  return Predicate.hasProperty(value, TypeId);
}

function makeImpl<T>(
  id: DataTypeUrl.DataTypeUrl,
  schema: S.Schema<T, Json.Value>,
): DataTypeImpl<T> {
  const impl = Object.create(DataTypeProto);
  impl.id = id;

  impl.schema = schema.annotations({
    [AnnotationId]: () => impl,
  });

  return impl;
}

function toSchemaImpl<T>(
  schema: S.Schema<T, Json.Value>,
): Either.Either<DataTypeSchema, EncodeError> {
  return encodeSchema(schema.ast);
}

export function make<T, E extends Json.Value>(
  id: DataTypeUrl.DataTypeUrl,
  schema: S.Schema<T, E>,
): Either.Either<DataType<T>, EncodeError> {
  // E is invariant in schema, therefore we cannot "easily" cast, this is kinda a hack, but whatever
  const covariantSchema = schema as unknown as S.Schema<T, Json.Value>;

  const impl = makeImpl(id, covariantSchema);

  return pipe(
    toSchemaImpl(impl.schema),
    Either.map(() => impl),
  );
}

export function toSchema<T>(dataType: DataType<T>): DataTypeSchema {
  const schema = toSchemaImpl(dataType.schema);

  // the value has been validated in `make<T>`
  return Either.getOrThrow(schema);
}

export { DataTypeSchema as Schema };
