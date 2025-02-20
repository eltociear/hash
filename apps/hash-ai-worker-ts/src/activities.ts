import type {
  Embedding,
  EntityEmbedding,
  GraphApi,
} from "@local/hash-graph-client";
import type { PropertyObject } from "@local/hash-graph-types/entity";
import type {
  DataTypeWithMetadata,
  EntityTypeWithMetadata,
  PropertyTypeWithMetadata,
} from "@local/hash-graph-types/ontology";
import type {
  CreateEmbeddingsParams,
  CreateEmbeddingsReturn,
} from "@local/hash-isomorphic-utils/ai-inference-types";
import type { ParseTextFromFileParams } from "@local/hash-isomorphic-utils/parse-text-from-file-types";
import type { CreateEmbeddingResponse } from "openai/resources";

import { getAiAssistantAccountIdActivity } from "./activities/get-ai-assistant-account-id-activity";
import { getDereferencedEntityTypesActivity } from "./activities/get-dereferenced-entity-types-activity";
import { getWebPageActivity } from "./activities/get-web-page-activity";
import { getWebSearchResultsActivity } from "./activities/get-web-search-results-activity";
import { inferEntitiesFromWebPageActivity } from "./activities/infer-entities-from-web-page-activity";
import { parseTextFromFile } from "./activities/parse-text-from-file";
import {
  createDataTypeEmbeddings,
  createEmbeddings,
  createEntityEmbeddings,
  createEntityTypeEmbeddings,
  createPropertyTypeEmbeddings,
} from "./activities/shared/embeddings";

export { createGraphActivities } from "./activities/graph";

export const createAiActivities = ({
  graphApiClient,
}: {
  graphApiClient: GraphApi;
}) => ({
  async parseTextFromFileActivity(
    params: ParseTextFromFileParams,
  ): Promise<void> {
    return parseTextFromFile({ graphApiClient }, params);
  },

  async createEmbeddingsActivity(
    params: CreateEmbeddingsParams,
  ): Promise<CreateEmbeddingsReturn> {
    return createEmbeddings(params);
  },

  async createDataTypeEmbeddingsActivity(params: {
    dataType: DataTypeWithMetadata;
  }): Promise<{
    embedding: Embedding;
    usage: CreateEmbeddingResponse.Usage;
  }> {
    return createDataTypeEmbeddings({
      dataType: params.dataType,
    });
  },

  async createPropertyTypeEmbeddingsActivity(params: {
    propertyType: PropertyTypeWithMetadata;
  }): Promise<{
    embedding: Embedding;
    usage: CreateEmbeddingResponse.Usage;
  }> {
    return createPropertyTypeEmbeddings({
      propertyType: params.propertyType,
    });
  },

  async createEntityTypeEmbeddingsActivity(params: {
    entityType: EntityTypeWithMetadata;
  }): Promise<{
    embedding: Embedding;
    usage: CreateEmbeddingResponse.Usage;
  }> {
    return createEntityTypeEmbeddings({
      entityType: params.entityType,
    });
  },

  async createEntityEmbeddingsActivity(params: {
    entityProperties: PropertyObject;
    propertyTypes: PropertyTypeWithMetadata[];
  }): Promise<{
    embeddings: EntityEmbedding[];
    usage: CreateEmbeddingResponse.Usage;
  }> {
    return createEntityEmbeddings({
      entityProperties: params.entityProperties,
      propertyTypes: params.propertyTypes.map((propertyType) => ({
        title: propertyType.schema.title,
        $id: propertyType.schema.$id,
      })),
    });
  },

  getWebSearchResultsActivity,

  getWebPageActivity,

  async getDereferencedEntityTypesActivity(
    params: Omit<
      Parameters<typeof getDereferencedEntityTypesActivity>[0],
      "graphApiClient"
    >,
  ) {
    return getDereferencedEntityTypesActivity({
      ...params,
      graphApiClient,
    });
  },

  async getAiAssistantAccountIdActivity(
    params: Omit<
      Parameters<typeof getAiAssistantAccountIdActivity>[0],
      "graphApiClient"
    >,
  ) {
    return getAiAssistantAccountIdActivity({
      ...params,
      graphApiClient,
    });
  },

  inferEntitiesFromWebPageActivity,
});
