import type { GraphApi } from "@local/hash-graph-client";
import type {
  InputNameForAction,
  OutputNameForAction,
} from "@local/hash-isomorphic-utils/flows/step-definitions";
import type { ProposedEntity } from "@local/hash-isomorphic-utils/flows/types";
import type { Entity } from "@local/hash-subgraph";
import { mapGraphApiEntityMetadataToMetadata } from "@local/hash-subgraph/stdlib";
import { StatusCode } from "@local/status";

import type { FlowActionActivity } from "./types";

export const persistEntityAction: FlowActionActivity<{
  graphApiClient: GraphApi;
}> = async ({ inputs, graphApiClient, userAuthentication }) => {
  const proposedEntityInput = inputs.find(
    ({ inputName }) =>
      inputName ===
      ("proposedEntity" satisfies InputNameForAction<"persistEntity">),
  )!;

  const proposedEntity = proposedEntityInput.payload.value as ProposedEntity;

  const { data: entityMetadata } = await graphApiClient.createEntity(
    /** @todo: allow overriding this via an input */
    userAuthentication.actorId,
    {
      entityTypeIds: [proposedEntity.entityTypeId],
      properties: proposedEntity.properties,
      /** @todo: allow overriding this via an input */
      draft: true,
      /** @todo: allow overriding this via an input */
      ownedById: userAuthentication.actorId,
      relationships: [],
    },
  );

  const entity: Entity = {
    metadata: mapGraphApiEntityMetadataToMetadata(entityMetadata),
    properties: proposedEntity.properties,
  };

  return {
    code: StatusCode.Ok,
    contents: [
      {
        outputs: [
          {
            outputName:
              "persistedEntity" as OutputNameForAction<"persistEntity">,
            payload: {
              kind: "Entity",
              value: entity,
            },
          },
        ],
      },
    ],
  };
};
