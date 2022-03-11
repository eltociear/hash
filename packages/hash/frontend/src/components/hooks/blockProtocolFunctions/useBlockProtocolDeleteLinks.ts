import { useMutation } from "@apollo/client";

import { BlockProtocolDeleteLinksFunction } from "blockprotocol";
import { deleteLinkMutation } from "@hashintel/hash-shared/queries/link.queries";
import { useCallback } from "react";
import {
  DeleteLinkMutation,
  DeleteLinkMutationVariables,
} from "../../../graphql/apiTypes.gen";

export const useBlockProtocolDeleteLinks = (): {
  deleteLinks: BlockProtocolDeleteLinksFunction;
  deleteLinksLoading: boolean;
  deleteLinksError: any;
} => {
  const [
    runDeleteLinkMutation,
    { loading: deleteLinksLoading, error: deleteLinksError },
  ] = useMutation<DeleteLinkMutation, DeleteLinkMutationVariables>(
    deleteLinkMutation,
  );

  const deleteLinks: BlockProtocolDeleteLinksFunction = useCallback(
    async (actions) => {
      const results: boolean[] = [];
      // TODO: Support multiple actions in one GraphQL mutation for transaction integrity and better status reporting
      for (const action of actions) {
        if (!action.sourceAccountId) {
          throw new Error("deleteLinks needs to be passed a sourceAccountId");
        }

        if (!action.sourceEntityId) {
          throw new Error("deleteLinks needs to be passed a sourceEntityId");
        }

        const { data } = await runDeleteLinkMutation({
          variables: {
            linkId: action.linkId,
            sourceAccountId: action.sourceAccountId,
            sourceEntityId: action.sourceEntityId,
          },
        });
        results.push(!!data?.deleteLink);
      }
      return results;
    },
    [runDeleteLinkMutation],
  );

  return {
    deleteLinks,
    deleteLinksLoading,
    deleteLinksError,
  };
};
