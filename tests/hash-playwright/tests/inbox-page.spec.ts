import {
  systemEntityTypes,
  systemLinkEntityTypes,
} from "@local/hash-isomorphic-utils/ontology-type-ids";
import { sleep } from "@local/hash-isomorphic-utils/sleep";
import type {
  GraphChangeNotificationProperties,
  OccurredInEntityProperties,
} from "@local/hash-isomorphic-utils/system-types/graphchangenotification";
import type { PageProperties } from "@local/hash-isomorphic-utils/system-types/shared";
import { extractOwnedByIdFromEntityId } from "@local/hash-subgraph";

import { createEntity, getUser } from "./shared/api-queries";
import { loginUsingTempForm } from "./shared/login-using-temp-form";
import type { APIRequestContext } from "./shared/runtime";
import { expect, test } from "./shared/runtime";

const createNotification = async ({
  draft,
  requestContext,
  targetEntityTitle,
}: {
  draft: boolean;
  requestContext: APIRequestContext;
  targetEntityTitle: string;
}) => {
  const user = await getUser(requestContext);
  if (!user) {
    throw new Error("Cannot create notification without authenticated user");
  }

  const ownedById = extractOwnedByIdFromEntityId(
    user.metadata.recordId.entityId,
  );

  const properties: PageProperties = {
    "https://hash.ai/@hash/types/property-type/title/": targetEntityTitle,
    "https://hash.ai/@hash/types/property-type/fractional-index/": "a0",
  };

  const targetEntity = await createEntity(requestContext, {
    draft,
    entityTypeId: systemEntityTypes.page.entityTypeId,
    ownedById,
    properties,
  });

  const notificationEntity = await createEntity(requestContext, {
    draft: false,
    entityTypeId: systemEntityTypes.graphChangeNotification.entityTypeId,
    ownedById,
    properties: {
      "https://hash.ai/@hash/types/property-type/graph-change-type/": "create",
    } as GraphChangeNotificationProperties,
  });

  await createEntity(requestContext, {
    draft,
    entityTypeId: systemLinkEntityTypes.occurredInEntity.linkEntityTypeId,
    linkData: {
      leftEntityId: notificationEntity.metadata.recordId.entityId,
      rightEntityId: targetEntity.metadata.recordId.entityId,
    },
    ownedById,
    properties: {
      "https://hash.ai/@hash/types/property-type/entity-edition-id/":
        targetEntity.metadata.temporalVersioning.decisionTime.start.limit,
    } as OccurredInEntityProperties,
  });

  return targetEntityTitle;
};

test("new notifications are shown on inbox page", async ({ page }) => {
  await loginUsingTempForm({
    page,
    userEmail: "alice@example.com",
    userPassword: "password",
  });

  await expect(page.locator("text=Welcome to HASH")).toBeVisible();

  await page.goto("/inbox");

  await page.waitForURL((url) => url.pathname === "/inbox");

  const draftNotificationTitle = new Date().toISOString();

  await expect(
    page.locator(`text=${draftNotificationTitle}`),
  ).not.toBeVisible();

  /** Check notifications linked to draft entities */
  await createNotification({
    draft: true,
    requestContext: page.request,
    targetEntityTitle: draftNotificationTitle,
  });

  /** Wait for the notification poll interval to expire */
  await sleep(12_000);

  await expect(page.locator(`text=${draftNotificationTitle}`)).toBeVisible();

  const nonDraftNotificationTitle = new Date().toISOString();

  await expect(
    page.locator(`text=${nonDraftNotificationTitle}`),
  ).not.toBeVisible();

  /** Check notifications linked to non-draft entities */
  await createNotification({
    draft: false,
    requestContext: page.request,
    targetEntityTitle: nonDraftNotificationTitle,
  });

  /** Wait for the notification poll interval to expire */
  await sleep(12_000);

  await expect(page.locator(`text=${nonDraftNotificationTitle}`)).toBeVisible();
});
