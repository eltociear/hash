import type { EntityUuid } from "@local/hash-graph-types/entity";
import type { OwnedById } from "@local/hash-graph-types/web";
import type {
  ExternalInputRequest,
  ExternalInputRequestSignal,
  ExternalInputResponseSignal,
  FlowInputs,
  ProgressLogSignal,
  SparseFlowRun,
} from "@local/hash-isomorphic-utils/flows/types";
import type {
  FlowRun,
  FlowRunStatus,
  StepRun,
} from "@local/hash-isomorphic-utils/graphql/api-types.gen";
import { FlowStepStatus } from "@local/hash-isomorphic-utils/graphql/api-types.gen";
import { StatusCode } from "@local/status";
import type { Client as TemporalClient } from "@temporalio/client";
import proto from "@temporalio/proto";

import { parseHistoryItemPayload } from "../src/temporal/parse-history-item-payload";

const eventTimeIsoStringFromEvent = (
  event?: proto.temporal.api.history.v1.IHistoryEvent,
) => {
  const { eventTime } = event ?? {};
  if (!eventTime?.seconds) {
    return;
  }

  return new Date(
    eventTime.seconds.toInt() * 1000 +
      (eventTime.nanos ? eventTime.nanos / 1_000_000 : 0),
  ).toISOString();
};

/**
 * Get details from an ActivityTaskScheduledEvent
 */
const getActivityScheduledDetails = (
  event: proto.temporal.api.history.v1.IHistoryEvent,
) => {
  if (
    event.eventType !==
    proto.temporal.api.enums.v1.EventType.EVENT_TYPE_ACTIVITY_TASK_SCHEDULED
  ) {
    throw new Error(
      `Unexpected event type ${event.eventType}, expected ${proto.temporal.api.enums.v1.EventType.EVENT_TYPE_ACTIVITY_TASK_SCHEDULED}`,
    );
  }

  if (!event.activityTaskScheduledEventAttributes?.activityId) {
    throw new Error("No activityId on scheduled event");
  }

  const scheduledAt = eventTimeIsoStringFromEvent(event);
  if (!scheduledAt) {
    throw new Error("No eventTime on scheduled event");
  }

  const inputs = parseHistoryItemPayload(
    event.activityTaskScheduledEventAttributes.input,
  );

  return {
    activityId: event.activityTaskScheduledEventAttributes.activityId,
    activityType: event.activityTaskScheduledEventAttributes.activityType?.name,
    inputs,
    scheduledAt,
    startedAt: undefined,
    attempt: 1,
  };
};

/**
 * Get the history event where this activity was last scheduled, which is the only one that contains the activityId,
 * and tells us when it was last scheduled.
 */
const getActivityStartedDetails = (
  events: proto.temporal.api.history.v1.IHistoryEvent[],
  attributes:
    | proto.temporal.api.history.v1.IActivityTaskStartedEventAttributes
    | proto.temporal.api.history.v1.IActivityTaskCompletedEventAttributes
    | proto.temporal.api.history.v1.IActivityTaskCanceledEventAttributes
    | proto.temporal.api.history.v1.IActivityTaskCancelRequestedEventAttributes
    | proto.temporal.api.history.v1.IActivityTaskTimedOutEventAttributes
    | proto.temporal.api.history.v1.IActivityTaskFailedEventAttributes,
) => {
  const { scheduledEventId } = attributes;

  const scheduledEvent = events.findLast(
    (item) =>
      item.eventId?.toString() === scheduledEventId?.toString() &&
      item.eventType ===
        proto.temporal.api.enums.v1.EventType
          .EVENT_TYPE_ACTIVITY_TASK_SCHEDULED,
  );

  if (!scheduledEvent) {
    throw new Error("Could not find scheduled event for activity in history");
  }

  const { activityId, activityType, scheduledAt, inputs } =
    getActivityScheduledDetails(scheduledEvent);

  const startedEvent = events.findLast(
    (item) =>
      item.eventType ===
        proto.temporal.api.enums.v1.EventType
          .EVENT_TYPE_ACTIVITY_TASK_STARTED &&
      item.activityTaskStartedEventAttributes?.scheduledEventId?.toString() ===
        scheduledEventId?.toString(),
  );

  return {
    activityId,
    activityType,
    scheduledAt,
    inputs,
    attempt: startedEvent?.activityTaskStartedEventAttributes?.attempt ?? 1,
    startedAt: eventTimeIsoStringFromEvent(startedEvent),
  };
};

const getFlowRunDetailedFields = async ({
  workflowId,
  temporalClient,
}: {
  workflowId: string;
  temporalClient: TemporalClient;
}): Promise<
  Pick<FlowRun, "inputs" | "inputRequests" | "outputs" | "steps">
> => {
  const handle = temporalClient.workflow.getHandle(workflowId);

  const workflow = await handle.describe();
  const { events } = await handle.fetchHistory();

  const workflowInputs = parseHistoryItemPayload(
    events?.find(
      (event) =>
        event.eventType ===
        proto.temporal.api.enums.v1.EventType
          .EVENT_TYPE_WORKFLOW_EXECUTION_STARTED,
    )?.workflowExecutionStartedEventAttributes?.input,
  ) as FlowInputs | undefined;

  const workflowOutputs = parseHistoryItemPayload(
    events?.find(
      (event) =>
        event.eventType ===
        proto.temporal.api.enums.v1.EventType
          .EVENT_TYPE_WORKFLOW_EXECUTION_COMPLETED,
    )?.workflowExecutionCompletedEventAttributes?.result,
  );

  const stepMap: { [activityId: string]: StepRun } = {};

  /**
   * Collect all progress signal events when building the step map,
   * and assign them to the appropriate step afterwards.
   */
  const progressSignalEvents: proto.temporal.api.history.v1.IHistoryEvent[] =
    [];

  const inputRequestsById: Record<string, ExternalInputRequest> = {};

  const workflowStoppedEarly = [
    "TERMINATED",
    "CANCELED",
    "TIMED_OUT",
    "FAILED",
  ].includes(workflow.status.name);

  if (events?.length) {
    /*
     * Walk backwards from the most recent event until we have populated the latest state data for each step
     */
    for (let i = events.length - 1; i >= 0; i--) {
      const event = events[i];
      if (!event) {
        throw new Error("Somehow out of bounds for events array");
      }

      if (
        event.workflowExecutionSignaledEventAttributes?.signalName ===
        "logProgress"
      ) {
        progressSignalEvents.push(event);
        continue;
      }

      if (
        event.workflowExecutionSignaledEventAttributes?.signalName ===
        "externalInputRequest"
      ) {
        const signalData = parseHistoryItemPayload(
          event.workflowExecutionSignaledEventAttributes.input,
        )?.[0] as ExternalInputRequestSignal | undefined;

        if (!signalData) {
          throw new Error(
            `No signal data on requestExternalInput signal event with id ${event.eventId?.toInt()}`,
          );
        }

        /**
         * This is a request for external input
         */
        const existingRequest = inputRequestsById[signalData.requestId];
        const raisedAt = eventTimeIsoStringFromEvent(event);
        if (!raisedAt) {
          throw new Error(
            `No eventTime on requestExternalInput signal event with id ${event.eventId?.toInt()}`,
          );
        }

        if (existingRequest) {
          existingRequest.data = signalData.data;
          existingRequest.raisedAt = raisedAt;
        } else {
          /**
           * If we haven't already populated the request record, it must not have been resolved yet,
           * because we are going backwards through the event history from most recent.
           * We would already have encountered the response signal if one had been provided.
           */
          inputRequestsById[signalData.requestId] = {
            ...signalData,
            raisedAt,
          };
        }
      }

      if (
        event.workflowExecutionSignaledEventAttributes?.signalName ===
        "externalInputResponse"
      ) {
        const signalData = parseHistoryItemPayload(
          event.workflowExecutionSignaledEventAttributes.input,
        )?.[0] as ExternalInputResponseSignal | undefined;

        if (!signalData) {
          throw new Error(
            `No signal data on requestExternalInput signal event with id ${event.eventId?.toInt()}`,
          );
        }
        const { requestId, data, type } = signalData;

        inputRequestsById[signalData.requestId] = {
          data: {} as never, // we will populate this when we hit the original request
          answers: "answers" in data ? data.answers : undefined,
          requestId,
          stepId: "unresolved",
          type,
          raisedAt: "", // we will populate this when we hit the original request
          resolvedAt: eventTimeIsoStringFromEvent(event),
        };
      }

      const nonScheduledAttributes =
        event.activityTaskStartedEventAttributes ??
        event.activityTaskCompletedEventAttributes ??
        event.activityTaskCanceledEventAttributes ??
        event.activityTaskCancelRequestedEventAttributes ??
        event.activityTaskFailedEventAttributes ??
        event.activityTaskTimedOutEventAttributes;

      if (
        !nonScheduledAttributes &&
        !event.activityTaskScheduledEventAttributes
      ) {
        // This is not an activity-related event. It may be a signal, which we handle in bulk below
        continue;
      }

      const {
        activityId,
        activityType,
        attempt,
        inputs,
        startedAt,
        scheduledAt,
      } = event.activityTaskScheduledEventAttributes
        ? getActivityScheduledDetails(event)
        : getActivityStartedDetails(events, nonScheduledAttributes!);

      if (activityType === "persistFlowActivity") {
        continue;
      }

      if (stepMap[activityId]) {
        // We've already encountered and therefore populated all the details for this step
        continue;
      }

      const activityRecord: StepRun = {
        stepId: activityId,
        stepType: activityType ?? "UNKNOWN",
        startedAt,
        scheduledAt,
        closedAt: workflowStoppedEarly ? workflow.closeTime?.toISOString() : "",
        inputs,
        logs: [],
        status: workflowStoppedEarly
          ? FlowStepStatus.Cancelled
          : FlowStepStatus.Scheduled,
        attempt,
      };

      stepMap[activityId] = activityRecord;

      switch (event.eventType) {
        case proto.temporal.api.enums.v1.EventType
          .EVENT_TYPE_ACTIVITY_TASK_SCHEDULED: {
          if (!workflowStoppedEarly) {
            activityRecord.status = FlowStepStatus.Scheduled;
          }
          break;
        }

        case proto.temporal.api.enums.v1.EventType
          .EVENT_TYPE_ACTIVITY_TASK_STARTED: {
          if (!workflowStoppedEarly) {
            activityRecord.status = FlowStepStatus.Started;
            activityRecord.lastFailure =
              event.activityTaskStartedEventAttributes?.lastFailure;
          }
          break;
        }

        case proto.temporal.api.enums.v1.EventType
          .EVENT_TYPE_ACTIVITY_TASK_COMPLETED: {
          activityRecord.outputs =
            parseHistoryItemPayload(
              event.activityTaskCompletedEventAttributes?.result,
            ) ?? [];

          if (
            /** @todo H-2604 have some kind of 'partially completed' status when reworking flow return codes */
            activityRecord.outputs.every(
              (output) => output.code !== StatusCode.Ok,
            )
          ) {
            activityRecord.status = FlowStepStatus.Failed;
          } else {
            activityRecord.status = FlowStepStatus.Completed;
          }

          activityRecord.closedAt = eventTimeIsoStringFromEvent(event);
          break;
        }

        case proto.temporal.api.enums.v1.EventType
          .EVENT_TYPE_ACTIVITY_TASK_FAILED: {
          activityRecord.status = FlowStepStatus.Failed;
          activityRecord.lastFailure =
            event.activityTaskFailedEventAttributes?.failure;
          activityRecord.retryState =
            event.activityTaskFailedEventAttributes?.retryState?.toString();
          activityRecord.closedAt = eventTimeIsoStringFromEvent(event);
          break;
        }

        case proto.temporal.api.enums.v1.EventType
          .EVENT_TYPE_ACTIVITY_TASK_TIMED_OUT: {
          activityRecord.status = FlowStepStatus.TimedOut;
          activityRecord.lastFailure =
            event.activityTaskTimedOutEventAttributes?.failure;
          activityRecord.retryState =
            event.activityTaskTimedOutEventAttributes?.retryState?.toString();
          activityRecord.closedAt = eventTimeIsoStringFromEvent(event);
          break;
        }

        case proto.temporal.api.enums.v1.EventType
          .EVENT_TYPE_ACTIVITY_TASK_CANCELED: {
          activityRecord.status = FlowStepStatus.Cancelled;
          activityRecord.closedAt = eventTimeIsoStringFromEvent(event);
          break;
        }

        case proto.temporal.api.enums.v1.EventType
          .EVENT_TYPE_ACTIVITY_TASK_CANCEL_REQUESTED: {
          activityRecord.status = FlowStepStatus.CancelRequested;
          break;
        }

        default:
          throw new Error(`Unhandled event type ${event.eventType}`);
      }
    }
  }

  /**
   * Assign logs to the appropriate step. The earliest logs will be at the end of the array,
   * as we walked the events from latest to earliest. We want the logs in ascending order, so go backwards again.
   */
  for (let i = progressSignalEvents.length - 1; i >= 0; i--) {
    const progressSignalEvent = progressSignalEvents[i];
    if (!progressSignalEvent) {
      throw new Error("Somehow out of bounds for progressSignalEvents array");
    }

    if (!progressSignalEvent.workflowExecutionSignaledEventAttributes) {
      throw new Error(
        `No signal attributes on progress signal event with id ${progressSignalEvent.eventId?.toInt()}`,
      );
    }

    const signalData = parseHistoryItemPayload(
      progressSignalEvent.workflowExecutionSignaledEventAttributes.input,
    )?.[0] as ProgressLogSignal | undefined;

    if (!signalData) {
      throw new Error(
        `No signal data on progress signal event with id ${progressSignalEvent.eventId?.toInt()}`,
      );
    }

    const { attempt, logs } = signalData;
    for (const log of logs) {
      const { stepId } = log;

      const activityRecord = stepMap[stepId];
      if (!activityRecord) {
        throw new Error(`No activity record found for step with id ${stepId}`);
      }

      if (log.type === "ProposedEntity" && attempt < activityRecord.attempt) {
        /**
         * If we have a proposed entity logged from a retried attempt, don't record it as nothing will happen with it.
         * By contrast, a PersistedEntity has already been committed to the database and is therefore relevant.
         *
         * @todo H-2545: heartbeat details of persisted entities from an activity so that if it's retried, the activity
         *    can pick up where it left off.
         */
        continue;
      }

      activityRecord.logs.push(log);
    }
  }

  const inputRequests = Object.values(inputRequestsById);
  for (const inputRequest of inputRequests) {
    if (!inputRequest.resolvedAt) {
      const step = stepMap[inputRequest.stepId];
      if (!step) {
        throw new Error(
          `Could not find step with id ${inputRequest.stepId} for input request with id ${inputRequest.requestId}`,
        );
      }
      step.status = FlowStepStatus.InformationRequired;
    }
  }

  if (!workflowInputs) {
    throw new Error("No workflow inputs found");
  }

  return {
    inputs: workflowInputs,
    outputs: workflowOutputs,
    inputRequests: Object.values(inputRequestsById),
    steps: Object.values(stepMap),
  };
};

export const getSparseFlowRunFromWorkflowId = async ({
  name,
  webId,
  workflowId,
  temporalClient,
}: {
  name: string;
  webId: OwnedById;
  workflowId: EntityUuid;
  temporalClient: TemporalClient;
}): Promise<SparseFlowRun> => {
  const handle = temporalClient.workflow.getHandle(workflowId);

  const { startTime, executionTime, closeTime, memo, status } =
    await handle.describe();

  return {
    name,
    flowDefinitionId:
      (memo?.flowDefinitionId as string | undefined) ?? "unknown",
    flowRunId: workflowId,
    status: status.name as FlowRunStatus,
    startedAt: startTime.toISOString(),
    executedAt: executionTime?.toISOString(),
    closedAt: closeTime?.toISOString(),
    webId,
  };
};

export const getFlowRunFromWorkflowId = async (args: {
  name: string;
  webId: OwnedById;
  workflowId: EntityUuid;
  temporalClient: TemporalClient;
}): Promise<FlowRun> => {
  const baseFields = await getSparseFlowRunFromWorkflowId(args);
  const detailedFields = await getFlowRunDetailedFields(args);

  return {
    ...baseFields,
    ...detailedFields,
  };
};
