import type {
  LlmToolDefinition,
  LlmUsage,
} from "../../shared/get-llm-response/types";
import { EntitySummary } from "./deduplication-agent/test-data";
import { getLlmResponse } from "../../shared/get-llm-response";
import { getToolCallsFromLlmAssistantMessage } from "../../shared/get-llm-response/llm-message";
import dedent from "dedent";
import type { PermittedOpenAiModel } from "../../shared/openai-client";
import { AnthropicMessageModel } from "../../shared/get-llm-response/anthropic-client";

/**
 * @todo
 * 1. identify where child and parent types are present, and advise the model to choose the more specific type
 * 2. merge summaries? any value to this – maybe so they are better described for other tools?
 * 3. consider if any surrounding context would be useful – e.g. anything to do with the research task?
 */
export const deduplicationAgentSystemPrompt = `
  You are a deduplication agent. Your task is to identify duplicate entities in a list of entities.
  
  Use your best judgement to determine which entities are duplicates, based on:
  1. The name of the entities
  2. Their types
  3. Their summaries
  
  Bear in mind that:
  1. the same entity may be described in different ways, or be named in slightly different ways
  2. the same entity may have different types, where the different types could conceivably apply to the same entity
  3. the same or very similar name may refer to different entities
  4. the same or very similar summary may refer to different entities
  
  If in doubt, do not merge entities. If you are confident that two entities are duplicates, merge them.
  
  Once you have identified duplicates, you must pick a single canonical entity to assign its duplicate(s) too.
  Choose the one with the best summary.
  
  If there are no duplicates, return an empty list for the 'duplicates' property.
  
  Here are the entities you need to consider:
`;

export type DuplicateReport = {
  canonicalLocalId: string;
  duplicateLocalIds: string[];
};

const toolName = "reportDuplicates";

const deduplicationAgentTool: LlmToolDefinition<typeof toolName> = {
  name: toolName,
  description:
    "Provide a list of duplicate entities and their canonical entity to merge them with",
  inputSchema: {
    type: "object",
    properties: {
      duplicates: {
        type: "array",
        items: {
          type: "object",
          properties: {
            canonicalLocalId: {
              type: "string",
              description: "The localId of the canonical entity",
            },
            duplicateLocalIds: {
              type: "array",
              description:
                "The localIds of the entities that are duplicates of the canonical entity",
              items: {
                type: "string",
              },
            },
          } satisfies Record<keyof DuplicateReport, unknown>,
          required: ["canonicalLocalId", "duplicateLocalIds"],
        },
      },
    },
    required: ["duplicates"],
  },
};

export const identifyDuplicates = async (
  entities: EntitySummary[],
  model: PermittedOpenAiModel | AnthropicMessageModel,
): Promise<
  { duplicates: DuplicateReport[] } & {
    usage: LlmUsage;
    totalRequestTime: number;
  }
> => {
  const llmResponse = await getLlmResponse(
    {
      systemPrompt: deduplicationAgentSystemPrompt,
      tools: [deduplicationAgentTool],
      messages: [
        {
          role: "user",
          content: [
            {
              type: "text",
              text: dedent(
                entities
                  .map(
                    ({ localId, name, entityTypeId, summary }) => `
                    Name: ${name}
                    Type: ${entityTypeId}
                    Summary: ${summary}
                    LocalId: ${localId}
                    `,
                  )
                  .join("\n"),
              ),
            },
          ],
        },
      ],
      model,
    },
    null,
  );

  if (llmResponse.status !== "ok") {
    throw new Error(
      `Failed to get LLM response: ${JSON.stringify(llmResponse)}`,
    );
  }

  const { message, usage, totalRequestTime } = llmResponse;

  const toolCalls = getToolCallsFromLlmAssistantMessage({ message });

  const firstToolCall = toolCalls[0];

  if (!firstToolCall) {
    throw new Error(
      `Expected tool calls in message: ${JSON.stringify(message, null, 2)}`,
    );
  }

  if (toolCalls.length > 1) {
    throw new Error(
      `Expected only one tool call in message: ${JSON.stringify(message, null, 2)}`,
    );
  }

  if (firstToolCall.name !== toolName) {
    throw new Error(
      `Expected tool call to be ${toolName}, received ${firstToolCall.name}`,
    );
  }

  const { duplicates } = firstToolCall.input as {
    duplicates: DuplicateReport[];
  };

  return { duplicates, totalRequestTime, usage };
};
