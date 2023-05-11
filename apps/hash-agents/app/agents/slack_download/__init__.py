"""A template agent, which provides a simple interface into LangChain's LLMMathChain."""

import json
import os
import time
from collections.abc import Callable
from datetime import datetime, timedelta, timezone
from pathlib import Path

import structlog
from beartype import beartype
from slack_sdk import WebClient
from slack_sdk.errors import SlackApiError
from slack_sdk.web import SlackResponse

logger = structlog.stdlib.get_logger(__name__)

HASH_GRAPH_CHANNEL_ID = "C03F7V6DU9M"
MESSAGES_PER_PAGE = 1_000
# It wants a _string_ of the epoch timestamp (number)...
OLDEST = str((datetime.now(tz=timezone.utc) - timedelta(days=7)).timestamp())
# OLDEST = str((datetime.now(tz=timezone.utc) - timedelta(days=30)).timestamp())
# OLDEST = str((datetime.now(tz=timezone.utc) - timedelta(days=180)).timestamp())


def handle_rate_limit(response_generator_func: Callable[[], SlackResponse]):
    response_generator = None

    while True:
        try:
            if response_generator is None:
                response_generator = iter(response_generator_func())
            response = next(response_generator)
            response.validate()
            yield response
            response_generator = response

        except StopIteration:
            break
        except SlackApiError as error:
            if error.response.status_code != 429:
                raise
            retry_after = int(error.response.headers["Retry-After"])
            logger.debug("Rate limited, retrying", retry_after=retry_after)
            time.sleep(retry_after)
            continue


def extract_messages(messages):
    return [
        # TODO: extract file information, etc. here
        {
            "text": message["text"],
            "user": message["user"],
            "ts": message["ts"],
        }
        for message in messages
        if message["type"] == "message"
    ]


def get_threaded_replies(client: WebClient, channel_id, thread_ts):
    replies = []
    for response in handle_rate_limit(
        lambda: client.conversations_replies(
            channel=channel_id,
            ts=thread_ts,
            limit=MESSAGES_PER_PAGE,
            oldest=OLDEST,
        ),
    ):
        response.validate()
        replies.extend(
            extract_messages(response["messages"][1:]),
        )  # Exclude the parent message

    return replies


@beartype
def execute() -> None:
    api_key = os.getenv("SLACK_API_KEY")
    client = WebClient(token=api_key)

    page = 0

    messages = {}

    for response in handle_rate_limit(
        lambda: client.conversations_history(
            channel=HASH_GRAPH_CHANNEL_ID,
            limit=MESSAGES_PER_PAGE,
            oldest=OLDEST,
        ),
    ):
        page += 1

        logger.info(
            "Retrieving page of channel history history",
            page=page,
            channel=HASH_GRAPH_CHANNEL_ID,
        )
        response.validate()

        for message in extract_messages(response["messages"]):
            messages[message["ts"]] = message

    logger.info(
        "Finished retrieving messages in channel",
        num_messages=len(messages),
        channel=HASH_GRAPH_CHANNEL_ID,
    )

    message_ts_to_replies = {}

    for message_ts in messages:
        logger.debug("Retrieving replies for message", message_ts=message_ts)
        message_ts_to_replies[message_ts] = get_threaded_replies(
            client,
            HASH_GRAPH_CHANNEL_ID,
            message_ts,
        )

    logger.info(
        "Finished retrieving all threads for messages",
        total_replies=sum(map(len, message_ts_to_replies.values())),
    )

    Path("out").mkdir(exist_ok=True)
    Path("out/messages.json").write_text(
        json.dumps(
            {"messages": messages, "message_ts_to_replies": message_ts_to_replies},
            indent=2,
        ),
    )
