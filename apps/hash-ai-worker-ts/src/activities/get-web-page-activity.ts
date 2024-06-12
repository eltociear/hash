import type { WebPage } from "@local/hash-isomorphic-utils/flows/types";
import { generateUuid } from "@local/hash-isomorphic-utils/generate-uuid";
import { Context } from "@temporalio/activity";
import { JSDOM } from "jsdom";
import puppeteer from "puppeteer-extra";
import StealthPlugin from "puppeteer-extra-plugin-stealth";
import sanitizeHtml from "sanitize-html";

import { logger } from "./shared/activity-logger";
import { requestExternalInput } from "./shared/request-external-input";
import { writeFileSync } from "node:fs";

puppeteer.use(StealthPlugin());

/**
 * The attributes that will be allowed in the HTML content.
 */
const allowedAttributes = [
  "href",
  "src",
  "onclick",
  "title",
  "alt",
  "aria",
  "label",
  "aria-*",
  "data-*",
];

/**
 * The tags that will always be filtered from the HTML content.
 *
 * @todo: consider whether there are other tags that aren't relevant
 * for LLM consumption.
 */
const disallowedTags = ["script", "style", "link", "canvas", "svg"];

/**
 * The tags that will be filtered from the HTML content if they don't have
 * any of the relevant attributes (defined in `allowedAttributes`).
 */
const disallowedTagsWithNoRelevantAttributes = [
  "div",
  "span",
  "strong",
  "b",
  "i",
  "em",
];

export const sanitizeHtmlForLlmConsumption = (params: {
  htmlContent: string;
  maximumNumberOfTokens?: number;
}): string => {
  const { htmlContent, maximumNumberOfTokens = 75_000 } = params;

  const sanitizedHtml = sanitizeHtml(htmlContent, {
    allowedTags: sanitizeHtml.defaults.allowedTags.filter(
      (tag) => !disallowedTags.includes(tag),
    ),
    allowedAttributes: { "*": allowedAttributes },
    disallowedTagsMode: "discard",
  });

  const dom = new JSDOM(sanitizedHtml);
  const document = dom.window.document;

  const elements = document.querySelectorAll(
    disallowedTagsWithNoRelevantAttributes.join(","),
  );

  for (const element of elements) {
    if (!element.attributes.length) {
      while (element.firstChild) {
        element.parentNode?.insertBefore(element.firstChild, element);
      }
      element.remove();
    }
  }

  const sanitizedHtmlWithNoDisallowedTags = document.body.innerHTML
    // Remove any whitespace between tags
    .replace(/>\s+</g, "><")
    .trim();

  const slicedSanitizedHtml = sanitizedHtmlWithNoDisallowedTags.slice(
    0,
    /**
     * Assume that each token is 4 characters long.
     *
     * @todo: use `js-tiktoken` to more accurately determine the number of tokens.
     */
    maximumNumberOfTokens * 4,
  );

  return slicedSanitizedHtml;
};

const getWebPageFromPuppeteer = async (url: string): Promise<WebPage> => {
  /** @todo: consider re-using the same `browser` instance across requests  */
  const browser = await puppeteer.launch({
    args: ["--lang=en-US"],
  });

  const page = await browser.newPage();

  await page.setExtraHTTPHeaders({
    /** @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Language */
    "Accept-Language": "en-US,en,q=0.5",
  });

  try {
    const timeout = 10_000;

    const navigationPromise = page.goto(url, {
      waitUntil: "networkidle2",
      timeout: timeout + 10_000,
    });

    const timeoutPromise = new Promise((resolve) => {
      setTimeout(() => resolve(undefined), timeout);
    });

    /**
     * Obtain the `innerHTML` of the page, whether or
     * not it has finished loading after the timeout. This means
     * that for web pages with a significant amount of content,
     * we are still able to return a partial result.
     */
    const htmlContent = await Promise.race([
      navigationPromise,
      timeoutPromise,
    ]).then(() => page.evaluate(() => document.body.innerHTML)); // Return partial content if timeout occurs

    const title = await page.title();

    await browser.close();

    return {
      htmlContent,
      title,
      url,
    };
  } catch (error) {
    await browser.close();

    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    const errMessage = (error as Error).message ?? "Unknown error";

    logger.error(`Failed to load URL ${url} in Puppeteer: ${errMessage}`);

    return {
      /**
       * @todo H-2604 consider returning this as a structured error that the calling code rather than the LLM can handle
       */
      htmlContent: `Could not load page: ${errMessage}`,
      title: `Error loading page: ${errMessage}`,
      url,
    };
  }
};

const getWebPageFromBrowser = async (url: string): Promise<WebPage> => {
  const externalResponse = await requestExternalInput({
    requestId: generateUuid(),
    stepId: Context.current().info.activityId,
    type: "get-urls-html-content",
    data: {
      urls: [url],
    },
  });

  const webPage = externalResponse.data.webPages[0];
  if (!webPage) {
    /** No webpage returned from external source, fallback to whatever Puppeteer can provide */
    return await getWebPageFromPuppeteer(url);
  }

  return webPage;
};

/**
 * Sites where the useful content is gated behind an authentication or paywall,
 * in which case we log a request for the content to be picked up by the user's browser.
 *
 * The user may not have access to these sites, and there may be unlisted sites we hit walls for
 * which the user _does_ have access to. The best solution would be some way of knowing which
 * sites specific user(s) can access.
 *
 * @todo vary these based on knowledge about which sites users can help us with
 * @todo be able to detect other arbitrary sites which hit auth/paywalls (e.g. via looking for 401 status codes)
 */
const domainsToRequestFromBrowser = ["crunchbase.com", "linkedin.com"];

export const getWebPageActivity = async (params: {
  url: string;
  sanitizeForLlm?: boolean;
}): Promise<WebPage> => {
  const { sanitizeForLlm, url } = params;

  const urlObject = new URL(url);

  const shouldAskBrowser =
    (domainsToRequestFromBrowser.includes(urlObject.host) ||
      domainsToRequestFromBrowser.some((domain) =>
        urlObject.host.endsWith(`.${domain}`),
      )) &&
    /**
     * @todo: find way to mock the temporal context to allow for accessing the
     * browser plugin in tests.
     */
    process.env.NODE_ENV !== "test";

  const { htmlContent, title } = shouldAskBrowser
    ? await getWebPageFromBrowser(url)
    : await getWebPageFromPuppeteer(url);

  if (sanitizeForLlm) {
    const sanitizedHtml = sanitizeHtmlForLlmConsumption({ htmlContent });

    return { htmlContent: sanitizedHtml, title, url };
  }

  return {
    title,
    url,
    htmlContent,
  };
};

const escapeHtmlToJsonString = (html: string) => {
  return html
    .replace(/\\/g, "\\\\") // Escape backslashes
    .replace(/"/g, "'") // Replace double quotes with single quotes
    .replace(/\n/g, "") // Remove newlines
    .replace(/\r/g, "") // Remove carriage returns
    .replace(/\t/g, ""); // Remove tabs
};

await getWebPageActivity({
  url: "https://openai.com/index/video-generation-models-as-world-simulators/",
  sanitizeForLlm: true,
}).then((webPage) => {
  writeFileSync(
    "html.txt",
    escapeHtmlToJsonString(webPage.htmlContent),
    "utf8",
  );
});
