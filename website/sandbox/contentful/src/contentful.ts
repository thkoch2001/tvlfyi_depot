import { createClient } from "contentful";
import type { ContentfulClientApi } from "contentful";

const space = process.env.CONTENTFUL_SPACE_ID;
const accessToken = process.env.CONTENTFUL_ACCESS_TOKEN;

let client: ContentfulClientApi;

// Idempotent way to get a reference to the Contentful client.
export const getClient = (): ContentfulClientApi => {
  if (typeof client !== "undefined") {
    return client;
  } else {
    if (typeof space === "string" && typeof accessToken === "string") {
      let client = createClient({
        space,
        accessToken,
      });

      return client;
    } else {
      throw new Error(
        "Please set CONTENTFUL_SPACE_ID and CONTENTFUL_ACCESS_TOKEN"
      );
    }
  }
};
