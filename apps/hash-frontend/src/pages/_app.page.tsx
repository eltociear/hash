/* eslint-disable import/first */
// @todo have webpack polyfill this
import { mapGqlSubgraphFieldsFragmentToSubgraph } from "@local/hash-isomorphic-utils/graph-queries";

require("setimmediate");

import "./globals.scss";
import "./prism.css";

import { ApolloProvider } from "@apollo/client/react";
import { TypeSystemInitializer } from "@blockprotocol/type-system";
import wasm from "@blockprotocol/type-system/wasm";
import type { EmotionCache } from "@emotion/react";
import { CacheProvider } from "@emotion/react";
import { createEmotionCache, theme } from "@hashintel/design-system/theme";
import type { FeatureFlag } from "@local/hash-isomorphic-utils/feature-flags";
import { featureFlags } from "@local/hash-isomorphic-utils/feature-flags";
import type { User } from "@local/hash-isomorphic-utils/system-types/user";
import type { EntityRootType, Subgraph } from "@local/hash-subgraph";
import { getRoots } from "@local/hash-subgraph/stdlib";
import { CssBaseline, GlobalStyles, ThemeProvider } from "@mui/material";
import { configureScope, ErrorBoundary } from "@sentry/nextjs";
import type { AppProps as NextAppProps } from "next/app";
import dynamic from "next/dynamic";
import { useRouter } from "next/router";
import { SnackbarProvider } from "notistack";
import type { FunctionComponent, PropsWithChildren } from "react";
import { Suspense, useEffect, useState } from "react";

import type {
  GetHashInstanceSettingsQueryQuery,
  HasAccessToHashQuery,
  MeQuery,
} from "../graphql/api-types.gen";
import { getHashInstanceSettings } from "../graphql/queries/knowledge/hash-instance.queries";
import { hasAccessToHashQuery, meQuery } from "../graphql/queries/user.queries";
import { apolloClient } from "../lib/apollo-client";
import type { MinimalUser } from "../lib/user-and-org";
import { constructMinimalUser } from "../lib/user-and-org";
import { DraftEntitiesContextProvider } from "../shared/draft-entities-context";
import { EntityTypesContextProvider } from "../shared/entity-types-context/provider";
import { FileUploadsProvider } from "../shared/file-upload-context";
import { KeyboardShortcutsContextProvider } from "../shared/keyboard-shortcuts-context";
import type { NextPageWithLayout } from "../shared/layout";
import { getLayoutWithSidebar, getPlainLayout } from "../shared/layout";
import { SidebarContextProvider } from "../shared/layout/layout-with-sidebar/sidebar-context";
import { NotificationEntitiesContextProvider } from "../shared/notification-entities-context";
import { PropertyTypesContextProvider } from "../shared/property-types-context";
import { RoutePageInfoProvider } from "../shared/routing";
import { ErrorFallback } from "./_app.page/error-fallback";
import type { AppPage } from "./shared/_app.util";
import { redirectInGetInitialProps } from "./shared/_app.util";
import { AuthInfoProvider, useAuthInfo } from "./shared/auth-info-context";
import { DataTypesContextProvider } from "./shared/data-types-context";
import { setSentryUser } from "./shared/sentry";
import { WorkspaceContextProvider } from "./shared/workspace-context";

// eslint-disable-next-line react/jsx-no-useless-fragment
const RenderChildren = ({ children }: PropsWithChildren) => <>{children}</>;

export const initWasm = async () => {
  let wasmModule;
  if (typeof window === "undefined") {
    // eslint-disable-next-line unicorn/prefer-node-protocol
    const { default: fs } = await import("fs/promises");

    // @ts-expect-error -- We need Node's native require here, and it's safe as this is a server-only block
    // eslint-disable-next-line @typescript-eslint/no-unsafe-call
    const wasmPath = __non_webpack_require__.resolve(
      "@blockprotocol/type-system/wasm",
    );
    const contents = await fs.readFile(wasmPath);

    wasmModule = await WebAssembly.compile(contents);
  } else {
    wasmModule = wasm;
  }
  await TypeSystemInitializer.initialize(wasmModule);
};

const InitTypeSystem = dynamic(
  async () => {
    await initWasm();

    return { default: RenderChildren };
  },
  {
    suspense: true,
  },
);

const clientSideEmotionCache = createEmotionCache();

type AppInitialProps = {
  initialAuthenticatedUserSubgraph?: Subgraph<EntityRootType>;
  user?: MinimalUser;
};

type AppProps = {
  emotionCache?: EmotionCache;
  Component: NextPageWithLayout;
} & AppInitialProps &
  NextAppProps;

const App: FunctionComponent<AppProps> = ({
  Component,
  pageProps,
  emotionCache = clientSideEmotionCache,
}) => {
  // Helps prevent tree mismatch between server and client on initial render
  const [ssr, setSsr] = useState(true);
  const router = useRouter();

  useEffect(() => {
    configureScope((scope) =>
      // eslint-disable-next-line no-console -- TODO: consider using logger
      console.log(`Build: ${scope.getSession()?.release ?? "not set"}`),
    );
    setSsr(false);
  }, []);

  const { authenticatedUser } = useAuthInfo();

  useEffect(() => {
    setSentryUser({ authenticatedUser });
  }, [authenticatedUser]);

  // App UI often depends on [shortname] and other query params. However,
  // router.query is empty during server-side rendering for pages that don’t use
  // getServerSideProps. By showing app skeleton on the server, we avoid UI
  // mismatches during rehydration and improve type-safety of param extraction.
  if (ssr || !router.isReady) {
    return (
      <Suspense>
        <InitTypeSystem />
      </Suspense>
    ); // Replace with app skeleton
  }

  const getLayout = Component.getLayout ?? getPlainLayout;

  return (
    <Suspense>
      <InitTypeSystem>
        <CacheProvider value={emotionCache}>
          <ThemeProvider theme={theme}>
            <CssBaseline />
            <RoutePageInfoProvider>
              <WorkspaceContextProvider>
                <KeyboardShortcutsContextProvider>
                  <SnackbarProvider maxSnack={3}>
                    <NotificationEntitiesContextProvider>
                      <DraftEntitiesContextProvider>
                        <EntityTypesContextProvider>
                          <PropertyTypesContextProvider includeArchived>
                            <DataTypesContextProvider>
                              <FileUploadsProvider>
                                <SidebarContextProvider>
                                  <ErrorBoundary
                                    beforeCapture={(scope) => {
                                      scope.setTag("error-boundary", "_app");
                                    }}
                                    fallback={(props) =>
                                      getLayoutWithSidebar(
                                        <ErrorFallback {...props} />,
                                      )
                                    }
                                  >
                                    {getLayout(<Component {...pageProps} />)}
                                  </ErrorBoundary>
                                </SidebarContextProvider>
                              </FileUploadsProvider>
                            </DataTypesContextProvider>
                          </PropertyTypesContextProvider>
                        </EntityTypesContextProvider>
                      </DraftEntitiesContextProvider>
                    </NotificationEntitiesContextProvider>
                  </SnackbarProvider>
                </KeyboardShortcutsContextProvider>
              </WorkspaceContextProvider>
            </RoutePageInfoProvider>
          </ThemeProvider>
        </CacheProvider>
        {/* "spin" is used in some inline styles which have been temporarily introduced in https://github.com/hashintel/hash/pull/1471 */}
        {/* @todo remove when inline styles are replaced with MUI styles */}
        <GlobalStyles
          styles={`
        @keyframes spin {
          from {
            transform: rotate(0deg);
          }
          to {
            transform: rotate(360deg);
          }
        };
      `}
        />
      </InitTypeSystem>
    </Suspense>
  );
};

const AppWithTypeSystemContextProvider: AppPage<AppProps, AppInitialProps> = (
  props,
) => {
  const { initialAuthenticatedUserSubgraph, user } = props;

  return (
    <ApolloProvider client={apolloClient}>
      <AuthInfoProvider
        initialAuthenticatedUserSubgraph={initialAuthenticatedUserSubgraph}
        key={user?.accountId}
      >
        <App {...props} />
      </AuthInfoProvider>
    </ApolloProvider>
  );
};

// The list of page pathnames that should be accessible whether or not the user is authenticated
const publiclyAccessiblePagePathnames = [
  "/[shortname]/[page-slug]",
  "/signin",
  "/signup",
  "/recovery",
  "/",
];

/**
 * A map from a feature flag, to the list of pages which should not be accessible
 * if that feature flag is not enabled for the user.
 */
const featureFlagHiddenPathnames: Record<FeatureFlag, string[]> = {
  pages: [],
  documents: [],
  canvases: [],
  notes: ["/notes"],
  workers: ["/goals", "/flows", "/workers"],
  ai: ["/goals"],
};

AppWithTypeSystemContextProvider.getInitialProps = async (appContext) => {
  const {
    ctx: { req, pathname, asPath },
  } = appContext;

  const { cookie } = req?.headers ?? {};

  /**
   * Fetch the authenticated user on the very first page load so it's available in the frontend –
   *   on subsequent loads it will be cached so long as the cookie value remains the same.
   * We leave it up to the client to re-fetch the user as necessary in response to user-initiated actions.
   */
  const initialAuthenticatedUserSubgraph = await apolloClient
    .query<MeQuery>({
      query: meQuery,
      context: { headers: { cookie } },
    })
    .then(({ data }) =>
      mapGqlSubgraphFieldsFragmentToSubgraph<EntityRootType<User>>(
        data.me.subgraph,
      ),
    )
    .catch(() => undefined);

  const userEntity = initialAuthenticatedUserSubgraph
    ? getRoots(initialAuthenticatedUserSubgraph)[0]
    : undefined;

  /** @todo: make additional pages publicly accessible */
  if (!userEntity) {
    // If the user is logged out and not on a page that should be publicly accessible...
    if (!publiclyAccessiblePagePathnames.includes(pathname)) {
      // ...redirect them to the login page
      redirectInGetInitialProps({
        appContext,
        location: `/signin${
          ["", "/", "/404"].includes(pathname) ? "" : `?return_to=${asPath}`
        }`,
      });
    }

    return {};
  }

  // The type system package needs to be initialized before calling `constructAuthenticatedUser`
  // await TypeSystemInitializer.initialize();

  const user = constructMinimalUser({ userEntity });

  // If the user is logged in but hasn't completed signup...
  if (!user.accountSignupComplete) {
    const hasAccessToHash = await apolloClient
      .query<HasAccessToHashQuery>({
        query: hasAccessToHashQuery,
        context: { headers: { cookie } },
      })
      .then(({ data }) => data.hasAccessToHash);

    // ...if they have access to HASH but aren't on the signup page...
    if (hasAccessToHash && !pathname.startsWith("/signup")) {
      // ...then redirect them to the signup page.
      redirectInGetInitialProps({ appContext, location: "/signup" });
      // ...if they don't have access to HASH but aren't on the home page...
    } else if (!hasAccessToHash && pathname !== "/") {
      // ...then redirect them to the home page.
      redirectInGetInitialProps({ appContext, location: "/" });
    }
  }

  // For each feature flag...
  for (const featureFlag of featureFlags) {
    /**
     * ...if the user has not enabled the feature flag,
     * and the page is a hidden pathname for that feature flag...
     */
    if (
      !user.enabledFeatureFlags.includes(featureFlag) &&
      featureFlagHiddenPathnames[featureFlag].includes(pathname)
    ) {
      const isUserAdmin = await apolloClient
        .query<GetHashInstanceSettingsQueryQuery>({
          query: getHashInstanceSettings,
          context: { headers: { cookie } },
        })
        .then(({ data }) => !!data.hashInstanceSettings?.isUserAdmin);

      if (!isUserAdmin) {
        // ...then redirect them to the home page instead.
        redirectInGetInitialProps({ appContext, location: "/" });
      }
    }
  }

  return { initialAuthenticatedUserSubgraph, user };
};

export default AppWithTypeSystemContextProvider;
