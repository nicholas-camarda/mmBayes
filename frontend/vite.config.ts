/// <reference types="vitest/config" />
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));

const pageEntries = {
  index: resolve(here, "index.html"),
  technical: resolve(here, "technical.html"),
} as const;

type BuildPage = keyof typeof pageEntries;

function resolveBuildPage(): BuildPage {
  const page = process.env.VITE_BUILD_PAGE;
  if (page === "index" || page === "technical") {
    return page;
  }
  throw new Error(
    "Set VITE_BUILD_PAGE=index or VITE_BUILD_PAGE=technical for production builds."
  );
}

// ES module scripts are blocked on file:// in normal browsers. Per-page IIFE bundles
// with cssCodeSplit disabled keep each dashboard self-contained for offline review
// and GitHub Pages (https://) alike.
export default defineConfig(({ command }) => {
  const isBuild = command === "build";
  const buildPage = isBuild ? resolveBuildPage() : undefined;

  return {
    plugins: [react()],
    base: "./",
    build: isBuild
      ? {
          outDir: "dist",
          modulePreload: false,
          cssCodeSplit: false,
          rollupOptions: {
            input: pageEntries[buildPage!],
            output: {
              format: "iife",
              entryFileNames: "assets/[name].js",
              chunkFileNames: "assets/[name].js",
              assetFileNames: "assets/[name][extname]",
            },
          },
        }
      : undefined,
    test: {
      environment: "jsdom",
      setupFiles: "./src/test/setup.ts",
    },
  };
});
