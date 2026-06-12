import { readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

function patchHtml(html) {
  const payloadScript =
    html.match(/<script src="\.\/dashboard_payloads\.js"><\/script>/)?.[0] ?? "";
  const appScriptMatch =
    html.match(/<script type="module" crossorigin src="\.\/assets\/([^"]+)"><\/script>/) ??
    html.match(/<script src="\.\/assets\/([^"]+)"><\/script>/);
  const appScript = appScriptMatch
    ? `<script src="./assets/${appScriptMatch[1]}"></script>`
    : "";

  html = html.replace(/<script src="\.\/dashboard_payloads\.js"><\/script>\s*/g, "");
  html = html.replace(
    /<script type="module" crossorigin src="\.\/assets\/[^"]+"><\/script>\s*/g,
    ""
  );
  html = html.replace(/<script src="\.\/assets\/[^"]+"><\/script>\s*/g, "");

  if (payloadScript || appScript) {
    html = html.replace(
      "</body>",
      `${payloadScript ? `  ${payloadScript}\n` : ""}${appScript ? `  ${appScript}\n` : ""}  </body>`
    );
  }

  html = html.replace(/<link rel="stylesheet" crossorigin href="\.\/assets\/([^"]+)">/g, '<link rel="stylesheet" href="./assets/$1">');

  return html;
}

for (const file of ["index.html", "technical.html"]) {
  const path = join("dist", file);
  writeFileSync(path, patchHtml(readFileSync(path, "utf8")));
}
