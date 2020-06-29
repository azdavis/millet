import { ExtensionContext } from "vscode";
import { LanguageClient } from "vscode-languageclient";
import * as path from "path";

export function activate(cx: ExtensionContext) {
  console.log("millet: activate");
  const serverOpts = {
    command: cx.asAbsolutePath(path.join("out", "millet-ls")),
  };
  const clientOpts = {
    documentSelector: [{ scheme: "file", language: "sml" }],
  };
  const client = new LanguageClient("millet-ls", serverOpts, clientOpts, true);
  cx.subscriptions.push(client.start());
}
