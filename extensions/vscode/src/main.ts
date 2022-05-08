import * as path from "path";
import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient";

let client: LanguageClient | null = null;

export async function activate(cx: vscode.ExtensionContext) {
  const config = vscode.workspace.getConfiguration("millet");
  if (!config.get("useLanguageServer") || client !== null) {
    return;
  }
  const serverOpts = {
    command: cx.asAbsolutePath(path.join("out", "lang-srv")),
  };
  const clientOpts = {
    documentSelector: [{ scheme: "file", language: "sml" }],
  };
  client = new LanguageClient("millet", serverOpts, clientOpts, true);
  cx.subscriptions.push(client.start());
}

export async function deactivate() {
  if (client === null) {
    return;
  }
  return client.stop();
}
