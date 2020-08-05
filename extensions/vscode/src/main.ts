import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient";
import * as path from "path";

let client: LanguageClient | null = null;

export async function activate(cx: vscode.ExtensionContext) {
  const config = vscode.workspace.getConfiguration("millet");
  if (!config.get("useLanguageServer") || client !== null) {
    return;
  }
  const serverOpts = {
    command: cx.asAbsolutePath(path.join("out", "millet-ls")),
  };
  const clientOpts = {
    documentSelector: [{ scheme: "file", language: "sml" }],
  };
  client = new LanguageClient("millet-ls", serverOpts, clientOpts, true);
  cx.subscriptions.push(client.start());
}

export async function deactivate() {
  if (client === null) {
    return;
  }
  return client.stop();
}
