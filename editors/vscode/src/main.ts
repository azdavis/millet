import * as path from "path";
import * as vscode from "vscode";
import {
  LanguageClient,
  type LanguageClientOptions,
  type ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | null = null;

const channel = vscode.window.createOutputChannel("millet client");

export async function activate(cx: vscode.ExtensionContext) {
  channel.appendLine("startup millet lsp client");
  const config = vscode.workspace.getConfiguration("millet");
  if (!config.get("server.enable") || client !== null) {
    return;
  }
  const ext = process.platform === "win32" ? ".exe" : "";
  const configPath = config.get("server.path");
  const serverOpts: ServerOptions = {
    command:
      typeof configPath === "string"
        ? configPath
        : cx.asAbsolutePath(path.join("out", `lang-srv${ext}`)),
  };
  const clientOpts: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "sml" }],
  };
  client = new LanguageClient("millet", serverOpts, clientOpts);
  client.start();
}

export async function deactivate() {
  channel.appendLine("shutdown millet lsp client");
  if (client === null) {
    return;
  }
  return client.stop();
}
