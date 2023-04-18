import * as path from "path";
import * as vscode from "vscode";
import {
  LanguageClient,
  type LanguageClientOptions,
  type ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | null = null;

const channel = vscode.window.createOutputChannel("millet client");

function mkNone(s: string | undefined): string | null {
  return s === undefined || s === "none" ? null : s;
}

export async function activate(cx: vscode.ExtensionContext) {
  channel.appendLine("start up millet client");
  const config = vscode.workspace.getConfiguration("millet");
  if (!config.get("server.enable") || client !== null) {
    return;
  }
  const ext = process.platform === "win32" ? ".exe" : "";
  const configPath = config.get("server.path");
  const serverOpts: ServerOptions = {
    command:
      typeof configPath === "string" && configPath.length !== 0
        ? configPath
        : cx.asAbsolutePath(path.join("out", `millet-ls${ext}`)),
  };
  // @sync(init-options)
  const initializationOptions = {
    token_hover: config.get("server.hover.token.enable"),
    format: mkNone(config.get("format.engine")),
    diagnostics: {
      on_change: config.get("server.diagnostics.onChange.enable"),
      more_info_hint: config.get("server.diagnostics.moreInfoHint.enable"),
      ignore: mkNone(config.get("server.diagnostics.ignore")),
    },
  };
  const clientOpts: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "sml" }],
    initializationOptions,
  };
  client = new LanguageClient("millet", serverOpts, clientOpts);
  client.start();
}

export async function deactivate() {
  channel.appendLine("shut down millet client");
  if (client === null) {
    return;
  }
  await client.stop();
  client = null;
}
