import * as vscode from "vscode";

console.log("got here 123");

export function activate(context: vscode.ExtensionContext) {
  console.log("got here 234");
  const disposable = vscode.commands.registerCommand(
    "extension.helloWorld",
    () => {
      vscode.window.showInformationMessage("Hello World!");
    },
  );
  context.subscriptions.push(disposable);
}
