const vscode = require('vscode');
const path = require('path');

class ATK16HoverProvider {
  async provideHover(document, position, token) {
    const range = document.getWordRangeAtPosition(position, /[\w-/.]+/);
    const labelText = document.getText(range);
    const subroutineDefinition = await findSubroutineInDocument(document, labelText);
    if (subroutineDefinition) {
      const hoverText = new vscode.MarkdownString(subroutineDefinition);
      hoverText.isTrusted = true;
      return new vscode.Hover(hoverText);
    }

    // Search for labels in included files
    for (let i = 0; i < document.lineCount; i++) {
      const line = document.lineAt(i);
      const includeMatch = line.text.match(/@include\s+(\S+)/);
      if (includeMatch) {
        const includedFilePath = path.join(
          path.dirname(document.fileName),
          `${includeMatch[1]}.atk16`
        );

        if (await vscode.workspace.fs.stat(vscode.Uri.file(includedFilePath))) {
          const includedDocument = await vscode.workspace.openTextDocument(
            includedFilePath
          );
          const subroutineDefinition = await findSubroutineInDocument(includedDocument, labelText);
          if (subroutineDefinition) {
            const hoverText = new vscode.MarkdownString(subroutineDefinition);
            hoverText.isTrusted = true;
            return new vscode.Hover(hoverText);
          }
        }
      }
    }

    return null;
  }
}

class ATK16DefinitionProvider {
  async provideDefinition(document, position, token) {
    const range = document.getWordRangeAtPosition(position, /[\w-/.]+/);
    const text = document.getText(range);

    const line = document.lineAt(position);
    const includeMatch = line.text.match(/@include\s+(\S+)/);
    const useMatch = line.text.match(/@use\s+(\S+):/);

    if (includeMatch && includeMatch[1] === text) {
      const includedFilePath = path.join(
        path.dirname(document.fileName),
        `${includeMatch[1]}.atk16`
      );

      try {
        await vscode.workspace.fs.stat(vscode.Uri.file(includedFilePath));
        return new vscode.Location(
          vscode.Uri.file(includedFilePath),
          new vscode.Position(0, 0)
        );
      } catch (err) {
        vscode.window.showErrorMessage(
          `ATK16: File not found: ${includedFilePath}`
        );
      }
    } else if (useMatch && useMatch[1] === text) {
      const includedFilePath = path.join(
        path.dirname(document.fileName),
        `${useMatch[1]}.py`
      );

      try {
        await vscode.workspace.fs.stat(vscode.Uri.file(includedFilePath));
        return new vscode.Location(
          vscode.Uri.file(includedFilePath),
          new vscode.Position(0, 0)
        );
      } catch (err) {
        vscode.window.showErrorMessage(
          `ATK16: File not found: ${includedFilePath}`
        );
      }
    } else {
      const labelText = document.getText(range);
      let definitionLocation = await findDefinitionInDocument(document, labelText);
      if (definitionLocation) {
        return definitionLocation;
      }

      // Search for labels in included files
      for (let i = 0; i < document.lineCount; i++) {
        const line = document.lineAt(i);
        const includeMatch = line.text.match(/@include\s+(\S+)/);
        if (includeMatch) {
          const includedFilePath = path.join(
            path.dirname(document.fileName),
            `${includeMatch[1]}.atk16`
          );

          if (await vscode.workspace.fs.stat(vscode.Uri.file(includedFilePath))) {
            const includedDocument = await vscode.workspace.openTextDocument(
              includedFilePath
            );
            definitionLocation = await findDefinitionInDocument(includedDocument, labelText);
            if (definitionLocation) {
              return definitionLocation;
            }
          }
        }
      }
    }

    return null;
  }
}

async function findDefinitionInDocument(document, labelText) {
  for (let i = 0; i < document.lineCount; i++) {
    const line = document.lineAt(i);
    if (line.text.includes(`@label ${labelText}`) || line.text.includes(`@let ${labelText}`)) {
      return new vscode.Location(document.uri, line.range.start);
    }
  }
  return null;
}

async function findSubroutineInDocument(document, labelText) {
  let name = null;
  const params = [];
  const returns = [];
  let extra = "";
  for (let i = 0; i < document.lineCount; i++) {
    const line = document.lineAt(i);
    if (line.text.includes(`@label ${labelText}`)) {
      for (let j = i + 1; j < document.lineCount; j++) {
        const docLine = document.lineAt(j);
        const docCommentMatch = docLine.text.match(/;;;\s+(.*)/);
        if (docCommentMatch) {
          const lineText = docCommentMatch[1];
          const parts = lineText.trim().split(/\s+/g);
          if (lineText.startsWith('subroutine')) {
             name = parts[parts.length - 1].trim();
          } else if (lineText.startsWith('param')) {
            const [_kw, reg, ...desc] = parts;
            params.push([reg.trim(), desc.filter(a => a).join(" ")])
          } else if (lineText.startsWith('return')) {
            const [_kw, reg, ...desc] = parts;
            returns.push([reg.trim(), desc.filter(a => a).join(" ")])
          } else {
            extra += `${lineText}\n`;
          }
        } else {
          break;
        }
      }
    }
  }

  if (name === null) return null;

  let definition = `### ${name}\n_Subroutine_\n\n`;
  definition += extra;
  if (params.length > 0) {
    definition += "\n\n|Reg|Parameter|\n";
    definition += "|:---|:---|\n";
  }
  for (const [reg, desc] of params) {
    definition += `|${reg}|${desc}|\n`;
  }

  if (returns.length > 0) {
    definition += "\n|Reg|Return|\n";
    definition += "|:---|:---|\n";
  }
  for (const [reg, desc] of returns) {
    definition += `|${reg}|${desc}|\n`;
  }
  return definition;
}

function activate(context) {
  context.subscriptions.push(
    vscode.languages.registerDefinitionProvider('atk16', new ATK16DefinitionProvider()),
    vscode.languages.registerHoverProvider('atk16', new ATK16HoverProvider())
  );
}

function deactivate() {}

module.exports = {
  activate,
  deactivate,
};