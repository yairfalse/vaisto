# Vaisto VS Code Extension

Language support for Vaisto - a statically-typed Scheme for the BEAM.

## Features

- Syntax highlighting for `.va` files
- Diagnostics (type errors, parse errors)
- Hover information (type signatures)
- Document symbols (outline)

## Requirements

- Vaisto compiler (`vaistoc`) must be installed and in your PATH
- Or configure the path in settings

## Installation

### From Source (Development)

1. Install dependencies:
   ```bash
   cd editors/vscode
   npm install
   ```

2. Open this folder in VS Code:
   ```bash
   code .
   ```

3. Press `F5` to launch the Extension Development Host

### Manual Installation

1. Build the extension:
   ```bash
   cd editors/vscode
   npm install
   npx vsce package
   ```

2. Install the `.vsix` file:
   - Open VS Code
   - Press `Cmd+Shift+P` (Mac) or `Ctrl+Shift+P` (Windows/Linux)
   - Run "Extensions: Install from VSIX..."
   - Select the generated `.vsix` file

## Configuration

| Setting | Default | Description |
|---------|---------|-------------|
| `vaisto.serverPath` | `vaistoc` | Path to the vaistoc executable |
| `vaisto.trace.server` | `off` | Trace communication with LSP server |

## Building vaistoc

If you don't have `vaistoc` in your PATH:

```bash
# From the vaisto project root
mix escript.build

# Add to PATH or configure in VS Code settings
export PATH="$PATH:/path/to/vaisto"
```

## Usage

1. Open a `.va` file
2. Hover over variables/functions to see types
3. Errors appear as red squiggles
4. Use `Cmd+Shift+O` to see document symbols

## Example

```scheme
; example.va
(defn add [a :int b :int] :int
  (+ a b))

(deftype Result
  (Ok v)
  (Error msg))

(let [x 42]
  (add x 1))  ; Hover over 'add' to see: (Int, Int) -> Int
```
