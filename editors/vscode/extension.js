const vscode = require('vscode');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');

let client;

function activate(context) {
    const config = vscode.workspace.getConfiguration('vaisto');
    const serverPath = config.get('serverPath', 'vaistoc');

    // Server options - run vaistoc lsp via stdio
    const serverOptions = {
        command: serverPath,
        args: ['lsp'],
        transport: TransportKind.stdio
    };

    // Client options
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'vaisto' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.va')
        }
    };

    // Create and start the client
    client = new LanguageClient(
        'vaisto',
        'Vaisto Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client (also starts the server) with error handling
    client.start().then(() => {
        console.log('Vaisto language server started');
    }).catch((error) => {
        const message = error && error.message ? error.message : String(error);
        console.error('Failed to start Vaisto language server:', message);
        vscode.window.showErrorMessage(
            `Failed to start Vaisto language server. ` +
            `Please ensure "vaistoc" is installed and in your PATH, ` +
            `or configure "vaisto.serverPath" in settings.`
        );
    });
}

function deactivate() {
    if (client) {
        return client.stop();
    }
}

module.exports = { activate, deactivate };
