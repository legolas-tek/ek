"use strict";
//
//  extension.ts
//  EK LSP VSCode Extension
// 
//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
var vscode_1 = require("vscode");
var node_1 = require("vscode-languageclient/node");
var client;
function activate(context) {
    var config = vscode_1.workspace.getConfiguration();
    var path = config.get('ek.languageServerPath');
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    var serverOptions = {
        run: { command: path },
        debug: {
            command: path,
            args: ['--log-level', 'debug']
        }
    };
    // Options to control the language client
    var clientOptions = {
        // Register the server for EK files
        documentSelector: [{ scheme: 'file', language: 'ek' }]
    };
    // Create the language client and start the client.
    client = new node_1.LanguageClient('eklsp', 'EK LSP', serverOptions, clientOptions);
    console.log(path);
    console.log(client);
    // Start the client. This will also launch the server
    client.start();
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
