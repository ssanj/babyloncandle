---
title: Running Scala Metals With Sublime Text on MacOSX
author: sanjiv sahayam
description: How to setup Scala Metals on Sublime Text on MacOSX Mojave
tags: scala, metals, sublimeide
comments: true
---

I've finally got [Scala Metals](https://scalameta.org/metals) working through Sublime Text 3 on MacOSX Mojave and I'm pretty happy about it! There are some quirks to getting it to work though, so I thought I'd document them here for anyone else who might be struggling to set this up.

![Metal Works](/images/metals/metals-sample.gif)

## Installation

- Use the package manager to install the *LSP* plugin from [tomv564/LSP](https://packagecontrol.io/packages/LSP) or from [Github](https://github.com/tomv564/LSP).
- Make sure you have a Java 8 JDK installed and that it is returned as the default JDK. You can find out what your default JDK is by running:

```{.terminal .scrollx}
  /usr/libexec/java-home
```

  If you don't see a JDK 1.8.x version listed, then have a look at [Invalid Java Version](#invalid-java-version-something-other-than-jdk-8)

- Make sure you have a supported Scala version (2.11 and 2.12):

  > Metals works only with Scala versions 2.12.8, 2.12.7, 2.11.12, 2.12.6, 2.12.5, 2.12.4, 2.11.11, 2.11.10 and 2.11.9. Note that 2.10.x and 2.13.x are not supported.

- Install [coursier](https://get-coursier.io/) through *Homebrew*:

```{.terminal .scrollx}
  brew tap coursier/formulas
  brew install coursier
```
  Once installed verify that you have the latest version with:

```{.terminal .scrollx}
  coursier --help
```

  Which should return you a version after `1.1.0-M9`. The latest version as of writing this post is:

```{.terminal .scrollx}
  Coursier 2.0.0-RC1
```
- Install [Scala Metals](https://scalameta.org/metals/) for [Sublime Text](https://scalameta.org/metals/docs/editors/sublime.html):

```{.terminal .scrollx}
  coursier bootstrap \
    --java-opt -Xss4m \
    --java-opt -Xms100m \
    --java-opt -Dmetals.client=sublime \
    org.scalameta:metals_2.12:0.6.1 \
    -r bintray:scalacenter/releases \
    -r sonatype:snapshots \
    -o /usr/local/bin/metals-sublime -f
```

  _The incantation above installs Metals `0.6.1`. Check the Metals site for the latest version_.

  Ensure the generated **metals-sublime** binary is available on your _$PATH_.

- Ensure you have [SBT](https://www.scala-sbt.org/) version 0.13.17+ or 1.x installed.


## Configuration

- Update your *key bindings* for LSP (**Preferences** > **Package Settings** > **LSP** > **Key Bindings**) as needed. The snippet below adds the **F12** binding for going to a definition of a symbol and the **CMD** + **ALT** + **H** binding for signature help and **SHIFT** + **F10** to import a project:
  

```{.json .scrollx}
    { "keys": ["f12"], "command": "lsp_symbol_definition"},
    { "keys": ["super+alt+h"], "command": "noop", "context": [{ "key": "lsp.signature_help", "operator": "equal", "operand": 0}], },
    { "keys": ["shift+f10"], "command": "lsp_execute", "args":{"command_name": "build-import", "command_args":{}}},
```

- Update your *settings* for LSP (**Preferences** > **Package Settings** > **LSP** > **Settings**) with the following:

```{.json .scrollx}
{
  "log_payloads": true,
  "log_debug": true,
  "log_stderr": true,
  "show_diagnostics_severity_level": 4,
  "show_code_actions_bulb": true,
  "complete_all_chars": true,
  "only_show_lsp_completions": true,
  "prefer_label_over_filter_text": true
}
```

  I've included a lot of diagnostic logging to ensure I can see when something goes wrong. If you are happy with your setup, feel free to remove this extra logging:

  - **log_payloads** - Log all payloads from the LSP server
  - **log_debug** - Extra verbose logging
  - **log_stderr** - Log server errors
  - **show_diagnostics_severity_level** - Bumped to 4 which is INFO and below (everything). 3 is default.

  These are the non-diagnostic settings:

  - **prefer_label_over_filter_text** - Set to **true** to return full function definitions as suggestions. If set to **false** returns only the function name.
  - **show_code_actions_bulb** - Show icon in the gutter when there is an action to take
  - **only_show_lsp_completions** - When set to **true** Turns off all other suggestions for completions.
  - **complete_all_chars** - Set to **true** to get completions for all characters

  You can find more detail on these settings in the [docs](https://lsp.readthedocs.io/en/latest/features/#configuring).

- Open a Scala project through Sublime Text.
- Launch the Command Palette with **CMD** + **SHIFT** + **P**  and choose **LSP: Enable Language Server in Project** > **metals**. This will create a *.metals* directory in your root folder. It will contain a *metals.log* file which you can scan for any errors.
- Open the console with **CTRL** + **`** to scan for any errors.
- Browse to a Scala file and open it. This needs to be done to trigger the import process.
- There should be a prompt asking you to import the project. Choose **import**. This will then proceed to download all your dependencies and create a *.bloop* directory in your root folder. You should see something like this in the console:


    > server: running 'sbt metalsEnable bloopInstall'

- Watch the console for any obvious errors such as:

    > no functionality will work

- If everything worked you should see something like this in the logs:
  
    > server: time: compiled project your_project_name in time_taken s

- If you didn't see the import dialog, press **SHIFT** + **F10**. If you saw any errors in the log go to [workarounds](#workarounds).


## Workarounds

### No Import Dialog

- Try **SHIFT** + **F10**. If that does not work follow the steps below.
- Launch the Command Palette with **CMD** + **SHIFT** + **P**  and choose **LSP: Disable Language Server in Project** > **metals**.
- Delete the *.metals* directory. If you haven't stopped the server, the *.metals* folder will keep reappearing.
- Close the project window. You may see a message about the metals server crashing. Choose **cancel** not to restart it.
- Open your project in Sublime Text again. 

- Launch the Command Palette with **CMD** + **SHIFT** + **P**  and choose **LSP: Enable Language Server in Project** > **metals**.
- Open a Scala file.
- You should now see the **import** dialog. Choose **import**.

_If you do not see the *import* dialog check the logs for one of the other errors listed below._

### Invalid SBT version

You'll see something like this in the Sublime Text console:

 > Automatic build import is not supported for sbt 0.13.xyz. To fix this problem, upgrade to sbt v0.13.17+

Bump your SBT version to at least *0.13.17* in your *project/properties* and then follow [No Import Dialog](#no-import-dialog)

### Invalid Java version (Something other than JDK 8)

Metals only works with JDK 8 at the moment.

> OpenJDK or Oracle Java 8. Eclipse OpenJ9 and Java 11 are not supported, please make sure the JAVA_HOME environment variable points to valid Java 8 installation.

Use */usr/libexec/java-home* to verify your JDK version. When I initially ran it, I had a bunch of JDKs installed:

```{.terminal .scrollx}
jdk-10.0.2.jdk
jdk1.8.0_181.jdk
jdk1.8.0_131.jdk
```

and *jdk-10.0.2* was chosen as the default - because it was the latest. And while the are a [number](https://superuser.com/questions/682260/how-can-i-set-environment-variables-for-gui-apps-in-os-x-mavericks) of [workarounds](https://stackoverflow.com/questions/1348842/what-should-i-set-java-home-to-on-osx/16428639) for [this](https://www.ibm.com/support/knowledgecenter/en/SSPJLC_7.6.2/com.ibm.si.mpl.doc/tshoot/ts_java_home.html), I chose to go the simple route and moved my *jdk-10.0.2* installation into an *other* folder since I didn't really use it:


```{.terminal .scrollx}
other //my jdk-10.0.2.jdk installation is in here
jdk1.8.0_181.jdk
jdk1.8.0_131.jdk
```

Consequently when I ran */usr/libexec/java-home*, *jdk1.8.0_181* was the default JDK - which is what I wanted. You can also use something like [Jenv](https://github.com/jenv/jenv) to manage your Java environments.

### no build target: using presentation compiler

This means that [bloop](https://github.com/scalacenter/bloop) has not run against your project, which means the project has not been imported. Follow [No Import Dialog](#no-import-dialog) to fix.

### Not a valid command: metalsEnable 

Sounds like this is some sort of corruption issue, documented [here](https://github.com/scalameta/metals/issues/685) or [here](https://github.com/scalameta/metals/issues/689). The log file should reference a *sbt-launch.jar* in the */tmp* directory:


 > /tmp/metals-some-long-hash/sbt-launch.jar

- Delete the above file
- Delete *project/target* directory
- Delete the  *~/.sbt/1.0/plugins/target/* directory
- Delete the *.bloop* directory in your project root

Follow [No Import Dialog](#no-import-dialog) to reimport the project.

### No .metals folder created

You launched the Command Palette with **CMD** + **SHIFT** + **P**  and choose **LSP: Enable Language Server in Project** > **metals**. Unfortunately no *.metals* folder appeared in your project root. 

Launch the Command Palette and choose **LSP: Disable Language Server in Project** > **metals**.

Give it a second and try enabling it again with **LSP: Enable Language Server in Project** > **metals**. This should hopefully create the *.metals* folder. If not try closing the project window in Sublime and trying this workaround again.

## What's not working

I couldn't get imports working. It looks like this works in Visual Studio Code though. At the moment I use my [Scuggest](https://github.com/ssanj/scuggest) plugin to fill this gap.

## Glossary

### All LSP Server Settings

- **complete_all_chars** true request completions for all characters, not just trigger characters
  - **only_show_lsp_completions** false disable sublime word completion and snippets from autocomplete lists
  - **completion_hint_type** "auto" override automatic completion hints with "detail", "kind" or "none"
  - **prefer_label_over_filter_text** false always use the "label" key instead of the "filterText" key in CompletionItems
  - **show_references_in_quick_panel** false show symbol references in Sublime's quick panel instead of the bottom panel
  - **quick_panel_monospace_font** false use monospace font for the quick panel
  - **show_status_messages** true show messages in the status bar for a few seconds
  - **show_view_status** true show permanent language server status in the status bar
  - **auto_show_diagnostics_panel** true open the diagnostics panel automatically if there are diagnostics
  - **show_diagnostics_phantoms** false show diagnostics as phantoms while the file has no changes
  - **show_diagnostics_count_in_view_status** false show errors and warnings count in the status bar
  - **show_diagnostics_in_view_status** true when on a diagnostic with the cursor, show the text in the status bar
  - **diagnostics_highlight_style** "underline" highlight style of code diagnostics, "underline" or "box"
  - **highlight_active_signature_parameter**: highlight the active parameter of the currently active signature
  - **document_highlight_style**: document highlight style: "underline", "stippled", "squiggly" or ""
  - **document_highlight_scopes**: customize your sublime text scopes for document highlighting
  - **diagnostics_gutter_marker** "dot" gutter marker for code diagnostics: "dot", "circle", "bookmark", "cross" or ""
  - **show_code_actions_bulb** false show a bulb in the gutter when code  * -* *actions* are available
log_debug false show debug logging in the sublime console
  - **log_server** true show server/logMessage notifications from language   - *servers* in the console
  - **log_stderr** false show language server stderr output in the console
log_payloads false show full JSON-RPC responses in the console

### All Keybindings

```{.json .scrollx}
[
    // Show Code Actions
    { "keys": ["super+."], "command": "lsp_code_actions" },

    // Show/Hide Diagnostics Panel
    { "keys": ["super+alt+m"], "command": "lsp_show_diagnostics_panel" },

    // Go To Next/Previous Diagnostics - THIS OVERRIDES DEFAULT SUBLIME KEYBINDINGS
    // { "keys": ["f4"], "command": "next_result" },
    // { "keys": ["shift+f4"], "command": "prev_result" },

    // Trigger Signature Help
    { "keys": ["super+alt+space"], "command": "noop", "context": [{ "key": "lsp.signature_help", "operator": "equal", "operand": 0}] },

    // Move Up/Down in Signature Help
    { "keys": ["up"], "command": "noop", "context": [{ "key": "lsp.signature_help", "operator": "equal", "operand": -1 }] },
    { "keys": ["down"], "command": "noop", "context": [{ "key": "lsp.signature_help", "operator": "equal", "operand": 1 }] },

     // Find Symbol References
    { "keys": ["shift+f12"], "command": "lsp_symbol_references" },

    // Go To Definition
    // {"keys": ["UNBOUND"], "command": "lsp_symbol_definition"},

    // Rename Symbol
    // { "keys": ["UNBOUND"], "command": "lsp_symbol_rename" },

    // Format Document
    // {"keys": ["UNBOUND"], "command": "lsp_format_document"},

    // Format Selection
    // {"keys": ["UNBOUND"], "command": "lsp_format_document_range"},

    // Document Symbols
    // {"keys": ["UNBOUND"], "command": "lsp_document_symbols"},

    // Symbol Hover
    // {"keys": ["UNBOUND"], "command": "lsp_hover"},
]
```