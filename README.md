# This app is intended to help one memorize static language things (phrases, words, etc.) easier and faster.
* A "Set" consists of its name and the Units one wants to memorize.
* A "Unit" consists of a text in the first language and a list of texts, which are translates of the text, in the second language.
* The "Memorizing Mode" page is something like interactive flashcards showing a randomly selected Unit text or one of Unit translates from a randomly selected Active Set while one is typing the answer.
* The "Statistics" page shows one's memorizing history (newest first) highlighting answered Units with green and shown Units or undone memorizing session with red.
# How to start
1. Create at least one Set by typing one's name in the input field and clicking on the "+" button on the "Sets" page.
2. Create at least one Unit inside the Set on the "Set" page (click a Set's name on the "Sets" page to get there) by clicking the "+" button and filling in newly created Unit fields.
3. Mark the Set as Active Set on the "Settings" page by clicking on its name and then the "Save" button.

# Nontouchscreen offline client build
https://monadosquito.github.io/language-memorizer

# Latest online client build
*platform.html* from 1.0.0 tag

# Latest server build
*server.exe* from 1.0.0 tag

# How to compile client and server with ghcjs and ghc (respectively)
Run the `nix-build -A prod` from the repository root directory.
### Warning
**This may take very long to compile for the first time.**
### Hint
The client HTMLs and server binary file will be inside *result* directory.

# How to run client in interpreted mode watching source code changes (e.g. for development) with ghci and ghcid
Run the `nix-shell --pure --run reload-platform` or `nix-shell -A clientDev --pure --run reload-platform` from the repository root directory.

# Also to run server
1. Set appropriate development database credentials in the *env.nix* file.
2. Create a PostgreSQL database from the *init-db.sql* file.
3. Run the `nix-shell -A server --pure --run reload` from the repository root directory.

### Warning
**This also may take very long (but not so much) to run for the first time.**

# Warnings
* **The `nix-shell -A server --pure --run reload` won't work before the "chore!: server" commit.**
* **The `nix-shell -A clientDev --pure --run reload-platform` won't work the before "chore!: server" commit.**
* **The `nix-shell --pure --run reload-platform` won't work after the "chore!: server" commit.**

# Hints
* One can omit the `--pure` flag so things from one's machine get available directly in the isolated environment or when passing the `--run` option a command.
* One can omit the `--run` option to get into the isolated environment without running any command.
* One can pass the `ghci` command instead of `reload-platform` or `reload` to get into GHCi equipped with the Haskell dependencies, which are only available in the isolated environment created by the `nix-shell`.

# Warning
**When running an online version of the client, a part of features won't work without running the server.**

# Platforms
* nontouchscreen
* touchscreen

# How to make nix-build and nix-shell available
Follow the instructions here https://nixos.org/guides/install-nix.html.

# Notes on commit messages
* "... (...)" means "..." for/with "(...)".
* Adjective-noun chain means first adjective or something compound and set off by parentheses is an existing feature meanwhile the other adjectives and the noun are new. It should be read left-associatively.
### Example
"(set, units) pagination settings can be seen as '((set, units) pagination) settings' and read as 'Add settings for added pagination for sets and units'".
* "type!:" or "type(scope(s))!:" mean breaking change. 
* "type(^scope):" means all scopes are concerned but this one.
* "type(platform):" means all scopes are concerned within the platform.
* "type(platform: scope(s)):" means scopes are concerned only within the platform.
* "type:" means all scopes of all platforms are concerned.
* No verb: means a default verb is assumed.

# Default verbs
* "feat" type: add
* "chore" type: Add "..." dependency(ies).

# Scopes
### Before "chore!: server" commit
* model
* utils
* views

### After "chore!: server" commit
* adapters
* core
* model
* ports
* utils
* views

# P.S.
I'm sorry one saw this.
