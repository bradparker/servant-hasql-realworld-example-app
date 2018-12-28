# Servant and Squeal / Hasql Example App

I've done this with Beam, now let's try Squeal with Hasql

## Setup

You'll need nix, and cabal-install.

1. Copy and allow the environment file.

```
$ cp .envrc.example .envrc
$ nix-shell --command 'direnv allow'
```

2. Init and start a postgres database cluster.

```
$ nix-shell --command 'cabal new-run servant-hasql-realworld-example-database init'
$ nix-shell --command 'cabal new-run servant-hasql-realworld-example-database start'
```

3. Create and migrate the database.

```
$ nix-shell --command 'cabal new-run servant-hasql-realworld-example-database setup'
```

## Running

```

$ nix-shell --command 'cabal new-run servant-hasql-realworld-example-app'
```
