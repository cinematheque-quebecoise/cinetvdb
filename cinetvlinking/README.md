# cinetvlinking

Programme qui produit des liens entre la base de données CineTV de la Cinémathèque québécoise et une source externe (ex. Wikidata).

## Usage

```
$ cinetvlinking-exe --help
Usage: cinetvlinking-exe [--version] [--help] [-v|--verbose] COMMAND
                         (-d|--cinetvdb CINETVDB) (-o|--outputdir OUTPUTDIR)
  Program description, also for command line arguments

Available options:
  --version                Show version
  --help                   Show this help text
  -v,--verbose             Verbose output?
  -d,--cinetvdb CINETVDB   File path of the CineTV Sqlite database file
  -o,--outputdir OUTPUTDIR Output directory for saved files.

Available commands:
  nom                      Link Nom table to Wikidata.
  pays                     Link Pays table to Wikidata.
  filmo                    Link Filmo table to Wikidata.
```

## Compilation

Pour la compilation, l'outil [Nix](https://nixos.org/) doit être installé sur votre système.

À partir de la racine de ce projet, lancer un environnement nix:

```
nix-shell
```

### Développement

Afin d'obtenir un cycle de compilation rapide pour le développement, nous
utilisons l'outil `Cabal` (devrait être disponible grace à `nix-shell`).

```
# Pour compiler
cabal build

# Pour tester l'exécutable
cabal run cq2rdf-exe -- --help
```

### Génération d'un exécutable

Générez l'exécutable avec :

```
$ nix-build release.nix
```

L'exécutable devient disponible dans le chemin relatif `./result/bin/cq2rdf-exe`.
