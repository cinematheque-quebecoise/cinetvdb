# cinetvlinking

Programme qui lie automatiquement des entités de CineTV vers Wikidata.

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
  filmo                    Link Filmo table to Wikidata.


$ cinetvlinking-exe nom --help
Usage: cinetvlinking-exe nom COMMAND
  Link Nom table to Wikidata.

Available options:
  -h,--help                Show this help text

Available commands:
  preprocess               Preprocess data for linking Nom table.
  evaluate                 Apply algorithm on annotated dataset.
  evaluate-result          Evaluate algorithm on generated dataset.
  apply                    Apply algorithm on unannotated dataset.


$ cinetvlinking-exe nom preprocess --help
Usage: cinetvlinking-exe nom preprocess TOTAL_DATA_SIZE VALIDATION_DATA_RATIO
  Preprocess data for linking Nom table.

Available options:
  -h,--help                Show this help text


$ cinetvlinking-exe nom evaluate --help
Usage: cinetvlinking-exe nom evaluate [-t|--test]
  Apply algorithm on annotated dataset.

Available options:
  -h,--help                Show this help text
  -t,--test                Final testin


$ cinetvlinking-exe nom evaluate-result --help
Usage: cinetvlinking-exe nom evaluate-result [-t|--test]
  Evaluate algorithm on generated dataset.

Available options:
  -h,--help                Show this help text
  -t,--test                Final testing


$ cinetvlinking-exe nom apply --help
Usage: cinetvlinking-exe nom apply [-r|--restart]
  Apply algorithm on unannotated dataset.

Available options:
  -h,--help                Show this help text
  -r,--restart             Algorithm application on ALL data (even if already
                           annotated)


$ cinetvlinking-exe nom interactive --help
Usage: cinetvlinking-exe nom interactive
  Apply algorithm interactively.

Available options:
  -h,--help                Show this help text


$ cinetvlinking-exe filmo --help
Usage: cinetvlinking-exe filmo [-r|--restart]
  Link Filmo table to Wikidata.

Available options:
  -h,--help                Show this help text
  -r,--restart             Algorithm application on ALL data (even if already
                           annotated)
```

### Ajouts dans Wikidata (À COMPLÉTER)

```
$ cinetvlinking-exe nom wikidata --help
Usage: cinetvlinking-exe nom wikidata [-r|--restart]
  Write linked to Wikidata

Available options:
  -h,--help                Show this help text
  -r,--restart             Algorithm application on ALL data (even if already
                           annotated)
```

Instructions pour la création d'un bot: https://www.wikidata.org/wiki/Wikidata:Bots

- Create user bot like "cqbot"
- Add operator to bot user page
- List all tasks on the bots user page
- Use assert parameter to ensure that no edits are made when logged off
-

## Environnement de développement

Le programme est conçu pour fonctionner sur GNU/Linux avec le programme `nix`.

Pour utiliser correctement les commandes ci-bas, entrez dans le bon environnement avec

```
nix-shell
```

### Compilation

Afin d'obtenir un cycle de compilation rapide pour le développement, nous
utilisons l'outil `Cabal` (devrait être disponible grace à `nix-shell`).

```
# Pour compiler
cabal build

# Pour tester l'exécutable
cabal run cinetvlinking-exe -- --help
```

### Exécution

Plusieurs commandes prédéfinies se trouvent dans le fichier `Makefile`. Par exemple:

```
make build
make preprocess
```

### Génération d'un exécutable

Générez l'exécutable avec :

```
$ nix-build release.nix
```

L'exécutable devient disponible dans le chemin relatif `./result/bin/cinetvlinking-exe`.
