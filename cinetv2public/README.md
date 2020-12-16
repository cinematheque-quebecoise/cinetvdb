# cinetv2public

Programme qui prend le fichier SQLite de CineTV contenant des champs privés et qui construit un nouveau fichier SQLite avec uniquement les champs qui peuvent être rendus publiques.

## Usage

```
$ cinetv2public -- -h
cinetv2public version 0.1.0

Usage:
  cinetv2public -s=<sqlitedbpath> -d=<outputdir>

Options:
  -h --help    show this
  -s <sqlitedbpath>    Path of SQLite db path
  -d <outputdir>    Directory of output file

# Exemple d'usage
$ cinetv2public -- -s $HOME/Documents/cinetv/cinetv-2019-07-12/cinetv-2019-07-12-ext.db -d $HOME/Documents/cinetv/cinetv-2019-07-12
```

La commande précédente produit un fichier SQLite nommé `cinetv-2019-07-12-publique.db`

## Exécution avec cabal

Pour lancer le programme avec `cabal`, entrez dans un environnement `Nix` avec:

```
$ nix-shell
```

Ensuite, vous pouvez exécuter le programme avec

```
$ cabal run cinetv2public -- -h
```

## Génération d'un exécutable

Pour générer un exécutable, exécutez la commande:

```
$ nix-build release.nix
```

Ensuite, vous trouverez l'exécutable dans le chemin `./result/bin/cinetv2public`.
