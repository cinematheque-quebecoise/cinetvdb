# cinetvdb

Répertoire qui contient des programmes reliés à CineTV de la Cinémathèque québécoise.

## Prérequis

1. Exporter la base de donnnées CineTV vers un dossier contenant des fichiers au format XLSX. Le nom du dossier doit être au format `DonnéesCQ_<DATE>` (ex. `DonnéesCQ_2019-07-12`)
2. Modifiez la variable `CINETV_XLSX_DIR` du fichier `Makefile` pour pointer vers le bon chemin vers `DonnéesCQ_<DATE>`
3. Récupérez les extensions de CineTV à partir du dépôt [https://gitlab.com/cinematheque-quebecoise/cinetvext](cinetvext)
4. Modifiez la variable `CINETV_CSV_EXTENSIONS` du fichier `Makefile` pour pointer vers le bon chemin vers `cinetvext`

## Environnement de développement

Le programme est conçu pour fonctionner sur GNU/Linux avec le programme `nix`.

Pour utiliser correctement les commandes ci-bas, entrez dans le bon environnement avec `nix-shell`.

## Compilation

```
$ make build
```

## Migration de CineTV vers une base de données SQLite

Dans le fichier `Makefile`, modifiez les variables `CINETV_XLSX_DIR`, `CINETV_CSV_EXTENSIONS` et `DESTDIR` vers les chemins désirés dans votre système. La variable `CINETV_XLSX_DIR` pointe vers le dossier qui contient les fichiers XLSX exportés de CineTV. La variable `CINETV_CSV_EXTENSIONS` pointe vers le dossier qui contient des fichiers CSV d'extensions de CineTV. La variable `DESTDIR` pointe vers le dossier de destination des résultats.

Pour exécuter la migration:

```
$ make run
```

Vous pouvez aussi appliquer l'algorithme de désambiguïsation d'entités de CineTV automatiquement. Il faut spécifier le chemin qui va contenir les entités désambuïsées dans la variable `CINETV_CSV_EXTENSIONS_AUTO` du `Makefile`.

```
$ make runwithlinking
```


## Publication des données

Pour publier une nouvelle version des données, vous devez générer un jeton à partir de l'interface web de Gitlab. Ce jeton s'appelle un « [Personal Access Token](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html) ».
Copiez/collez ce jeton dans le fichier `.gitlab-token` à partir de la racine de ce projet.

Pour exécuter la publication:

```
$ make release
```

La publication va téléverser sur Gitlab le fichier `cinetv-<DATE>-publique.db` qui a été générer par la commande `make run`.

