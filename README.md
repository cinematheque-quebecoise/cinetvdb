# cinetvdb

Répertoire qui contient des programmes reliés à CineTV de la Cinémathèque québécoise.

## Environnement de développement

Conçu pour fonctionner sur GNU/Linux avec le programme `nix`.

Pour utiliser correctement les commandes ci-bas, entrez dans le bon environnement avec `nix-shell`.

## Compilation

```
$ make build
```

## Migration de CineTV vers une base de données SQLite

Dans le fichier `Makefile`, modifiez les variables `CINETV_XLSX_DIR`, `CINETV_CSV_EXTENSIONS` et `DESTDIR` vers les chemins désirés dans votre système. La variable `CINETV_XLSX_DIR` pointe vers le dossier qui contient les fichiers XLSX exportés de CineTV. La variable `CINETV_CSV_EXTENSIONS` pointe vers le dossier qui contient des fichiers CSV d'extensions de CineTV.

Pour exécuter la migration:

```
$ make run
```

## Publication des données

Pour publier une nouvelle version des données, vous devez générer un jeton à partir de l'interface web de Gitlab. Ce jeton est appellé un « [Personal Access Token](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html) ».

Cette publication va téléverser le fichier `cinetv-<DATE>-publique.db` qui a été générer par la commande `make run`.

Pour exécuter la publication:

```
$ make release token=<COPIER VOTRE JETON ICI>
```
