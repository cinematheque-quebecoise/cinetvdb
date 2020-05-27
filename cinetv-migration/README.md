# cinetvdb

## Étapes

- Prend en entrée le dossier généré par une exportation de CineTV au format suivant: `DonnéesCQ_2019-07-12`.
- Optionnellement, peut prendre en entrée un autre dossier qui contient des fichiers CSV sur des données supplémentaires à inclure dans la base de données.
- Transforme les fichier XLSX vers le format CSV.
- Génère, à partir des fichiers CSV, une base de données Sqlite3 au format suivant: `cinetv_2019-07-12.db`.
- Applique une série d'algorithme qui génère un dossier d'extension de CineTV au format suivant: `DonnéesCQ_ext`. Des exemples d'algorithmes sont la liaison des films et personnes vers Wikidata.
- Génère, à partir des fichiers CSV, une base de données Sqlite3 au format suivant: `cinetv_2019-07-12_ext.db`.
- Migre la base de données vers une autre base de données Sqlite3 qui contient les données qui peuvent être rendues publiques au format suivant: `cinetv_2019-07-12_pub.db`

Les données supplémentaires de CineTV sont disponibles dans un dossier comme `DonnéesCQ_ext`. Pendant l'application de la génération de `cinetv_<DATE>.db`, on va modifier les données dans `DonnéesCQ_ext`. P. ex., si une personne est retirée de CineTV, on la retire des données de `DonnéesCQ_ext`. Si une nouvelle personne est ajouté, on la rajoute dans `DonnéesCQ_ext`. On a également la possibilité de TOUT regénéré.

## Utilisation

`cinetv-release -d <CINETV_XLSX_DIR> <CINETV_EXT_DIR> -o <SQLITE_DIR>` (ex. cinetv-release -d ~/Documents/DonnéesCQ_2019-07-12 -o ~/Documents/cinetv)

La commande précédente appelle les commandes suivantes :

- `cinetv2sqlite -d <CINETV_XLSX_DIR> -o <SQLITE_DIR>` (ex. `cinetv2sqlite -d ~/Documents/DonnéesCQ_2019-07-12 -o ~/Documents/cinetv` produit le fichier `~/Documents/cinetv/cinetv-2019-07-12.db`)
- `cinetvext -d <CINETV_XLSX_DIR> -e <CINETV_EXT_DIR> -o <SQLITE_DIR>` (ex. `cinetv-ext -d ~/Documents/DonnéesCQ_2019-07-12 -e ~/Documents/DonnéesCQ_EXT -o ~/Documents/cinetv` produit le fichier `~/Documents/cinetv/cinetv-2019-07-12-ext.db`)
- `cinetv-exe migrate -s <CINETVDB> -o <SQLITE_DIR>` (ex. `cinetv-exe migrate -s ~/Documents/cinetv/cinetv-2019-07-12-ext.db -o ~/Documents/cinetv` produit le fichier `~/Documents/cinetv/cinetv-2019-07-12-publique.db`)

## Execute

* Run `stack exec -- cinetvdb-exe` to see "We're inside the application!"
* With `stack exec -- cinetvdb-exe --verbose` you will see the same message,
  with more logging.

## Run tests

`stack test`
