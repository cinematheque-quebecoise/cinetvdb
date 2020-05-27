# Ontologie de cinémathèque

## À propos 

L'ontologie de cinémathèque fournit les concepts et propriétés principaux pour décrire les données d'une cinémathèque dans le Web Sémantique.

## Contribution

Le développement de l'ontologie se fait à l'aide de l'outil graphique [Eddy](https://github.com/obdasystems/eddy) qui utilise le formalisme [Graphol](http://www.obdasystems.com/graphol). 

L'ontologie est ensuite exportée au format RDF [Turtle](https://fr.wikipedia.org/wiki/Turtle_(syntaxe)) qui est envoyé sur ce répertoire Git.

Vous devez commiter le fichier exporté par `Eddy`. Par la suite, un post-commit-hook va convertir le fichier vers le format `cmtqo-{version}.ttl` selon les données dans le fichier `metadata.yml`.