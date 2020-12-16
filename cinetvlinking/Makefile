# Date of data exportation.
DATE=2019-07-12
CINETV_SQLITE=${HOME}/Documents/cinetv/cinetv-${DATE}/cinetv-${DATE}-publique.db
CINETV_CSV_EXTENSIONS=${HOME}/git/cq/cinetvext
DESTDIR=data

build:
	cabal build

preprocess: ${CINETV_CSV_EXTENSIONS} ${CINETV_SQLITE} ${DESTDIR}
	cabal run -- cinetvlinking-exe nom preprocess ${CINETV_CSV_EXTENSIONS} 750 0.6 -d ${CINETV_SQLITE} -o ${DESTDIR}

evaluate: ${CINETV_SQLITE} ${DESTDIR}
	cabal run -- cinetvlinking-exe nom evaluate -d ${CINETV_SQLITE} -o ${DESTDIR}

evaluate-test: ${CINETV_SQLITE} ${DESTDIR}
	cabal run -- cinetvlinking-exe nom evaluate -t -d ${CINETV_SQLITE} -o ${DESTDIR}

evaluate-result: ${CINETV_SQLITE} ${DESTDIR}
	cabal run -- cinetvlinking-exe nom evaluate-result -d ${CINETV_SQLITE} -o ${DESTDIR}

evaluate-test-result: ${CINETV_SQLITE} ${DESTDIR}
	cabal run -- cinetvlinking-exe nom evaluate-result -t -d ${CINETV_SQLITE} -o ${DESTDIR}

interactive: ${CINETV_SQLITE} ${DESTDIR}
	cabal run -- cinetvlinking-exe nom interactive -d ${CINETV_SQLITE} -o ${DESTDIR}

apply: ${CINETV_SQLITE} ${DESTDIR}
	cabal run -- cinetvlinking-exe nom apply -d ${CINETV_SQLITE} -o ${DESTDIR}
