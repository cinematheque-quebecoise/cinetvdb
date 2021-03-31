# Date of data exportation.
DATE=2019-07-12
# Location of XLSX files
CINETV_XLSX_DIR=${HOME}/Documents/DonnéesCQ_$(DATE)
# Location of CineTV data extensions
CINETV_CSV_EXTENSIONS=${HOME}/git/cq/cinetvext
# Destination directory of produced files
DESTDIR=${HOME}/Documents/cinetv
CINETV_CSV_EXTENSIONS_AUTO=${DESTDIR}/CineTVExtGenere
# Executable name
EXEC=cinetv2sqlite
GITLAB_PROJECT_ID=19038139
VERSION=`grep "version\s*=" default.nix | sed "s/.*\"\(.*\)\".*/\1/"`
VERSION := $(shell grep "version\s*=" default.nix | sed "s/.*\"\(.*\)\".*/\1/")
GITLAB_TOKEN := $(shell cat .gitlab-token)

build:
	nix-build release.nix

run: ${CINETV_XLSX_DIR} ${CINETV_CSV_EXTENSIONS}
	./result/bin/$(EXEC) -d ${CINETV_XLSX_DIR} -e ${CINETV_CSV_EXTENSIONS} -o ${DESTDIR}

runwithlinking: ${CINETV_XLSX_DIR} ${CINETV_CSV_EXTENSIONS}
	./result/bin/$(EXEC) -d ${CINETV_XLSX_DIR} -e ${CINETV_CSV_EXTENSIONS} -a ${CINETV_CSV_EXTENSIONS_AUTO} -o ${DESTDIR}

# WARNING: The following rule uploads the CineTV public SQLite database to a private Gitlab repository. BE SURE TO DOUBLE CHECK THAT ITS THE RIGHT SQLITE DATABASE THAT WILL BE SENT!
#
# If release tag does not exist on Gitlab server, it returns a 403 Forbidden HTTP code.
# @param token - Private Gitlab token
# $ make release
release: $(DESTDIR)/cinetv-$(DATE)/cinetv-$(DATE)-csv.tar.gz $(DESTDIR)/cinetv-$(DATE)/cinetv-$(DATE)-sqlite.tar.gz .gitlab-token
	./upload-release.sh \
		"cinetvdb v$(VERSION)" \
		"v$(VERSION)" \
		$(GITLAB_PROJECT_ID) \
		"New release of cinetvdb v$(VERSION)" \
		$(GITLAB_TOKEN) \
		$(DESTDIR)/cinetv-$(DATE)/cinetv-$(DATE)-csv.tar.gz \
		$(DESTDIR)/cinetv-$(DATE)/cinetv-$(DATE)-sqlite.tar.gz

clean:
	rm $(EXEC)
