# Date of data exportation.
DATE=2019-07-12
# Location of XLSX files
CINETV_XLSX_DIR=${HOME}/Documents/DonnéesCQ_$(DATE)
# Location of CineTV data extensions
CINETV_CSV_EXTENSIONS=${HOME}/Nextcloud/Documents/cmtq/CineTVExt
CINETV_CSV_EXTENSIONS_AUTO=${HOME}/Nextcloud/Documents/cmtq/CineTVExtGenere
# Destination directory of produced files
DESTDIR=${HOME}/Documents/cinetv
# Executable name
EXEC=cinetv2sqlite
GITLAB_PROJECT_ID=19038139
VERSION=`grep "version\s*=" default.nix | sed "s/.*\"\(.*\)\".*/\1/"`
VERSION := $(shell grep "version\s*=" default.nix | sed "s/.*\"\(.*\)\".*/\1/")

build:
	nix-build release.nix

run:
	./result/bin/$(EXEC) -d ${CINETV_XLSX_DIR} -e ${CINETV_CSV_EXTENSIONS} -o ${DESTDIR}

runwithlinking:
	./result/bin/$(EXEC) -d ${CINETV_XLSX_DIR} -e ${CINETV_CSV_EXTENSIONS} -a ${CINETV_CSV_EXTENSIONS_AUTO} -o ${DESTDIR}

# WARNING: To following rule uploads the CineTV public SQLite database to a private Gitlab repository. BE SURE TO DOUBLE CHECK THAT ITS THE RIGHT SQLITE DATABASE THAT WILL BE SENT!
#
# If release tag does not exist on Gitlab server, it returns a 403 Forbidden HTTP code.
# @param token - Private Gitlab token
# $ make release token=<YOURTOKEN>
release: $(DESTDIR)/cinetv-$(DATE)/cinetv-$(DATE)-publique.db
	./upload-release.sh \
		"cinetvdb v$(VERSION)" \
		"v$(VERSION)" \
		$(GITLAB_PROJECT_ID) \
		"New release of cinetvdb v$(VERSION)" \
		$(token) \
		$<

clean:
	rm $(EXEC)
