PROJECT = netronner

DEPS = cowboy jiffy
dep_cowboy = git git://github.com/extend/cowboy.git 1.0.0
dep_jiffy = git git://github.com/davisp/jiffy.git 0.13.0

CT_OPTS += -cover ./test/cover.spec

include erlang.mk


run: rel
	_rel/netronner_node/bin/netronner_node console
tarball: rel
	GIT_COMMIT=$$(git log --pretty=format:'%H' -n 1); \
	IS_SNAPSHOT=$$(git status --porcelain | grep -q '' && echo "-SNAPSHOT" || echo ""); \
	cd _rel; \
	tar cvzf netronner-api-$$GIT_COMMIT$$IS_SNAPSHOT.tar.gz netronner_node	
container-build:
	docker run -v $$(readlink -f .):/netronner-src -ti --rm caligin/debian-erlang:d7e17.4 /bin/bash -c "cd /netronner-src && make -f Makefile.container"
