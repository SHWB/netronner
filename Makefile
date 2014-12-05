PROJECT = netronner
DEPS = cowboy jiffy
include erlang.mk

# todo: fix deps at version
#  {cowboy, ".*", {git, "git://github.com/extend/cowboy.git", {tag, "1.0.0"}}},
#  {jiffy , ".*", {git, "git://github.com/davisp/jiffy.git", {tag, "0.13.0"}}}


run: rel
	_rel/netronner_node/bin/netronner_node console
tarball: rel
	GIT_COMMIT=$$(git log --pretty=format:'%H' -n 1); \
	IS_SNAPSHOT=$$(git status --porcelain | grep -q '' && echo "-SNAPSHOT" || echo ""); \
	cd _rel; \
	tar cvzf netronner-api-$$GIT_COMMIT$$IS_SNAPSHOT.tar.gz netronner_node	
container-build:
	docker run -v $$(readlink -f .):/netronner-src -ti --rm caligin/debian-erlang /bin/bash -c "cd /netronner-src && make -f Makefile.container"
