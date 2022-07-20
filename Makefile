PLATFORM ?= musl64
STRIP = strip
PKG ?= simhash
COMPILER = ghc923

ifeq ($(PLATFORM),aarch64-multiplatform-musl)
STRIP = aarch64-linux-gnu-strip
else
ifeq ($(PLATFORM),muslpi)
STRIP = armv6l-unknown-linux-musleabihf-strip
COMPILER = ghc884
else

endif

endif

all: package

dist/$(PLATFORM):
	mkdir -p $@

dist/$(PLATFORM)/%: dist/$(PLATFORM)
	nix-build -A projectCross.$(PLATFORM).hsPkgs.$(PKG).components.exes.$(shell basename $@) --argstr compiler-nix-name $(COMPILER)
	cp -f result/bin/$(shell basename $@) $@
	chmod +w $@
	nix-shell --run "$(STRIP) -s $@" --argstr compiler-nix-name $(COMPILER) --arg crossPlatforms "ps: with ps; [$(PLATFORM)]"
	chmod -w $@

simhash-infer-learn: dist/$(PLATFORM)/simhash-infer-learn
simhash-infer: dist/$(PLATFORM)/simhash-infer
simhash-train: dist/$(PLATFORM)/simhash-train

package: simhash-train simhash-infer simhash-infer-learn
	cd dist/$(PLATFORM) && tar cjvf ../simhash-linux-$(PLATFORM).tar.bz2 *


plan-sha256:
	nix-build -A plan-nix.passthru.calculateMaterializedSha | bash

materialized:
	rm -r nix/materialized
	nix-build 2>&1 | grep -om1 '/nix/store/.*-updateMaterialized' | bash

clean:
	rm -rf dist

help:
	@echo make PLATFORM=muslpi
	@echo make PLATFORM=musl64
	@echo make PLATFORM=aarch64-multiplatform-musl
	@echo make clean
	@echo make plan-sha256
	@echo make materialized
