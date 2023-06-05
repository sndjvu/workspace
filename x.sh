#!/bin/sh

set -o errexit -o nounset

ARCH="${ARCH:-linux-amd64}"

git config user.name "Cole Miller"
git config user.email "m@cole-miller.net"

export CARGO_REGISTRIES_CRATES_IO_PROTOCOL=sparse

deps() {
	curl -L "https://github.com/alecthomas/chroma/releases/download/v2.7.0/chroma-2.7.0-$ARCH.tar.gz" | tar xzf - chroma
}

check() {
	cargo test --workspace --all-features --no-run --profile ci
	cargo test --workspace --all-features --profile ci
}

www() {
	if test "$GITHUB_ACTIONS" != "true"; then
		echo "fatal: not running on GitHub Actions"
		return 1
	fi

	RUSTDOCFLAGS='--cfg sndjvu_doc_cfg' \
		cargo doc --workspace --all-features --no-deps --target x86_64-unknown-linux-gnu
	mkdir www/rustdoc
	cp -r target/x86_64-unknown-linux-gnu/doc/* www/rustdoc

	cp crates/sndjvu/sndjvu.h www/sndjvu.h
	./chroma -l c -s friendly -f html --html-inline-styles <crates/sndjvu/sndjvu.h >www/sndjvu.h.html

	find . -maxdepth 1 -mindepth 1 '!' '(' -name .git -or -name www ')' -exec rm -r '{}' '+'
	find www -maxdepth 1 -mindepth 1 -exec mv -t . '{}' '+'
	rmdir www
	touch .nojekyll

	git checkout --orphan temp
	git add -A
	git commit -m "Prepare www.sndjvu.org"

	git branch gh-pages
	git push -f origin gh-pages
}

ci() {
	deps
	check
	www
}

"$@"
