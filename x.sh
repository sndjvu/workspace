#!/bin/sh

set -o errexit -o nounset

git config user.name "Cole Miller"
git config user.email "m@cole-miller.net"

www() {
	RUSTDOCFLAGS='--cfg sndjvu_doc_cfg' \
		cargo doc --workspace --all-features --no-deps --target x86_64-unknown-linux-gnu
	mkdir www/rustdoc
	cp -r target/x86_64-unknown-linux-gnu/doc/* www/rustdoc

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
	if test "$GITHUB_ACTIONS" != "true"; then
		echo "fatal: not running on GitHub Actions"
		return 1
	fi

	cargo test --workspace --all-features --no-run --profile ci
	cargo test --workspace --all-features --profile ci

	www
}

"$@"
