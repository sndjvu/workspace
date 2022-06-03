#!/bin/sh

set -o errexit -o nounset

git config user.name "Cole Miller"
git config user.email "m@cole-miller.net"

www() {
	if test "$GITHUB_ACTIONS" != "true"; then
		echo "fatal: not running on GitHub Actions"
		return 1
	fi
	
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

"$@"
