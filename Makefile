SHA:=$(shell git rev-parse --short=8 --verify HEAD)

deploy:
	git stash --include-untracked
	git checkout development
	stack exec site clean
	stack exec site build
	git fetch --all
	git checkout -b master --track origin/master
	rsync -a --filter='P generated/'  \
		 --filter='P .gitignore'  \
		 --filter='P .git/'       \
		 --filter='P .stack-work' \
		 --delete-excluded        \
		 generated/out/ .
	git add -A
	git commit -m "Publishing ${SHA}"
	git push origin master:master
	git checkout development
	git branch -D master
	git stash pop

css:
	@sass --load-path=source/assets/stylesheets/ source/assets/stylesheets/main.scss

js:
	@scripts/cacophony -f source/assets/javascripts/main.js -d source/assets/javascripts/

post:
	@scripts/create post

page:
	@scripts/create page

watch:
	@stack exec site clean
	@stack exec site watch
