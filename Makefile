SHA:=$(shell git rev-parse --short=8 --verify HEAD)

deploy:
	git stash
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
	@sass --scss source/assets/stylesheets/main.scss

post:
	@scripts/create post

page:
	@scripts/create page
