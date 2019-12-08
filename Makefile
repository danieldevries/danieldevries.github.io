SHA:=$(shell git rev-parse --short=8 --verify HEAD)

css:
	@sass --scss source/assets/stylesheets/main.scss

post:
	@scripts/create post

page:
	@scripts/create page
