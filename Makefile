css:
	@sass --load-path=source/assets/stylesheets/ source/assets/stylesheets/main.scss

js:
	@scripts/cacophony -f source/assets/javascripts/main.js -d source/assets/javascripts/

post:
	@scripts/create post

page:
	@scripts/create page

build:
	@stack build

watch:
	@stack exec site clean
	@stack exec site watch

check:
	@stack exec site check
