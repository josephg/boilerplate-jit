
all:
	coffee -bc *.coffee

watch:
	watchify -t coffeeify --extension=".coffee" fuzzer.coffee -o bundle.js -v

