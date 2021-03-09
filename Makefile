watch-test:
	ghcid --command 'stack ghci --test --main-is weather-api:test:weather-api-test' --test 'main' --warnings

watch-devel:
	ghcid --command "stack ghci" --test "DevelMain.update"
