build: clean
	@go install .

remove:
	@rm -f ${HOME}/go/bin/tsuku

clean:
	@go clean