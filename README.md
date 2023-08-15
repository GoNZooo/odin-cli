# odin-cli

A library for using types to specify how command line arguments should be parsed.

## Usage

```odin
// This is a union specifying a command, i.e. `unthread analyze-lock-file ...`
Command :: union {
	AnalyzeLockFile,
}

// The arguments that `analyze-lock-file` expects are specified in the payload
// and should be a struct.
AnalyzeLockFile :: struct {
	// We can optionally specify short and long names, as well as say that an
	// argument is required. If we don't specify anything only the long name
	// will be generated and the argument will be optional (zero-initialized).
	filename: string `cli:"f,filename/required"`,
}

main :: proc() {
	arguments := os.args
	if len(arguments) < 2 {
		fmt.println("Usage: unthread <command> arguments...")
		os.exit(1)
	}

	// When we specify here that `Command` is the type that we want to parse
	// `cli` will look at the possible union variants it has and try to parse
	// them as kebab-case as well as their payloads.
	command, cli_error := cli.parse_arguments_as_type(arguments[1:], Command)
	if cli_error != nil {
		fmt.println("Failed to parse arguments: ", cli_error)
		os.exit(1)
	}

	// Down here we know that we've successfully parsed an argument, so we can
	// run the corresponding logic.
	switch c in command {
	case AnalyzeLockFile:
		run_analyze_lock_file(c)
	}
}
```
