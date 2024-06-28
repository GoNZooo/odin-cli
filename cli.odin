package cli

import "base:intrinsics"
import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:reflect"
import "core:slice"
import "core:strconv"
import "core:strings"
import "core:testing"

Struct_Cli_Info :: struct {
	type:   typeid,
	size:   int,
	name:   string,
	fields: []Field_Cli_Info,
}

struct_cli_info_destroy :: proc(cli_info: Struct_Cli_Info, allocator := context.allocator) {
	for field in cli_info.fields {
		field_cli_info_destroy(field, allocator)
	}

	delete(cli_info.name, allocator)
	delete(cli_info.fields, allocator)
}

Field_Cli_Info :: struct {
	name:           string,
	cli_short_name: string,
	cli_long_name:  string,
	type:           typeid,
	offset:         uintptr,
	required:       bool,
	size:           int,
}

field_cli_info_destroy :: proc(field: Field_Cli_Info, allocator := context.allocator) {
	delete(field.cli_short_name, allocator)
	delete(field.cli_long_name, allocator)
}

Union_Cli_Info :: struct {
	size:       int,
	tag_offset: uintptr,
	variants:   []Variant_Cli_Info,
	start_tag:  int,
}

union_cli_info_destroy :: proc(cli_info: Union_Cli_Info, allocator := context.allocator) {
	delete(cli_info.variants, allocator)
}

Variant_Cli_Info :: struct {
	payload: typeid,
}

Cli_Tag_Values :: struct {
	short:    string,
	long:     string,
	required: bool,
}

cli_tag_values_destroy :: proc(tag_values: Cli_Tag_Values, allocator := context.allocator) {
	delete(tag_values.short, allocator)
	delete(tag_values.long, allocator)
}

Test_Command :: union {
	Test_Struct,
}

Test_Command2 :: union {
	Test_Struct,
	Other_Struct,
}

Test_Command_No_Nil :: union #no_nil {
	Test_Struct,
	Other_Struct,
}

Other_Struct :: struct {
	field_one: string,
}

Test_Struct :: struct {
	field_one:   string `cli:"1,field-one"`,
	field_two:   int `cli:"2,field-two/required"`,
	field_three: bool `cli:"field-three/required"`,
	no_tag:      f32,
}

Test_Struct_Different_Order :: struct {
	field_three: bool `cli:"field-three/required"`,
	field_one:   string `cli:"1,field-one"`,
	no_tag:      f32,
	field_two:   int `cli:"2,field-two/required"`,
}

Test_Struct_With_Long_Short_Name :: struct {
	field_one: string `cli:"f1,field-one/required"`,
}

Cli_Parse_Error :: union {
	mem.Allocator_Error,
	Cli_Value_Parse_Error,
}

Cli_Value_Parse_Error :: struct {
	value:   string,
	type:    typeid,
	message: string,
}

parse_arguments_as_type :: proc(
	arguments: []string,
	$T: typeid,
	allocator := context.allocator,
) -> (
	value: T,
	remaining_arguments: []string,
	error: Cli_Parse_Error,
) {
	if len(arguments) == 0 {
		return value, remaining_arguments, Cli_Value_Parse_Error {
			value = "",
			type = T,
			message = "no arguments to read provided",
		}
	}

	if reflect.is_struct(type_info_of(T)) {
		cli_info := struct_decoding_info(T, allocator) or_return
		defer struct_cli_info_destroy(cli_info, allocator)
		if arguments[0] == "help" || arguments[0] == "-h" || arguments[0] == "--help" {
			print_help_for_struct_and_exit(cli_info)
		}
		bytes, remaining := parse_arguments_with_struct_cli_info(
			cli_info,
			arguments,
			allocator,
		) or_return
		defer delete(bytes, allocator)
		v := mem.reinterpret_copy(T, raw_data(bytes))

		return v, remaining, nil
	}

	if reflect.is_union(type_info_of(T)) {
		cli_info := union_decoding_info(T, allocator) or_return
		defer union_cli_info_destroy(cli_info, allocator)
		if arguments[0] == "help" || arguments[0] == "-h" || arguments[0] == "--help" {
			print_help_for_union_and_exit(cli_info)
		}

		bytes, remaining := parse_arguments_with_union_cli_info(
			cli_info,
			arguments,
			allocator,
		) or_return
		defer delete(bytes, allocator)
		v := mem.reinterpret_copy(T, raw_data(bytes))

		return v, remaining, nil
	}

	remaining_arguments = arguments[1:]

	when T == string {
		return arguments[0], remaining_arguments, nil
	} else when T == int {
		i, ok := strconv.parse_int(arguments[0], 10)
		if !ok {
			return 0, arguments, Cli_Value_Parse_Error {
				value = arguments[0],
				type = T,
				message = fmt.tprintf("invalid integer value: '%s'", arguments[0]),
			}
		}

		return i, remaining_arguments, nil
	} else when T == f32 {
		f, ok := strconv.parse_f32(arguments[0])
		if !ok {
			return 0, arguments, Cli_Value_Parse_Error {
				value = arguments[0],
				type = T,
				message = fmt.tprintf("invalid float value: '%s'", arguments[0]),
			}
		}

		return f, remaining_arguments, nil
	} else when T == f64 {
		f, ok := strconv.parse_f64(arguments[0])
		if !ok {
			return 0, arguments, Cli_Value_Parse_Error {
				value = arguments[0],
				type = T,
				message = fmt.tprintf("invalid float value: '%s'", arguments[0]),
			}
		}

		return f, remaining_arguments, nil
	} else when T == bool {
		if arguments[0] == "true" {
			return true, remaining_arguments, nil
		} else if arguments[0] == "false" {
			return false, remaining_arguments, nil
		} else {
			return false, arguments, Cli_Value_Parse_Error {
				value = arguments[0],
				type = T,
				message = fmt.tprintf("invalid boolean value: '%s'", arguments[0]),
			}
		}
	}

	return value, []string{}, nil
}

@(test)
test_parse_arguments_as_type :: proc(t: ^testing.T) {
	context.logger = log.create_console_logger()

	expected_arguments := []string{"bar"}
	s, remaining_arguments, error := parse_arguments_as_type(
		{"foo", "bar"},
		string,
		context.allocator,
	)
	testing.expect_value(t, error, nil)
	testing.expect_value(t, s, "foo")
	testing.expect(
		t,
		slice.equal(remaining_arguments, expected_arguments),
		fmt.tprintf("Expected remaining arguments to equal %v, got: %v", remaining_arguments),
	)

	i: int
	expected_arguments = []string{}
	i, remaining_arguments, error = parse_arguments_as_type({"123"}, int, context.allocator)
	testing.expect_value(t, error, nil)
	testing.expect_value(t, i, 123)
	testing.expect(
		t,
		slice.equal(remaining_arguments, expected_arguments),
		fmt.tprintf("Expected remaining arguments to equal %v, got: %v", remaining_arguments),
	)

	float32: f32
	expected_arguments = []string{"hello", "there"}
	float32, remaining_arguments, error = parse_arguments_as_type(
		{"123.456", "hello", "there"},
		f32,
		context.allocator,
	)
	testing.expect_value(t, error, nil)
	testing.expect_value(t, float32, 123.456)
	testing.expect(
		t,
		slice.equal(remaining_arguments, expected_arguments),
		fmt.tprintf("Expected remaining arguments to equal %v, got: %v", remaining_arguments),
	)

	float64: f64
	expected_arguments = []string{}
	float64, remaining_arguments, error = parse_arguments_as_type(
		{"123.456"},
		f64,
		context.allocator,
	)
	testing.expect_value(t, error, nil)
	testing.expect_value(t, float64, 123.456)
	testing.expect(
		t,
		slice.equal(remaining_arguments, expected_arguments),
		fmt.tprintf("Expected remaining arguments to equal %v, got: %v", remaining_arguments),
	)

	b: bool
	expected_arguments = []string{"true"}
	b, remaining_arguments, error = parse_arguments_as_type(
		{"true", "true"},
		bool,
		context.allocator,
	)
	testing.expect_value(t, error, nil)
	testing.expect_value(t, b, true)
	testing.expect(
		t,
		slice.equal(remaining_arguments, expected_arguments),
		fmt.tprintf("Expected remaining arguments to equal %v, got: %v", remaining_arguments),
	)

	ts: Test_Struct
	expected_arguments = []string{"rest", "of", "arguments"}
	ts, remaining_arguments, error = parse_arguments_as_type(
		{
			"-2",
			"123",
			"--field-one",
			"foo",
			"--no-tag",
			"123.456",
			"--field-three",
			"true",
			"rest",
			"of",
			"arguments",
		},
		Test_Struct,
		context.allocator,
	)
	testing.expect_value(t, error, nil)
	testing.expect_value(
		t,
		ts,
		Test_Struct{field_one = "foo", field_two = 123, field_three = true, no_tag = 123.456},
	)
	testing.expect(
		t,
		slice.equal(remaining_arguments, expected_arguments),
		fmt.tprintf("Expected remaining arguments to equal %v, got: %v", remaining_arguments),
	)

	tc: Test_Command
	expected_arguments = []string{"rest", "of", "arguments"}
	tc, remaining_arguments, error = parse_arguments_as_type(
		{
			"test-struct",
			"-2",
			"123",
			"--field-one",
			"foo",
			"--no-tag",
			"123.456",
			"--field-three",
			"true",
			"rest",
			"of",
			"arguments",
		},
		Test_Command,
		context.allocator,
	)
	testing.expect_value(t, error, nil)
	testing.expect_value(
		t,
		tc,
		Test_Struct{field_one = "foo", field_two = 123, field_three = true, no_tag = 123.456},
	)
	testing.expect(
		t,
		slice.equal(remaining_arguments, expected_arguments),
		fmt.tprintf("Expected remaining arguments to equal %v, got: %v", remaining_arguments),
	)
}

@(private = "file")
make_argument_map :: proc(
	arguments: []string,
	cli_info: Struct_Cli_Info,
	allocator := context.allocator,
) -> (
	result: map[string]string,
	remaining_arguments: []string,
	error: Cli_Parse_Error,
) {
	result = make(map[string]string, 0, allocator) or_return

	for i := 0; i < len(arguments); {
		if !strings.has_prefix(arguments[i], "-") {
			return result, arguments[i:], nil
		}

		argument := arguments[i]
		without_dash := strings.trim_left(argument, "-")
		field_type: typeid
		for field in cli_info.fields {
			if field.cli_short_name == without_dash || field.cli_long_name == without_dash {
				field_type = field.type
				break
			}
		}
		if i + 1 >= len(arguments) {
			if field_type == bool {
				result[without_dash] = "true"
				i += 1
				continue
			}

			return result, arguments[i:], Cli_Value_Parse_Error {
				value = argument,
				type = field_type,
				message = "Expecting value for argument",
			}
		}
		value := arguments[i + 1]
		result[without_dash] = value
		i += 2
	}

	return result, []string{}, nil
}

@(test)
test_make_argument_map :: proc(t: ^testing.T) {
	context.logger = log.create_console_logger()

	arguments := []string {
		"-2",
		"123",
		"--field-one",
		"foo",
		"--no-tag",
		"123.456",
		"--field-three",
		"true",
		"rest",
		"of",
		"arguments",
	}
	expected_remaining_arguments := []string{"rest", "of", "arguments"}
	result, remaining_arguments, error := make_argument_map(
		arguments,
		Struct_Cli_Info{},
		context.allocator,
	)
	testing.expect_value(t, error, nil)
	testing.expect_value(t, result["2"], "123")
	testing.expect_value(t, result["field-one"], "foo")
	testing.expect_value(t, result["no-tag"], "123.456")
	testing.expect_value(t, result["field-three"], "true")
	testing.expect(
		t,
		slice.equal(remaining_arguments, expected_remaining_arguments),
		fmt.tprintf("Expected remaining arguments to equal %v, got: %v", remaining_arguments),
	)

	arguments2 := []string {
		"-2",
		"123",
		"--field-one",
		"foo",
		"--no-tag",
		"123.456",
		"--field-three",
		"true",
		"--inverted",
	}
	result2, remaining_arguments2, error2 := make_argument_map(
		arguments2,
		Struct_Cli_Info {
			fields = []Field_Cli_Info {
				{name = "inverted", type = bool, cli_long_name = "inverted"},
			},
		},
		context.allocator,
	)
	testing.expect_value(t, error2, nil)
	testing.expect_value(t, result2["2"], "123")
	testing.expect_value(t, result2["field-one"], "foo")
	testing.expect_value(t, result2["no-tag"], "123.456")
	testing.expect_value(t, result2["field-three"], "true")
	testing.expect_value(t, result2["inverted"], "true")
	testing.expect(
		t,
		slice.equal(remaining_arguments2, []string{}),
		fmt.tprintf(
			"Expected remaining arguments to equal %v, got: %v",
			[]string{},
			remaining_arguments2,
		),
	)

	arguments3 := []string {
		"-2",
		"123",
		"--field-one",
		"foo",
		"--no-tag",
		"123.456",
		"--field-three",
		"true",
		"--count",
	}
	_, _, error3 := make_argument_map(
		arguments3,
		Struct_Cli_Info {
			fields = []Field_Cli_Info{{name = "count", type = int, cli_long_name = "count"}},
		},
		context.allocator,
	)
	testing.expect_value(
		t,
		error3,
		Cli_Value_Parse_Error {
			value = "--count",
			type = int,
			message = "Expecting value for argument",
		},
	)
}

@(private = "file")
parse_arguments_with_struct_cli_info :: proc(
	cli_info: Struct_Cli_Info,
	arguments: []string,
	allocator := context.allocator,
) -> (
	result: []byte,
	remaining_arguments: []string,
	error: Cli_Parse_Error,
) {
	value_bytes := make([]byte, cli_info.size, allocator)
	argument_map, remaining := make_argument_map(arguments, cli_info, context.allocator) or_return
	defer delete(argument_map)
	for field in cli_info.fields {
		map_value, has_value := argument_map[field.cli_long_name]
		if !has_value && field.cli_short_name != "" {
			map_value, has_value = argument_map[field.cli_short_name]
		}
		if !has_value && field.required {
			error = Cli_Value_Parse_Error {
				message = fmt.tprintf("missing required argument: '%s'", field.cli_long_name),
			}

			return []byte{}, arguments, error
		} else if !has_value {
			continue
		}
		if has_value && field.type == bool && map_value == "" {
			map_value = "true"
		}
		parsed_value := parse_argument_as_type(map_value, field.type, allocator) or_return
		defer delete(parsed_value, allocator)
		copy(value_bytes[field.offset:], parsed_value)
	}

	return value_bytes, remaining, nil
}

@(test)
test_parse_arguments_with_struct_cli_info :: proc(t: ^testing.T) {
	context.logger = log.create_console_logger()

	arguments := []string {
		"-2",
		"123",
		"--field-one",
		"foo",
		"--no-tag",
		"123.456",
		"--field-three",
		"true",
		"rest",
		"of",
		"arguments",
	}
	expected_remaining_arguments := []string{"rest", "of", "arguments"}
	ts_cli_info, cli_info_error := struct_decoding_info(Test_Struct, context.allocator)
	testing.expect_value(t, cli_info_error, nil)
	ts_bytes, remaining_arguments, error := parse_arguments_with_struct_cli_info(
		ts_cli_info,
		arguments,
		context.allocator,
	)
	testing.expect_value(t, error, nil)
	ts := mem.reinterpret_copy(Test_Struct, raw_data(ts_bytes))
	testing.expect_value(
		t,
		ts,
		Test_Struct{field_one = "foo", field_two = 123, field_three = true, no_tag = 123.456},
	)
	testing.expect(
		t,
		slice.equal(remaining_arguments, expected_remaining_arguments),
		fmt.tprintf("Expected remaining arguments to equal %v, got: %v", remaining_arguments),
	)

	ts_cli_info2, cli_info_error2 := struct_decoding_info(
		Test_Struct_Different_Order,
		context.allocator,
	)
	testing.expect_value(t, cli_info_error2, nil)
	ts_bytes2, remaining_arguments2, error2 := parse_arguments_with_struct_cli_info(
		ts_cli_info2,
		arguments,
		context.allocator,
	)
	testing.expect_value(t, error2, nil)
	ts2 := mem.reinterpret_copy(Test_Struct_Different_Order, raw_data(ts_bytes2))
	testing.expect_value(
		t,
		ts2,
		Test_Struct_Different_Order {
			field_one = "foo",
			field_two = 123,
			field_three = true,
			no_tag = 123.456,
		},
	)
	testing.expect(
		t,
		slice.equal(remaining_arguments2, expected_remaining_arguments),
		fmt.tprintf("Expected remaining arguments to equal %v, got: %v", remaining_arguments2),
	)

	arguments = []string{"-f1", "foo", "--field-2", "123", "--field-3", "true", "rest"}
	expected_remaining_arguments = []string{"rest"}
	ts_cli_info3, cli_info_error3 := struct_decoding_info(
		Test_Struct_With_Long_Short_Name,
		context.allocator,
	)
	testing.expect_value(t, cli_info_error3, nil)
	ts_bytes3, remaining_arguments3, error3 := parse_arguments_with_struct_cli_info(
		ts_cli_info3,
		arguments,
		context.allocator,
	)
	testing.expect_value(t, error3, nil)
	ts3 := mem.reinterpret_copy(Test_Struct_With_Long_Short_Name, raw_data(ts_bytes3))
	testing.expect_value(t, ts3, Test_Struct_With_Long_Short_Name{field_one = "foo"})
	testing.expect(
		t,
		slice.equal(remaining_arguments3, expected_remaining_arguments),
		fmt.tprintf(
			"Expected remaining arguments to equal %v, got: %v",
			expected_remaining_arguments,
			remaining_arguments3,
		),
	)
}

@(private = "file")
parse_arguments_with_union_cli_info :: proc(
	cli_info: Union_Cli_Info,
	arguments: []string,
	allocator := context.allocator,
) -> (
	result: []byte,
	remaining_arguments: []string,
	error: Cli_Parse_Error,
) {
	value_bytes := make([]byte, cli_info.size, allocator) or_return

	for variant, i in cli_info.variants {
		variant_cli_info := struct_decoding_info(variant.payload, allocator) or_return
		// defer delete(variant_cli_info.name, allocator)
		// defer delete(variant_cli_info.fields, allocator)
		defer struct_cli_info_destroy(variant_cli_info, allocator)
		if arguments[0] == variant_cli_info.name {
			variant_tag := i + cli_info.start_tag
			copy(value_bytes[cli_info.tag_offset:], mem.any_to_bytes(variant_tag))

			payload_bytes, remaining := parse_arguments_with_struct_cli_info(
				variant_cli_info,
				arguments[1:],
				allocator,
			) or_return
			defer delete(payload_bytes, allocator)
			copy(value_bytes[0:], payload_bytes)

			return value_bytes, remaining, nil
		}
	}

	return result, arguments, Cli_Value_Parse_Error {
		message = fmt.tprintf("Unable to parse any variants from union '%v'", cli_info),
	}
}

@(private = "file")
parse_argument_as_type :: proc(
	argument: string,
	t: typeid,
	allocator := context.allocator,
) -> (
	result: []byte,
	error: Cli_Parse_Error,
) {
	if t == string {
		v := mem.any_to_bytes(argument)
		cloned := slice.clone(v, allocator) or_return

		return cloned, nil
	} else if t == int {
		i, ok := strconv.parse_int(argument, 10)
		if !ok {
			error = Cli_Value_Parse_Error {
				message = fmt.tprintf("invalid integer: '%s'", argument),
			}

			return result, error
		}
		v := mem.any_to_bytes(i)
		cloned := slice.clone(v, allocator) or_return

		return cloned, nil
	} else if t == f32 {
		f, ok := strconv.parse_f32(argument)
		if !ok {
			error = Cli_Value_Parse_Error {
				message = fmt.tprintf("invalid float: '%s'", argument),
			}

			return result, error
		}
		v := mem.any_to_bytes(f)
		cloned := slice.clone(v, allocator) or_return

		return cloned, nil
	} else if t == f64 {
		f, ok := strconv.parse_f64(argument)
		if !ok {
			error = Cli_Value_Parse_Error {
				message = fmt.tprintf("invalid float: '%s'", argument),
			}

			return result, error
		}
		v := mem.any_to_bytes(f)
		cloned := slice.clone(v, allocator) or_return

		return cloned, nil
	} else if t == bool {
		v: []byte
		if argument == "true" {
			v = mem.any_to_bytes(true)
		} else if argument == "false" {
			v = mem.any_to_bytes(false)
		} else {
			error = Cli_Value_Parse_Error {
				message = fmt.tprintf("invalid boolean: '%s'", argument),
			}

			return result, error
		}
		cloned := slice.clone(v, allocator) or_return

		return cloned, nil
	} else {
		error = Cli_Value_Parse_Error {
			message = fmt.tprintf("unsupported type: %v", t),
		}

		return result, error
	}
}

@(test)
test_parse_argument_as_type :: proc(t: ^testing.T) {
	context.logger = log.create_console_logger()

	tid: typeid

	tid = string
	bytes, error := parse_argument_as_type("foo", tid, context.allocator)
	s := mem.reinterpret_copy(string, raw_data(bytes))
	testing.expect_value(t, error, nil)
	testing.expect(t, s == "foo", fmt.tprintf("Expected 'foo', got '%s'", s))

	tid = int
	bytes, error = parse_argument_as_type("123", tid, context.allocator)
	i := mem.reinterpret_copy(int, raw_data(bytes))
	testing.expect_value(t, error, nil)
	testing.expect(t, i == 123, fmt.tprintf("Expected 123, got %d", i))

	tid = f32
	bytes, error = parse_argument_as_type("123.456", tid, context.allocator)
	float32 := mem.reinterpret_copy(f32, raw_data(bytes))
	testing.expect_value(t, error, nil)
	testing.expect(t, float32 == 123.456, fmt.tprintf("Expected 123.456, got %f", float32))

	tid = f64
	bytes, error = parse_argument_as_type("123.456", tid, context.allocator)
	float64 := mem.reinterpret_copy(f64, raw_data(bytes))
	testing.expect_value(t, error, nil)
	testing.expect(t, float64 == 123.456, fmt.tprintf("Expected 123.456, got %f", float64))

	tid = bool
	bytes, error = parse_argument_as_type("true", tid, context.allocator)
	boolean := mem.reinterpret_copy(bool, raw_data(bytes))
	testing.expect_value(t, error, nil)
	testing.expect(
		t,
		boolean,
		fmt.tprintf("Expected true, got %v (%v)", boolean, typeid_of(type_of(boolean))),
	)
}

struct_decoding_info :: proc(
	type: typeid,
	allocator := context.allocator,
) -> (
	cli_info: Struct_Cli_Info,
	error: mem.Allocator_Error,
) {
	type_info := type_info_of(type)
	cli_info.size = type_info.size
	cli_info.type = type
	cli_info.name = union_variant_name(fmt.tprintf("%v", type), allocator)
	struct_fields := reflect.struct_fields_zipped(type)
	cli_info.fields = make([]Field_Cli_Info, len(struct_fields), allocator) or_return

	for f, i in struct_fields {
		tag := reflect.struct_tag_get(f.tag, "cli")
		tag_values := cli_tag_values(f.name, reflect.Struct_Tag(tag), allocator) or_return
		field_type_info := type_info_of(f.type.id)
		cli_info.fields[i].name = f.name
		cli_info.fields[i].type = f.type.id
		cli_info.fields[i].cli_short_name = tag_values.short
		cli_info.fields[i].cli_long_name = tag_values.long
		cli_info.fields[i].offset = f.offset
		cli_info.fields[i].required = tag_values.required
		cli_info.fields[i].size = field_type_info.size
	}

	return cli_info, nil
}

@(test)
test_struct_decoding_info :: proc(t: ^testing.T) {
	context.logger = log.create_console_logger()
	_tracking_allocator: mem.Tracking_Allocator
	mem.tracking_allocator_init(&_tracking_allocator, context.allocator)
	tracking_allocator := mem.tracking_allocator(&_tracking_allocator)

	cli_info, allocator_error := struct_decoding_info(Test_Struct, tracking_allocator)
	if allocator_error != nil {
		fmt.panicf("Allocator error: %s", allocator_error)
	}
	fields := []Field_Cli_Info {
		{
			name = "field_one",
			type = string,
			cli_short_name = "1",
			cli_long_name = "field-one",
			offset = 0,
			required = false,
			size = 16,
		},
		{
			name = "field_two",
			type = int,
			cli_short_name = "2",
			cli_long_name = "field-two",
			offset = 16,
			required = true,
			size = 8,
		},
		{
			name = "field_three",
			type = bool,
			cli_short_name = "",
			cli_long_name = "field-three",
			offset = 24,
			required = true,
			size = 1,
		},
		{
			name = "no_tag",
			type = f32,
			cli_short_name = "",
			cli_long_name = "no-tag",
			offset = 28,
			required = false,
			size = 4,
		},
	}
	testing.expect_value(t, cli_info.type, Test_Struct)
	testing.expect_value(t, cli_info.name, "test-struct")
	for f, i in fields {
		testing.expect(
			t,
			cli_info.fields[i] == f,
			fmt.tprintf("Expected %v, got %v", f, cli_info.fields[i]),
		)
	}

	struct_cli_info_destroy(cli_info, tracking_allocator)
	expect_no_leaks(t, _tracking_allocator)

	// Test for long short name
	cli_info, allocator_error = struct_decoding_info(
		Test_Struct_With_Long_Short_Name,
		tracking_allocator,
	)
	if allocator_error != nil {
		fmt.panicf("Allocator error: %s", allocator_error)
	}
	fields = []Field_Cli_Info {
		{
			name = "field_one",
			type = string,
			cli_short_name = "f1",
			cli_long_name = "field-one",
			offset = 0,
			required = true,
			size = 16,
		},
	}
	for f, i in fields {
		testing.expect(
			t,
			cli_info.fields[i] == f,
			fmt.tprintf("Expected %v, got %v", f, cli_info.fields[i]),
		)
	}

	struct_cli_info_destroy(cli_info, tracking_allocator)
	expect_no_leaks(t, _tracking_allocator)
}

@(private = "file")
field_name_to_long_name :: proc(name: string, allocator := context.allocator) -> string {
	return strings.to_kebab_case(name, allocator)
}

@(private = "file")
union_variant_name :: proc(name: string, allocator := context.allocator) -> string {
	return strings.to_kebab_case(name, allocator)
}

@(private = "file")
union_decoding_info :: proc(
	type: typeid,
	allocator := context.allocator,
) -> (
	cli_info: Union_Cli_Info,
	error: mem.Allocator_Error,
) {
	type_info := type_info_of(type)
	cli_info.size = type_info.size
	named, named_ok := type_info.variant.(reflect.Type_Info_Named)
	assert(named_ok, fmt.tprintf("Expected named type, got %v", type_info.variant))
	union_info, union_ok := named.base.variant.(reflect.Type_Info_Union)
	assert(union_ok, fmt.tprintf("Expected union type, got %v", named.base.variant))
	cli_info.tag_offset = union_info.tag_offset
	cli_info.start_tag = 0 if union_info.no_nil else 1

	variant_count := len(union_info.variants)
	variants := make([]Variant_Cli_Info, variant_count, allocator) or_return
	defer if error != nil {
		delete(variants, allocator)
	}
	for variant, i in union_info.variants {
		variants[i] = Variant_Cli_Info {
			payload = variant.id,
		}
	}

	cli_info.variants = variants

	return cli_info, nil
}

@(test)
test_union_decoding_info :: proc(t: ^testing.T) {
	context.logger = log.create_console_logger()
	_tracking_allocator: mem.Tracking_Allocator
	mem.tracking_allocator_init(&_tracking_allocator, context.allocator)
	tracking_allocator := mem.tracking_allocator(&_tracking_allocator)

	cli_info, decoding_info_error := union_decoding_info(Test_Command, tracking_allocator)
	testing.expect_value(t, decoding_info_error, nil)
	testing.expect_value(t, cli_info.size, 40)
	testing.expect_value(t, cli_info.tag_offset, 32)
	testing.expect_value(t, cli_info.start_tag, 1)
	testing.expect_value(t, len(cli_info.variants), 1)
	if len(cli_info.variants) == 1 {
		testing.expect_value(t, cli_info.variants[0].payload, Test_Struct)
	}
	delete(cli_info.variants, tracking_allocator)

	expect_no_leaks(t, _tracking_allocator)

	cli_info2, decoding_info_error2 := union_decoding_info(Test_Command_No_Nil)
	testing.expect_value(t, decoding_info_error2, nil)
	testing.expect_value(t, cli_info2.size, 40)
	testing.expect_value(t, cli_info2.tag_offset, 32)
	testing.expect_value(t, cli_info2.start_tag, 0)
	testing.expect_value(t, len(cli_info2.variants), 2)
	if len(cli_info2.variants) == 2 {
		testing.expect_value(t, cli_info2.variants[0].payload, Test_Struct)
		testing.expect_value(t, cli_info2.variants[1].payload, Other_Struct)
	}
}

@(test)
test_field_name_to_long_name :: proc(t: ^testing.T) {
	testing.expect_value(t, field_name_to_long_name("foo"), "foo")
	testing.expect_value(t, field_name_to_long_name("foo_bar"), "foo-bar")
	testing.expect_value(t, field_name_to_long_name("foo_bar_baz"), "foo-bar-baz")
}

@(private = "file")
cli_tag_values :: proc(
	field_name: string,
	tag: reflect.Struct_Tag,
	allocator := context.allocator,
) -> (
	cli_tag_values: Cli_Tag_Values,
	error: mem.Allocator_Error,
) {
	tag_value := string(tag)
	if tag_value == "" {
		long_name := field_name_to_long_name(field_name, allocator)

		return Cli_Tag_Values{long = long_name}, nil
	}

	keywords: []string
	defer delete(keywords, allocator)

	keyword_split := strings.split(tag_value, "/", allocator)
	defer delete(keyword_split, allocator)

	tag_value = keyword_split[0]
	if len(keyword_split) == 2 {
		keywords = strings.split(keyword_split[1], ",", allocator)
		tag_value = keyword_split[0]
	}

	values := strings.split(tag_value, ",", allocator)
	defer delete(values, allocator)

	required := len(keywords) > 0 && keywords[0] == "required"
	switch len(values) {
	case 1:
		long := strings.clone(values[0], allocator) or_return

		return Cli_Tag_Values{long = long, required = required}, nil
	case 2:
		short := strings.clone(values[0], allocator) or_return
		long := strings.clone(values[1], allocator) or_return

		return Cli_Tag_Values{short = short, long = long, required = required}, nil
	case:
		fmt.panicf("invalid `cli` tag format: '%s', should be `n,name`", tag)
	}
}

@(private = "package")
write_help_text_for_struct :: proc(
	b: ^strings.Builder,
	cli_info: Struct_Cli_Info,
	allocator := context.allocator,
) -> mem.Allocator_Error {
	strings.write_string(b, cli_info.name)
	strings.write_byte(b, ' ')

	for f, i in cli_info.fields {
		write_help_text_for_field(b, f)
		if i < len(cli_info.fields) - 1 {
			strings.write_byte(b, ' ')
		}
	}

	return nil
}

@(test)
test_write_help_text_for_struct :: proc(t: ^testing.T) {
	context.logger = log.create_console_logger()

	cli_info, decoding_info_error := struct_decoding_info(Test_Struct)
	testing.expect_value(t, decoding_info_error, nil)

	b, builder_init_error := strings.builder_make_none()
	testing.expect_value(t, builder_init_error, nil)
	write_help_text_for_struct(&b, cli_info)
	help_text := strings.to_string(b)

	expected_help_text := "test-struct [-1|--field-one string] -2|--field-two int --field-three bool [--no-tag f32]"
	testing.expect(
		t,
		help_text == expected_help_text,
		fmt.tprintf("Expected help text to equal '%s', got '%s'", expected_help_text, help_text),
	)
	if help_text != expected_help_text {
		for i := 0; i < len(help_text) && i < len(expected_help_text); i += 1 {
			c := help_text[i]
			if c != expected_help_text[i] {
				fmt.printf("Difference at index %d: '%c' != '%c'\n", i, c, expected_help_text[i])
				break
			}
		}
	}
}

@(private = "file")
write_help_text_for_union :: proc(
	b: ^strings.Builder,
	cli_info: Union_Cli_Info,
	allocator := context.allocator,
) -> mem.Allocator_Error {
	for v, i in cli_info.variants {
		struct_cli_info, cli_info_error := struct_decoding_info(v.payload, allocator)
		if cli_info_error != nil {
			fmt.panicf("Error getting cli info for variant %s: %s", v.payload, cli_info_error)
		}
		write_help_text_for_struct(b, struct_cli_info, allocator)
		if i < len(cli_info.variants) - 1 {
			strings.write_byte(b, '\n')
		}
	}

	return nil
}

@(test)
test_write_help_text_for_union :: proc(t: ^testing.T) {
	context.logger = log.create_console_logger()

	cli_info, decoding_info_error := union_decoding_info(Test_Command2)
	testing.expect_value(t, decoding_info_error, nil)
	b, builder_init_error := strings.builder_make_none()
	testing.expect_value(t, builder_init_error, nil)
	write_help_text_for_union(&b, cli_info)

	help_text := strings.to_string(b)
	expected_help_text :=
		"test-struct [-1|--field-one string] -2|--field-two int --field-three bool [--no-tag f32]\n" +
		"other-struct [--field-one string]"
	testing.expect(
		t,
		help_text == expected_help_text,
		fmt.tprintf("Expected help text to equal '%s', got '%s'", expected_help_text, help_text),
	)
}

print_help_for_union_type_and_exit :: proc(
	type: typeid,
	allocator := context.allocator,
) -> mem.Allocator_Error {
	cli_info := union_decoding_info(type, allocator) or_return
	print_help_for_union_and_exit(cli_info, allocator) or_return

	return nil
}

print_help_for_struct_type_and_exit :: proc(
	type: typeid,
	allocator := context.allocator,
) -> mem.Allocator_Error {
	cli_info := struct_decoding_info(type, allocator) or_return
	print_help_for_struct_and_exit(cli_info, allocator) or_return

	return nil
}

@(private = "file")
write_help_text_for_field :: proc(b: ^strings.Builder, field: Field_Cli_Info) {
	if !field.required {
		strings.write_byte(b, '[')
	}
	if field.cli_short_name != "" {
		strings.write_string(b, "-")
		strings.write_string(b, field.cli_short_name)
		strings.write_string(b, "|")
	}
	strings.write_string(b, "--")
	strings.write_string(b, field.cli_long_name)
	strings.write_string(b, " ")
	reflect.write_typeid_builder(b, field.type)
	if !field.required {
		strings.write_byte(b, ']')
	}
}

@(private = "file")
print_help_for_union_and_exit :: proc(
	cli_info: Union_Cli_Info,
	allocator := context.allocator,
) -> mem.Allocator_Error {
	b := strings.builder_make_none(allocator) or_return
	write_help_text_for_union(&b, cli_info, allocator) or_return
	strings.write_byte(&b, '\n')
	text := strings.to_string(b)
	fmt.print(text)

	os.exit(0)
}

@(private = "file")
print_help_for_struct_and_exit :: proc(
	cli_info: Struct_Cli_Info,
	allocator := context.allocator,
) -> mem.Allocator_Error {
	b := strings.builder_make_none(allocator) or_return
	write_help_text_for_struct(&b, cli_info, allocator) or_return
	strings.write_byte(&b, '\n')
	text := strings.to_string(b)
	fmt.print(text)

	os.exit(0)
}

@(test)
test_memory_leaks_1 :: proc(t: ^testing.T) {
	_tracking_allocator: mem.Tracking_Allocator
	mem.tracking_allocator_init(&_tracking_allocator, context.allocator)
	tracking_allocator := mem.tracking_allocator(&_tracking_allocator)

	context.logger = log.create_console_logger()
	context.allocator = tracking_allocator

	Command :: union {
		Encode,
		Decode,
	}

	Encode :: struct {
		N:     int `cli:"N,num-code"`,
		K:     int `cli:"K,num-data"`,
		w:     int `cli:"w,word-size"`,
		input: string `cli:"i,input/required"`,
		shard: string `cli:"s,shard/required"`,
	}

	Decode :: struct {
		N:      int `cli:"N,num-code"`,
		K:      int `cli:"K,num-data"`,
		w:      int `cli:"w,word-size"`,
		output: string `cli:"o,output/required"`,
		shard:  string `cli:"s,shard/required"`,
	}

	arguments := []string{"decode", "--output", "blah.txt", "--shard", "encoded", "-N", "5"}
	command, _, error := parse_arguments_as_type(arguments, Command, tracking_allocator)
	testing.expect_value(t, error, nil)

	testing.expect_value(
		t,
		command,
		Decode{N = 5, K = 0, w = 0, output = "blah.txt", shard = "encoded"},
	)
	expect_no_leaks(t, _tracking_allocator)
}

@(test)
test_edyu1 :: proc(t: ^testing.T) {
	context.logger = log.create_console_logger()

	Command :: union {
		Encode,
		Decode,
	}

	Encode :: struct {
		N:     int `cli:"N,num-code"`,
		K:     int `cli:"K,num-data"`,
		w:     int `cli:"w,word-size"`,
		input: string `cli:"i,input/required"`,
		shard: string `cli:"s,shard/required"`,
	}

	Decode :: struct {
		N:      int `cli:"N,num-code"`,
		K:      int `cli:"K,num-data"`,
		w:      int `cli:"w,word-size"`,
		output: string `cli:"o,output/required"`,
		shard:  string `cli:"s,shard/required"`,
	}

	arguments := []string{"decode", "--output", "blah.txt", "--shard", "encoded", "-N", "5"}
	command, remaining, error := parse_arguments_as_type(arguments, Command, context.allocator)
	testing.expect_value(t, error, nil)

	testing.expect_value(
		t,
		command,
		Decode{N = 5, K = 0, w = 0, output = "blah.txt", shard = "encoded"},
	)
	testing.expect_value(t, len(remaining), 0)
}

expect_no_leaks :: proc(t: ^testing.T, allocator: mem.Tracking_Allocator) -> bool {
	if len(allocator.allocation_map) != 0 {
		fmt.printf("Expected no leaks, got %d\n", len(allocator.allocation_map))
		for _, v in allocator.allocation_map {
			fmt.printf("\t%s: %d\n", v.location, v.size)
		}
		testing.expect_value(t, len(allocator.allocation_map), 0)

		return false
	}

	return true
}
