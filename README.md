# typescript-to-gdscript

Convert TypeScript type definitions into concrete models in GDScript for Godot 4.4

This utility enables a pipeline where TypeScript files containing interfaces can be transformed into gdscript classes.

The classes can then ingest data sent across http or WebSocket requests with some minimal validation against the TypeScript interface.

This enables strongly typed communication between TypeScript and the Godot game engine.

## Usage

Usage:

```
typescript-to-gdscript [--debug-print] [--debug-trace] templatefile.gd.tmpl outputdir input1.ts [input2.ts...]
```

Reads all interfaces from input.ts files exports them to `outputdir/[InterfaceName].gd` files.

* Uses templatefile.gd.tmpl as the template.
* When `--debug-print` is provided the program will emit extra debug information on stderr.
* When `--debug-trace` is provided the program will emit even more debug information (including previous stack trace branches )

## Output

The example template outputs a gdscript *model* class that extends `Reference` and has a constructor that accepts an optional `Dictionary` object, as well as an `update` method which allows the model to be updated in place.

* Properties of the incoming interface are mapped to additional *model* class files and imported automatically with `preload`.
* Optional properties will be skipped (not updated in the model) when they are missing (undefined values in JSON are skipped during stringification)
* Nullable properties will be assigned to `null` if the incoming value is null.

### Class Documentation

#### `GeneratedClass.new(src: Dictionary = {}) -> GeneratedClass`:

Create a new instance of the generated class. All properties from the TypeScript interface will be
loaded out of the src `Dictionary` and properties of other `GeneratedClass` types will be instantiated.

See `update()` for more details.

#### `func update(src: Dictionary) -> void`:

Pass an incoming json object to the update method or constructor to initialize the instance.
* Optional properties will remain unset. You can check their status by calling `.is_set(property_name)`
* Nullable properties may remain unset if the property type is a gdscript builtin type. Call `is_null(property_name)` to check if the property is null.
* Check `.is_initialized()` to see if the `.update()` method has been called yet with a non-empty object.

#### `func for_json() -> Dictionary`:

Call this method to convert the object back into a `Dictionary` so it can be serialized to JSON data.

#### `func is_set(property_name: String) -> bool`:

Check to see if an optional property was set.

#### `func unset(property_name: String) -> void`:

Unset an optional property.

#### `func is_null(property_name: String) -> bool`:

Check to see if a nullable property was null.
* This method is necessary because gdscript builtin types cannot be null, and there is no `Union` type in gdscript.

#### `func set_null(property_name: String) -> void`:

Set a property to null.
* If the `typeof()` the property is `TYPE_OBJECT` or `TYPE_NIL` then that property will also be set to `null`.

#### `func is_partial_deep() -> bool`:

Returns `true` if the current instance has been flagged as `partial_deep` meaning
* All properties are optional
* All child property instances should also be created with the `partial_deep` flag

#### `func is_initialized() -> bool`:

Returns true when `update()` has been called with a non-empty `Dictionary`

#### `func keys() -> Array`:

Returns the set of keys where `is_set(key)` would return true

## Directives

Comments can contain directives to help out with conversion

* `@typescript-to-gdscript-type: int|float|String`: Forces the type of a property.
* `@typescript-to-gdscript-skip`: This type will not be imported, and will be completely ignored by this program.
* `@typescript-to-gdscript-gd-impl`: This type will be imported, but the gdscript file will not be generated.
  * This is useful for interface union types that would generate one type or another based on the kind property for example.
  * See [any-kind.ts](./test-fixtures/any-kind.ts) for an example.
  * The default template will import these types from the parent directory ("../")

## Supported TypeScript Builtin and Utility Types

* `Omit<T, k...>`: Keys specified by k... will be omitted. (This only works with the extends keyword)
* `Readonly<T>`: Readonly is discarded and T is used. (This only works with the extends keyword)
* `Date`: Must supply an `Iso8601Date` class to handle the ISO8601 timestamp string from the json value.
* `Array<T>`: Treated the same as `T[]`
* `Record<K, T>`: Treated as a Dictionary of `<string, T>` since that's what JSON will supply.
* `Array<Record<K,T>>`, `Record<K,Array<T>>` and other nested combinations are supported.

## Templates

Templates use the [tinytemplate syntax](https://docs.rs/tinytemplate/latest/tinytemplate/syntax/index.html). See the [example template](./gdscript-model.gd.tmpl).

### Example Output

```Python
extends Reference
# Model for TestInterface typescript interface in "test-fixtures/test-interface.ts"

# Generated by typescript-to-gdscript. Do not edit by hand!
# You can extend this in another class to override behaviors

const AnyKind = preload("../AnyKind.gd")
const Iso8601Date = preload("../Iso8601Date.gd")
const ImportedInterface = preload("./ImportedInterface.gd")
const ImportedPartialTypeRef = preload("./ImportedPartialTypeRef.gd")
const PartialTypeRef = preload("./PartialTypeRef.gd")

# Tracks the null/optional status of builtin properties that are not nullable in gdscript
var __assigned_properties = {}
# Check is_initialized() to detect if this object contains data.
var __initialized = false
var __partial_deep := false

var id: float
var str_key: String
var float_key: float
var bool_key: bool
# optional
var optional_date: Iso8601Date setget __set_optional_date
# optional Iso8601Date | null
var nullable_optional_date: Iso8601Date setget __set_nullable_optional_date
var date: Iso8601Date
# Literally "abcd"
var str_lit: String
# Literally 1
var int_lit: int
# Literally 1.0
var float_lit: float
# Literally "training" | "full"
var str_union: String
var intf_union: AnyKind
# Literally true
var true_lit: bool
var partial_type_ref: PartialTypeRef
var imported: ImportedInterface
var imported_partial_type_ref: ImportedPartialTypeRef
# TestInterface, Record<string, TestInterface>
var record_object: Dictionary
# ImportedInterface[]
var array: Array

func _init(src: Dictionary = {}, partial_deep = false) -> void:
	__partial_deep = __partial_deep || partial_deep
	if src:
		update(src)

func __set_optional_date(value: Iso8601Date):
	__assigned_properties.optional_date = true if value != null else null
	optional_date = value

func __set_nullable_optional_date(value: Iso8601Date):
	__assigned_properties.nullable_optional_date = true if value != null else null
	nullable_optional_date = value


func update(src: Dictionary) -> void:
	# custom import logic can be added by overriding this function
	__initialized = true
	if !__partial_deep || "id" in src:
		__assigned_properties.id = true
		id = src.id
	if !__partial_deep || "strKey" in src:
		__assigned_properties.str_key = true
		str_key = src.strKey
	if !__partial_deep || "floatKey" in src:
		__assigned_properties.float_key = true
		float_key = src.floatKey
	if !__partial_deep || "boolKey" in src:
		__assigned_properties.bool_key = true
		bool_key = src.boolKey
	if "optionalDate" in src:
		__assigned_properties.optional_date = true
		optional_date = Iso8601Date.new(src.optionalDate, __partial_deep)
	if "nullableOptionalDate" in src:
		__assigned_properties.nullable_optional_date = true if src.nullableOptionalDate != null else null
		nullable_optional_date = Iso8601Date.new(src.nullableOptionalDate, __partial_deep) if src.nullableOptionalDate != null else null
	if !__partial_deep || "date" in src:
		__assigned_properties.date = true
		date = Iso8601Date.new(src.date, __partial_deep)
	if !__partial_deep || "strLit" in src:
		__assigned_properties.str_lit = true
		str_lit = src.strLit
	if !__partial_deep || "intLit" in src:
		__assigned_properties.int_lit = true
		int_lit = src.intLit
	if !__partial_deep || "floatLit" in src:
		__assigned_properties.float_lit = true
		float_lit = src.floatLit
	if !__partial_deep || "strUnion" in src:
		__assigned_properties.str_union = true
		str_union = src.strUnion
	if !__partial_deep || "intfUnion" in src:
		__assigned_properties.intf_union = true
		intf_union = AnyKind.new(src.intfUnion, __partial_deep)
	if !__partial_deep || "trueLit" in src:
		__assigned_properties.true_lit = true
		true_lit = src.trueLit
	if !__partial_deep || "partialTypeRef" in src:
		__assigned_properties.partial_type_ref = true
		partial_type_ref = PartialTypeRef.new(src.partialTypeRef, __partial_deep)
	if !__partial_deep || "imported" in src:
		__assigned_properties.imported = true
		imported = ImportedInterface.new(src.imported, __partial_deep)
	if !__partial_deep || "importedPartialTypeRef" in src:
		__assigned_properties.imported_partial_type_ref = true
		imported_partial_type_ref = ImportedPartialTypeRef.new(src.importedPartialTypeRef, __partial_deep)
	if !__partial_deep || "recordObject" in src:
		__assigned_properties.record_object = true
		record_object = {}
		for __key__ in src.recordObject:
			var __value__ = src.recordObject[__key__]
			record_object[__key__] = TestInterface.new(__value__, __partial_deep)
	if !__partial_deep || "array" in src:
		__assigned_properties.array = true
		array = []
		for __item__ in src.array:
			var __value__ = ImportedInterface.new(__item__, __partial_deep)
			array.append(__value__)


func for_json() -> Dictionary:
	# custom logic to serialize to dict/array/primitive for json
	var result = {}
	if !__initialized:
		return result
	if !__partial_deep || is_set("id"):
		result.id = id
	if !__partial_deep || is_set("str_key"):
		result.strKey = str_key
	if !__partial_deep || is_set("float_key"):
		result.floatKey = float_key
	if !__partial_deep || is_set("bool_key"):
		result.boolKey = bool_key
	if is_set("optional_date"):
		result.optionalDate = optional_date.for_json()
	if is_set("nullable_optional_date"):
		result.nullableOptionalDate = nullable_optional_date.for_json() if nullable_optional_date != null else null
	if !__partial_deep || is_set("date"):
		result.date = date.for_json()
	if !__partial_deep || is_set("str_lit"):
		result.strLit = str_lit
	if !__partial_deep || is_set("int_lit"):
		result.intLit = int_lit
	if !__partial_deep || is_set("float_lit"):
		result.floatLit = float_lit
	if !__partial_deep || is_set("str_union"):
		result.strUnion = str_union
	if !__partial_deep || is_set("intf_union"):
		result.intfUnion = intf_union.for_json()
	if !__partial_deep || is_set("true_lit"):
		result.trueLit = true_lit
	if !__partial_deep || is_set("partial_type_ref"):
		result.partialTypeRef = partial_type_ref.for_json()
	if !__partial_deep || is_set("imported"):
		result.imported = imported.for_json()
	if !__partial_deep || is_set("imported_partial_type_ref"):
		result.importedPartialTypeRef = imported_partial_type_ref.for_json()
	if !__partial_deep || is_set("record_object"):
		result.recordObject = {}
		for __key__ in record_object:
			var __value__ = record_object[__key__]
			result.recordObject[__key__] = __value__.for_json()
	if !__partial_deep || is_set("array"):
		result.array = []
		for __item__ in array:
			var __value__ = __item__.for_json()
			result.array.append(__value__)

	return result

# Unset a property (as if it was never assigned)
func unset(property_name) -> void:
    __assigned_properties.erase(property_name)

# Checks to see whether an optional property has been assigned or not.
# Works for non-optional properties too though if update() has been called
# then they should always be true.
func is_set(property_name: String) -> bool:
    return __initialized && property_name in __assigned_properties

# Check to see if the incoming value was null....
# Godot builtin types don't support nullability but TypeScript primitives do
func is_null(property_name: String) -> bool:
    return __initialized && property_name in __assigned_properties && __assigned_properties[property_name] == null

# Set a property value to null
func set_null(property_name: String) -> void:
    __assigned_properties[property_name] = null
    if property_name in self && typeof(self[property_name]) in [TYPE_OBJECT, TYPE_NIL]:
        self[property_name] = null

# True if this object has been flagged as a partial_deep instance
func is_partial_deep() -> bool:
    return __partial_deep

# True if update() has been called
func is_initialized() -> bool:
    return __initialized

# Keys where is_set(key) returns true
func keys() -> Array:
    return __assigned_properties.keys() if __initialized else []

# Duplicate this instance into a new instance
func duplicate():
    return get_script().new(for_json())
```

## Limitations

* Generic type parameters can't be used as property types as we would be unable to determine the gdscript class name for this type.
* Any Date typed values will require implementation of an `Iso8601Date` class which is not provided.
* Validation is extremely limited
    * The values of literal types or literal type unions have comments but are not enforced.
* Enums with string expressions for values are coverted to dictionaries since gdscript doesn't support that.
* Union types:
	* Unions containing JavaScript primitive types become untyped in gdscript. Currently there is no type checking on these
	* Unions containing other types are not allowed (except `T | null`)
* Type literal expressions are not allowed for properties or as the type of collection based properties. Since they are anonymous we can't generate a class for them
* Extending the generated classes is not effective because generated classes won't import the extended class
	* For now use composition instead of inheritance for adding behaviors
	* TODO: `@typescript-to-gdscript-gd-abstract` which is similar to `@typescript-to-gdscript-gd-impl` directive. Instead of skipping this directive will mark the generated gdscript class as **abstract** and expects the user to implement a class with the same name in ../ which extends the generated class and adds behaviors etc.

# Getting Started

## Set up Rust

- Install rustup
  `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh` or `rustup`
- Install `rust-analyzer` and `CodeLLDB` extensions in vscode.
- Install [Deno](https://deno.land/manual@v1.28.3/getting_started/installation#download-and-install) for TypeScript formatting and linting.
- Restart VsCode
- Run `cargo fetch`

## Building and coding with Rust

`cargo run -- [arguments]` to run the program.

Add `#![allow(warnings)]` to the top of the file to ignore warnings while writing code
