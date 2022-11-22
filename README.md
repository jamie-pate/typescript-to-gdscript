# typescript-to-gdscript

Convert TypeScript type definitions into concrete models in GDScript for godot

This utility enables a pipeline where TypeScript files containing interfaces can be transformed into gdscript classes.

The classes can then ingest data sent across http or WebSocket requests with some minimal validation against the TypeScript interface.

This enables strongly typed communication between TypeScript and the Godot game engine.

## Usage

Usage:

```
typescript-to-gdscript [--debug-print] templatefile.gd.tmpl outputdir input1.ts [input2.ts...]
```

Reads all interfaces from input.ts files exports them to `outputdir/[InterfaceName].gd` files.
Uses templatefile.gd.tmpl as the template.

When `--debug-print` is provided the program will emit extra debug information on stderr.

## Output

The example template outputs a gdscript 'model' class that extends `Reference` and has a constructor that accepts an optional `Dictionary` object, as well as an `update` method which allows the model to be updated in place.

* Properties of the incoming interface are mapped to additional 'model' class files and imported automatically with `preload`.
* Optional properties will be skipped (not updated in the model) when they are missing (undefined values in JSON are skipped during stringification)
* Nullable properties will be assigned to null if the incoming value is null.

### Class Documentation

`func update(src: Dictionary) -> void`: Pass an incoming json object to the update method or constructor to initialize the instance.
    * Optional properties will remain unset. You can check their status by calling `.is_set(property_name)`
    * Nullable properties may remain unset if the property type is a gdscript builtin type. Call `is_null(property_name)` to check if the property is null.
    * Check `.is_initialized()` to see if the `.update()` method has been called yet with a non-empty object.

`func for_json() -> Dictionary`: Call this method to convert the object back into a `Dictionary` so it can be serialized to JSON data.

`func is_set(property_name: String) -> bool`: Check to see if an optional property was set.
    * This method is neccesary to avoid overriding `_get` and `_set` and `_get_property_list`, which are kind of broken in Godot 3.

`func is_null(property_name: String) -> bool`: Check to see if a nullable property was null.
    * This method is neccesary because gdscript builtin types cannot be null, and there is no `Union` type in gdscript.

`func set_null(property_name: String) -> bool`: Set a property to null.
    * If the `typeof()` the property is `TYPE_OBJECT` or `TYPE_NIL` then that property will also be set to `null`.

`func is_initialized() -> bool`: Returns true when `update()` has been called with a non-empty `Dictionary`

## Directives

Comments can contain directives to help out with conversion

* `@typescript-to-gdscript-type: int|float|String`: Forces the type of a property.
* `@typescript-to-gdscript-skip`: This type will not be imported, and will be completely ignored by this program.
* `@typescript-to-gdscript-gd-impl`: This type will be imported, but the gdscript file will not be generated.
    * This is useful for interface union types that would generate one type or another based on the kind property for example.
    * See [any-kind.ts](./test-fixtures/any-kind.ts) for an example.
    * The default template will import these types from the parent directory ('../')

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

# Tracks the null/optional status of builtin properties that are not nullable in gdscript
var __assigned_properties = {}
# Check is_initialized() to detect if this object contains data.
var __initialized = false

var id: float
var str_key: String
var float_key: float
var bool_key: bool
var optional_date: Iso8601Date
# Iso8601Date | null
var nullable_optional_date: Iso8601Date
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
var imported: ImportedInterface
var record_object: TestInterface
var array: Array

func _init(src: Dictionary = {}) -> void:
    update(src)

func update(src: Dictionary) -> void:
    # custom import logic can be added by overriding this function

    __initialized = __initialized || len(src) > 0
    __assigned_properties.id = true
    id = src.id
    __assigned_properties.str_key = true
    str_key = src.strKey
    __assigned_properties.float_key = true
    float_key = src.floatKey
    __assigned_properties.bool_key = true
    bool_key = src.boolKey
    if src.optionalDate in src:
        __assigned_properties.optional_date = true
        optional_date = Iso8601Date.new(src.optionalDate)
    if src.nullableOptionalDate in src:
        __assigned_properties.nullable_optional_date = true if src.nullableOptionalDate != null else null
        nullable_optional_date = Iso8601Date.new(src.nullableOptionalDate) if src.nullableOptionalDate != null else null
    __assigned_properties.date = true
    date = Iso8601Date.new(src.date)
    __assigned_properties.str_lit = true
    str_lit = src.strLit
    __assigned_properties.int_lit = true
    int_lit = src.intLit
    __assigned_properties.float_lit = true
    float_lit = src.floatLit
    __assigned_properties.str_union = true
    str_union = src.strUnion
    __assigned_properties.intf_union = true
    intf_union = AnyKind.new(src.intfUnion)
    __assigned_properties.true_lit = true
    true_lit = src.trueLit
    __assigned_properties.imported = true
    imported = ImportedInterface.new(src.imported)
    __assigned_properties.record_object = true
    record_object = {}
    for __key__ in src.recordObject:
        var __value__ = src.recordObject[__key__]
        record_object[__key__] = TestInterface.new(__value__)
    __assigned_properties.array = true
    array = []
    for __item__ in src.array:
        var __value__ = ImportedInterface.new(__item__)
        array.append(__value__)


func for_json() -> Dictionary:
    # custom logic to serialize to dict/array/primitive for json
    var result = {}
    if !__initialized:
        return result
    result.id = id
    result.strKey = str_key
    result.floatKey = float_key
    result.boolKey = bool_key
    if is_set('optional_date'):
        result.optionalDate = optional_date.for_json()
    if is_set('nullable_optional_date'):
        result.nullableOptionalDate = nullable_optional_date.for_json() if nullable_optional_date != null else null
    result.date = date.for_json()
    result.strLit = str_lit
    result.intLit = int_lit
    result.floatLit = float_lit
    result.strUnion = str_union
    result.intfUnion = intf_union.for_json()
    result.trueLit = true_lit
    result.imported = imported.for_json()
    result.recordObject = {}
    for __key__ in record_object:
        var __value__ = record_object[__key__]
        result.recordObject[__key__] = __value__.for_json()
    result.array = []
    for __item__ in array:
        var __value__ = __item__.for_json()
        result.array.append(__value__)

    return result

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
func set_null(property_name: String) -> bool:
    __assigned_properties[property_name] = null
    if property_name in self && typeof(self[property_name]) in [TYPE_OBJECT, TYPE_NIL]:
        self[property_name] = null

# True if update() has been called
func is_initialized() -> bool:
    return __initialized
```

## Limitations

* Generic type parameters aren't supported well. We'd have to get into extensible sub-classes to model them properly. Use `@typescript-to-gdscript-gd-impl` to opt out here...
* TypeScript Enums with assignment expressions don't work (yet)
* Any Date typed values will require implementation of an Iso8601Date class which is not provided.
* Validation is extremely limited
    * The values of literal types or literal type unions have comments but are not enforced.

# Getting Started
## Set up Rust

- Install rustup
  `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh` or `rustup`
- Install `rust-analyzer` and `CodeLLDB` extensions in vscode.
- Restart VsCode

## Building and coding with Rust

`cargo run -- [arguments]` to run the program.

Add `#![allow(warnings)]` to the top of the file to ignore warnings while writing code
