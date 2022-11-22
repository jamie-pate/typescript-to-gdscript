use std::{collections::HashMap, path::PathBuf};

use indoc::formatdoc;
use serde::Serialize;

pub const DEFAULT_INDENT: &'static str = "    ";

pub const STATE_VARS: &'static str = "\
# Tracks the null/optional status of builtin properties that are not nullable in gdscript
var __assigned_properties = {}
# Check is_initialized() to detect if this object contains data.
var __initialized = false";

pub const STATE_METHODS: &'static str = "\
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

# True if update() has been called
func is_initialized() -> bool:
    return __initialized";

#[derive(Serialize, Debug)]
pub struct ModelImportContext {
    pub name: String,
    pub src: String,
    pub gd_impl: bool,
}

// Extra template data that's required if the variable is an array or dictionary
#[derive(Serialize, Debug, Clone)]
pub struct ModelVarCollection {
    // Initial value (empty array, dict, PoolStringArray, etc)
    pub init: String,
    pub is_array: bool,
    pub is_dict: bool,
    pub nullable: bool,
}

lazy_static! {
    static ref BUILTIN_DEFAULTS: HashMap<&'static str, &'static str> = {
        let mut b = HashMap::new();
        for builtin in [
            ("", "ERROR"),
            ("Array", "[]"),
            ("Dictionary", "{}"),
            // default 'null' values for godot primitives..
            // unfortunately there's no way to make them nullable.
            ("String", "\"\""),
            ("int", "0"),
            ("float", "0f"),
            ("bool", "false")
        ] {
            b.insert(builtin.0, builtin.1);
        }
        b
    };
}

pub fn is_builtin(name: &str) -> bool {
    BUILTIN_DEFAULTS.contains_key(name)
}

impl ModelValueCtor {
    pub fn empty(nullable: bool) -> Self {
        ModelValueCtor::new("", nullable)
    }
    pub fn new(name: &str, nullable: bool) -> Self {
        let builtin = is_builtin(name);
        let null_value = BUILTIN_DEFAULTS.get(name).unwrap_or(&"null");
        // TODO: add other builtin conversions from json here
        let new_str = if !builtin {
            format!("{name}.new")
        } else {
            "".to_string()
        };
        let (lparen_str, rparen_str) = if new_str != "" { ("(", ")") } else { ("", "") };
        ModelValueCtor {
            name: name.to_string(),
            builtin,
            start: format!("{}{}", new_str, lparen_str),
            end: if !nullable {
                rparen_str.to_string()
            } else {
                format!("{} if ", rparen_str)
            },
            suffix: if nullable {
                Some(format!(" != null else {}", null_value))
            } else {
                None
            },
        }
    }
}

// usage: `{ctor.start}__value__{ctor.end} in the template
#[derive(Serialize, Debug)]
pub struct ModelValueCtor {
    pub name: String,
    pub builtin: bool,
    pub start: String,
    pub end: String,
    // suffix such for cases where we need to put the input value in twice
    // usage: `{ctor.start}__value__{ctor.end}{{if ctor.suffix}}__value__{ctor.suffix}{{endif}}
    // for cases like `T.new(__value__) if __value != null else null`
    pub suffix: Option<String>,
}

impl ModelValueForJson {
    pub fn empty(nullable: bool) -> Self {
        ModelValueForJson::new("", nullable)
    }
    pub fn new(name: &str, nullable: bool) -> Self {
        let parens = name != "";
        let builtin = is_builtin(name);
        // TODO: add other builtin 'for_json' behaviors here
        let for_json_call = if !builtin { ".for_json()" } else { "" }.to_string();
        let (null_test_start, null_test_end) = if builtin {
            (" if !is_null('", "')")
        } else {
            (" if ", " != null")
        };
        ModelValueForJson {
            name: name.to_string(),
            builtin,
            for_json_call,
            suffix: if nullable {
                Some((
                    null_test_start.to_string(),
                    format!("{} else null", null_test_end),
                ))
            } else {
                None
            },
        }
    }
}
#[derive(Serialize, Debug)]
pub struct ModelValueForJson {
    pub name: String,
    pub builtin: bool,
    pub for_json_call: String,
    pub suffix: Option<(String, String)>,
}

#[derive(Serialize, Debug)]
pub struct ModelEnumMember {
    pub name: String,
    pub value: String,
}

#[derive(Serialize, Debug)]
pub struct ModelEnum {
    pub name: String,
    pub members: Vec<ModelEnumMember>,
}

#[derive(Serialize, Debug)]
pub struct ModelVarDescriptor {
    // name of the variable in the gdscript object (camel_case)
    pub name: String,
    // comments
    pub comment: Option<String>,
    // gdscript type of the variable
    pub decl_type: String,
    // optional initializer for declaration
    pub decl_init: Option<String>,
    // name of the variable in the typescript interface (pascalCase)
    pub src_name: String,
    // Constructor expression.
    pub ctor: ModelValueCtor,
    // Conversion to json
    pub for_json: ModelValueForJson,
    // maybe a collection
    pub collection: Option<ModelVarCollection>,
    // if the whole thing is optional
    pub optional: bool,
    // Gdscript primitives are non_nullable...
    pub non_nullable: bool,
}

impl ModelVarDescriptor {
    pub fn render(&self, indent: &str) -> ModelVar {
        ModelVar {
            init: self.render_init(indent),
            for_json: self.render_for_json(indent),
        }
    }
    pub fn render_init(&self, indent: &str) -> String {
        let src = "src";
        let mut parts: Vec<String> = Vec::new();
        let src_specifier = format!("{}.{}", src, self.src_name);
        let (name, src_name) = (&self.name, &self.src_name);
        let (ctor_start, ctor_end) = { (&self.ctor.start, &self.ctor.end) };
        let src_nullable = if let Some(collection) = &self.collection {
            collection.nullable
        } else {
            self.ctor.suffix.is_some()
        };
        let assigned_value = if src_nullable {
            format!("true if {src_specifier} != null else null")
        } else {
            "true".to_string()
        };
        let get_init_value = |specifier, maybe_suffix: &Option<String>| {
            if let Some(suffix) = maybe_suffix {
                format!("{ctor_start}{specifier}{ctor_end}{specifier}{suffix}")
            } else {
                format!("{ctor_start}{specifier}{ctor_end}")
            }
        };
        let mut indent_level = 0;
        if self.optional {
            parts.push(format!("if {src_specifier} in {src}:"));
            indent_level += 1;
        }
        if let Some(collection) = &self.collection {
            let c_init = &collection.init;

            parts.push(add_indent(
                formatdoc! {"
                __assigned_properties.{name} = {assigned_value}
                {name} = {c_init}"
                },
                indent,
                indent_level,
            ));
            if collection.nullable {
                parts.push(add_indent(
                    format!("if {src_specifier} != null:"),
                    indent,
                    indent_level,
                ));
                indent_level += 1;
            }

            // TODO: PoolStringArray, PoolIntArray etc?
            let maybe_suffix =
                ModelValueCtor::new("__nullable__", self.ctor.suffix.is_some()).suffix;
            if collection.is_array {
                let init_value = get_init_value("__item__", &maybe_suffix);
                parts.push(add_indent(
                    formatdoc! {"
                        for __item__ in {src_specifier}:
                            var __value__ = {init_value}
                            {name}.append(__value__)"
                    },
                    indent,
                    indent_level,
                ));
            } else if collection.is_dict {
                let init_value = get_init_value("__value__", &maybe_suffix);
                parts.push(add_indent(
                    formatdoc! {"
                        for __key__ in {src_specifier}:
                            var __value__ = {src_specifier}[__key__]
                            {name}[__key__] = {init_value}"
                    },
                    indent,
                    indent_level,
                ))
            } else {
                panic!("This should never happen");
            }
        } else {
            let init_value = get_init_value(&src_specifier, &self.ctor.suffix);
            parts.push(add_indent(
                formatdoc! {"
                __assigned_properties.{name} = {assigned_value}
                {name} = {init_value}"
                },
                indent,
                indent_level,
            ));
        }
        fn normalize_indent(indent: &str, l: &str) -> String {
            if indent != DEFAULT_INDENT {
                l.replace(DEFAULT_INDENT, indent)
            } else {
                l.to_string()
            }
        }
        // everything needs to start at indent 1, so add one indent to every non-empty line
        parts
            .into_iter()
            .map(|p| {
                p.split("\n")
                    .map(|l| normalize_indent(indent, l))
                    .collect::<Vec<_>>()
            })
            .flatten()
            .map(|l| format!("{}{}", indent, l))
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn render_for_json(&self, indent: &str) -> String {
        let dest = "result";
        let mut parts: Vec<String> = Vec::new();
        let dest_specifier = format!("{}.{}", dest, self.src_name);
        let (name, dest_name) = (&self.name, &self.src_name);
        let for_json_call = &self.for_json.for_json_call;
        let dest_nullable = if let Some(collection) = &self.collection {
            collection.nullable
        } else {
            self.for_json.suffix.is_some()
        };
        let get_for_json_value = |specifier, maybe_suffix: &Option<(String, String)>| {
            if let Some((null_check_start, null_check_end)) = maybe_suffix {
                format!("{specifier}{for_json_call}{null_check_start}{specifier}{null_check_end}")
            } else {
                format!("{specifier}{for_json_call}")
            }
        };
        let mut indent_level = 0;

        if self.optional {
            parts.push(format!("if is_set('{name}'):"));
            indent_level += 1;
        }
        if let Some(collection) = &self.collection {
            let c_init = &collection.init;

            if collection.nullable {
                parts.push(add_indent(
                    formatdoc! {"
                        if is_null('{name}'):
                            {dest_specifier} = null
                        else:"},
                    indent,
                    indent_level,
                ));
                indent_level += 1;
            }
            parts.push(add_indent(
                formatdoc! {"
                {dest_specifier} = {c_init}"
                },
                indent,
                indent_level,
            ));

            // TODO: PoolStringArray, PoolIntArray etc?
            let maybe_suffix =
                ModelValueForJson::new("__nullable__", self.for_json.suffix.is_some()).suffix;
            if collection.is_array {
                let for_json_value = get_for_json_value("__item__", &maybe_suffix);
                parts.push(add_indent(
                    formatdoc! {"
                        for __item__ in {name}:
                            var __value__ = {for_json_value}
                            {dest_specifier}.append(__value__)"
                    },
                    indent,
                    indent_level,
                ));
            } else if collection.is_dict {
                let for_json_value = get_for_json_value("__value__", &maybe_suffix);
                parts.push(add_indent(
                    formatdoc! {"
                        for __key__ in {name}:
                            var __value__ = {name}[__key__]
                            {dest_specifier}[__key__] = {for_json_value}"
                    },
                    indent,
                    indent_level,
                ))
            } else {
                panic!("This should never happen");
            }
        } else {
            let for_json_value = get_for_json_value(&name, &self.for_json.suffix);
            parts.push(add_indent(
                formatdoc! {"
                {dest_specifier} = {for_json_value}"
                },
                indent,
                indent_level,
            ));
        }
        fn normalize_indent(indent: &str, l: &str) -> String {
            if indent != DEFAULT_INDENT {
                l.replace(DEFAULT_INDENT, indent)
            } else {
                l.to_string()
            }
        }
        // everything needs to start at indent 1, so add one indent to every non-empty line
        parts
            .into_iter()
            .map(|p| {
                p.split("\n")
                    .map(|l| normalize_indent(indent, l))
                    .collect::<Vec<_>>()
            })
            .flatten()
            .map(|l| add_indent(l, indent, 1))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

fn add_indent(str: String, indent: &str, level: usize) -> String {
    if level == 0 {
        str
    } else {
        let indent_str = indent.repeat(level);
        str.split("\n")
            .map(|l| {
                if l.is_empty() {
                    l.to_string()
                } else {
                    format!("{indent_str}{l}")
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}

#[derive(Serialize, Debug)]
pub struct ModelSrcType {
    pub name: String,
    pub init: Option<String>,
}

#[derive(Serialize, Debug)]
pub struct ModelVar {
    pub init: String,
    pub for_json: String,
}

#[derive(Serialize, Debug)]
pub struct ModelContext {
    pub class_name: String,
    pub canonical_src_filepath: PathBuf,
    // Dictionary | Array
    pub comment: Option<String>,
    pub src_type: Option<ModelSrcType>,
    pub imports: Vec<ModelImportContext>,
    pub enums: Vec<ModelEnum>,
    pub vars: Vec<ModelVar>,
    pub var_descriptors: Vec<ModelVarDescriptor>,
    pub state_vars: String,
    pub state_methods: String,
}

#[cfg(test)]
mod tests {
    use convert_case::{Case, Casing};
    use indoc::indoc;

    use crate::model_context::{ModelVarCollection, DEFAULT_INDENT};

    use super::{add_indent, is_builtin, ModelValueCtor, ModelValueForJson, ModelVarDescriptor};

    impl ModelVarDescriptor {
        fn for_test(src_name: &str, optional: bool, type_name: &str, type_nullable: bool) -> Self {
            ModelVarDescriptor {
                name: src_name.to_string().to_case(Case::Snake),
                src_name: src_name.to_string(),
                comment: Some("comment".to_string()),
                decl_type: "".to_string(),
                decl_init: None,
                ctor: ModelValueCtor::new(type_name, type_nullable),
                for_json: ModelValueForJson::new(type_name, type_nullable),
                collection: None,
                optional: optional,
                non_nullable: is_builtin(type_name),
            }
        }
    }

    fn indent(str: &str) -> String {
        add_indent(str.to_string(), DEFAULT_INDENT, 1)
    }

    #[test]
    fn test_add_indent() {
        let str = "abcd\n\
            efgh\n\
            \n\
            ijkl\n\
            ";
        let rendered = add_indent(str.to_string(), DEFAULT_INDENT, 1);
        assert_eq!(
            rendered,
            format!(
                "\
            {DEFAULT_INDENT}abcd\n\
            {DEFAULT_INDENT}efgh\n\
            \n\
            {DEFAULT_INDENT}ijkl\n\
            "
            ),
            "Indent level 1"
        );

        let rendered = add_indent(str.to_string(), DEFAULT_INDENT, 2);
        assert_eq!(
            rendered,
            format!(
                "\
                {DEFAULT_INDENT}{DEFAULT_INDENT}abcd\n\
                {DEFAULT_INDENT}{DEFAULT_INDENT}efgh\n\
                \n\
                {DEFAULT_INDENT}{DEFAULT_INDENT}ijkl\n\
                "
            ),
            "Indent level 2"
        );
    }

    #[test]
    fn render_init() {
        let model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(
                "\
                __assigned_properties.prop_name = true\n\
                prop_name = Intf.new(src.propName)\
            "
            )
        );
    }

    #[test]
    fn render_init_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", true);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(
                "\
                __assigned_properties.prop_name = true if src.propName != null else null\n\
                prop_name = Intf.new(src.propName) if src.propName != null else null\
            "
            )
        );
    }
    // if the target is non nullable then don't add any null checking
    // it should be an error if the src is null
    fn render_init_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "String", false);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(
                "\
                __assigned_properties.prop_name = true\n\
                prop_name = src.propName\
            "
            )
        );
    }

    #[test]
    // Sometimes the typescript type is nullable but the gdscript type is not
    fn render_init_nullable_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "String", true);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(
                "\
                __assigned_properties.prop_name = true if src.propName != null else null\n\
                prop_name = src.propName if src.propName != null else \"\"\
            "
            )
        );
    }

    #[test]
    fn render_init_optional() {
        let model = ModelVarDescriptor::for_test("propName", true, "Intf", false);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if src.propName in src:
                    __assigned_properties.prop_name = true
                    prop_name = Intf.new(src.propName)\
            "})
        );
    }

    #[test]
    fn render_init_optional_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "Intf", true);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if src.propName in src:
                    __assigned_properties.prop_name = true if src.propName != null else null
                    prop_name = Intf.new(src.propName) if src.propName != null else null\
            "})
        );
    }

    // if the target is non nullable then don't add any null checking
    // it should be an error if the src is null
    #[test]
    fn render_init_optional_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "String", false);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if src.propName in src:
                    __assigned_properties.prop_name = true
                    prop_name = src.propName\
            "})
        );
    }

    // Sometimes the typescript type is nullable but the gdscript type is not
    #[test]
    fn render_init_optional_nullable_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "String", true);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if src.propName in src:
                    __assigned_properties.prop_name = true if src.propName != null else null
                    prop_name = src.propName if src.propName != null else \"\"\
            "})
        );
    }

    #[test]
    fn render_init_array() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            init: "[]".to_string(),
            is_array: true,
            is_dict: false,
            nullable: false,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                __assigned_properties.prop_name = true
                prop_name = []
                for __item__ in src.propName:
                    var __value__ = Intf.new(__item__)
                    prop_name.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_init_dict() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            init: "{}".to_string(),
            is_array: false,
            is_dict: true,
            nullable: false,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                __assigned_properties.prop_name = true
                prop_name = {}
                for __key__ in src.propName:
                    var __value__ = src.propName[__key__]
                    prop_name[__key__] = Intf.new(__value__)\
            "})
        );
    }

    #[test]
    fn render_init_nullable_array() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            init: "[]".to_string(),
            is_array: true,
            is_dict: false,
            nullable: true,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                __assigned_properties.prop_name = true if src.propName != null else null
                prop_name = []
                if src.propName != null:
                    for __item__ in src.propName:
                        var __value__ = Intf.new(__item__)
                        prop_name.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_init_nullable_dict() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            init: "{}".to_string(),
            is_array: false,
            is_dict: true,
            nullable: true,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                __assigned_properties.prop_name = true if src.propName != null else null
                prop_name = {}
                if src.propName != null:
                    for __key__ in src.propName:
                        var __value__ = src.propName[__key__]
                        prop_name[__key__] = Intf.new(__value__)\
            "})
        );
    }

    #[test]
    fn render_init_nullable_array_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", true);
        model.collection = Some(ModelVarCollection {
            init: "[]".to_string(),
            is_array: true,
            is_dict: false,
            nullable: true,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                __assigned_properties.prop_name = true if src.propName != null else null
                prop_name = []
                if src.propName != null:
                    for __item__ in src.propName:
                        var __value__ = Intf.new(__item__) if __item__ != null else null
                        prop_name.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_init_nullable_dict_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", true);
        model.collection = Some(ModelVarCollection {
            init: "{}".to_string(),
            is_array: false,
            is_dict: true,
            nullable: true,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                __assigned_properties.prop_name = true if src.propName != null else null
                prop_name = {}
                if src.propName != null:
                    for __key__ in src.propName:
                        var __value__ = src.propName[__key__]
                        prop_name[__key__] = Intf.new(__value__) if __value__ != null else null\
            "})
        );
    }

    #[test]
    fn render_init_optional_nullable_array_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "Intf", true);
        model.collection = Some(ModelVarCollection {
            init: "[]".to_string(),
            is_array: true,
            is_dict: false,
            nullable: true,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if src.propName in src:
                    __assigned_properties.prop_name = true if src.propName != null else null
                    prop_name = []
                    if src.propName != null:
                        for __item__ in src.propName:
                            var __value__ = Intf.new(__item__) if __item__ != null else null
                            prop_name.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_init_optional_nullable_dict_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "Intf", true);
        model.collection = Some(ModelVarCollection {
            init: "{}".to_string(),
            is_array: false,
            is_dict: true,
            nullable: true,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if src.propName in src:
                    __assigned_properties.prop_name = true if src.propName != null else null
                    prop_name = {}
                    if src.propName != null:
                        for __key__ in src.propName:
                            var __value__ = src.propName[__key__]
                            prop_name[__key__] = Intf.new(__value__) if __value__ != null else null\
            "})
        );
    }

    // non-nullable nullables can be null when they are added to an array
    // (unless we start supporting PoolStringArray etc, then we have to deoptimize for this case)
    #[test]
    fn render_init_optional_nullable_array_of_builtin_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "int", true);
        model.collection = Some(ModelVarCollection {
            init: "[]".to_string(),
            is_array: true,
            is_dict: false,
            nullable: true,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if src.propName in src:
                    __assigned_properties.prop_name = true if src.propName != null else null
                    prop_name = []
                    if src.propName != null:
                        for __item__ in src.propName:
                            var __value__ = __item__ if __item__ != null else null
                            prop_name.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_init_optional_nullable_dict_of_builtin_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "int", true);
        model.collection = Some(ModelVarCollection {
            init: "{}".to_string(),
            is_array: false,
            is_dict: true,
            nullable: true,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if src.propName in src:
                    __assigned_properties.prop_name = true if src.propName != null else null
                    prop_name = {}
                    if src.propName != null:
                        for __key__ in src.propName:
                            var __value__ = src.propName[__key__]
                            prop_name[__key__] = __value__ if __value__ != null else null\
            "})
        );
    }

    #[test]
    fn render_for_json() {
        let model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(
                "\
                result.propName = prop_name.for_json()\
            "
            )
        );
    }

    #[test]
    fn render_for_json_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", true);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(
                "\
                result.propName = prop_name.for_json() if prop_name != null else null\
            "
            )
        );
    }
    // if the target is non nullable then don't add any null checking
    // it should be an error if the src is null
    fn render_for_json_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "String", false);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(
                "\
                result.propName = prop_name\
            "
            )
        );
    }

    #[test]
    // Sometimes the typescript type is nullable but the gdscript type is not
    fn render_for_json_nullable_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "String", true);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(
                "\
                result.propName = prop_name if !is_null('prop_name') else null\
            "
            )
        );
    }

    #[test]
    fn render_for_json_optional() {
        let model = ModelVarDescriptor::for_test("propName", true, "Intf", false);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if is_set('prop_name'):
                    result.propName = prop_name.for_json()\
            "})
        );
    }

    #[test]
    fn render_for_json_optional_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "Intf", true);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if is_set('prop_name'):
                    result.propName = prop_name.for_json() if prop_name != null else null\
            "})
        );
    }

    // if the target is non nullable then don't add any null checking
    // it should be an error if the src is null
    #[test]
    fn render_for_json_optional_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "String", false);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if is_set('prop_name'):
                    result.propName = prop_name\
            "})
        );
    }

    // Sometimes the typescript type is nullable but the gdscript type is not
    #[test]
    fn render_for_json_optional_nullable_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "String", true);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if is_set('prop_name'):
                    result.propName = prop_name if !is_null('prop_name') else null\
            "})
        );
    }

    #[test]
    fn render_for_json_array() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            init: "[]".to_string(),
            is_array: true,
            is_dict: false,
            nullable: false,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                result.propName = []
                for __item__ in prop_name:
                    var __value__ = __item__.for_json()
                    result.propName.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_for_json_dict() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            init: "{}".to_string(),
            is_array: false,
            is_dict: true,
            nullable: false,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                result.propName = {}
                for __key__ in prop_name:
                    var __value__ = prop_name[__key__]
                    result.propName[__key__] = __value__.for_json()\
            "})
        );
    }

    #[test]
    fn render_for_json_nullable_array() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            init: "[]".to_string(),
            is_array: true,
            is_dict: false,
            nullable: true,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if is_null('prop_name'):
                    result.propName = null
                else:
                    result.propName = []
                    for __item__ in prop_name:
                        var __value__ = __item__.for_json()
                        result.propName.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_for_json_nullable_dict() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            init: "{}".to_string(),
            is_array: false,
            is_dict: true,
            nullable: true,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if is_null('prop_name'):
                    result.propName = null
                else:
                    result.propName = {}
                    for __key__ in prop_name:
                        var __value__ = prop_name[__key__]
                        result.propName[__key__] = __value__.for_json()\
            "})
        );
    }

    #[test]
    fn render_for_json_nullable_array_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", true);
        model.collection = Some(ModelVarCollection {
            init: "[]".to_string(),
            is_array: true,
            is_dict: false,
            nullable: true,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if is_null('prop_name'):
                    result.propName = null
                else:
                    result.propName = []
                    for __item__ in prop_name:
                        var __value__ = __item__.for_json() if __item__ != null else null
                        result.propName.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_for_json_nullable_dict_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", true);
        model.collection = Some(ModelVarCollection {
            init: "{}".to_string(),
            is_array: false,
            is_dict: true,
            nullable: true,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if is_null('prop_name'):
                    result.propName = null
                else:
                    result.propName = {}
                    for __key__ in prop_name:
                        var __value__ = prop_name[__key__]
                        result.propName[__key__] = __value__.for_json() if __value__ != null else null\
            "})
        );
    }

    #[test]
    fn render_for_json_optional_nullable_array_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "Intf", true);
        model.collection = Some(ModelVarCollection {
            init: "[]".to_string(),
            is_array: true,
            is_dict: false,
            nullable: true,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if is_set('prop_name'):
                    if is_null('prop_name'):
                        result.propName = null
                    else:
                        result.propName = []
                        for __item__ in prop_name:
                            var __value__ = __item__.for_json() if __item__ != null else null
                            result.propName.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_for_json_optional_nullable_dict_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "Intf", true);
        model.collection = Some(ModelVarCollection {
            init: "{}".to_string(),
            is_array: false,
            is_dict: true,
            nullable: true,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if is_set('prop_name'):
                    if is_null('prop_name'):
                        result.propName = null
                    else:
                        result.propName = {}
                        for __key__ in prop_name:
                            var __value__ = prop_name[__key__]
                            result.propName[__key__] = __value__.for_json() if __value__ != null else null\
            "})
        );
    }

    // non-nullable nullables can be null when they are added to an array
    // (unless we start supporting PoolStringArray etc, then we have to deoptimize for this case)
    #[test]
    fn render_for_json_optional_nullable_array_of_builtin_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "int", true);
        model.collection = Some(ModelVarCollection {
            init: "[]".to_string(),
            is_array: true,
            is_dict: false,
            nullable: true,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if is_set('prop_name'):
                    if is_null('prop_name'):
                        result.propName = null
                    else:
                        result.propName = []
                        for __item__ in prop_name:
                            var __value__ = __item__ if __item__ != null else null
                            result.propName.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_for_json_optional_nullable_dict_of_builtin_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "int", true);
        model.collection = Some(ModelVarCollection {
            init: "{}".to_string(),
            is_array: false,
            is_dict: true,
            nullable: true,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(indoc! {"
                if is_set('prop_name'):
                    if is_null('prop_name'):
                        result.propName = null
                    else:
                        result.propName = {}
                        for __key__ in prop_name:
                            var __value__ = prop_name[__key__]
                            result.propName[__key__] = __value__ if __value__ != null else null\
            "})
        );
    }
}
