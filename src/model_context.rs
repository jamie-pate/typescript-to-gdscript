use std::{collections::HashMap, path::PathBuf, string};

use deno_ast::swc::ast::Id;
use indoc::formatdoc;
use serde::Serialize;

pub const DEFAULT_INDENT: &'static str = "\t";

pub const STATE_VARS: &'static str = "\
# Tracks the null/optional status of builtin properties that are not nullable in gdscript
var __assigned_properties = {}
# Check is_initialized() to detect if this object contains data.
var __initialized = false";

pub const STATE_METHODS: &'static str = "\
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
    return get_script().new(for_json())";

#[derive(Serialize, Debug, Clone)]
pub struct ModelImportContext {
    pub name: String,
    pub src: String,
    pub gd_impl: bool,
    // Typescript symbol/context id of this var descriptor
    pub id: Id,
}

// Extra template data that's required if the variable is an array or dictionary
#[derive(Serialize, Debug, Clone)]
pub struct ModelVarCollection {
    // TODO: probably should be an enum? Dict|Array?
    pub is_array: bool,
    pub is_dict: bool,
    pub nullable: bool,
    // If item_collection is not None, then this is a nested collection
    pub item_collection: Option<Box<ModelVarCollection>>,
}

impl ModelVarCollection {
    pub fn init(&self) -> &str {
        if self.is_array {
            "[]"
        } else if self.is_dict {
            "{}"
        } else {
            panic!("Must be array or dict");
        }
    }

    pub fn gd_type(&self) -> &str {
        if self.is_array {
            "Array"
        } else if self.is_dict {
            "Dictionary"
        } else {
            panic!("Must be array or dict");
        }
    }
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
            ("float", "0.0"),
            ("bool", "false"),
            ("any", "null"),
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
        let (lparen_str, rparen_str) = if new_str != "" {
            ("(", ", __partial_deep)")
        } else {
            ("", "")
        };
        ModelValueCtor {
            name: name.to_string(),
            builtin,
            start: format!("{}{}", new_str, lparen_str),
            end: if !nullable {
                rparen_str.to_string()
            } else {
                format!("{} if typeof(", rparen_str)
            },
            suffix: if nullable {
                Some(format!(") != TYPE_NIL else {}", null_value))
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
    // for cases like `T.new(__value__) if typeof(__value__) != TYPE_NIL else null`
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
            (" if !is_null(\"", "\")")
        } else {
            (" if typeof(", ") != TYPE_NIL")
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

#[derive(Serialize, Debug, Clone)]
pub struct ModelEnumMember {
    pub name: String,
    pub value: String,
}

#[derive(Serialize, Debug, Clone)]
pub struct ModelEnum {
    pub name: String,
    pub have_string_members: bool,
    pub members: Vec<ModelEnumMember>,
}

#[derive(Serialize, Debug)]
pub struct ModelVarDescriptor {
    // name of the variable in the gdscript object (camel_case)
    pub name: String,
    // comments
    pub comment: Option<String>,
    // gdscript type of the variable
    // it could be None for the "any" type
    pub decl_type: Option<String>,
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
    // Imported references required by this var
    pub imports: Vec<ModelImportContext>,
    // Enums for this property
    pub enums: Vec<ModelEnum>,
}

impl ModelVarDescriptor {
    pub fn render(&self, indent: &str) -> ModelVar {
        ModelVar {
            init: self.render_init(indent),
            for_json: self.render_for_json(indent),
        }
    }

    fn render_init_item_collection(
        &self,
        collection: &ModelVarCollection,
        level: usize,
        src_specifier: &str,
        dest_specifier: &str,
        indent_level: usize,
        init_parts: &Vec<String>,
        parts: &mut Vec<String>,
        indent: &str,
    ) {
        let ws = indent;
        // TODO: PoolStringArray, PoolIntArray etc?
        let mut level_init_parts = init_parts.clone();
        let maybe_suffix = if collection.item_collection.is_none() {
            ModelValueCtor::new("__nullable__", self.ctor.suffix.is_some()).suffix
        } else {
            None
        };
        if let Some(suffix) = maybe_suffix {
            level_init_parts.push(suffix);
        }
        // level suffix
        let ls = if level > 0 {
            level.to_string()
        } else {
            "".to_string()
        };

        let (mut collection_specifier, mut init_value) =
            if let Some(ic) = &collection.item_collection {
                (format!("__coll__{ls}"), ic.init().to_string())
            } else {
                ("".to_string(), "".to_string())
            };
        let mut next_src_specifier = "".to_string();
        let mut next_dest_specifier = "".to_string();
        if collection.is_array {
            if init_value.is_empty() {
                init_value = level_init_parts.join(&format!("__item__{ls}"));
            }
            if collection_specifier.is_empty() {
                collection_specifier = format!("__value__{ls}");
            }
            parts.push(add_indent(
                formatdoc! {"
                for __item__{ls} in {src_specifier}:
                {ws}var {collection_specifier} = {init_value}
                {ws}{dest_specifier}.append({collection_specifier})"},
                indent,
                indent_level,
            ));
            (next_src_specifier, next_dest_specifier) =
                (format!("__item__{ls}"), collection_specifier);
        } else if collection.is_dict {
            if init_value.is_empty() {
                init_value = level_init_parts.join(&format!("__value__{ls}"));
            };
            parts.push(add_indent(
                formatdoc! {"
                        for __key__{ls} in {src_specifier}:
                        {ws}var __value__{ls} = {src_specifier}[__key__{ls}]"},
                indent,
                indent_level,
            ));
            parts.push(add_indent(
                if collection_specifier.is_empty() {
                    formatdoc! {"{ws}{dest_specifier}[__key__{ls}] = {init_value}"}
                } else {
                    formatdoc! {"
                        {ws}var {collection_specifier} = {init_value}
                        {ws}{dest_specifier}[__key__{ls}] = {collection_specifier}"
                    }
                },
                indent,
                indent_level,
            ));
            (next_src_specifier, next_dest_specifier) =
                (format!("__value__{ls}"), collection_specifier);
        } else {
            panic!("This should never happen");
        }
        if let Some(item_collection) = &collection.item_collection {
            // nested container uses recursive calls to render
            self.render_init_item_collection(
                item_collection,
                level + 1,
                &next_src_specifier,
                &next_dest_specifier,
                indent_level + 1,
                &init_parts,
                parts,
                indent,
            );
        }
    }

    pub fn render_init(&self, indent: &str) -> String {
        let ws = DEFAULT_INDENT;
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
            format!("true if typeof({src_specifier}) != TYPE_NIL else null")
        } else {
            "true".to_string()
        };
        let mut init_parts: Vec<String> = vec![ctor_start.into(), ctor_end.into()];
        let mut indent_level = 1;
        if self.optional {
            parts.push(format!("if \"{src_name}\" in {src}:"));
        } else {
            parts.push(format!("if !__partial_deep || \"{src_name}\" in {src}:"));
        }
        if let Some(collection) = &self.collection {
            let c_init = collection.init();

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
                    format!("if typeof({src_specifier}) != TYPE_NIL:"),
                    indent,
                    indent_level,
                ));
                indent_level += 1;
            }
            self.render_init_item_collection(
                collection,
                0,
                &src_specifier,
                name,
                indent_level,
                &mut init_parts,
                &mut parts,
                indent,
            );
        } else {
            if let Some(suffix) = &self.ctor.suffix {
                init_parts.push(suffix.to_string());
            }
            let init_value: String = init_parts.join(&src_specifier);
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

    fn specifier_for_json(
        &self,
        specifier: &str,
        for_json_call: &str,
        maybe_suffix: &Option<(String, String)>,
    ) -> String {
        let for_json_parts: Vec<String> =
            if let Some((null_check_start, null_check_end)) = maybe_suffix {
                vec![
                    "".to_string(),
                    format!("{for_json_call}{null_check_start}"),
                    null_check_end.to_string(),
                ]
            } else {
                vec!["".to_string(), for_json_call.to_string()]
            };
        return for_json_parts.join(specifier);
    }

    fn render_for_json_item_collection(
        &self,
        collection: &ModelVarCollection,
        level: usize,
        src_specifier: &str,
        dest_specifier: &str,
        indent_level: usize,
        parts: &mut Vec<String>,
        indent: &str,
    ) {
        let ws = indent;
        // TODO: PoolStringArray, PoolIntArray etc?
        let maybe_suffix = if collection.item_collection.is_none() {
            ModelValueForJson::new("__nullable__", self.for_json.suffix.is_some()).suffix
        } else {
            None
        };
        // level suffix
        let ls = if level > 0 {
            level.to_string()
        } else {
            "".to_string()
        };

        let (mut collection_specifier, mut for_json_value) =
            if let Some(ic) = &collection.item_collection {
                (format!("__coll__{ls}"), ic.init().to_string())
            } else {
                ("".to_string(), "".to_string())
            };
        let mut next_src_specifier = "".to_string();
        let mut next_dest_specifier = "".to_string();
        let for_json_call = &self.for_json.for_json_call;
        if collection.is_array {
            if for_json_value.is_empty() {
                for_json_value =
                    self.specifier_for_json(&format!("__item__{ls}"), for_json_call, &maybe_suffix);
            }
            if collection_specifier.is_empty() {
                collection_specifier = format!("__value__{ls}");
            }

            parts.push(add_indent(
                formatdoc! {"
                    for __item__{ls} in {src_specifier}:
                    {ws}var {collection_specifier} = {for_json_value}
                    {ws}{dest_specifier}.append({collection_specifier})"
                },
                indent,
                indent_level,
            ));
            (next_src_specifier, next_dest_specifier) =
                (format!("__item__{ls}"), collection_specifier);
        } else if collection.is_dict {
            if for_json_value.is_empty() {
                for_json_value = self.specifier_for_json(
                    &format!("__value__{ls}"),
                    for_json_call,
                    &maybe_suffix,
                );
            }

            parts.push(add_indent(
                formatdoc! {"
                    for __key__{ls} in {src_specifier}:
                    {ws}var __value__{ls} = {src_specifier}[__key__{ls}]"
                },
                indent,
                indent_level,
            ));
            parts.push(add_indent(
                if collection_specifier.is_empty() {
                    formatdoc! {"{ws}{dest_specifier}[__key__{ls}] = {for_json_value}"}
                } else {
                    formatdoc! {
                        "{ws}var {collection_specifier} = {for_json_value}
                        {ws}{dest_specifier}[__key__{ls}] = {collection_specifier}"
                    }
                },
                indent,
                indent_level,
            ));
            (next_src_specifier, next_dest_specifier) =
                (format!("__value__{ls}"), collection_specifier);
        } else {
            panic!("This should never happen");
        }
        if let Some(item_collection) = &collection.item_collection {
            // nested container uses recursive calls to render
            self.render_for_json_item_collection(
                item_collection,
                level + 1,
                &next_src_specifier,
                &next_dest_specifier,
                indent_level + 1,
                parts,
                indent,
            );
        }
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

        let mut indent_level = 1;

        if self.optional {
            parts.push(format!("if is_set(\"{name}\"):"));
        } else {
            parts.push(format!("if !__partial_deep || is_set(\"{name}\"):"));
        }
        let nullable = ModelValueForJson::new("__nullable__", self.for_json.suffix.is_some());
        let maybe_suffix = if self.collection.is_some() {
            &nullable.suffix
        } else {
            &self.for_json.suffix
        };
        if let Some(collection) = &self.collection {
            let c_init = &collection.init();
            let ws = DEFAULT_INDENT;

            if collection.nullable {
                parts.push(add_indent(
                    formatdoc! {"
                        if is_null(\"{name}\"):
                        {ws}{dest_specifier} = null
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
            self.render_for_json_item_collection(
                collection,
                0,
                &name,
                &dest_specifier,
                indent_level,
                &mut parts,
                indent,
            );
        } else {
            let for_json_value = self.specifier_for_json(name, for_json_call, maybe_suffix);
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
    pub partial_deep: bool,
    pub var_descriptors: Vec<ModelVarDescriptor>,
    pub state_vars: String,
    pub state_methods: String,
}

#[cfg(test)]
pub mod tests {
    use convert_case::{Case, Casing};
    use deno_ast::swc::ast::Id;
    use indoc::{formatdoc, indoc};

    use crate::model_context::{ModelVarCollection, DEFAULT_INDENT};

    use super::{
        add_indent, is_builtin, ModelImportContext, ModelValueCtor, ModelValueForJson,
        ModelVarDescriptor,
    };

    const ws: &'static str = DEFAULT_INDENT;

    impl ModelVarDescriptor {
        fn for_test(src_name: &str, optional: bool, type_name: &str, type_nullable: bool) -> Self {
            ModelVarDescriptor {
                name: src_name.to_string().to_case(Case::Snake),
                src_name: src_name.to_string(),
                comment: Some("comment".to_string()),
                decl_type: if type_name != "any" {
                    Some(type_name.to_string())
                } else {
                    None
                },
                decl_init: None,
                ctor: ModelValueCtor::new(type_name, type_nullable),
                for_json: ModelValueForJson::new(type_name, type_nullable),
                collection: None,
                optional: optional,
                non_nullable: is_builtin(type_name),
                imports: Vec::new(),
                enums: Vec::new(),
            }
        }
    }

    pub fn indent(str: String) -> String {
        add_indent(str, DEFAULT_INDENT, 1)
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
            indent(formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true\n\
                {ws}prop_name = Intf.new(src.propName, __partial_deep)\
            "})
        );
    }

    #[test]
    fn render_init_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", true);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null\n\
                {ws}prop_name = Intf.new(src.propName, __partial_deep) if typeof(src.propName) != TYPE_NIL else null\
            "})
        );
    }
    // if the target is non nullable then don't add any null checking
    // it should be an error if the src is null
    fn render_init_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "String", false);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true\n\
                {ws}prop_name = src.propName\
            "})
        );
    }

    #[test]
    // Sometimes the typescript type is nullable but the gdscript type is not
    fn render_init_nullable_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "String", true);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null\n\
                {ws}prop_name = src.propName if typeof(src.propName) != TYPE_NIL else \"\"\
            "})
        );
    }

    #[test]
    fn render_init_optional() {
        let model = ModelVarDescriptor::for_test("propName", true, "Intf", false);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if \"propName\" in src:
                {ws}__assigned_properties.prop_name = true
                {ws}prop_name = Intf.new(src.propName, __partial_deep)\
            "})
        );
    }

    #[test]
    fn render_init_optional_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "Intf", true);
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = Intf.new(src.propName, __partial_deep) if typeof(src.propName) != TYPE_NIL else null\
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
            indent(formatdoc! {"
                if \"propName\" in src:
                {ws}__assigned_properties.prop_name = true
                {ws}prop_name = src.propName\
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
            indent(formatdoc! {"
                if \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = src.propName if typeof(src.propName) != TYPE_NIL else \"\"\
            "})
        );
    }

    #[test]
    fn render_init_array() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            is_array: true,
            is_dict: false,
            nullable: false,
            item_collection: None,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true
                {ws}prop_name = []
                {ws}for __item__ in src.propName:
                {ws}{ws}var __value__ = Intf.new(__item__, __partial_deep)
                {ws}{ws}prop_name.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_init_dict() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: false,
            item_collection: None,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true
                {ws}prop_name = {{}}
                {ws}for __key__ in src.propName:
                {ws}{ws}var __value__ = src.propName[__key__]
                {ws}{ws}prop_name[__key__] = Intf.new(__value__, __partial_deep)\
            "})
        );
    }

    #[test]
    fn render_init_nullable_array() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            is_array: true,
            is_dict: false,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = []
                {ws}if typeof(src.propName) != TYPE_NIL:
                {ws}{ws}for __item__ in src.propName:
                {ws}{ws}{ws}var __value__ = Intf.new(__item__, __partial_deep)
                {ws}{ws}{ws}prop_name.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_init_nullable_dict() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = {{}}
                {ws}if typeof(src.propName) != TYPE_NIL:
                {ws}{ws}for __key__ in src.propName:
                {ws}{ws}{ws}var __value__ = src.propName[__key__]
                {ws}{ws}{ws}prop_name[__key__] = Intf.new(__value__, __partial_deep)\
            "})
        );
    }

    #[test]
    fn render_init_nullable_array_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", true);
        model.collection = Some(ModelVarCollection {
            is_array: true,
            is_dict: false,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = []
                {ws}if typeof(src.propName) != TYPE_NIL:
                {ws}{ws}for __item__ in src.propName:
                {ws}{ws}{ws}var __value__ = Intf.new(__item__, __partial_deep) if typeof(__item__) != TYPE_NIL else null
                {ws}{ws}{ws}prop_name.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_init_nullable_dict_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", true);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = {{}}
                {ws}if typeof(src.propName) != TYPE_NIL:
                {ws}{ws}for __key__ in src.propName:
                {ws}{ws}{ws}var __value__ = src.propName[__key__]
                {ws}{ws}{ws}prop_name[__key__] = Intf.new(__value__, __partial_deep) if typeof(__value__) != TYPE_NIL else null\
            "})
        );
    }

    #[test]
    fn render_init_optional_nullable_array_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "Intf", true);
        model.collection = Some(ModelVarCollection {
            is_array: true,
            is_dict: false,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = []
                {ws}if typeof(src.propName) != TYPE_NIL:
                {ws}{ws}for __item__ in src.propName:
                {ws}{ws}{ws}var __value__ = Intf.new(__item__, __partial_deep) if typeof(__item__) != TYPE_NIL else null
                {ws}{ws}{ws}prop_name.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_init_optional_nullable_dict_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "Intf", true);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = {{}}
                {ws}if typeof(src.propName) != TYPE_NIL:
                {ws}{ws}for __key__ in src.propName:
                {ws}{ws}{ws}var __value__ = src.propName[__key__]
                {ws}{ws}{ws}prop_name[__key__] = Intf.new(__value__, __partial_deep) if typeof(__value__) != TYPE_NIL else null\
            "})
        );
    }

    // non-nullable nullables can be null when they are added to an array
    // (unless we start supporting PoolStringArray etc, then we have to deoptimize for this case)
    #[test]
    fn render_init_optional_nullable_array_of_builtin_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "int", true);
        model.collection = Some(ModelVarCollection {
            is_array: true,
            is_dict: false,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = []
                {ws}if typeof(src.propName) != TYPE_NIL:
                {ws}{ws}for __item__ in src.propName:
                {ws}{ws}{ws}var __value__ = __item__ if typeof(__item__) != TYPE_NIL else null
                {ws}{ws}{ws}prop_name.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_init_optional_nullable_dict_of_builtin_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "int", true);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = {{}}
                {ws}if typeof(src.propName) != TYPE_NIL:
                {ws}{ws}for __key__ in src.propName:
                {ws}{ws}{ws}var __value__ = src.propName[__key__]
                {ws}{ws}{ws}prop_name[__key__] = __value__ if typeof(__value__) != TYPE_NIL else null\
            "})
        );
    }

    #[test]
    fn render_for_json() {
        let model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}result.propName = prop_name.for_json()\
            "})
        );
    }

    #[test]
    fn render_for_json_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", true);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}result.propName = prop_name.for_json() if typeof(prop_name) != TYPE_NIL else null\
            "})
        );
    }
    // if the target is non nullable then don't add any null checking
    // it should be an error if the src is null
    fn render_for_json_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "String", false);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}result.propName = prop_name\
            "})
        );
    }

    #[test]
    // Sometimes the typescript type is nullable but the gdscript type is not
    fn render_for_json_nullable_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "String", true);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}result.propName = prop_name if !is_null(\"prop_name\") else null\
            "})
        );
    }

    #[test]
    fn render_for_json_optional() {
        let model = ModelVarDescriptor::for_test("propName", true, "Intf", false);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if is_set(\"prop_name\"):
                {ws}result.propName = prop_name.for_json()\
            "})
        );
    }

    #[test]
    fn render_for_json_optional_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "Intf", true);
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if is_set(\"prop_name\"):
                {ws}result.propName = prop_name.for_json() if typeof(prop_name) != TYPE_NIL else null\
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
            indent(formatdoc! {"
                if is_set(\"prop_name\"):
                {ws}result.propName = prop_name\
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
            indent(formatdoc! {"
                if is_set(\"prop_name\"):
                {ws}result.propName = prop_name if !is_null(\"prop_name\") else null\
            "})
        );
    }

    #[test]
    fn render_for_json_array() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            is_array: true,
            is_dict: false,
            nullable: false,
            item_collection: None,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}result.propName = []
                {ws}for __item__ in prop_name:
                {ws}{ws}var __value__ = __item__.for_json()
                {ws}{ws}result.propName.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_for_json_dict() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: false,
            item_collection: None,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}result.propName = {{}}
                {ws}for __key__ in prop_name:
                {ws}{ws}var __value__ = prop_name[__key__]
                {ws}{ws}result.propName[__key__] = __value__.for_json()\
            "})
        );
    }

    #[test]
    fn render_for_json_nullable_array() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            is_array: true,
            is_dict: false,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}if is_null(\"prop_name\"):
                {ws}{ws}result.propName = null
                {ws}else:
                {ws}{ws}result.propName = []
                {ws}{ws}for __item__ in prop_name:
                {ws}{ws}{ws}var __value__ = __item__.for_json()
                {ws}{ws}{ws}result.propName.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_for_json_nullable_dict() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", false);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}if is_null(\"prop_name\"):
                {ws}{ws}result.propName = null
                {ws}else:
                {ws}{ws}result.propName = {{}}
                {ws}{ws}for __key__ in prop_name:
                {ws}{ws}{ws}var __value__ = prop_name[__key__]
                {ws}{ws}{ws}result.propName[__key__] = __value__.for_json()\
            "})
        );
    }

    #[test]
    fn render_for_json_nullable_array_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", true);
        model.collection = Some(ModelVarCollection {
            is_array: true,
            is_dict: false,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}if is_null(\"prop_name\"):
                {ws}{ws}result.propName = null
                {ws}else:
                {ws}{ws}result.propName = []
                {ws}{ws}for __item__ in prop_name:
                {ws}{ws}{ws}var __value__ = __item__.for_json() if typeof(__item__) != TYPE_NIL else null
                {ws}{ws}{ws}result.propName.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_for_json_nullable_dict_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "Intf", true);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}if is_null(\"prop_name\"):
                {ws}{ws}result.propName = null
                {ws}else:
                {ws}{ws}result.propName = {{}}
                {ws}{ws}for __key__ in prop_name:
                {ws}{ws}{ws}var __value__ = prop_name[__key__]
                {ws}{ws}{ws}result.propName[__key__] = __value__.for_json() if typeof(__value__) != TYPE_NIL else null\
            "})
        );
    }

    #[test]
    fn render_for_json_optional_nullable_array_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "Intf", true);
        model.collection = Some(ModelVarCollection {
            is_array: true,
            is_dict: false,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if is_set(\"prop_name\"):
                {ws}if is_null(\"prop_name\"):
                {ws}{ws}result.propName = null
                {ws}else:
                {ws}{ws}result.propName = []
                {ws}{ws}for __item__ in prop_name:
                {ws}{ws}{ws}var __value__ = __item__.for_json() if typeof(__item__) != TYPE_NIL else null
                {ws}{ws}{ws}result.propName.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_for_json_optional_nullable_dict_of_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "Intf", true);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if is_set(\"prop_name\"):
                {ws}if is_null(\"prop_name\"):
                {ws}{ws}result.propName = null
                {ws}else:
                {ws}{ws}result.propName = {{}}
                {ws}{ws}for __key__ in prop_name:
                {ws}{ws}{ws}var __value__ = prop_name[__key__]
                {ws}{ws}{ws}result.propName[__key__] = __value__.for_json() if typeof(__value__) != TYPE_NIL else null\
            "})
        );
    }

    // non-nullable nullables can be null when they are added to an array
    // (unless we start supporting PoolStringArray etc, then we have to deoptimize for this case)
    #[test]
    fn render_for_json_optional_nullable_array_of_builtin_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "int", true);
        model.collection = Some(ModelVarCollection {
            is_array: true,
            is_dict: false,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if is_set(\"prop_name\"):
                {ws}if is_null(\"prop_name\"):
                {ws}{ws}result.propName = null
                {ws}else:
                {ws}{ws}result.propName = []
                {ws}{ws}for __item__ in prop_name:
                {ws}{ws}{ws}var __value__ = __item__ if typeof(__item__) != TYPE_NIL else null
                {ws}{ws}{ws}result.propName.append(__value__)\
            "})
        );
    }

    #[test]
    fn render_for_json_optional_nullable_dict_of_builtin_nullable() {
        let mut model = ModelVarDescriptor::for_test("propName", true, "int", true);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: true,
            item_collection: None,
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if is_set(\"prop_name\"):
                {ws}if is_null(\"prop_name\"):
                {ws}{ws}result.propName = null
                {ws}else:
                {ws}{ws}result.propName = {{}}
                {ws}{ws}for __key__ in prop_name:
                {ws}{ws}{ws}var __value__ = prop_name[__key__]
                {ws}{ws}{ws}result.propName[__key__] = __value__ if typeof(__value__) != TYPE_NIL else null\
            "})
        );
    }

    #[test]
    fn render_init_dict_of_arrays() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "B", true);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: true,
            item_collection: Some(Box::new(ModelVarCollection {
                is_array: true,
                is_dict: false,
                nullable: true,
                item_collection: None,
            })),
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(
                formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = {{}}
                {ws}if typeof(src.propName) != TYPE_NIL:
                {ws}{ws}for __key__ in src.propName:
                {ws}{ws}{ws}var __value__ = src.propName[__key__]
                {ws}{ws}{ws}var __coll__ = []
                {ws}{ws}{ws}prop_name[__key__] = __coll__
                {ws}{ws}{ws}for __item__1 in __value__:
                {ws}{ws}{ws}{ws}var __value__1 = B.new(__item__1, __partial_deep) if typeof(__item__1) != TYPE_NIL else null
                {ws}{ws}{ws}{ws}__coll__.append(__value__1)\
                "}
                .to_string()
            )
        );
    }

    #[test]
    fn render_init_array_of_dicts() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "B", true);
        model.collection = Some(ModelVarCollection {
            is_array: true,
            is_dict: false,
            nullable: true,
            item_collection: Some(Box::new(ModelVarCollection {
                is_array: false,
                is_dict: true,
                nullable: true,
                item_collection: None,
            })),
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(
                formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = []
                {ws}if typeof(src.propName) != TYPE_NIL:
                {ws}{ws}for __item__ in src.propName:
                {ws}{ws}{ws}var __coll__ = {{}}
                {ws}{ws}{ws}prop_name.append(__coll__)
                {ws}{ws}{ws}for __key__1 in __item__:
                {ws}{ws}{ws}{ws}var __value__1 = __item__[__key__1]
                {ws}{ws}{ws}{ws}__coll__[__key__1] = B.new(__value__1, __partial_deep) if typeof(__value__1) != TYPE_NIL else null\
                "}
                .to_string()
            )
        );
    }

    #[test]
    fn render_init_dict_of_arrays_of_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "int", true);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: true,
            item_collection: Some(Box::new(ModelVarCollection {
                is_array: true,
                is_dict: false,
                nullable: true,
                item_collection: None,
            })),
        });
        let rendered = model.render_init(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(
                formatdoc! {"
                if !__partial_deep || \"propName\" in src:
                {ws}__assigned_properties.prop_name = true if typeof(src.propName) != TYPE_NIL else null
                {ws}prop_name = {{}}
                {ws}if typeof(src.propName) != TYPE_NIL:
                {ws}{ws}for __key__ in src.propName:
                {ws}{ws}{ws}var __value__ = src.propName[__key__]
                {ws}{ws}{ws}var __coll__ = []
                {ws}{ws}{ws}prop_name[__key__] = __coll__
                {ws}{ws}{ws}for __item__1 in __value__:
                {ws}{ws}{ws}{ws}var __value__1 = __item__1 if typeof(__item__1) != TYPE_NIL else null
                {ws}{ws}{ws}{ws}__coll__.append(__value__1)\
                "}
                .to_string()
            )
        );
    }

    #[test]
    fn render_for_json_dict_of_arrays() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "B", true);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: false,
            item_collection: Some(Box::new(ModelVarCollection {
                is_array: true,
                is_dict: false,
                nullable: true,
                item_collection: None,
            })),
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}result.propName = {{}}
                {ws}for __key__ in prop_name:
                {ws}{ws}var __value__ = prop_name[__key__]
                {ws}{ws}var __coll__ = []
                {ws}{ws}result.propName[__key__] = __coll__
                {ws}{ws}for __item__1 in __value__:
                {ws}{ws}{ws}var __value__1 = __item__1.for_json() if typeof(__item__1) != TYPE_NIL else null
                {ws}{ws}{ws}__coll__.append(__value__1)\
            "})
        );
    }

    #[test]
    fn render_for_json_array_of_dicts() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "B", true);
        model.collection = Some(ModelVarCollection {
            is_array: true,
            is_dict: false,
            nullable: false,
            item_collection: Some(Box::new(ModelVarCollection {
                is_array: false,
                is_dict: true,
                nullable: true,
                item_collection: None,
            })),
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}result.propName = []
                {ws}for __item__ in prop_name:
                {ws}{ws}var __coll__ = {{}}
                {ws}{ws}result.propName.append(__coll__)
                {ws}{ws}for __key__1 in __item__:
                {ws}{ws}{ws}var __value__1 = __item__[__key__1]
                {ws}{ws}{ws}__coll__[__key__1] = __value__1.for_json() if typeof(__value__1) != TYPE_NIL else null\
            "})
        );
    }

    #[test]
    fn render_for_json_dict_of_arrays_of_builtin() {
        let mut model = ModelVarDescriptor::for_test("propName", false, "int", true);
        model.collection = Some(ModelVarCollection {
            is_array: false,
            is_dict: true,
            nullable: false,
            item_collection: Some(Box::new(ModelVarCollection {
                is_array: true,
                is_dict: false,
                nullable: true,
                item_collection: None,
            })),
        });
        let rendered = model.render_for_json(DEFAULT_INDENT);
        assert_eq!(
            rendered,
            indent(formatdoc! {"
                if !__partial_deep || is_set(\"prop_name\"):
                {ws}result.propName = {{}}
                {ws}for __key__ in prop_name:
                {ws}{ws}var __value__ = prop_name[__key__]
                {ws}{ws}var __coll__ = []
                {ws}{ws}result.propName[__key__] = __coll__
                {ws}{ws}for __item__1 in __value__:
                {ws}{ws}{ws}var __value__1 = __item__1 if typeof(__item__1) != TYPE_NIL else null
                {ws}{ws}{ws}__coll__.append(__value__1)\
            "})
        );
    }
}
