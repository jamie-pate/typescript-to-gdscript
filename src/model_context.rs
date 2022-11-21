use std::{collections::HashSet, path::PathBuf};

use serde::Serialize;

#[derive(Serialize)]
pub struct ModelImportContext {
    pub name: String,
    pub src: String,
    pub gd_impl: bool,
}

// Extra template data that's required if the variable is an array or dictionary
#[derive(Serialize, Debug)]
pub struct ModelVarCollection {
    // Initial value (empty array, dict, PoolStringArray, etc)
    pub init: String,
    pub is_array: bool,
    pub is_dict: bool,
}

lazy_static! {
    static ref BUILTINS: HashSet<&'static str> = {
        let mut b = HashSet::new();
        for builtin in ["", "Array", "Dictionary"] {
            b.insert(builtin);
        }
        b
    };
}

pub fn is_builtin(name: &str) -> bool {
    BUILTINS.contains(name)
}

impl ModelValueCtor {
    pub fn empty(nullable: bool) -> Self {
        ModelValueCtor::new("", nullable)
    }
    pub fn new(name: &str, nullable: bool) -> Self {
        let parens = name != "";
        let builtin = is_builtin(name);
        // TODO: add other builtin conversions from json here
        let new_str = if !builtin { ".new" } else { "" };
        let (lparen_str, rparen_str) = if parens { ("(", ")") } else { ("", "") };
        ModelValueCtor {
            name: name.to_string(),
            builtin,
            start: format!("{}{}{}", name, new_str, lparen_str),
            end: if !nullable {
                rparen_str.to_string()
            } else {
                format!("{} if ", rparen_str)
            },
            suffix: if nullable {
                Some(" != null else null".to_string())
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
        let for_json_str = if !builtin { ".for_json()" } else { "" }.to_string();
        ModelValueForJson {
            name: name.to_string(),
            builtin,
            start: String::from(""),
            end: if nullable { format!("{} if ", for_json_str) } else { for_json_str },
            suffix: if nullable {
                Some(" != null else null".to_string())
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
    pub start: String,
    pub end: String,
    pub suffix: Option<String>,
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

#[derive(Serialize)]
pub struct ModelVarInit {
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
}

#[derive(Serialize)]
pub struct ModelSrcType {
    pub name: String,
    pub init: Option<String>,
}

#[derive(Serialize)]
pub struct ArrayItemInit {
    pub ctor: String,
}

#[derive(Serialize)]
pub struct ModelContext {
    pub class_name: String,
    pub canonical_src_filepath: PathBuf,
    // Dictionary | Array
    pub comment: Option<String>,
    pub src_type: Option<ModelSrcType>,
    pub imports: Vec<ModelImportContext>,
    pub enums: Vec<ModelEnum>,
    pub vars: Vec<ModelVarInit>,
}
