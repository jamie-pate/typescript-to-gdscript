use std::{collections::HashSet, path::PathBuf};

use serde::Serialize;

#[derive(Serialize)]
pub struct ModelImportContext {
    pub name: String,
    pub src: String,
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

impl ModelValueCtor {
    pub fn new(name: &str) -> Self {
        ModelValueCtor::new_(name, false)
    }
    pub fn nullable(name: &str) -> Self {
        ModelValueCtor::new_(name, true)
    }
    // todo: lazy static?
    pub fn empty() -> Self {
        ModelValueCtor::new_("", false)
    }
    fn new_(name: &str, nullable: bool) -> Self {
        let parens = name != "";
        let builtin = BUILTINS.contains(name);
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
            suffix: if !nullable {
                None
            } else {
                Some(" != null else null".to_string())
            },
        }
    }
    pub fn set_nullable(&mut self) {
        if self.suffix.is_some() {
            panic!("Don't want to overwrite ctor suffix{:#?}", &self);
        }
        self.end = format!("{} if ", self.end);
        self.suffix = Some(" != null else null".to_string())
    }

    pub fn rename(&self, name: &str) -> Self {
        ModelValueCtor::new_(name, self.suffix.is_some())
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
    // maybe a collection
    pub collection: Option<ModelVarCollection>,
    // if the whole thing is optional
    pub optional: bool,
}

#[derive(Serialize)]
pub struct ModelSrcType {
    pub name: String,
    pub init: Option<String>,
    // if this is present then the source data is an array type.
    // this ctor expression will be used to create each item.
    pub array_item_ctor: Option<ModelValueCtor>,
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
    pub vars: Vec<ModelVarInit>,
}
