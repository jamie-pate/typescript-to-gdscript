use std::path::PathBuf;

use serde::Serialize;

#[derive(Serialize)]
pub struct ModelImportContext {
    name: String,
    src: String,
}

// Extra template data that's required if the variable is an array or dictionary
#[derive(Serialize)]
pub struct ModelVarCollection {
    // Initial value (empty array, dict, PoolStringArray, etc)
    pub init: String,
    pub is_array: bool,
    pub is_dict: bool,
}

// usage: `{ctor.start}__value__{ctor.end} in the template
#[derive(Serialize)]
pub struct ModelValueCtor {
    pub start: String,
    pub end: String,
}

#[derive(Serialize)]
pub struct ModelVarInit {
    // name of the variable in the gdscript object (camel_case)
    pub name: String,
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
