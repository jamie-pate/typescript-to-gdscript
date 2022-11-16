#![allow(warnings)]

use convert_case::Case;
use convert_case::Casing;
use deno_ast::parse_module;
use deno_ast::swc::ast::Decl;
use deno_ast::swc::ast::Expr;
use deno_ast::swc::ast::Id;
use deno_ast::swc::ast::Ident;
use deno_ast::swc::ast::Import;
use deno_ast::swc::ast::ImportDecl;
use deno_ast::swc::ast::ImportNamedSpecifier;
use deno_ast::swc::ast::ImportSpecifier;
use deno_ast::swc::ast::Module;
use deno_ast::swc::ast::ModuleDecl;
use deno_ast::swc::ast::ModuleItem;
use deno_ast::swc::ast::TsInterfaceDecl;
use deno_ast::MediaType;
use deno_ast::ParseParams;
use deno_ast::ParsedSource;
use deno_ast::SourceTextInfo;

use deno_ast::swc::ast::TsKeywordType;
use deno_ast::swc::ast::TsKeywordTypeKind;
use deno_ast::swc::ast::TsPropertySignature;
use deno_ast::swc::ast::TsType;
use deno_ast::swc::ast::TsTypeElement;
use deno_ast::swc::ast::TsTypeRef;
use deno_ast::swc::ast::TsUnionOrIntersectionType;
use deno_ast::swc::atoms::Atom;
use deno_ast::swc::common::serializer::Type;
use model_context::ModelContext;
use model_context::ModelValueCtor;
use model_context::ModelVarInit;
use tinytemplate::format_unescaped;
use tinytemplate::TinyTemplate;

use std::any::Any;
use std::collections::HashMap;
use std::env::args;
use std::env::current_dir;
use std::env::current_exe;
use std::ffi::OsStr;
use std::fs::create_dir_all;
use std::fs::read_to_string;
use std::fs::write;
use std::path;
use std::path::Path;
use std::path::PathBuf;
use std::process::exit;

use crate::model_context::ModelVarCollection;
//use std::fs::File;

const TEMPLATE_NAME: &str = "gdscript-model";
pub mod model_context;

fn main() {
    let mut debug_print: bool = false;
    let mut output_dir: Option<PathBuf> = None;
    let mut template_filename: Option<PathBuf> = None;
    let mut template_str: Option<String> = None;
    let mut src_files: Vec<PathBuf> = Vec::new();

    for arg in args().skip(1) {
        // TODO: in parallel!
        if arg == "--debug-print" {
            debug_print = true;
        } else if arg.starts_with("--") {
            eprintln!("Unexpected flag {}", arg);
            usage();
        } else if template_filename == None {
            template_filename = Some(PathBuf::from(arg));
        } else if output_dir == None {
            output_dir = Some(PathBuf::from(arg));
        } else {
            src_files.push(PathBuf::from(arg));
        }
    }
    if let (Some(output_dir), Some(template_filename)) = (output_dir, template_filename) {
        let template_str = read_to_string(&template_filename).expect(&format!(
            "Failed to load template file {:?}",
            &template_filename
        ));
        let mut template = TinyTemplate::new();
        template
            .add_template(TEMPLATE_NAME, &template_str)
            .expect(&format!(
                "Unable to compile template {:?}",
                &template_filename
            ));
        template.set_default_formatter(&format_unescaped);

        if src_files.len() == 0 {
            eprintln!("No files processed");
            usage();
            exit(1);
        }
        create_dir_all(&output_dir).expect(&format!("Creating directory {:?}", &output_dir));
        let mut imports: HashMap<PathBuf, ParsedSource> = HashMap::new();
        let directory = current_dir().expect("current_dir failed somehow");
        for filename in src_files.iter() {
            let canonical_filepath = canonical_module_filename(&directory, &filename);
            parse_all_imports(&mut imports, &canonical_filepath, debug_print);
        }
        for filename in src_files.iter() {
            let canonical_filepath = canonical_module_filename(&directory, &filename);
            convert(
                &imports,
                &output_dir,
                &canonical_filepath,
                &template_filename,
                &template,
                debug_print,
            );
        }
    } else {
        usage();
        exit(1);
    }
}

fn usage() {
    eprintln!(
        "Usage: {} [--debug-print] templatefile.gd.tmpl outputdir input1.ts [input2.ts...]",
        exe_name().expect("weird, exe name could not be retrieved")
    );
    eprintln!("Reads all interfaces from input.ts files exports them to outputdir/<InterfaceName>.gd files.");
    eprintln!("Uses templatefile.gd.tmpl as the template.");
}

fn exe_name() -> Option<String> {
    return current_exe().ok()?.file_name()?.to_str()?.to_owned().into();
}

fn read_and_parse_file(filename: &Path) -> ParsedSource {
    let source_text =
        read_to_string(filename).expect(&format!("Unable to read file {:?}", filename));
    let text_info = SourceTextInfo::new(source_text.into());
    return parse_module(ParseParams {
        specifier: String::from(
            filename
                .to_str()
                .expect(&format!("filename invalid {:?}", filename)),
        ),
        media_type: MediaType::TypeScript,
        text_info,
        capture_tokens: true,
        maybe_syntax: None,
        scope_analysis: false,
    })
    .expect("Parsing Failed");
}

fn canonical_module_filename<'a>(directory: &Path, filename: &'a Path) -> PathBuf {
    // poor mans 'module resolution'. Just assume every module is a ts file.
    // might need to actually resolve modules later?

    let mut result = filename.to_path_buf();
    if let Some(ext) = result.extension() {
        if ext == "js" {
            result = filename.with_extension("");
        }
    }
    result = result.with_extension("ts");

    if !result.has_root() {
        result = directory.join(result.strip_prefix("./").unwrap_or(&result));
    }
    return result.with_extension("ts");
}

fn parse_all_imports(
    imports: &mut HashMap<PathBuf, ParsedSource>,
    canonical_filepath: &Path,
    debug_print: bool,
) {
    let parsed_source = read_and_parse_file(&canonical_filepath);
    // TODO: preload and cache this and pass it to convert()
    let directory = canonical_filepath
        .parent()
        .expect(&format!("invalid filename {:?}", canonical_filepath));
    let module = parsed_source.module();
    for node in module.body.iter() {
        if let ModuleItem::ModuleDecl(ModuleDecl::Import(import)) = node {
            let m_import_src = PathBuf::from(import.src.value.to_string());
            let m_filename = canonical_module_filename(directory, &m_import_src);
            parse_all_imports(imports, &m_filename, debug_print);
        }
    }
    imports.insert(canonical_filepath.to_path_buf(), parsed_source);
    if debug_print {
        dbg!("parsed imports from", &canonical_filepath);
    }
}

struct ModuleContext<'a> {
    stack: &'a mut Vec<String>,
    debug_print: bool,
    canonical_filepath: &'a Path,
    imports: &'a HashMap<PathBuf, ParsedSource>,
    import_specifiers: &'a mut HashMap<Id, PathBuf>,
}
fn convert(
    imports: &HashMap<PathBuf, ParsedSource>,
    output_dir: &Path,
    canonical_filepath: &Path,
    template_filename: &Path,
    template: &TinyTemplate,
    debug_print: bool,
) {
    let parsed_source = imports.get(canonical_filepath).expect(&format!(
        "Module {:?} is missing but should previously have been parsed",
        canonical_filepath
    ));
    let module = &parsed_source.module();
    let mut import_specifiers: HashMap<Id, PathBuf> = HashMap::new();
    let directory = canonical_filepath.parent().expect("filepath is an orphan");
    let mut models: Vec<ModelContext> = Vec::new();

    for node in module.body.iter() {
        if let ModuleItem::ModuleDecl(ModuleDecl::Import(import)) = node {
            let m_import_src = PathBuf::from(import.src.value.to_string());
            let m_filename = canonical_module_filename(directory, &m_import_src);
            for s in &import.specifiers[..] {
                if let ImportSpecifier::Named(ns) = s {
                    import_specifiers.insert(ns.local.to_id(), canonical_filepath.to_path_buf());
                }
            }
        }
    }
    let mut stack: Vec<String> = Vec::new();
    let mut context = ModuleContext {
        stack: &mut stack,
        debug_print,
        canonical_filepath,
        imports,
        import_specifiers: &mut import_specifiers,
    };
    for node in module.body.iter() {
        if let ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) = node {
            let decl = &export.decl;
            match decl {
                Decl::TsInterface(intf) => {
                    models.push(get_intf_model(&mut context, &intf));
                }
                _ => (),
            }
        }
    }
    for model in models.into_iter() {
        let output_filepath = output_dir.join(&model.class_name).with_extension("gd");
        let file_path = model.canonical_src_filepath.to_path_buf();
        let output = template.render(TEMPLATE_NAME, &model).expect(&format!(
            "Unable to render template {:?} -> {:?}",
            &file_path, &output_filepath
        ));
        write(&output_filepath, output)
            .expect(&format!("Unable to write file {:?}", &output_filepath));
        println!("{:?}", &output_filepath);
    }
}

fn get_intf_model(context: &mut ModuleContext, intf: &TsInterfaceDecl) -> ModelContext {
    let (symbol, _tag) = intf.id.to_id();
    ModelContext {
        class_name: String::from(&*symbol),
        canonical_src_filepath: context.canonical_filepath.to_path_buf(),
        comment: None,
        src_type: Some(model_context::ModelSrcType {
            name: "Dictionary".to_string(),
            init: Some("{}".to_string()),
            array_item_ctor: None,
        }),
        imports: Vec::new(),
        vars: intf
            .body
            .body
            .iter()
            .map(|e| get_intf_model_var(context, e))
            // flatten filters out None!
            .flatten()
            .collect(),
    }
}

struct TypeResolution {
    name: Option<String>,
    ctor: Option<ModelValueCtor>,
    collection: Option<ModelVarCollection>,
    comment: Option<String>,
}

impl From<&str> for TypeResolution {
    fn from(name: &str) -> Self {
        TypeResolution {
            name: Some(String::from(name)),
            ctor: None,
            collection: None,
            comment: None,
        }
    }
}

fn resolve_type_ref(context: &mut ModuleContext, type_ref: &TsTypeRef) -> TypeResolution {
    if let Some(ident) = &type_ref.type_name.as_ident() {
        let id = ident.to_id();
        let (symbol, _tag) = id.clone();
        let sstr = &*symbol;
        let resolved_type = resolve_specifier_type(&context, id);
        if let (Some(t), ctor, collection) = resolved_type {
            TypeResolution::from(t.as_str())
        } else {
            match (sstr) {
                // lazy matchup.. should probably try to resolve these in imports first
                "Record" => {
                    let mut result = TypeResolution::from("Dictionary");
                    result.collection = Some(ModelVarCollection {
                        init: "{}".to_string(),
                        is_array: false,
                        is_dict: true,
                    });
                    result
                }
                s => TypeResolution::from(s),
            }
        }
    } else {
        panic!(
            "IDK what to do with {:#?} {:#?}",
            type_ref.type_name, &context.stack
        );
    }
}

// type name, ctor, collection, comment
fn resolve_type(context: &mut ModuleContext, ts_type: &TsType) -> TypeResolution {
    match (ts_type) {
        TsType::TsTypeRef(type_ref) => {
            context
                .stack
                .push(format!("resolve type_ref {:?}", type_ref));
            let result = resolve_type_ref(context, &type_ref);
            context.stack.pop();
            result
        }
        TsType::TsArrayType(array_type) => {
            context
                .stack
                .push(format!("resolve array_type {:?}", array_type));
            let mut result = resolve_type(context, &array_type.elem_type);
            result.collection = Some(ModelVarCollection {
                init: "[]".to_string(),
                is_array: true,
                is_dict: false,
            });
            context.stack.pop();
            result
        }
        TsType::TsKeywordType(kw_type) => {
            context
                .stack
                .push(format!("resolve keyword_type {:?}", kw_type));
            let result = match (kw_type.kind) {
                TsKeywordTypeKind::TsNumberKeyword => TypeResolution::from("float"),
                // Godot 3 uses 64 bit ints in 64 bit builds but probably we shouldn't use this type anyways, idk
                TsKeywordTypeKind::TsBigIntKeyword => TypeResolution::from("int"),
                TsKeywordTypeKind::TsBooleanKeyword => TypeResolution::from("bool"),
                TsKeywordTypeKind::TsStringKeyword => TypeResolution::from("String"),
                kind => panic!(
                    "IDK what to do with keyword type {:#?} {:#?}",
                    kind, context.stack
                ),
            };
            context.stack.pop();
            result
        }
        TsType::TsUnionOrIntersectionType(ui_type) => {
            context
                .stack
                .push(format!("resolve union_or_intersection_type {:?}", ui_type));
            let result = match ui_type {
                TsUnionOrIntersectionType::TsUnionType(union_type) => {
                    if (union_type.types.len() != 2) {
                        panic!("Only two types allowed in a union type (the 2nd one must be null) {:#?}", context.stack);
                    }
                    let mut it = union_type.types.iter();
                    let (a, b) = (it.next().unwrap(), it.next().unwrap());
                    context.stack.push(format!("resolve union_type {:?}", a));
                    let mut result = resolve_type(context, &a);
                    result.comment = Some(String::from("Nullable!"));
                    context.stack.pop();
                    result
                }
                TsUnionOrIntersectionType::TsIntersectionType(_) => {
                    panic!("Intersection types are not allowed {:#?}", context.stack);
                }
            };
            context.stack.pop();
            result
        }
        _ => panic!(
            "IDK what to do with this type {:#?} {:#?}",
            ts_type, context.stack
        ),
    }
}

fn get_intf_model_var(
    context: &mut ModuleContext,
    type_element: &TsTypeElement,
) -> Option<ModelVarInit> {
    if let TsTypeElement::TsPropertySignature(prop_sig) = type_element {
        let src_name = if let Expr::Ident(id) = &*prop_sig.key {
            Some(id.sym.to_string())
        } else {
            None
        };

        if context.debug_print {
            dbg!(&src_name);
        }
        let type_ann = prop_sig
            .type_ann
            .as_ref()
            .expect(&format!("IDK what to do with this type {:#?}", prop_sig));
        let empty = String::from("");
        context.stack.push(format!(
            "get_intf_model_var {:?}",
            &src_name.as_ref().unwrap_or(&empty)
        ));
        let resolved = resolve_type(context, &*type_ann.type_ann);
        context.stack.pop();

        if let (Some(src_name), Some(type_name)) = (src_name, resolved.name) {
            //  = prop_sig.type_ann?.type_ann;
            return Some(model_context::ModelVarInit {
                name: src_name.to_case(Case::Snake),
                decl_type: type_name,
                decl_init: None,
                src_name,
                ctor: resolved.ctor.unwrap_or(ModelValueCtor {
                    start: "".to_string(),
                    end: "".to_string(),
                }),
                collection: resolved.collection,
            });
        }
    }
    if context.debug_print {
        dbg!(type_element);
    }
    return None;
}

// TODO: resolve specifiers across modules until we find one that has @typescript-to-gdscript-type: int or something?
// might need a tuple or something with more details like ctor etc
fn resolve_specifier_type(
    context: &ModuleContext,
    id: Id,
) -> (
    Option<String>,
    Option<ModelValueCtor>,
    Option<ModelVarCollection>,
) {
    // TsInterfaceDecl?
    return (None, None, None);
}
