#![allow(warnings)]
#[macro_use]
extern crate lazy_static;

use convert_case::Case;
use convert_case::Casing;
use deno_ast::parse_module;
use deno_ast::{
    swc::ast::{
        Decl, Expr, Id, Ident, Import, ImportDecl, ImportNamedSpecifier, ImportSpecifier, Module,
        ModuleDecl, ModuleItem, Stmt, TsInterfaceDecl,
    },
    MediaType, ParseParams, ParsedSource, SourceTextInfo,
};

use deno_ast::swc::{
    ast::{
        TsKeywordType, TsKeywordTypeKind, TsPropertySignature, TsType, TsTypeElement, TsTypeRef,
        TsUnionOrIntersectionType,
    },
    atoms::Atom,
    common::{comments::Comments, serializer::Type, BytePos, Span},
};
use model_context::{
    ModelContext, ModelImportContext, ModelSrcType, ModelValueCtor, ModelVarCollection,
    ModelVarInit,
};
use pathdiff::diff_paths;
use regex::Regex;
use tinytemplate::{format_unescaped, TinyTemplate};

use std::collections::HashSet;
use std::{
    any::Any,
    collections::HashMap,
    env::{args, current_dir, current_exe},
    ffi::OsStr,
    fs::{create_dir_all, read_to_string, write},
    path::{self, Path, PathBuf},
    process::exit,
};

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
                &filename,
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
    imported_modules: &mut HashMap<PathBuf, ParsedSource>,
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
            parse_all_imports(imported_modules, &m_filename, debug_print);
        }
    }
    imported_modules.insert(canonical_filepath.to_path_buf(), parsed_source);
    if debug_print {
        dbg!("parsed imports from", &canonical_filepath);
    }
}

struct ModuleRegexes {
    gd_type: Regex,
}
struct ModuleContext<'a> {
    regexes: ModuleRegexes,
    comment_pos: Option<&'a Span>,
    resolving_local: HashSet<Id>,
    stack: Vec<String>,
    debug_print: bool,
    relative_filepath: &'a Path,
    canonical_filepath: &'a Path,
    output_type_name: String,
    parsed_source: &'a ParsedSource,
    imported_modules: &'a HashMap<PathBuf, ParsedSource>,
    import_specifiers: HashMap<Id, PathBuf>,
}

impl<'a> ModuleContext<'a> {
    fn new(
        debug_print: bool,
        relative_filepath: &'a Path,
        canonical_filepath: &'a Path,
        parsed_source: &'a ParsedSource,
        imported_modules: &'a HashMap<PathBuf, ParsedSource>,
    ) -> Self {
        ModuleContext::_new(
            debug_print,
            relative_filepath,
            canonical_filepath,
            parsed_source,
            imported_modules,
            None,
            None,
        )
    }
    fn _new(
        debug_print: bool,
        relative_filepath: &'a Path,
        canonical_filepath: &'a Path,
        parsed_source: &'a ParsedSource,
        imported_modules: &'a HashMap<PathBuf, ParsedSource>,
        regexes: Option<&'a ModuleRegexes>,
        stack: Option<&'a Vec<String>>,
    ) -> Self {
        ModuleContext {
            regexes: ModuleRegexes {
                gd_type: Regex::new(r"@typescript-to-gdscript-type:\s+(\w+)").unwrap(),
            },
            stack: if let Some(stack) = stack {
                stack.to_owned()
            } else {
                Vec::new()
            },
            output_type_name: "".into(),
            comment_pos: None,
            resolving_local: HashSet::new(),
            debug_print,
            relative_filepath,
            canonical_filepath,
            parsed_source,
            imported_modules,
            import_specifiers: HashMap::new(),
        }
    }

    fn inherit(
        &'a self,
        relative_filepath: &'a Path,
        canonical_filepath: &'a Path,
        parsed_source: &'a ParsedSource,
    ) -> Self {
        ModuleContext::_new(
            self.debug_print,
            relative_filepath,
            canonical_filepath,
            parsed_source,
            self.imported_modules,
            Some(&self.regexes),
            Some(&self.stack),
        )
    }
}

fn convert(
    imports: &HashMap<PathBuf, ParsedSource>,
    output_dir: &Path,
    filename: &Path,
    canonical_filepath: &Path,
    template_filename: &Path,
    template: &TinyTemplate,
    debug_print: bool,
) {
    let parsed_source = imports.get(canonical_filepath).expect(&format!(
        "Module {:?} is missing but should previously have been parsed",
        canonical_filepath
    ));
    let module = parsed_source.module();
    let mut stack: Vec<String> = Vec::new();
    let mut models: Vec<ModelContext> = Vec::new();
    let mut context = ModuleContext::new(
        debug_print,
        filename,
        canonical_filepath,
        parsed_source,
        imports,
    );
    extract_import_specifiers(&mut context, module);

    for node in module.body.iter() {
        match node {
            ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) => {
                let decl = &export.decl;
                if let Decl::TsInterface(intf) = decl {
                    context.comment_pos = Some(&intf.span);
                    models.push(get_intf_model(&mut context, &intf));
                    context.comment_pos = None;
                }
            },
            ModuleItem::Stmt(Stmt::Decl(decl)) => {
                if let Decl::TsInterface(intf) = decl {
                    context.comment_pos = Some(&intf.span);
                    models.push(get_intf_model(&mut context, &intf));
                    context.comment_pos = None;
                }
            },
            _ => {}
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
    context.output_type_name = String::from(&*symbol);
    let mut imports: HashMap<String, String> = HashMap::new();
    let vars = intf
        .body
        .body
        .iter()
        .map(|e| get_intf_model_var(context, &mut imports, e))
        // flatten filters out None!
        .flatten()
        .collect();
    ModelContext {
        class_name: String::from(&*symbol),
        canonical_src_filepath: context.canonical_filepath.to_path_buf(),
        comment: Some(format!(
            "Model for {} typescript interface in {:?}",
            symbol, context.relative_filepath
        )),
        src_type: Some(ModelSrcType {
            name: "Dictionary".to_string(),
            init: Some("{}".to_string()),
            array_item_ctor: None,
        }),
        vars,
        imports: imports
            .into_iter()
            .map(|(src, name)| ModelImportContext { src, name })
            .collect(),
    }
}

struct TypeResolution {
    name: String,
    ctor: Option<ModelValueCtor>,
    collection: Option<ModelVarCollection>,
    comment: Option<String>,
}

impl From<&str> for TypeResolution {
    fn from(name: &str) -> Self {
        TypeResolution {
            name: String::from(name),
            ctor: None,
            collection: None,
            comment: None,
        }
    }
}

impl From<&Ident> for TypeResolution {
    fn from(ident: &Ident) -> Self {
        let name: &str = &*ident.to_id().0;
        TypeResolution {
            name: String::from(name),
            ctor: None,
            collection: None,
            comment: None,
        }
    }
}

fn resolve_type_ref(
    context: &mut ModuleContext,
    imports: &mut HashMap<String, String>,
    type_ref: &TsTypeRef,
) -> TypeResolution {
    if let Some(ident) = &type_ref.type_name.as_ident() {
        let id = ident.to_id();
        context.stack.push(format!("resolve_type_ref {:?}", &id));
        let resolved_type = if let Some(t) = resolve_imported_specifier_type(context, &id) {
            t
        } else {
            resolve_local_specifier_type(context, imports, type_ref, &id)
        };
        add_import(context, imports, &resolved_type);
        context.stack.pop();
        resolved_type
    } else {
        panic!(
            "IDK what to do with {:#?} {:#?}",
            type_ref.type_name, &context.stack
        );
    }
}

// type name, ctor, collection, comment
fn resolve_type(
    context: &mut ModuleContext,
    imports: &mut HashMap<String, String>,
    ts_type: &TsType,
) -> TypeResolution {
    match (ts_type) {
        TsType::TsTypeRef(type_ref) => {
            context
                .stack
                .push(format!("resolve type_ref {:?}", type_ref));
            let result = resolve_type_ref(context, imports, &type_ref);
            context.stack.pop();
            result
        }
        TsType::TsArrayType(array_type) => {
            context
                .stack
                .push(format!("resolve array_type {:?}", array_type));
            let mut result = resolve_type(context, imports, &array_type.elem_type);
            result.name = "Array".to_string();
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
            let mut result = match (kw_type.kind) {
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
            update_resolve_from_comments(context, &mut result, context.comment_pos.unwrap());
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
                    let mut result = resolve_type(context, imports, &a);
                    // Add a suffix to check for null and avoid the constructor if the value is null.
                    // NOTE: We only have to do this if resolve_type created a constructor
                    if let Some(ctor) = result.ctor.as_mut() {
                        ctor.set_nullable();
                    }
                    result.comment = Some(format!("{} | null", &result.name.clone()));
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
    imports: &mut HashMap<String, String>,
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
        let resolved = resolve_type(context, imports, &*type_ann.type_ann);
        context.stack.pop();
        if let Some(src_name) = src_name {
            //  = prop_sig.type_ann?.type_ann;
            return Some(ModelVarInit {
                name: src_name.to_case(Case::Snake),
                comment: resolved.comment,
                decl_type: resolved.name,
                decl_init: None,
                src_name,
                ctor: resolved.ctor.unwrap_or(ModelValueCtor::empty()),
                collection: resolved.collection,
            });
        }
    }
    if context.debug_print {
        dbg!(type_element);
    }
    return None;
}

fn resolve_local_specifier_type(
    context: &mut ModuleContext,
    imports: &mut HashMap<String, String>,
    type_ref: &TsTypeRef,
    id: &Id,
) -> TypeResolution {
    if !context.resolving_local.contains(&id) {
        context.resolving_local.insert(id.clone());
        let mut result: Option<TypeResolution> = None;
        for node in context.parsed_source.module().body.iter() {
            match node {
                ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) => {
                    let decl = &export.decl;
                    let result_and_id = resolve_type_decl(context, imports, &decl);
                    if let Some((r, (result_sym, _))) = result_and_id {
                        if String::from(&*result_sym) == String::from(&*id.0) {
                            add_import(context, imports, &r);
                            result = Some(r);
                        }
                    }
                }
                ModuleItem::Stmt(Stmt::Decl(decl)) => {
                    let result_and_id = resolve_type_decl(context, imports, &decl);
                    if let Some((r, (result_sym, _))) = result_and_id {
                        if String::from(&*result_sym) == String::from(&*id.0) {
                            add_import(context, imports, &r);
                            result = Some(r);
                        }
                    }
                }
                _ => {}
            }
            if result.is_some() {
                break;
            }
        }
        if !context.resolving_local.remove(&id) {
            panic!("Id {:?} should have been resolving but it's gone", *id);
        }
        if let Some(result) = result {
            return result;
        }
    }
    // if we were already resolving a type that hasn't been imported
    // or we just can't find the type then maybe it's a TypeScript builtin?
    // TODO: is there a deno_ast helper for these?
    let sstr = &*id.0;
    match (sstr) {
        "Record" => {
            let ctor_type: Option<TypeResolution> = if let Some(prop_type) =
                type_ref.type_params.as_ref().and_then(|p| p.params.get(1))
            {
                Some(resolve_type(context, imports, &prop_type))
            } else {
                None
            };
            let mut result = ctor_type.unwrap_or_else(|| TypeResolution::from("Dictionary"));
            result.collection = Some(ModelVarCollection {
                init: "{}".to_string(),
                is_array: false,
                is_dict: true,
            });
            result
        }
        "Array" => {
            let ctor_type: Option<TypeResolution> = if let Some(prop_type) =
                type_ref.type_params.as_ref().and_then(|p| p.params.get(0))
            {
                Some(resolve_type(context, imports, &prop_type))
            } else {
                None
            };
            let mut result = ctor_type.unwrap_or_else(|| TypeResolution::from("Array"));
            result.collection = Some(ModelVarCollection {
                init: "[]".to_string(),
                is_array: true,
                is_dict: false,
            });
            result
        }
        _ => panic!(
            "Unable to resolve type {:?} from {:?}, is it a built in type? {:#?}...{:#?}",
            &id, context.relative_filepath, type_ref, context.stack
        ),
    }
}

// TODO: resolve specifiers across modules until we find one that has @typescript-to-gdscript-type: int or something?
fn resolve_imported_specifier_type(context: &mut ModuleContext, id: &Id) -> Option<TypeResolution> {
    let mut result: Option<TypeResolution> = None;
    if let Some(module_path) = context.import_specifiers.get(id) {
        context.stack.push(format!(
            "resolve imported type_ref {:?} -> ${:?}",
            id, module_path
        ));
        if let Some(parsed_source) = context.imported_modules.get(module_path) {
            let dir = context.canonical_filepath.parent().expect(&format!(
                "Expected {:?} to have a parent",
                &context.canonical_filepath
            ));
            let relative_filepath = diff_paths(module_path, dir).expect(&format!(
                "Pathdiff {:?} relative to {:?}",
                module_path,
                context.canonical_filepath.parent()
            ));
            context.stack.push(String::from("found parsed module"));
            let mut import_specifiers: HashMap<Id, PathBuf> = HashMap::new();
            let module = parsed_source.module();
            let mut imported_imports: HashMap<String, String> = HashMap::new();
            let mut import_context =
                context.inherit(relative_filepath.as_path(), module_path, parsed_source);
            extract_import_specifiers(&mut import_context, module);
            for node in module.body.iter() {
                if let ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) = node {
                    import_context
                        .stack
                        .push(format!("imported export decl {:?}", export));
                    import_context.comment_pos = Some(&export.span);

                    if let Some((r, decl_id)) =
                        resolve_type_decl(&mut import_context, &mut imported_imports, &export.decl)
                    {
                        if String::from(&*decl_id.0) == String::from(&*id.0) {
                            result = Some(r);
                        } else {
                            result = None;
                        }
                    }
                    import_context.comment_pos = None;
                    import_context.stack.pop();
                }
                if result.is_some() {
                    break;
                }
            }

            if result.is_none() {
                panic!(
                    "no type found for {:?} in {:?} imported modules {:#?}",
                    id, &relative_filepath, context.stack
                );
            }
            context.stack.pop();
        } else {
            panic!(
                "Expected to be able to resolve {:?} in imported modules {:#?}",
                id, context.stack
            );
        }
        context.stack.pop();
    }
    result
}

fn extract_import_specifiers(context: &mut ModuleContext, module: &Module) {
    let directory = context.canonical_filepath.parent().expect(&format!(
        "Filepath should not be an orphan {:?}",
        context.canonical_filepath
    ));
    for node in module.body.iter() {
        if let ModuleItem::ModuleDecl(ModuleDecl::Import(import)) = node {
            let m_import_src = PathBuf::from(import.src.value.to_string());
            let import_filename = canonical_module_filename(directory, &m_import_src);
            for s in &import.specifiers[..] {
                if let ImportSpecifier::Named(ns) = s {
                    context
                        .import_specifiers
                        .insert(ns.local.to_id(), import_filename.to_path_buf());
                }
            }
        }
    }
}

fn resolve_type_decl(
    context: &mut ModuleContext,
    imports: &mut HashMap<String, String>,
    decl: &Decl,
) -> Option<(TypeResolution, Id)> {
    let result: Option<(TypeResolution, Id)>;
    match decl {
        Decl::TsTypeAlias(alias) => {
            result = Some((
                resolve_type(context, imports, &alias.type_ann),
                alias.id.to_id(),
            ));
            if let Some((r, _)) = &result {
                add_import(context, imports, &r);
            }
        }
        Decl::TsInterface(intf) => {
            let id = intf.id.to_id();
            let mut tr = TypeResolution::from(&intf.id);
            tr.ctor = Some(ModelValueCtor::new(&tr.name));
            result = Some((tr, id))
        }
        _ => {
            dbg!(decl);
            panic!("IDK{:#?}", context.stack);
        }
    }
    result
}

fn update_resolve_from_comments(
    context: &mut ModuleContext,
    result: &mut TypeResolution,
    span: &Span,
) {
    let c = context.parsed_source.comments().as_single_threaded();
    if let Some(comments) = c.get_leading(span.lo) {
        for c in comments.iter() {
            if let Some(gd_type) = context
                .regexes
                .gd_type
                .captures(&c.text)
                .and_then(|c| c.get(1))
            {
                result.name = gd_type.as_str().into();
            }
        }
    }
}

fn add_import(context: &ModuleContext, imports: &mut HashMap<String, String>, r: &TypeResolution) {
    // assume it's an imported type if there's a constructor
    if let Some(ctor) = &r.ctor{
        if context.output_type_name != r.name && !ctor.builtin {
            imports.insert(ctor.name.to_string(), format!("\"./{}.gd\"", &r.name));
        }
    }
}
