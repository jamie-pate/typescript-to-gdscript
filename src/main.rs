#![allow(warnings)]
#[macro_use]
extern crate lazy_static;

use convert_case::Case;
use convert_case::Casing;
use deno_ast::parse_module;
use deno_ast::swc::ast::BindingIdent;
use deno_ast::swc::ast::Pat;
use deno_ast::swc::ast::TsLit;
use deno_ast::SourcePos;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
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
const TYPE_DIRECTIVE: &str = "@typescript-to-gdscript-type";
const SKIP_DIRECTIVE: &str = "@typescript-to-gdscript-skip";
const GD_IMPL_DIRECTIVE: &str = "@typescript-to-gdscript-gd-impl";
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
        let mut import_stack: Vec<PathBuf> = Vec::new();
        for filename in src_files.iter() {
            let canonical_filepath = canonical_module_filename(&directory, &filename);
            parse_all_imports(
                &mut import_stack,
                &mut imports,
                &canonical_filepath,
                debug_print,
            );
        }
        if debug_print {
            eprintln!("{} files parsed", imports.len());
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

fn read_and_parse_file(import_stack: &mut Vec<PathBuf>, filename: &Path) -> ParsedSource {
    let source_text = read_to_string(filename).expect(&format!(
        "Unable to read file {:?}\n${:#?}",
        filename, import_stack
    ));
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
    import_stack: &mut Vec<PathBuf>,
    imported_modules: &mut HashMap<PathBuf, ParsedSource>,
    canonical_filepath: &Path,
    debug_print: bool,
) {
    if imported_modules.contains_key(canonical_filepath) {
        // don't reparse if we already parsed it
        return;
    }
    let parsed_source = read_and_parse_file(import_stack, &canonical_filepath);
    if debug_print {
        eprintln!("parsing imports from {:?}", canonical_filepath);
    }
    import_stack.push(canonical_filepath.to_path_buf());
    // TODO: preload and cache this and pass it to convert()
    let directory = canonical_filepath
        .parent()
        .expect(&format!("invalid filename {:?}", canonical_filepath));
    let module = parsed_source.module();
    for node in module.body.iter() {
        if let ModuleItem::ModuleDecl(ModuleDecl::Import(import)) = node {
            let m_import_src = PathBuf::from(import.src.value.to_string());
            let m_filename = canonical_module_filename(directory, &m_import_src);
            if m_import_src.starts_with(".") {
                if (import_stack.contains(&m_filename)) {
                    if debug_print {
                        eprintln!(
                            "Import cycle detected, not importing {:?} from {:?}",
                            m_import_src, canonical_filepath
                        );
                    }
                    break;
                }
                parse_all_imports(import_stack, imported_modules, &m_filename, debug_print);
            }
        }
    }
    imported_modules.insert(canonical_filepath.to_path_buf(), parsed_source);
    if debug_print {
        eprintln!("parsed imports from {:?}", canonical_filepath);
    }
    import_stack.pop();
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
                gd_type: Regex::new(&format!("{}:\\s+(\\w+)", TYPE_DIRECTIVE)).unwrap(),
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
                    context.comment_pos = Some(&export.span);
                    dbg_comment_pos(&context, "convert ExportDecl");
                    if !should_skip(&context) {
                        context.stack.push(format!(
                            "{:?}:{:?} {}",
                            filename,
                            intf.id.to_id(),
                            get_text(&context)
                        ));
                        models.push(get_intf_model(&mut context, &intf));
                        context.stack.pop();
                    }
                    context.comment_pos = None;
                }
            }
            ModuleItem::Stmt(Stmt::Decl(decl)) => {
                if let Decl::TsInterface(intf) = decl {
                    context.comment_pos = Some(&intf.span);
                    dbg_comment_pos(&context, "convert Decl");
                    if !should_skip(&context) {
                        context.stack.push(format!(
                            "{:?}:{:?}, {}",
                            filename,
                            intf.id.to_id(),
                            get_text(&context)
                        ));
                        models.push(get_intf_model(&mut context, &intf));
                        context.stack.pop();
                    }
                    context.comment_pos = None;
                }
            }
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
            .map(|(name, src)| ModelImportContext { src, name })
            .collect(),
    }
}

#[derive(Debug)]
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
        let mut resolved_type = if let Some(t) = resolve_imported_specifier_type(context, &id) {
            t
        } else {
            resolve_local_specifier_type(context, imports, type_ref, &id)
        };
        // Special case where we want to exclude the contents of this part of the tree
        // for example we encountered a GD_IMPL_DIRECTIVE on an interface union
        if resolved_type.name == "" {
            resolved_type.name = (&*id.0).to_string();
            if let Some(ctor) = resolved_type.ctor {
                resolved_type.ctor = Some(ctor.rename(&resolved_type.name));
            }
        }
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
                // used internally, probably not useful though
                TsKeywordTypeKind::TsNullKeyword => TypeResolution::from("null"),
                kind => panic!(
                    "IDK what to do with keyword type {:#?} {:#?}",
                    kind, context.stack
                ),
            };
            update_resolve_from_comments(context, &mut result);
            context.stack.pop();
            result
        }
        TsType::TsUnionOrIntersectionType(ui_type) => {
            context
                .stack
                .push(format!("resolve union_or_intersection_type {:?}", ui_type));
            let result = match ui_type {
                TsUnionOrIntersectionType::TsUnionType(union_type) => {
                    if (union_type.types.len() < 2) {
                        panic!(
                            "At least two types required in a union type {:#?}",
                            context.stack
                        );
                    }
                    let mut it = union_type.types.iter();
                    let (a, b) = (it.next().unwrap(), it.next().unwrap());

                    let mut a_type = resolve_type(context, imports, &a);
                    let mut b_type = resolve_type(context, imports, &b);
                    let b_is_null = b_type.name == "null";
                    context.stack.push(format!("resolve union_type {:?}", a));
                    let result = if union_type.types.len() == 2 && b_is_null {
                        // Add a suffix to check for null and avoid the constructor if the value is null.
                        // NOTE: We only have to do this if resolve_type created a constructor
                        if let Some(ctor) = a_type.ctor.as_mut() {
                            ctor.set_nullable();
                        }
                        a_type.comment = Some(format!("{} | null", &a_type.name.clone()));
                        a_type
                    } else {
                        let all_types: Vec<TypeResolution> = union_type
                            .types
                            .iter()
                            .map(|t| resolve_type(context, imports, &t))
                            .collect();
                        if !all_types.iter().all(|rt| rt.name == a_type.name) {
                            if have_directive(context, GD_IMPL_DIRECTIVE) {
                                a_type = TypeResolution::from("");
                                a_type.ctor = Some(ModelValueCtor::new(&a_type.name));
                            } else {
                                let all_types = all_types
                                    .into_iter()
                                    .map(|tr| tr.name)
                                    .collect::<Vec<String>>();
                                let pos = context.comment_pos.unwrap();
                                if context.debug_print {
                                    eprintln!(
                                        "COMMENTS: {:#?}",
                                        context
                                            .parsed_source
                                            .comments()
                                            .as_single_threaded()
                                            .get_leading(pos.lo)
                                    );
                                }
                                let pos_src =
                                    context.parsed_source.text_info().range_text(&pos.range());
                                panic!("Unsupported type, union types must all have the same godot type unless \
                                    you supply {} directive. {:?}\n{:#?}\n${:#?}",
                                    GD_IMPL_DIRECTIVE,
                                    all_types
                                    , context.stack, pos_src);
                            }
                        }
                        // TODO: lousy hack, add a property to denote this?
                        let literal_comments = all_types
                            .iter()
                            .map(|rt| &rt.comment)
                            .flatten()
                            .filter(|c| c.starts_with("Literally "))
                            .map(|c| c.replace("Literally ", ""))
                            .collect::<Vec<String>>()
                            .join(" | ");
                        if literal_comments.len() == all_types.len() {
                            a_type.comment = Some(format!("Literally {}", literal_comments));
                        } else if all_types.len() > 0 {
                            a_type.comment = Some(
                                all_types
                                    .iter()
                                    .map(|rt| rt.comment.to_owned())
                                    .flatten()
                                    .collect::<Vec<String>>()
                                    .join(" | "),
                            );
                        }
                        a_type
                    };
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
        TsType::TsLitType(lit_type) => {
            context
                .stack
                .push(format!("resolve TsLitType {:?}", lit_type));
            let result = match (&lit_type.lit) {
                TsLit::Number(number) => {
                    let s = if let Some(atom) = &number.raw {
                        atom.to_string()
                    } else {
                        number.value.to_string()
                    };
                    let mut result = if s.contains(".") {
                        TypeResolution::from("float")
                    } else {
                        TypeResolution::from("int")
                    };
                    result.comment = Some(format!("Literally {}", s));
                    result
                }
                TsLit::Str(str) => {
                    let mut result = TypeResolution::from("String");
                    result.comment = Some(format!("Literally \"{}\"", str.value));
                    result
                }
                TsLit::Bool(bool) => {
                    let mut result = TypeResolution::from("bool");
                    result.comment = Some(format!("Literally {}", bool.value));
                    result
                }
                TsLit::BigInt(big_int) => {
                    let mut result = TypeResolution::from("int");
                    result.comment = Some(format!("Literally {}", big_int.value));
                    result
                }
                _ => panic!(
                    "IDK what to do with this type {:#?} {:#?}",
                    ts_type, context.stack
                ),
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
            "get_intf_model_var {:?} {:?}",
            &src_name.as_ref().unwrap_or(&empty),
            get_text(context)
        ));
        let resolved = resolve_type(context, imports, &*type_ann.type_ann);
        context.stack.pop();
        if let Some(src_name) = src_name {
            let mut ctor = if let Some(ctor) = resolved.ctor {
                ctor
            } else {
                ModelValueCtor::empty()
            };

            //  = prop_sig.type_ann?.type_ann;
            return Some(ModelVarInit {
                name: src_name.to_case(Case::Snake),
                comment: resolved.comment,
                decl_type: resolved.name,
                decl_init: None,
                src_name,
                ctor,
                collection: resolved.collection,
                optional: prop_sig.optional,
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
    let mut decl_resolved: Option<TypeResolution> = None;
    context
        .stack
        .push(format!("resolve_local_specifier_type {:?}", id));
    if !context.resolving_local.contains(&id) {
        context.resolving_local.insert(id.clone());
        for node in context.parsed_source.module().body.iter() {
            match node {
                ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) => {
                    let decl = &export.decl;
                    let resolved = resolve_type_decl(context, imports, &decl, &id);
                    if let Some(r) = resolved {
                        add_import(context, imports, &r);
                        decl_resolved = Some(r);
                        break;
                    }
                }
                ModuleItem::Stmt(Stmt::Decl(decl)) => {
                    let resolved = resolve_type_decl(context, imports, &decl, &id);
                    if let Some(r) = resolved {
                        add_import(context, imports, &r);
                        decl_resolved = Some(r);
                        break;
                    }
                }
                _ => {}
            }
        }
        if !context.resolving_local.remove(&id) {
            panic!("Id {:?} should have been resolving but it's gone", *id);
        }
    }
    // if we were already resolving a type that hasn't been imported
    // or we just can't find the type then maybe it's a TypeScript builtin?
    // TODO: is there a deno_ast helper for these?
    let sstr = &*id.0;
    let result = decl_resolved.unwrap_or_else(|| match (sstr) {
        "Date" => {
            let mut result = TypeResolution::from("String");
            result.ctor = Some(ModelValueCtor::new("ISO8601Date"));
            result
        }
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
            "Unable to resolve type {:?} from {:?}, is it a built in type or generic type parameter? {:#?}...{:#?}",
            &id, context.relative_filepath, type_ref, context.stack
        ),
    });
    context.stack.pop();
    result
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
                    import_context.comment_pos = Some(&export.span);
                    import_context.stack.push(format!(
                        "imported export decl {:?}:{:?} {}",
                        relative_filepath,
                        export,
                        get_text(&import_context)
                    ));
                    dbg_comment_pos(&import_context, "import ExportDecl");

                    if !should_skip(&context) {
                        if let Some(r) = resolve_type_decl(
                            &mut import_context,
                            &mut imported_imports,
                            &export.decl,
                            &id,
                        ) {
                            result = Some(r);
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
    // if this id is supplied, only match declarations for this id's symbol
    match_id: &Id,
) -> Option<TypeResolution> {
    let mut resolved_and_id: Option<(TypeResolution, Id)> = None;
    match decl {
        Decl::TsTypeAlias(alias) => {
            resolved_and_id = Some((
                resolve_type(context, imports, &alias.type_ann),
                alias.id.to_id().clone(),
            ));
            if let Some((r, _)) = &resolved_and_id {
                add_import(context, imports, &r);
            }
        }
        Decl::TsInterface(intf) => {
            let id = intf.id.to_id();
            let mut tr = TypeResolution::from(&intf.id);
            tr.ctor = Some(ModelValueCtor::new(&tr.name));
            resolved_and_id = Some((tr, id.clone()));
        }
        Decl::Class(cls_decl) => {
            let id = cls_decl.ident.to_id();
            if match_id.0 == id.0 {
                panic!(
                    "\
                    Conversion of class declarations is unsupported.\n\
                    Create an interface and have the class implement that interface instead\n\
                    {:#?}",
                    context.stack
                );
            } else {
                if context.debug_print {
                    eprintln!("Skip Class Decl {:?}", id.0);
                }
            }
        }
        Decl::Var(var_decl) => {
            // TODO: check to make sure the match id isn't in this list and panic if it is
            let names: Vec<Option<&BindingIdent>> =
                var_decl.decls.iter().map(|d| d.name.as_ident()).collect();
            if context.debug_print {
                eprintln!("Skip Var Decl {:?}", names);
            }
        }
        Decl::Fn(fn_decl) => {
            let id = fn_decl.ident.to_id();
            if match_id.0 == id.0 {
                panic!(
                    "\
                    Conversion of function declarations is unsupported.\n\
                    Create an interface and have the class implement that interface instead\n\
                    {:#?}",
                    context.stack
                );
            } else {
                if context.debug_print {
                    eprintln!("Skip Class Decl {:?}", id.0);
                }
            }
        }
        _ => {
            dbg!(decl);
            panic!("IDK {:#?}", context.stack);
        }
    }
    // check if it matches
    if let Some((result, id)) = resolved_and_id {
        if match_id.0 == id.0 {
            Some(result)
        } else {
            None
        }
    } else {
        None
    }
}

fn should_skip(context: &ModuleContext) -> bool {
    for directive in [SKIP_DIRECTIVE, GD_IMPL_DIRECTIVE].into_iter() {
        if have_directive(context, directive) {
            if context.debug_print {
                eprintln!(
                    "Skipping due to {} directive: {:?}",
                    GD_IMPL_DIRECTIVE,
                    get_text(context)
                );
            }
            return true;
        }
    }
    false
}

fn have_directive(context: &ModuleContext, directive: &str) -> bool {
    let span = context.comment_pos.unwrap();
    let c = context.parsed_source.comments().as_single_threaded();
    if let Some(comments) = c.get_leading(span.lo) {
        for c in comments.iter() {
            if (context.debug_print) {
                eprintln!("Checking comment for {}: {:?}", directive, c.text);
            }
            if c.text.contains(directive) {
                return true;
            }
        }
    }
    return false;
}

fn get_text<'a>(context: &'a ModuleContext) -> &'a str {
    context
        .parsed_source
        .text_info()
        .range_text(&context.comment_pos.unwrap().range())
}

fn update_resolve_from_comments(context: &ModuleContext, result: &mut TypeResolution) {
    let c = context.parsed_source.comments().as_single_threaded();
    let span = context.comment_pos.unwrap();
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

fn dbg_comment_pos(context: &ModuleContext, comment: &str) {
    if context.debug_print {
        let pos = context.comment_pos.unwrap();
        let pos_src = context.parsed_source.text_info().range_text(&pos.range());
        eprintln!(
            "Setting comment pos {},  {:?}:\n{:#?}\n{:#?}",
            comment,
            pos,
            context
                .parsed_source
                .comments()
                .as_single_threaded()
                .get_leading(pos.lo)
                .iter()
                .map(|c| c.iter().map(|cc| &cc.text))
                .flatten()
                .collect::<Vec<&Atom>>(),
            pos_src
        )
    }
}

fn add_import(context: &ModuleContext, imports: &mut HashMap<String, String>, r: &TypeResolution) {
    // assume it's an imported type if there's a constructor
    if let Some(ctor) = &r.ctor {
        if context.output_type_name != r.name && !ctor.builtin {
            imports.insert(ctor.name.to_string(), format!("\"./{}.gd\"", &r.name));
        }
    }
}
