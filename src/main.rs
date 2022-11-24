#![allow(warnings)]
#[macro_use]
extern crate lazy_static;

use convert_case::{Case, Casing};
use deno_ast::{
    parse_module,
    swc::{
        ast::{
            BindingIdent, Decl, Expr, Id, Ident, Import, ImportDecl, ImportNamedSpecifier,
            ImportSpecifier, Lit, Module, ModuleDecl, ModuleItem, Pat, Stmt, TsEnumDecl,
            TsEnumMemberId, TsExprWithTypeArgs, TsInterfaceDecl, TsKeywordType, TsKeywordTypeKind,
            TsLit, TsPropertySignature, TsType, TsTypeElement, TsTypeRef,
            TsUnionOrIntersectionType, TsUnionType,
        },
        atoms::Atom,
        common::{comments::Comments, serializer::Type, util::iter::IteratorExt, BytePos, Span},
    },
    MediaType, ParseParams, ParsedSource, SourcePos, SourceRange, SourceRanged,
    SourceRangedForSpanned, SourceTextInfo,
};

use model_context::{
    is_builtin, ModelContext, ModelEnum, ModelImportContext, ModelSrcType, ModelValueCtor,
    ModelValueForJson, ModelVarCollection, ModelVarDescriptor, DEFAULT_INDENT, STATE_METHODS,
    STATE_VARS,
};
use pathdiff::diff_paths;
use regex::Regex;
use substring::Substring;
use tinytemplate::{format_unescaped, TinyTemplate};

use std::{
    any::Any,
    collections::{HashMap, HashSet},
    env::{args, current_dir, current_exe},
    ffi::OsStr,
    fs::{create_dir_all, read_to_string, write},
    path::{self, Path, PathBuf},
    process::exit,
};

use crate::model_context::ModelEnumMember;

const TERM_WIDTH: usize = 180;
const ISO_DATE_TYPE_NAME: &str = "Iso8601Date";
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
        let template_indent = detect_indent(&template_str, debug_print);
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
            parse_recursive(
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
                &template_indent,
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
    return parse_module(ParseParams {
        specifier: String::from(
            filename
                .to_str()
                .expect(&format!("filename invalid {:?}", filename)),
        ),
        media_type: MediaType::TypeScript,
        text_info: SourceTextInfo::new(source_text.into()),
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

fn parse_recursive(
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
                parse_recursive(import_stack, imported_modules, &m_filename, debug_print);
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
    type_directive_consumed: bool,
    pos: Vec<Span>,
    resolving_local: HashSet<Id>,
    stack: Vec<String>,
    debug_print: bool,
    indent: String,
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
        indent: &str,
        relative_filepath: &'a Path,
        canonical_filepath: &'a Path,
        parsed_source: &'a ParsedSource,
        imported_modules: &'a HashMap<PathBuf, ParsedSource>,
    ) -> Self {
        ModuleContext::_new(
            debug_print,
            indent,
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
        indent: &str,
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
            type_directive_consumed: false,
            pos: Vec::new(),
            resolving_local: HashSet::new(),
            debug_print,
            indent: indent.to_string(),
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
            &self.indent,
            relative_filepath,
            canonical_filepath,
            parsed_source,
            self.imported_modules,
            Some(&self.regexes),
            Some(&self.stack),
        )
    }

    fn get_span_text(&'a self, span: &Span) -> &'a str {
        self.parsed_source.text_info().range_text(&span.range())
    }

    fn get_text(&'a self, limit: usize) -> &'a str {
        let result = self.get_span_text(self.pos.last().unwrap());
        if limit > 0 && result.len() > limit {
            result.substring(0, limit)
        } else {
            result
        }
    }

    fn get_span_info(&self, span: &Span, stack: bool) -> String {
        let result = format!(
            "{}:{}\n{}",
            &self.relative_filepath.to_string_lossy(),
            &span
                .range()
                .start_line_fast(&self.parsed_source.text_info()),
            self.get_span_text(span)
        );
        if self.debug_print && stack {
            let stack = self
                .stack
                .clone()
                .into_iter()
                .collect::<Vec<String>>()
                .join("\n\t");
            format!("{}\nstack:\n\t{}", result, stack)
        } else {
            result
        }
    }

    fn get_info(&self) -> String {
        self.pos
            .iter()
            .enumerate()
            .map(|(i, span)| self.get_span_info(span, i == self.pos.len() - 1))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

fn convert(
    imports: &HashMap<PathBuf, ParsedSource>,
    output_dir: &Path,
    filename: &Path,
    canonical_filepath: &Path,
    template_filename: &Path,
    template: &TinyTemplate,
    template_indent: &str,
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
        template_indent,
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
                    context.pos.push(export.span.to_owned());
                    dbg_pos(&context, "convert ExportDecl");
                    if !should_skip(&context) {
                        context.stack.push(format!(
                            "{:?}:{:?} {:?}",
                            filename,
                            intf.id.to_id(),
                            context.get_text(TERM_WIDTH)
                        ));
                        models.push(get_intf_model(&mut context, &intf, None, true));
                        context.stack.pop();
                    }
                    context.pos.pop();
                }
            }
            ModuleItem::Stmt(Stmt::Decl(Decl::TsInterface(intf))) => {
                context.pos.push(intf.span.to_owned());
                dbg_pos(&context, "convert Decl");
                if !should_skip(&context) {
                    context.stack.push(format!(
                        "{:?}:{:?}, {}",
                        filename,
                        intf.id.to_id(),
                        context.get_text(TERM_WIDTH)
                    ));
                    models.push(get_intf_model(&mut context, &intf, None, true));
                    context.stack.pop();
                }
                context.pos.pop();
            }
            _ => {}
        }
    }
    if (!stack.len() == 0) {
        panic!("Stack is not being cleared");
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

fn ts_enum_to_model_enum(context: &ModuleContext, ts_enum: &TsEnumDecl) -> ModelEnum {
    let id = ts_enum.id.to_id();
    let member_types = ts_enum
        .members
        .iter()
        .map(|m| {
            if let Some(init_expr) = m.init.as_ref() {
                if let Expr::Lit(lit_expr) = init_expr.as_ref() {
                    (Some(resolve_lit_type(context, &lit_to_ts_lit(lit_expr))), m)
                } else {
                    panic!(
                        "Unsupported enum type not supported {:?} {:?}\n{}",
                        id,
                        init_expr,
                        context.get_info()
                    );
                }
            } else {
                (None, m)
            }
        })
        .collect::<Vec<_>>();
    let mut type_set: Vec<_> = member_types
        .iter()
        .map(|(t, _)| if let Some(t) = t { &t.type_name } else { "" })
        .collect::<HashSet<_>>()
        .into_iter()
        .collect();
    let invalid_types: Vec<&str> = type_set
        .iter()
        // TODO: better way to deref_iter
        .map(|s| *s)
        .filter(|s| !["int", "String", ""].contains(s))
        .collect();
    if invalid_types.len() > 0 {
        panic!(
            "Forbidden enum member types detected: {}\n{}",
            invalid_types.join(", "),
            context.get_info()
        );
    }
    if type_set.len() > 1 {
        panic!(
            "Mixed enum member types are not allowed: {}\n{}",
            type_set.into_iter().collect::<Vec<_>>().join(", "),
            context.get_info()
        );
    }
    ModelEnum {
        name: (&*id.0).to_string(),
        have_string_members: member_types.iter().any(|(t, _)| {
            if let Some(t) = t {
                t.type_name == "String"
            } else {
                false
            }
        }),
        members: member_types
            .iter()
            .enumerate()
            .map(|(i, (t, m))| ModelEnumMember {
                name: match &m.id {
                    TsEnumMemberId::Ident(ident) => (&*ident.to_id().0).to_string(),
                    TsEnumMemberId::Str(str) => (&*str.value).to_string(),
                },
                value: if let Some(t) = t {
                    t.literal.clone().expect(&format!(
                        "Expected a literal value for {:?}\n{}",
                        m.init,
                        context.get_info()
                    ))
                } else {
                    i.to_string()
                },
            })
            .collect::<Vec<_>>(),
    }
}

fn get_extends_intf_model(
    context: &mut ModuleContext,
    imports: &mut HashMap<String, ModelImportContext>,
    expr: &TsExprWithTypeArgs,
) -> ModelContext {
    if let Expr::Ident(ident) = expr.expr.as_ref() {
        let id = ident.to_id();
        let mut resolved = if let Some(t) = resolve_imported_specifier_type(context, &id) {
            t
        } else if let Some(t) = resolve_local_specifier_type(context, &id) {
            t
        } else if &id.0 == "Omit" {
            let args = expr
                .type_args
                .as_ref()
                .expect("Expect Omit to have type_args");
            let arg0 = args.params.get(0).unwrap();

            let mut t = resolve_type(context, arg0);
            let mut possible_imports: HashMap<String, ModelImportContext> = HashMap::new();
            t.omit = Some(
                args.params
                    .iter()
                    .skip(1)
                    .map(|k| resolve_type(context, k))
                    .collect(),
            );
            t
        } else {
            if context.debug_print {
                dbg!(expr);
            }
            panic!(
                "Couldn't resolve identifier that's extended {:?}\n{}",
                id.0,
                context.get_info()
            );
        };

        get_intf_model(
            context,
            &resolved
                .interface_decl
                .expect("Interface decl should be here"),
            resolved.omit.as_mut().and_then(|o| {
                Some(
                    o.drain(..)
                        .map(|t| t.literal)
                        .flatten()
                        .collect::<HashSet<_>>(),
                )
            }),
            false,
        )
    } else {
        panic!(
            "IDK what to do with this extends expression:{:#?}\n{}",
            expr,
            context.get_info()
        );
    }
}

fn get_intf_model(
    context: &mut ModuleContext,
    intf: &TsInterfaceDecl,
    omit: Option<HashSet<String>>,
    top_level: bool,
) -> ModelContext {
    let (symbol, _tag) = intf.id.to_id();
    if top_level {
        if !context.output_type_name.is_empty() {
            panic!("Nested top level get_intf_model")
        }
        context.output_type_name = String::from(&*symbol);
    }
    context
        .stack
        .push(format!("get_intf_model {}", context.output_type_name));
    let mut import_map: HashMap<String, ModelImportContext> = HashMap::new();
    let intf_var_descriptors = intf
        .body
        .body
        .iter()
        .map(|e| get_intf_model_var(context, &mut import_map, e))
        // flatten filters out None!
        .flatten()
        .filter(|e| {
            if let Some(o) = &omit {
                o.contains(&e.src_name)
            } else {
                true
            }
        })
        .collect::<Vec<_>>();
    let extended_descriptor_iterator = intf
        .extends
        .iter()
        .map(|e| get_extends_intf_model(context, &mut import_map, e).var_descriptors)
        .flatten();
    let mut var_descriptors: Vec<_> = extended_descriptor_iterator
        .chain(intf_var_descriptors)
        .collect();
    let enums: Vec<_> = var_descriptors
        .iter_mut()
        .map(|v| v.enums.drain(..))
        .flatten()
        .collect();
    for import in var_descriptors
        .iter_mut()
        .map(|v| v.imports.drain(..))
        .flatten()
    {
        import_map.insert(import.name.clone(), import);
    }
    let mut imports: Vec<_> = import_map.into_values().collect();
    imports.sort_by(|a, b| {
        if a.gd_impl == b.gd_impl {
            a.name.cmp(&b.name)
        } else {
            // sort by whether it's a generated class or not
            // place gd_impl at the top
            b.gd_impl.cmp(&a.gd_impl)
        }
    });

    context.stack.pop();
    context.output_type_name = "".to_string();
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
        }),
        state_methods: STATE_METHODS.to_string(),
        state_vars: STATE_VARS.to_string(),
        vars: var_descriptors
            .iter()
            .map(|d| d.render(&context.indent))
            .collect(),
        var_descriptors,
        enums,
        imports,
    }
}

#[derive(Debug, Clone)]
struct TypeResolution {
    // type name for the property declaration
    // for collections this may be Array/Dictionary
    // in that case the actual_type contains the real type name
    type_name: String,
    // type name for the constructor/for_json
    // if the property is a collection type then
    // this is the type for items that are added to the collection.
    // if this is None, then type_name is a builtin type that requires
    // no extra work to serialize/deserialize
    ctor_type: Option<String>,
    nullable: bool,
    collection: Option<ModelVarCollection>,
    comment: Option<String>,
    literal: Option<String>,
    gd_impl: bool,
    interface_decl: Option<TsInterfaceDecl>,
    enums: Vec<ModelEnum>,
    omit: Option<Vec<TypeResolution>>,
    imports: Vec<ModelImportContext>,
}

impl TypeResolution {
    fn new(name: &str) -> Self {
        TypeResolution {
            type_name: String::from(name),
            ctor_type: None,
            nullable: false,
            collection: None,
            comment: None,
            literal: None,
            gd_impl: false,
            interface_decl: None,
            omit: None,
            enums: Vec::new(),
            imports: Vec::new(),
        }
    }

    fn gd_impl() -> Self {
        let mut result = TypeResolution::new("");
        result.gd_impl = true;
        result
    }

    fn add_import(&mut self, context: &ModuleContext) {
        if let Some(type_name) = &self.ctor_type {
            if &context.output_type_name != type_name && !is_builtin(type_name) {
                self.imports.push(ModelImportContext {
                    name: type_name.clone(),
                    src: format!("{}.gd", &type_name),
                    gd_impl: self.gd_impl,
                })
            }
        }
    }

    fn merge_imports<'a, I>(&mut self, vals: &[TypeResolution]) {
        for t in vals {
            let mut clone = t.imports.clone();
            self.imports.append(&mut clone);
        }
    }

    fn take_imports(&mut self, vals: &mut [TypeResolution]) {
        for t in vals {
            self.imports.append(&mut t.imports);
        }
    }
}

impl From<Ident> for TypeResolution {
    fn from(ident: Ident) -> Self {
        TypeResolution {
            type_name: String::from(&*ident.to_id().0),
            ctor_type: None,
            nullable: false,
            collection: None,
            literal: None,
            comment: None,
            gd_impl: false,
            interface_decl: None,
            omit: None,
            enums: Vec::new(),
            imports: Vec::new(),
        }
    }
}

fn resolve_type_ref(context: &mut ModuleContext, type_ref: &TsTypeRef) -> TypeResolution {
    if let Some(ident) = &type_ref.type_name.as_ident() {
        let id = ident.to_id();
        context.stack.push(format!("resolve_type_ref {:?}", &id));
        let mut resolved_type = if let Some(t) = resolve_imported_specifier_type(context, &id) {
            t
        } else {
            resolve_local_specifier_type_or_builtin(context, type_ref, &id)
        };
        // Special case where we want to exclude the contents of this part of the tree
        // for example we encountered a GD_IMPL_DIRECTIVE on an interface union
        if resolved_type.gd_impl {
            if resolved_type.type_name == "" {
                resolved_type.type_name = (&*id.0).to_string();
                resolved_type.ctor_type = Some(resolved_type.type_name.clone());
            }
        }
        resolved_type.add_import(context);
        context.stack.pop();
        resolved_type
    } else {
        panic!(
            "IDK what to do with {:#?}\n{}",
            type_ref.type_name,
            context.get_info()
        );
    }
}

fn resolve_union_type(context: &mut ModuleContext, union_type: &TsUnionType) -> TypeResolution {
    if (union_type.types.len() < 2) {
        panic!(
            "At least two types required in a union type\n{}",
            context.get_info()
        );
    }
    let mut it = union_type.types.iter();
    let (a, b) = (it.next().unwrap(), it.next().unwrap());
    context.stack.push(format!(
        "resolve union_type ({:?} types: <\n\t{:?},\n\t{:?},\n\t ...?>)",
        union_type.types.len(),
        a,
        b
    ));
    let mut a_type = resolve_type(context, &a);
    let mut b_type = resolve_type(context, &b);
    let b_is_null = b_type.type_name == "null";

    let result = if union_type.types.len() == 2 && b_is_null {
        // Add a suffix to check for null and avoid the constructor if the value is null.
        // NOTE: We only have to do this if resolve_type created a constructor
        if let Some(collection) = &mut a_type.collection {
            collection.nullable = true;
        } else {
            a_type.nullable = true;
        }
        a_type.comment = Some(format!("{} | null", &a_type.type_name.clone()));
        a_type
    } else {
        let mut all_types: Vec<TypeResolution> = union_type
            .types
            .iter()
            .map(|t| resolve_type(context, &t))
            .collect();
        context.stack.push(format!(
            "union_types: {:?}",
            all_types
                .iter()
                .map(|tr| &tr.type_name)
                .collect::<Vec<&String>>()
        ));
        let mut result = if !all_types.iter().all(|rt| rt.type_name == a_type.type_name) {
            if have_directive(context, GD_IMPL_DIRECTIVE) {
                TypeResolution::gd_impl()
            } else {
                if all_types.iter().all(|t| is_builtin(&t.type_name)) {
                    let mut result = TypeResolution::new("any");
                    result.comment = Some(
                        all_types
                            .iter()
                            .map(|tr| tr.type_name.clone())
                            .collect::<Vec<_>>()
                            .join(" | "),
                    );
                    result.nullable = all_types.iter().any(|tr| {
                        if let Some(l) = tr.literal.as_ref() {
                            l == "null"
                        } else {
                            false
                        }
                    });
                    result
                } else {
                    let all_types = all_types
                        .iter()
                        .map(|tr| tr.type_name.clone())
                        .collect::<Vec<String>>();
                    let pos = context.pos.last().unwrap();
                    if context.debug_print {
                        dbg_pos(context, "Union");
                    }
                    panic!(
                        "Unsupported type, union types must all have the same godot type unless \
                        you supply {} directive or they must all be builtin types. {:?}\n{:#?}",
                        GD_IMPL_DIRECTIVE,
                        all_types,
                        context.get_info()
                    );
                }
            }
        } else {
            a_type.clone()
        };
        let literal_comments = all_types
            .iter()
            .map(|rt| rt.literal.as_deref())
            .flatten()
            .collect::<Vec<&str>>();
        if literal_comments.len() == all_types.len() {
            result.comment = Some(format!("Literally {}", literal_comments.join(" | ")));
        } else if all_types.len() > 0 {
            result.comment = Some(
                all_types
                    .iter()
                    .map(|rt| rt.comment.to_owned())
                    .flatten()
                    .collect::<Vec<String>>()
                    .join(" | "),
            );
        }
        result.take_imports(all_types.as_mut_slice());
        context.stack.pop();
        result
    };
    context.stack.pop();
    result
}

// type name, ctor, collection, comment
fn resolve_type(context: &mut ModuleContext, ts_type: &TsType) -> TypeResolution {
    match (ts_type) {
        TsType::TsTypeRef(type_ref) => {
            context
                .stack
                .push(format!("resolve type_ref {:?}", type_ref.type_name));
            let result = resolve_type_ref(context, &type_ref);
            context.stack.pop();
            result
        }
        TsType::TsArrayType(array_type) => {
            context
                .stack
                .push(format!("resolve array_type {:?}", array_type));
            let mut result = resolve_type(context, &array_type.elem_type);
            let ctor_type = result.type_name;
            result.type_name = "Array".to_string();
            result.collection = Some(ModelVarCollection {
                init: "[]".to_string(),
                is_array: true,
                is_dict: false,
                nullable: false,
            });
            result.ctor_type = Some(ctor_type);
            context.stack.pop();
            result
        }
        TsType::TsKeywordType(kw_type) => {
            context
                .stack
                .push(format!("resolve keyword_type {:?}", kw_type));
            let mut result = match (kw_type.kind) {
                TsKeywordTypeKind::TsNumberKeyword => TypeResolution::new("float"),
                // Godot 3 uses 64 bit ints in 64 bit builds but probably we shouldn't use this type anyways, idk
                TsKeywordTypeKind::TsBigIntKeyword => TypeResolution::new("int"),
                TsKeywordTypeKind::TsBooleanKeyword => TypeResolution::new("bool"),
                TsKeywordTypeKind::TsStringKeyword => TypeResolution::new("String"),
                // used internally, probably not useful though
                TsKeywordTypeKind::TsNullKeyword => TypeResolution::new("null"),
                TsKeywordTypeKind::TsAnyKeyword => TypeResolution::new("any"),
                kind => panic!(
                    "IDK what to do with keyword type {:#?}\n{}",
                    kind,
                    context.get_info()
                ),
            };
            result = update_resolve_from_comments(context, result);
            context.stack.pop();
            result
        }
        TsType::TsUnionOrIntersectionType(ui_type) => {
            context
                .stack
                .push("resolve union_or_intersection_type".into());
            let result = match ui_type {
                TsUnionOrIntersectionType::TsUnionType(union_type) => {
                    resolve_union_type(context, union_type)
                }
                TsUnionOrIntersectionType::TsIntersectionType(_) => {
                    panic!("Intersection types are not allowed\n{}", context.get_info());
                }
            };
            context.stack.pop();
            result
        }
        TsType::TsLitType(lit_type) => {
            context
                .stack
                .push(format!("resolve TsLitType {:?}", lit_type));
            let result = resolve_lit_type(context, &lit_type.lit);
            context.stack.pop();
            result
        }
        // TODO: add array length check?
        TsType::TsTupleType(tuple_type) => {
            let elem_types = tuple_type
                .elem_types
                .iter()
                .map(|t| resolve_type(context, &t.ty))
                .collect::<Vec<_>>();
            let mut result = elem_types.get(0).unwrap().clone();
            if !elem_types.iter().all(|t| t.type_name == result.type_name) {
                panic!(
                    "Only homogenous tuple types are allowed {:#?}\n{}",
                    tuple_type.elem_types,
                    context.get_info()
                );
            }
            result.collection = Some(ModelVarCollection {
                init: "[]".to_string(),
                is_array: true,
                is_dict: false,
                nullable: false,
            });
            result
        }
        _ => panic!(
            "IDK what to do with this type {:#?}\n{}",
            ts_type,
            context.get_info()
        ),
    }
}

fn lit_to_ts_lit(ts_lit: &Lit) -> TsLit {
    match ts_lit {
        Lit::Num(number) => TsLit::Number(number.clone()),
        Lit::Str(str) => TsLit::Str(str.clone()),
        Lit::Bool(bool) => TsLit::Bool(bool.clone()),
        Lit::BigInt(big_int) => TsLit::BigInt(big_int.clone()),
        _ => panic!(
            "Unsupported literal type conversion from Lit to TsLit: {:?}",
            ts_lit
        ),
    }
}

fn resolve_lit_type(context: &ModuleContext, lit: &TsLit) -> TypeResolution {
    match (lit) {
        TsLit::Number(number) => {
            let s = if let Some(atom) = &number.raw {
                atom.to_string()
            } else {
                number.value.to_string()
            };
            let mut result = if s.contains(".") {
                TypeResolution::new("float")
            } else {
                TypeResolution::new("int")
            };
            result.comment = Some(format!("Literally {}", &s));
            result.literal = Some(s);
            result
        }
        TsLit::Str(str) => {
            let mut result = TypeResolution::new("String");
            let str_literal = format!("\"{}\"", str.value);
            result.comment = Some(format!("Literally {}", str_literal));
            result.literal = Some(str_literal);
            result
        }
        TsLit::Bool(bool) => {
            let mut result = TypeResolution::new("bool");
            result.comment = Some(format!("Literally {}", bool.value));
            result.literal = Some(bool.value.to_string());
            result
        }
        TsLit::BigInt(big_int) => {
            let mut result = TypeResolution::new("int");
            result.comment = Some(format!("Literally {}", big_int.value));
            result.literal = Some(big_int.value.to_string());
            result
        }
        _ => panic!(
            "IDK what to do with this type {:#?}\n{}",
            lit,
            context.get_info()
        ),
    }
}

fn get_intf_model_var(
    context: &mut ModuleContext,
    imports: &mut HashMap<String, ModelImportContext>,
    type_element: &TsTypeElement,
) -> Option<ModelVarDescriptor> {
    if let TsTypeElement::TsPropertySignature(prop_sig) = type_element {
        let src_name = if let Expr::Ident(id) = &*prop_sig.key {
            Some(id.sym.to_string())
        } else {
            None
        };

        if context.debug_print {
            dbg!(&src_name);
        }
        let type_ann = prop_sig.type_ann.as_ref().expect(&format!(
            "IDK what to do with this type {:#?}\n{}",
            prop_sig,
            context.get_info()
        ));
        let empty = String::from("");
        context.stack.push(format!(
            "get_intf_model_var {:?} {:?}",
            &src_name.as_ref().unwrap_or(&empty),
            context.get_text(TERM_WIDTH)
        ));
        context.pos.push(prop_sig.span);
        context.type_directive_consumed = false;
        let resolved = resolve_type(context, &*type_ann.type_ann);
        context.pos.pop();
        context.stack.pop();
        if let Some(src_name) = src_name {
            let (ctor, for_json) = if let Some(ctor_type) = resolved.ctor_type {
                (
                    ModelValueCtor::new(&ctor_type, resolved.nullable),
                    ModelValueForJson::new(&ctor_type, resolved.nullable),
                )
            } else {
                // This can happen for builtin types where we should not import a type
                (
                    ModelValueCtor::new(&resolved.type_name, resolved.nullable),
                    ModelValueForJson::new(&resolved.type_name, resolved.nullable),
                )
            };

            let comment = if prop_sig.optional {
                Some(if let Some(c) = resolved.comment {
                    format!("optional {}", c)
                } else {
                    "optional".to_string()
                })
            } else {
                resolved.comment
            };
            return Some(ModelVarDescriptor {
                name: src_name.to_case(Case::Snake),
                comment,
                // all collections and builtin types (so far) are non-nullable
                non_nullable: resolved.collection.is_some() || ctor.builtin,
                decl_type: if resolved.type_name != "any" {
                    Some(resolved.type_name)
                } else {
                    None
                },
                decl_init: None,
                src_name,
                ctor,
                for_json,
                collection: resolved.collection,
                optional: prop_sig.optional,
                enums: resolved.enums,
                imports: resolved.imports,
            });
        }
    }
    if context.debug_print {
        dbg!(type_element);
    }
    return None;
}

fn resolve_local_specifier_type(context: &mut ModuleContext, id: &Id) -> Option<TypeResolution> {
    let mut decl_resolved: Option<TypeResolution> = None;
    context
        .stack
        .push(format!("resolve_local_specifier_type {:?}", id));
    if !context.resolving_local.contains(&id) {
        context.resolving_local.insert(id.clone());
        for node in context.parsed_source.module().body.iter() {
            match node {
                ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) => {
                    context.pos.push(export.span.to_owned());
                    let decl = &export.decl;
                    let resolved = resolve_type_decl(context, &decl, &id);
                    if let Some(mut r) = resolved {
                        r.add_import(context);
                        decl_resolved = Some(r);
                    }
                    context.pos.pop();
                    if decl_resolved.is_some() {
                        break;
                    }
                }
                ModuleItem::Stmt(Stmt::Decl(decl)) => {
                    let resolved = resolve_type_decl(context, &decl, &id);
                    if let Some(mut r) = resolved {
                        r.add_import(context);
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
    context.stack.pop();
    decl_resolved
}

fn resolve_local_specifier_type_or_builtin(
    context: &mut ModuleContext,
    type_ref: &TsTypeRef,
    id: &Id,
) -> TypeResolution {
    let mut decl_resolved: Option<TypeResolution> = resolve_local_specifier_type(context, id);
    // if we were already resolving a type that hasn't been imported
    // or we just can't find the type then maybe it's a TypeScript builtin?
    // TODO: is there a deno_ast helper for these?
    let sstr = &*id.0;
    let result = decl_resolved.unwrap_or_else(|| match (sstr) {
        "Date" => {
            // gd_impl type Iso8601Date.gd must be provided
            // TODO: create reference implementation gdscript code?
            let mut result = TypeResolution::gd_impl();
            result.type_name = ISO_DATE_TYPE_NAME.to_string();
            result.ctor_type = Some(result.type_name.clone());
            result
        }
        "Record" => {
            let ctor_type: Option<TypeResolution> = if let Some(prop_type) =
                type_ref.type_params.as_ref().and_then(|p| p.params.get(1))
            {
                Some(resolve_type(context, &prop_type))
            } else {
                None
            };
            let mut result = ctor_type.unwrap_or_else(|| TypeResolution::new("Dictionary"));
            result.collection = Some(ModelVarCollection {
                init: "{}".to_string(),
                is_array: false,
                is_dict: true,
                nullable: false,
            });
            result
        }
        "Array" => {
            let ctor_type: Option<TypeResolution> = if let Some(prop_type) =
                type_ref.type_params.as_ref().and_then(|p| p.params.get(0))
            {
                Some(resolve_type(context, &prop_type))
            } else {
                None
            };
            let mut result = ctor_type.unwrap_or_else(|| TypeResolution::new("Array"));
            result.collection = Some(ModelVarCollection {
                init: "[]".to_string(),
                is_array: true,
                is_dict: false,
                nullable: false,
            });
            result
        }
        _ => {
            if context.debug_print {
                dbg!(type_ref);
            }
            panic!(
                "Unable to resolve type {} while exporting {:?}:{}.\n\
                \tIs it a built in type or generic type parameter?\n\
                \tYou can prefix {} with a comment containing {} or {} to skip this type.
                \n{}",
                &id.0,
                context.relative_filepath,
                context.output_type_name,
                context.output_type_name,
                SKIP_DIRECTIVE,
                GD_IMPL_DIRECTIVE,
                context.get_info()
            )
        }
    });
    context.stack.pop();
    result
}
fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
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
            context
                .stack
                .push(format!("found parsed module {:?}", &relative_filepath));
            let mut import_specifiers: HashMap<Id, PathBuf> = HashMap::new();
            let module = parsed_source.module();
            let mut import_context =
                context.inherit(relative_filepath.as_path(), module_path, parsed_source);
            extract_import_specifiers(&mut import_context, module);
            for node in module.body.iter() {
                if let ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) = node {
                    import_context.pos.push(export.span.to_owned());
                    import_context.stack.push(format!(
                        "imported export decl {:?} {}",
                        relative_filepath,
                        import_context
                            .get_text(TERM_WIDTH)
                            .chars()
                            .take(180)
                            .collect::<String>()
                    ));
                    dbg_pos(&import_context, "import ExportDecl");

                    result = resolve_type_decl(&mut import_context, &export.decl, &id);
                    import_context.pos.pop();
                    import_context.stack.pop();
                }
                if result.is_some() {
                    break;
                }
            }

            if result.is_none() {
                panic!(
                    "no type found for {} in {:?} imported modules\n{}",
                    id.0,
                    &relative_filepath,
                    context.get_info()
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
    decl: &Decl,
    // if this id is supplied, only match declarations for this id's symbol
    match_id: &Id,
) -> Option<TypeResolution> {
    let mut result: Option<TypeResolution> = None;
    match decl {
        Decl::TsTypeAlias(alias) => {
            result = if match_id.0 == alias.id.to_id().0 {
                context.pos.push(alias.span.to_owned());
                context.stack.push(format!(
                    "resolve_type_decl:TsTypeAlias {}",
                    alias.id.to_id().0
                ));
                let mut result = resolve_type(context, &alias.type_ann);
                result.add_import(context);
                context.stack.pop();
                context.pos.pop();
                Some(result)
            } else {
                None
            }
        }
        Decl::TsInterface(intf) => {
            result = if match_id.0 == intf.id.to_id().0 {
                context.pos.push(intf.span.to_owned());
                let id = intf.id.to_id();
                let mut result: TypeResolution = intf.id.clone().into();
                result.interface_decl = Some(intf.as_ref().clone());
                result.ctor_type = Some(result.type_name.clone());
                context.pos.pop();
                Some(result)
            } else {
                None
            }
        }
        Decl::Class(cls_decl) => {
            let id = cls_decl.ident.to_id();
            if match_id.0 == id.0 {
                panic!(
                    "\
                    Conversion of class declarations {:?} is unsupported.\n\
                    Create an interface and have the class implement that interface instead\n\
                    {}",
                    id.0,
                    context.get_info()
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
                    {}",
                    context.get_info()
                );
            } else {
                if context.debug_print {
                    eprintln!("Skip Class Decl {:?}", id.0);
                }
            }
        }
        Decl::TsEnum(ts_enum) => {
            if match_id.0 == ts_enum.id.to_id().0 {
                let enum_model = ts_enum_to_model_enum(context, ts_enum);
                let mut resolved = TypeResolution::new(if enum_model.have_string_members {
                    "String"
                } else {
                    "int"
                });
                resolved.enums.push(enum_model);
                result = Some(resolved);
            }
        }
        _ => {
            dbg!(decl);
            panic!("IDK {}", context.get_info());
        }
    }
    result
}

fn should_skip(context: &ModuleContext) -> bool {
    for directive in [SKIP_DIRECTIVE, GD_IMPL_DIRECTIVE].into_iter() {
        if have_directive(context, directive) {
            if context.debug_print {
                eprintln!(
                    "Skipping due to {} directive: {:?}",
                    GD_IMPL_DIRECTIVE,
                    context.get_text(TERM_WIDTH)
                );
            }
            return true;
        }
    }
    false
}

fn have_directive(context: &ModuleContext, directive: &str) -> bool {
    let c = context.parsed_source.comments().as_single_threaded();
    for span in context.pos.iter().rev() {
        if let Some(comments) = c.get_leading(span.lo) {
            for c in comments.iter() {
                if (context.debug_print) {
                    eprintln!(
                        "Checking comment for {}: {:?} on {}",
                        directive,
                        c.text,
                        context.get_span_text(span)
                    );
                }
                if c.text.contains(directive) {
                    return true;
                }
            }
        }
    }
    return false;
}

fn update_resolve_from_comments(
    context: &mut ModuleContext,
    result: TypeResolution,
) -> TypeResolution {
    let mut result = result;
    let c = context.parsed_source.comments().as_single_threaded();
    let span = context.pos.last().unwrap();
    if let Some(comments) = c.get_leading(span.lo) {
        for c in comments.iter() {
            if let Some(gd_type) = context
                .regexes
                .gd_type
                .captures(&c.text)
                .and_then(|c| c.get(1))
            {
                // we only want to consume the type directive once for a given property signature...
                // maybe we want to change this to a number later and allow multiple directives
                if !context.type_directive_consumed {
                    context.type_directive_consumed = true;
                    result = result.clone();
                    result.type_name = gd_type.as_str().into();
                }
            }
        }
    }
    result
}

fn dbg_pos(context: &ModuleContext, comment: &str) {
    if context.debug_print {
        let pos = context.pos.last().unwrap();
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

fn detect_indent(template_str: &str, debug_print: bool) -> String {
    let space_str = if let Some(m) = Regex::new("(?m)^[ \t]+").unwrap().find(&template_str) {
        if debug_print {
            eprint!("Detected ");
        }
        m.as_str()
    } else {
        if debug_print {
            eprintln!("Unable to detect indentation");
            eprint!("Using default ");
        }
        DEFAULT_INDENT
    };
    if debug_print {
        let char_name = if let Some(char) = space_str.chars().nth(0) {
            if char == ' ' {
                "spaces"
            } else {
                "tabs"
            }
        } else {
            "unknown"
        };
        let num = space_str.len();
        eprintln!("indentation of {num} {char_name}.");
    }
    space_str.to_string()
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, path::PathBuf};

    use deno_ast::{
        parse_module,
        swc::{
            ast::{
                Decl, ExportDecl, Module, ModuleDecl, ModuleItem, Stmt, TsEnumDecl, TsInterfaceDecl,
            },
            common::iter::Iter,
        },
        MediaType, ParseParams, ParsedSource, SourceTextInfo,
    };

    use crate::{
        extract_import_specifiers, get_intf_model,
        model_context::{ModelContext, ModelImportContext, ModelVarDescriptor, DEFAULT_INDENT},
        ts_enum_to_model_enum, ModuleContext, GD_IMPL_DIRECTIVE, TYPE_DIRECTIVE,
    };

    const ts_intf_union: &'static str = "
        interface AKind {
            kind: 'a'
        }
        interface BKind {
            kind: 'b'
        }
        // {{}}
        export type AnyKind = AKind | BKind

        export interface Intf {
            anyKindProp: AnyKind;
        }
    ";
    struct TestContext {
        imports: HashMap<PathBuf, ParsedSource>,
        filename: PathBuf,
    }

    fn parse_from_string<'a>(filename: &str, source: &str) -> TestContext {
        let filename: PathBuf = filename.into();
        let parsed_source = parse_module(ParseParams {
            specifier: filename.to_string_lossy().into(),
            media_type: MediaType::TypeScript,
            text_info: SourceTextInfo::new(source.into()),
            capture_tokens: true,
            maybe_syntax: None,
            scope_analysis: false,
        })
        .unwrap();

        let mut imports: HashMap<PathBuf, ParsedSource> = HashMap::new();
        imports.insert(filename.to_path_buf(), parsed_source);
        let mut model: Option<ModelContext> = None;
        TestContext { imports, filename }
    }

    fn module_context<'a>(test_context: &'a TestContext) -> ModuleContext<'a> {
        let parsed_source_ref = test_context.imports.get(&test_context.filename).unwrap();
        ModuleContext::new(
            false,
            DEFAULT_INDENT,
            &test_context.filename,
            &test_context.filename,
            parsed_source_ref,
            &test_context.imports,
        )
    }

    fn get_exported_intfs(module: &Module) -> Vec<(&Box<TsInterfaceDecl>, &ExportDecl)> {
        module
            .body
            .iter()
            .map(|node| {
                if let ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) = node {
                    Some(export)
                } else {
                    None
                }
            })
            .flatten()
            .map(|export| {
                if let Decl::TsInterface(intf) = &export.decl {
                    Some((intf, export))
                } else {
                    None
                }
            })
            .flatten()
            .collect()
    }

    fn get_first_exported_intf(module: &Module) -> (&Box<TsInterfaceDecl>, &ExportDecl) {
        get_exported_intfs(module).into_iter().next().unwrap()
    }

    #[test]
    fn iso_date_type() {
        let src = "export interface Intf { date: Date }";
        let mut test_context = parse_from_string("iso-date-type.ts", &src);
        let mut context = module_context(&test_context);
        let module = context.parsed_source.module();
        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
        let imports: Vec<ModelImportContext> = model
            .imports
            .into_iter()
            .filter(|i| &i.name == "Iso8601Date")
            .collect();
        assert_eq!(
            imports.len(),
            1,
            "Should be one import for Iso8601Date type {:?}",
            imports
        );
        let import = imports.get(0).unwrap();
        assert_eq!(
            import.gd_impl, true,
            "Import should be marked as gd_impl: {:?}\n{}",
            import, src
        );
    }

    #[test]
    fn gd_impl_for_intf_union() {
        let src = ts_intf_union.replace("{{}}", &GD_IMPL_DIRECTIVE);
        let mut test_context = parse_from_string("any-kind.ts", &src);
        let mut context = module_context(&test_context);
        let module = context.parsed_source.module();
        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
        let import = model
            .imports
            .iter()
            .filter(|i| &i.name == "AnyKind")
            .next()
            .unwrap();
        assert_eq!(
            import.gd_impl, true,
            "Import should be marked as gd_impl: {:?}\n{}",
            import, src
        );
    }

    #[test]
    #[should_panic]
    fn no_gd_impl_for_intf_union() {
        let mut test_context = parse_from_string("any-kind.ts", ts_intf_union);
        let mut context = module_context(&test_context);
        let module = context.parsed_source.module();
        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
    }

    #[test]
    fn nullable_primitives() {
        let src = format!(
            "
            export interface Intf {{
                str: string | null;
                float: number | null;
                /* {}: int */
                int: number | null;
                bool: boolean | null;
            }}
        ",
            TYPE_DIRECTIVE
        );
        let mut test_context = parse_from_string("nullable-primitives.ts", &src);
        let mut context = module_context(&test_context);
        let module = context.parsed_source.module();
        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
        let mut vars: HashMap<String, &ModelVarDescriptor> = HashMap::new();
        for var in model.var_descriptors.iter() {
            assert_eq!(
                var.ctor.builtin, true,
                "{} should be builtin",
                var.ctor.name
            );
            assert_eq!(
                var.ctor.suffix.is_some(),
                true,
                "{} has no suffix?",
                var.ctor.name
            );
            vars.insert(var.src_name.to_string(), var);
        }
        assert_eq!(
            vars.get("str").unwrap().ctor.suffix.as_ref().unwrap(),
            " != null else \"\""
        );
        assert_eq!(
            vars.get("int").unwrap().ctor.suffix.as_ref().unwrap(),
            " != null else 0"
        );
        assert_eq!(
            vars.get("float").unwrap().ctor.suffix.as_ref().unwrap(),
            " != null else 0f"
        );
        assert_eq!(
            vars.get("bool").unwrap().ctor.suffix.as_ref().unwrap(),
            " != null else false"
        );
    }

    #[test]
    fn nullable_collections() {
        let src = "
            interface Member {
                kind: \"member\";
            }
            export interface Intf {
                array: Member[] | null;
                arrayGeneric: Array<Member> | null;
                record: Record<string, Member> | null;
                optionalNullableArrayOfStringOrNull?: Array<string | null> | null;
                optionalNullableArrayOfMemberOrNull?: Array<Member | null> | null;
            }
        ";
        let mut test_context = parse_from_string("nullable-collections.ts", src);
        let mut context = module_context(&test_context);
        let module = context.parsed_source.module();
        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
        let mut vars: HashMap<String, &ModelVarDescriptor> = HashMap::new();
        for var in model.var_descriptors.iter() {
            assert_eq!(
                var.optional,
                var.name.starts_with("optional"),
                "{} is not optional",
                var.name
            );
            assert_eq!(
                var.collection.is_some(),
                true,
                "{} has no collection",
                var.name
            );
            assert_eq!(
                var.collection.as_ref().unwrap().nullable,
                true,
                "{} collection is not nullable",
                var.name
            );
            assert_eq!(
                var.ctor.suffix.is_some(),
                var.name.starts_with("optional"),
                "{} has no suffix?",
                var.name
            );
            vars.insert(var.src_name.to_string(), var);
        }

        // TODO: these tests are wrong
        // test for collection.nullable , optional, ctor.nullable etc.
        let (array, arrayGeneric, record, optAOfStrOrNull, optAOfMemberOrNull) = (
            vars.get("array").unwrap(),
            vars.get("arrayGeneric").unwrap(),
            vars.get("record").unwrap(),
            vars.get("optionalNullableArrayOfStringOrNull").unwrap(),
            vars.get("optionalNullableArrayOfMemberOrNull").unwrap(),
        );
        assert_eq!(
            optAOfStrOrNull.ctor.suffix.as_ref().unwrap(),
            " != null else \"\""
        );
        assert_eq!(
            optAOfMemberOrNull.ctor.suffix.as_ref().unwrap(),
            " != null else null"
        );
    }

    #[test]
    fn auto_enum() {
        let src = "
        enum Enum {
            one, two, three
        }
        ";

        let mut test_context = parse_from_string("nullable-primitives.ts", &src);
        let mut context = module_context(&test_context);
        let module = context.parsed_source.module();
        let mut e: Option<&TsEnumDecl> = None;
        for node in module.body.iter() {
            if let ModuleItem::Stmt(Stmt::Decl(Decl::TsEnum(ts_enum))) = node {
                context.pos.push(ts_enum.span);
                e = Some(ts_enum);
                break;
            }
        }
        assert_eq!(e.is_some(), true);
        if let Some(e) = e {
            let model = ts_enum_to_model_enum(&context, e);
            assert_eq!(model.members.len(), e.members.len());
            assert_eq!(model.members.get(0).unwrap().value, "0");
        }
    }

    #[test]
    fn assign_enum() {
        let src = "
        enum Enum {
            one=1, two=2, three=3
        }
        ";
        let mut test_context = parse_from_string("nullable-primitives.ts", &src);
        let mut context = module_context(&test_context);
        context.debug_print = true;
        let module = context.parsed_source.module();
        let mut e: Option<&TsEnumDecl> = None;
        for node in module.body.iter() {
            if let ModuleItem::Stmt(Stmt::Decl(Decl::TsEnum(ts_enum))) = node {
                context.pos.push(ts_enum.span);
                e = Some(ts_enum);
                break;
            }
        }
        assert_eq!(e.is_some(), true);
        if let Some(e) = e {
            let model = ts_enum_to_model_enum(&context, e);
            assert_eq!(model.members.len(), 3);
            assert_eq!(model.members.get(0).unwrap().value, "1");
        }
    }

    #[test]
    fn assign_str_enum() {
        let src = "
        enum Enum {
            one='one', two='two', three='three'
        }
        ";
        let mut test_context = parse_from_string("nullable-primitives.ts", &src);
        let mut context = module_context(&test_context);
        let module = context.parsed_source.module();
        let mut e: Option<&TsEnumDecl> = None;
        for node in module.body.iter() {
            if let ModuleItem::Stmt(Stmt::Decl(Decl::TsEnum(ts_enum))) = node {
                context.pos.push(ts_enum.span);
                e = Some(ts_enum);
                break;
            }
        }
        assert_eq!(e.is_some(), true);
        if let Some(e) = e {
            let model = ts_enum_to_model_enum(&context, e);
            assert_eq!(model.members.len(), 3);
            assert_eq!(model.members.get(0).unwrap().value, "\"one\"");
        }
    }

    #[test]
    fn any_type() {
        let src = "export interface Intf { value: any }";
        let mut test_context = parse_from_string("any-type.ts", &src);
        let mut context = module_context(&test_context);
        let module = context.parsed_source.module();
        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
        assert_eq!(model.imports.len(), 0);
        assert_eq!(model.var_descriptors.len(), 1);
        let var = model.var_descriptors.get(0).unwrap();
        assert_eq!(var.ctor.builtin, true);
        assert_eq!(var.decl_type.is_none(), true);
        assert_eq!(var.ctor.start, "");
        assert_eq!(var.ctor.end, "");
        assert_eq!(var.collection.is_none(), true);
    }

    #[test]
    fn tuple_type() {
        let src = "export interface Intf { value: [string, string] }";
        let mut test_context = parse_from_string("tuple-type.ts", &src);
        let mut context = module_context(&test_context);
        let module = context.parsed_source.module();
        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
        assert_eq!(model.imports.len(), 0);
        assert_eq!(model.var_descriptors.len(), 1);
        let var = model.var_descriptors.get(0).unwrap();
        assert_eq!(var.ctor.builtin, true);
        assert_eq!(var.decl_type.as_ref().unwrap(), "String");
        assert_eq!(var.ctor.start, "");
        assert_eq!(var.ctor.end, "");
        assert_eq!(&var.collection.as_ref().unwrap().is_array, &true);
    }

    #[test]
    fn extends_interface() {
        let src = "
            interface B { bvalue: string; }
            export interface A extends B { avalue: string }
        ";
        let mut test_context = parse_from_string("extends-interface.ts", &src);
        let mut context = module_context(&test_context);
        let module = context.parsed_source.module();
        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
        assert_eq!(model.var_descriptors.len(), 2);
    }

    #[test]
    fn extends_imported_interface() {
        let base = "
            export interface B { bvalue: string; }
        ";
        let src = "
            import { B } from \"./base-interface.ts\";
            export interface A extends B { avalue: string; }
        ";
        let mut base_filename = "base-interface.ts";
        let mut base_test_context = parse_from_string(base_filename, &base);
        let mut base_context = module_context(&base_test_context);

        let mut test_context = parse_from_string("extends-interface.ts", &src);
        test_context
            .imports
            .insert(base_filename.into(), base_context.parsed_source.to_owned());
        let mut context = module_context(&test_context);

        let module = context.parsed_source.module();
        extract_import_specifiers(&mut context, module);

        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
        assert_eq!(model.var_descriptors.len(), 2);
    }

    #[test]
    #[should_panic]
    fn omit_type() {
        let src = "
            interface A {
                a: true;
                b: true;
            }
            export type B = Omit<A, \"a\">;
        ";
        let mut test_context = parse_from_string("omit.ts", &src);
        let mut context = module_context(&test_context);

        let module = context.parsed_source.module();
        extract_import_specifiers(&mut context, module);

        get_first_exported_intf(module);
    }

    #[test]
    #[should_panic]
    fn omit_property() {
        let src = "
            interface A {
                a: true;
                // not supported, (what is the gdscript class name?)
                b: Omit<B, \"c\">
            }
            type B = {
                b: true;
                c: true
            }
        ";
        let mut test_context = parse_from_string("omit.ts", &src);
        let mut context = module_context(&test_context);

        let module = context.parsed_source.module();
        extract_import_specifiers(&mut context, module);

        get_first_exported_intf(module);
    }

    #[test]
    fn extends_omit() {
        let src = "
            interface A {
                a: true;
                c: true;
            }
            export interface B extends Omit<A, \"a\"> {
                b: true;
            };
        ";
        let mut test_context = parse_from_string("extends-omit.ts", &src);
        let mut context = module_context(&test_context);

        let module = context.parsed_source.module();
        extract_import_specifiers(&mut context, module);

        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);

        assert_eq!(model.var_descriptors.len(), 1);
    }

    #[test]
    fn property_extends_omit() {
        let src = "
            interface A {
                a: true;
                c: true;
            }
            interface B extends Omit<A, \"a\"> {
                b: true;
            };
            export interface C {
                value: B
            }
        ";
        let mut test_context = parse_from_string("extends-omit.ts", &src);
        let mut context = module_context(&test_context);

        let module = context.parsed_source.module();
        extract_import_specifiers(&mut context, module);

        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);

        assert_eq!(model.var_descriptors.len(), 1);
        assert_eq!(model.var_descriptors.get(0).unwrap().name, "value");
        assert_eq!(model.imports.len(), 1);
        assert_eq!(model.imports.get(0).unwrap().name, "B")
    }

    #[test]
    fn imported_enum_property() {
        let enum_src = "
            export enum Enum {One=1,Two=2}
        ";
        let src = "
            import { Enum } from \"enum.ts\";
            export interface A { enum: Enum; }
        ";
        let mut enum_filename = "enum.ts";
        let mut enum_test_context = parse_from_string(enum_filename, &enum_src);
        let mut enum_context = module_context(&enum_test_context);

        let mut test_context = parse_from_string("imports-enum.ts", &src);
        test_context
            .imports
            .insert(enum_filename.into(), enum_context.parsed_source.to_owned());
        let mut context = module_context(&test_context);

        let module = context.parsed_source.module();
        extract_import_specifiers(&mut context, module);

        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
        assert_eq!(model.var_descriptors.len(), 1);
        assert_eq!(model.enums.len(), 1);
    }

    #[test]
    fn enum_property() {
        let src = "
            export enum Enum {One=1,Two=2}
            export interface A { enum: Enum; }
        ";
        let mut test_context = parse_from_string("imports-enum.ts", &src);
        let mut context = module_context(&test_context);

        let module = context.parsed_source.module();
        extract_import_specifiers(&mut context, module);

        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
        assert_eq!(model.var_descriptors.len(), 1);
        assert_eq!(model.enums.len(), 1);
        assert_eq!(model.enums.get(0).unwrap().have_string_members, false);
        assert_eq!(
            model
                .var_descriptors
                .get(0)
                .unwrap()
                .decl_type
                .as_ref()
                .unwrap(),
            "int"
        )
    }

    #[test]
    fn string_enum_member_expressions() {
        // there is no such thing as a string enum in gdscript,
        // All enums are just named int values.
        // The template should use a dictionary to emulate this behavior
        let src = "
            export enum Enum {One=\"one\",Two=\"two\"}
            export interface A { enum: Enum; }
        ";
        let mut test_context = parse_from_string("imports-enum.ts", &src);
        let mut context = module_context(&test_context);

        let module = context.parsed_source.module();
        extract_import_specifiers(&mut context, module);

        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
        assert_eq!(model.var_descriptors.len(), 1);
        assert_eq!(model.enums.len(), 1);
        assert_eq!(model.enums.get(0).unwrap().have_string_members, true);
        assert_eq!(
            model
                .var_descriptors
                .get(0)
                .unwrap()
                .decl_type
                .as_ref()
                .unwrap(),
            "String"
        )
    }

    #[test]
    #[should_panic]
    fn mixed_enum_member_expressions() {
        let src = "
            export enum Enum {One=1,Two=\"two\"}
            export interface A { enum: Enum; }
        ";
        let mut test_context = parse_from_string("imports-enum.ts", &src);
        let mut context = module_context(&test_context);

        let module = context.parsed_source.module();
        extract_import_specifiers(&mut context, module);

        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
    }

    #[test]
    #[should_panic]
    fn float_enum_member_expressions() {
        let src = "
            export enum Enum {One=1.5,Two=2.5}
            export interface A { enum: Enum; }
        ";
        let mut test_context = parse_from_string("imports-enum.ts", &src);
        let mut context = module_context(&test_context);

        let module = context.parsed_source.module();
        extract_import_specifiers(&mut context, module);

        let (intf, export) = get_first_exported_intf(module);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(&mut context, &intf, None, true);
    }
}
