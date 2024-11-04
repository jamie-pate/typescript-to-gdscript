#![allow(warnings)]
#[macro_use]
extern crate lazy_static;

use convert_case::{Case, Casing};
use deno_ast::{
    parse_module,
    swc::{
        ast::{
            BindingIdent, Decl, Expr, Id, Ident, Import, ImportDecl, ImportNamedSpecifier,
            ImportSpecifier, Lit, Module, ModuleDecl, ModuleItem, Pat, Stmt, TsEntityName,
            TsEnumDecl, TsEnumMemberId, TsExprWithTypeArgs, TsInterfaceDecl, TsKeywordType,
            TsKeywordTypeKind, TsLit, TsPropertySignature, TsType, TsTypeAliasDecl, TsTypeAnn,
            TsTypeElement, TsTypeParamInstantiation, TsTypeRef, TsUnionOrIntersectionType,
            TsUnionType,
        },
        atoms::Atom,
        common::{
            comments::Comments, serializer::Type, util::iter::IteratorExt, BytePos, Span,
            SyntaxContext,
        },
    },
    MediaType, ModuleSpecifier, ParseParams, ParsedSource, SourcePos, SourceRange, SourceRanged,
    SourceRangedForSpanned, SourceTextInfo,
};

use lazy_static::__Deref;
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
    fmt::Display,
    fs::{create_dir_all, read_to_string, write},
    ops::DerefMut,
    path::{self, Path, PathBuf},
    process::exit,
    rc::Rc,
};

use indoc::indoc;

use crate::model_context::ModelEnumMember;

const TERM_WIDTH: usize = 180;
const ISO_DATE_TYPE_NAME: &str = "Iso8601Date";
const TEMPLATE_NAME: &str = "gdscript-model";

const TYPE_DIRECTIVE: &str = "@typescript-to-gdscript-type";
const SKIP_DIRECTIVE: &str = "@typescript-to-gdscript-skip";
const GD_IMPL_DIRECTIVE: &str = "@typescript-to-gdscript-gd-impl";

pub mod model_context;

fn main() {
    let mut output_dir: Option<PathBuf> = None;
    let mut template_filename: Option<PathBuf> = None;
    let mut template_str: Option<String> = None;
    let mut src_files: Vec<PathBuf> = Vec::new();

    let mut context = Context::new();

    for arg in args().skip(1) {
        // TODO: in parallel!
        if arg == "--debug-print" {
            context.debug_print = true;
        } else if arg == "--debug-trace" {
            context.stack.show_trace = true;
            context.debug_print = true;
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
        context.indent = detect_indent(&template_str, context.debug_print);
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
        let directory = current_dir().expect("current_dir failed somehow");
        let mut import_stack: Vec<PathBuf> = Vec::new();
        for filename in src_files.iter() {
            let canonical_filepath = canonical_module_filename(&directory, &filename);
            parse_recursive(
                &mut import_stack,
                &mut context.imported_modules,
                &canonical_filepath,
                context.debug_print,
            );
        }
        if context.debug_print {
            eprintln!("{} files parsed", context.imported_modules.len());
        }
        for filename in src_files.iter() {
            let canonical_filepath = canonical_module_filename(&directory, &filename);
            convert(
                &output_dir,
                &filename,
                &canonical_filepath,
                &template_filename,
                &template,
                &mut context,
            );
        }
    } else {
        usage();
        exit(1);
    }
}

fn usage() {
    eprintln!(
        "Usage: {} [--debug-print] [--debug-trace] templatefile.gd.tmpl outputdir input1.ts [input2.ts...]",
        exe_name().expect("weird, exe name could not be retrieved")
    );
    eprintln!(indoc! {"
        Reads all interfaces from input.ts files exports them to outputdir/<InterfaceName>.gd files
        using the templatefile.gd.tmpl template to generate the classes.

        --debug-print: Print extra debug output
        --debug-trace: Print even more debug output (adds a trace of previous debug stack entries)
    "});
}

fn exe_name() -> Option<String> {
    return current_exe().ok()?.file_name()?.to_str()?.to_owned().into();
}

fn read_and_parse_file(import_stack: &mut Vec<PathBuf>, filename: &Path) -> ParsedSource {
    let source_text = read_to_string(filename).expect(&format!(
        "Unable to read file {:?}\n${:#?}",
        filename, import_stack
    ));
    let specifier = ModuleSpecifier::parse(&format!(
        "file://{}",
        filename
            .to_str()
            .expect(&format!("filename invalid {:?}", filename))
    ))
    .expect(&format!("can't parse filename {:?}", filename));
    return parse_module(ParseParams {
        specifier: specifier,
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
    if !result.starts_with(".") && !result.has_root() {
        // this is a node_js module and I don't want to go
        // searching for types from that rat's nest
        return result;
    }
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
    imported_modules: &mut HashMap<PathBuf, Rc<ParsedSource>>,
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
                    continue;
                }
                parse_recursive(import_stack, imported_modules, &m_filename, debug_print);
            }
        }
    }
    imported_modules.insert(canonical_filepath.to_path_buf(), parsed_source.into());
    if debug_print {
        eprintln!("parsed imports from {:?}", canonical_filepath);
    }
    import_stack.pop();
}

struct ModuleRegexes {
    gd_type: Regex,
}
lazy_static! {
    static ref REGEXES: ModuleRegexes = ModuleRegexes {
        gd_type: Regex::new(&format!("{}:\\s+(\\w+)", TYPE_DIRECTIVE)).unwrap(),
    };
}

struct TraceStack {
    stack: Vec<String>,
    trace: Vec<String>,
    show_trace: bool,
}

impl TraceStack {
    fn new() -> Self {
        TraceStack {
            stack: Vec::new(),
            trace: Vec::new(),
            show_trace: false,
        }
    }

    fn push(&mut self, str: String) {
        self.trace
            .push(format!("{}{}", ".".repeat(self.stack.len() + 1), &str));
        self.stack.push(str);
    }

    fn pop(&mut self) -> Option<String> {
        assert!(!self.stack.is_empty());
        let result = self.stack.pop();
        if self.stack.is_empty() {
            self.trace.clear();
        }
        result
    }

    fn first(&self) -> &str {
        if let Some(first) = self.stack.get(0) {
            first
        } else {
            "NO STACK!"
        }
    }
}

impl Display for TraceStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let trace = if self.show_trace {
            format!("\ntrace:\n\t{}", self.trace.join("\t\n"))
        } else {
            "".to_string()
        };
        write!(f, "stack:\n\t{}{}", self.stack.join("\t\n"), trace)
    }
}

struct Context {
    debug_print: bool,
    indent: String,
    imported_modules: HashMap<PathBuf, Rc<ParsedSource>>,
    stack: TraceStack,
    // Flag which specifies that if we are resolving a supported generic
    // builtin type (Omit, Partial, PartialDeep, etc) then we should resolve through
    // the type argument until we find an interface declaration.
    resolve_to_intf: bool,
}

impl Context {
    fn new() -> Self {
        Context {
            debug_print: false,
            indent: DEFAULT_INDENT.to_string(),
            imported_modules: HashMap::new(),
            stack: TraceStack::new(),
            resolve_to_intf: false,
        }
    }

    fn get_info(&self, mc: &ModuleContext) -> String {
        if self.debug_print {
            format!("{}\nstack:\n\t{}", mc.get_info_(), self.stack)
        } else {
            format!("{}\n{}", mc.get_info_(), self.stack.first())
        }
    }
}

#[derive(Debug, Clone)]
struct ModuleContext {
    type_directive_consumed: bool,
    pos: Vec<Span>,
    resolving_local: HashSet<Id>,
    output_type_name: String,
    relative_filepath: PathBuf,
    canonical_filepath: PathBuf,
    parsed_source: Rc<ParsedSource>,
    decl_stack: Vec<ResolutionDecl>,
    id_stack: Vec<Id>,
    type_args: Vec<Vec<Box<TsType>>>,
    import_specifiers: HashMap<Id, PathBuf>,
}

impl ModuleContext {
    fn new(
        relative_filepath: &Path,
        canonical_filepath: &Path,
        parsed_source: Rc<ParsedSource>,
    ) -> Self {
        ModuleContext {
            output_type_name: "".into(),
            type_directive_consumed: false,
            pos: Vec::new(),
            relative_filepath: relative_filepath.into(),
            canonical_filepath: canonical_filepath.into(),
            parsed_source,
            resolving_local: HashSet::new(),
            import_specifiers: HashMap::new(),
            type_args: Vec::new(),
            decl_stack: Vec::new(),
            id_stack: Vec::new(),
        }
    }

    fn get_span_text(&self, span: &Span) -> &str {
        self.parsed_source.text_info().range_text(&span.range())
    }

    fn get_text(&self, limit: usize) -> &str {
        let result = self.get_span_text(self.pos.last().unwrap());
        if limit > 0 && result.len() > limit {
            result.substring(0, limit)
        } else {
            result
        }
    }

    fn get_span_info(&self, span: &Span) -> String {
        format!(
            "{}:{}\n{}",
            &self.relative_filepath.to_string_lossy(),
            &self.parsed_source.text_info().line_index(span.start()),
            &self.get_span_text(span)
        )
    }

    fn get_info_(&self) -> String {
        self.pos
            .iter()
            .map(|span| self.get_span_info(span))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

fn convert(
    output_dir: &Path,
    filename: &Path,
    canonical_filepath: &Path,
    template_filename: &Path,
    template: &TinyTemplate,
    global: &mut Context,
) {
    let parsed_source = Rc::clone(global.imported_modules.get(canonical_filepath).expect(
        &format!(
            "Module {:?} is missing but should previously have been parsed",
            canonical_filepath
        ),
    ));
    let mut context = ModuleContext::new(filename, canonical_filepath, Rc::clone(&parsed_source));
    let mut module = parsed_source.module();
    let models = extract_module_models(global, &mut context, module);
    for model in models.into_iter() {
        let output_filepath = output_dir
            .join(&model.class_name.to_case(Case::Snake))
            .with_extension("gd");
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

fn extract_module_models(
    global: &mut Context,
    context: &mut ModuleContext,
    module: &Module,
) -> Vec<ModelContext> {
    extract_import_specifiers(context, module);
    let mut models: Vec<ModelContext> = Vec::new();
    let mut internal_models: HashMap<Id, ModelContext> = HashMap::new();
    for node in module.body.iter() {
        match node {
            ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) => {
                context.pos.push(export.span.to_owned());
                let skip = should_skip(global, context);
                context.pos.pop();
                if skip {
                    continue;
                }
                let decl = &export.decl;
                let mut resolves_to_intf = true;

                let res_decl = match decl {
                    Decl::TsInterface(intf) => Some((
                        intf.id.to_id(),
                        ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
                    )),
                    Decl::TsTypeAlias(type_alias) => {
                        if global.resolve_to_intf {
                            panic!("Recursive resolve_to_intf");
                        }
                        global.resolve_to_intf = true;
                        context.pos.push(export.span.to_owned());
                        context.id_stack.push(type_alias.id.to_id().to_owned());
                        let resolved_intf = resolve_type(global, context, &type_alias.type_ann);
                        context.id_stack.pop();
                        context.pos.pop();
                        global.resolve_to_intf = false;
                        resolves_to_intf =
                            matches!(resolved_intf.decl, Some(ResolutionDecl::TsInterfaceDecl(_)));
                        Some((
                            type_alias.id.to_id(),
                            ResolutionDecl::TsTypeAliasDecl(type_alias.to_owned()),
                        ))
                    }
                    _ => None,
                };
                if let Some((id, res_decl)) = res_decl {
                    context.pos.push(export.span.to_owned());
                    if !should_skip(global, &context) && resolves_to_intf {
                        global.stack.push(format!(
                            "{:?}:{:?} {:?}",
                            context.relative_filepath,
                            id,
                            context.get_text(TERM_WIDTH)
                        ));
                        models.push(get_intf_model(
                            global,
                            context,
                            &res_decl,
                            HashSet::new(),
                            None,
                        ));
                        global.stack.pop();
                    }
                    context.pos.pop();
                }
            }
            ModuleItem::Stmt(Stmt::Decl(Decl::TsInterface(intf))) => {
                context.pos.push(intf.span.to_owned());
                if !should_skip(global, &context) {
                    let id = intf.id.to_id();
                    global.stack.push(format!(
                        "{:?}:{:?}, {}",
                        context.relative_filepath,
                        id,
                        context.get_text(TERM_WIDTH)
                    ));
                    internal_models.insert(
                        id,
                        get_intf_model(
                            global,
                            context,
                            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
                            HashSet::new(),
                            None,
                        ),
                    );
                    global.stack.pop();
                }
                context.pos.pop();
            }
            _ => {}
        }
    }
    // pull in internal models that are needed
    let mut iterate_again = true;
    while iterate_again {
        let mut new_models: Vec<ModelContext> = Vec::new();
        for model in models.iter() {
            for var in &model.imports {
                if let Some(i_model) = internal_models.remove(&var.id) {
                    new_models.push(i_model);
                }
            }
        }
        iterate_again = new_models.len() > 0;
        if iterate_again {
            models.append(&mut new_models);
        }
    }
    if (!global.stack.stack.len() == 0) {
        panic!("Stack is not being cleared");
    }
    models
}

// caller is responsible for populating context.id_stack with the ts_enum id
fn ts_enum_to_model_enum(
    global: &Context,
    context: &mut ModuleContext,
    ts_enum: &TsEnumDecl,
) -> ModelEnum {
    let id = ts_enum.id.to_id();
    let member_types = ts_enum
        .members
        .iter()
        .map(|m| {
            if let Some(init_expr) = m.init.as_ref() {
                if let Expr::Lit(lit_expr) = init_expr.as_ref() {
                    (
                        Some(resolve_lit_type(global, context, &lit_to_ts_lit(lit_expr))),
                        m,
                    )
                } else {
                    panic!(
                        "Unsupported enum type not supported {:?} {:?}\n{}",
                        id,
                        init_expr,
                        global.get_info(context)
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
            global.get_info(context)
        );
    }
    if type_set.len() > 1 {
        panic!(
            "Mixed enum member types are not allowed: {}\n{}",
            type_set.into_iter().collect::<Vec<_>>().join(", "),
            global.get_info(context)
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
                        global.get_info(context)
                    ))
                } else {
                    i.to_string()
                },
            })
            .collect::<Vec<_>>(),
    }
}

fn resolve_interface_specifier_type(
    global: &mut Context,
    context: &mut ModuleContext,
    id: &Id,
) -> Option<(TypeResolution, Option<ModuleContext>)> {
    if let Some((t, import_context)) = resolve_imported_specifier_type(global, context, &id) {
        Some((t, Some(import_context)))
    } else if let Some(t) = resolve_local_specifier_type(global, context, &id) {
        Some((t, None))
    } else {
        None
    }
}

fn generic_type_arg0_ident<'a>(
    global: &Context,
    context: &ModuleContext,
    id: &Id,
    expr: &'a TsExprWithTypeArgs,
) -> (&'a TsTypeParamInstantiation, Id) {
    let args = expr.type_args.as_ref().expect(&format!(
        "Expect {} to have type_args\n{}",
        id.0,
        global.get_info(context)
    ));
    let arg0Id = args
        .params
        .get(0)
        .and_then(|p| p.as_ts_type_ref())
        .and_then(|t| t.type_name.as_ident())
        .expect(&format!(
            "Expected {}<T{}>'s T to be a type ref\n{}",
            id.0,
            if args.params.len() > 1 { ",..." } else { "" },
            global.get_info(context)
        ))
        .to_id()
        .to_owned();
    (args, arg0Id)
}

fn get_extends_intf_model(
    global: &mut Context,
    context: &mut ModuleContext,
    expr: &TsExprWithTypeArgs,
    extending: &Ident,
) -> ModelContext {
    if let Expr::Ident(ident) = expr.expr.as_ref() {
        let id = ident.to_id();
        global.stack.push(format!(
            "Searching for {} extended by {}",
            &id.0,
            &extending.to_id().0
        ));
        let (mut resolved, mut import_context) = if &id.0 == "PartialDeep" {
            let (_, arg0_id) = generic_type_arg0_ident(global, context, &id, expr);
            let (mut resolved, import_context) =
                resolve_interface_specifier_type(global, context, &arg0_id).expect(&format!(
                    "Expect PartialDeep<t>'s T to resolve to an interface\n{}",
                    global.get_info(context)
                ));
            // way too hard to support 'deep partial' since we'd have to write 'partial' versions of all the types
            // instead we mark them partial_deep and dynamically make them partial during construction
            resolved.partial_deep = true;
            (resolved, import_context)
        } else if let Some((t, import_context)) =
            resolve_interface_specifier_type(global, context, &id)
        {
            (t, import_context)
        } else if &id.0 == "Omit" {
            let (args, arg_0_id) = generic_type_arg0_ident(global, context, &id, expr);

            let (mut t, mut import_context) =
                resolve_interface_specifier_type(global, context, &arg_0_id).expect(&format!(
                    "Expect Omit<T,...>'s T to resolve to an Interface\n{}",
                    global.get_info(context)
                ));
            let intf_context = import_context.as_mut().unwrap_or(context);
            t.omit.append(
                &mut args
                    .params
                    .iter()
                    .skip(1)
                    .map(|k| resolve_type(global, context, k).string_literal_value())
                    .collect(),
            );
            (t, None)
        } else if &id.0 == "Readonly" {
            let (_, arg_0_id) = generic_type_arg0_ident(global, context, &id, expr);
            resolve_interface_specifier_type(global, context, &arg_0_id)
                .expect("Expect Readonly<T>'s T to resolve to an Interface")
        } else if &id.0 == "Partial" {
            let (_, arg_0_id) = generic_type_arg0_ident(global, context, &id, expr);
            let (mut resolved, import_context) =
                resolve_interface_specifier_type(global, context, &arg_0_id)
                    .expect("Expect Readonly<T>'s T to resolve to an Interface");
            resolved.partial = true;
            (resolved, import_context)
        } else {
            if global.debug_print {
                dbg!(expr);
            }
            eprintln!(
                "import_specifier for {:?}: {}",
                id,
                context
                    .import_specifiers
                    .get(&id)
                    .and_then(|p| p.to_str())
                    .unwrap_or("not found")
            );
            panic!(
                "Couldn't resolve identifier {} that's extended\n{}",
                String::from(&*id.0),
                global.get_info(context)
            );
        };
        let mut intf_context = import_context.as_mut().unwrap_or(context);
        intf_context.pos.push(resolved.pos);
        if let Some(args) = expr.type_args.as_ref() {
            intf_context.type_args.push(args.params.clone());
        }
        let mut model = get_intf_model(
            global,
            intf_context,
            &resolved
                .decl
                .expect("Interface or Type Alias decl should be here"),
            resolved.omit.drain(..).collect::<HashSet<_>>(),
            Some(&extending),
        );
        if resolved.partial || resolved.partial_deep {
            for d in model.var_descriptors.iter_mut() {
                d.optional = true;
            }
        }
        model.partial_deep = resolved.partial_deep;
        if expr.type_args.is_some() {
            intf_context.type_args.pop();
        }
        intf_context.pos.pop();
        global.stack.pop();

        model
    } else {
        panic!(
            "IDK what to do with this extends expression:{:#?}\n{}",
            expr,
            global.get_info(context)
        );
    }
}

fn get_intf_model(
    global: &mut Context,
    context: &mut ModuleContext,
    decl: &ResolutionDecl,
    mut omit: HashSet<String>,
    extending: Option<&Ident>,
) -> ModelContext {
    let mut owned_intf_context: ModuleContext;
    let mut decl_span_current_context = true;
    let (id, type_params, decl_span, aliased_type, intf, context) = match decl {
        ResolutionDecl::TsInterfaceDecl(intf) => {
            let id = intf.id.to_id();
            (
                id,
                &intf.type_params,
                intf.span,
                None,
                intf.to_owned(),
                context,
            )
        }
        ResolutionDecl::TsTypeAliasDecl(t) => {
            let id = t.id.to_id();
            let mut type_alias = t.to_owned();
            context.pos.push(type_alias.span);
            global
                .stack
                .push(format!("resolve TypeAliasDecl {}", type_alias.id.to_id().0));
            if global.resolve_to_intf {
                panic!(
                    "Unexpected recursive resolve_to_intf, {}",
                    global.get_info(context)
                );
            }
            global.resolve_to_intf = true;
            let aliased_type = resolve_type(global, context, &type_alias.type_ann);
            global.resolve_to_intf = false;
            global.stack.pop();
            context.pos.pop();
            // if we are following a typeref then we need to swap to that intf's context
            let (intf, ctxt) =
                if let (Some(ResolutionDecl::TsInterfaceDecl(intf)), Some(mut intf_context)) =
                    (&aliased_type.decl, aliased_type.decl_context.to_owned())
                {
                    owned_intf_context = intf_context;
                    decl_span_current_context = false;
                    (intf.to_owned(), &mut owned_intf_context)
                } else {
                    panic!(
                        "Unable to find interface that {} is derived from.\n{}",
                        id.0,
                        global.get_info(context)
                    );
                };
            (id, &t.type_params, t.span, Some(aliased_type), intf, ctxt)
        }
    };
    let sstr = id.0.to_string();
    if extending.is_none() {
        if type_params.is_some() {
            panic!(
                "Conversion of generic interface types is forbidden. Unable to determine exported type name.You may want to use {} or {}\n{}",
                SKIP_DIRECTIVE, GD_IMPL_DIRECTIVE, global.get_info(context)
            )
        }
        if !context.output_type_name.is_empty() {
            panic!("Nested top level get_intf_model")
        }
        context.output_type_name = sstr.clone();
    }
    global.stack.push(format!(
        "get_intf_model {} in {:?}",
        if let Some(extending) = extending {
            format!("{} extending {}", sstr, extending)
        } else {
            sstr
        },
        context.relative_filepath
    ));
    if decl_span_current_context {
        context.pos.push(decl_span);
    }
    context.decl_stack.push(decl.clone());
    context.id_stack.push(id.to_owned());
    let mut import_map: HashMap<String, ModelImportContext> = HashMap::new();
    let mut partial_deep = false;
    if let Some(aliased_type) = aliased_type.as_ref() {
        partial_deep = partial_deep || aliased_type.partial_deep;
        omit.extend(aliased_type.omit.to_owned().into_iter());
    }
    let intf_var_descriptors = intf
        .body
        .body
        .iter()
        .map(|e| get_intf_model_var(global, context, &mut import_map, e))
        // flatten filters out None!
        .flatten()
        .filter(|e| !omit.contains(&e.src_name))
        .collect::<Vec<_>>();
    let extended_types = intf
        .extends
        .iter()
        .map(|e| get_extends_intf_model(global, context, e, &intf.id))
        .collect::<Vec<_>>();
    let var_names = intf_var_descriptors
        .iter()
        .map(|d| d.src_name.clone())
        .collect::<HashSet<_>>();

    partial_deep = partial_deep || extended_types.iter().any(|t| t.partial_deep);
    let extended_descriptor_iterator = extended_types
        .into_iter()
        .map(|t| t.var_descriptors)
        .flatten()
        .filter(|d| !var_names.contains(&d.src_name));
    let mut var_descriptors: Vec<_> = extended_descriptor_iterator
        .chain(intf_var_descriptors)
        .collect();
    let enums: Vec<_> = var_descriptors
        .iter_mut()
        .map(|v| v.enums.iter())
        .flatten()
        .map(|e| e.clone())
        .collect();
    for import in var_descriptors
        .iter_mut()
        .map(|v| v.imports.iter())
        .flatten()
    {
        import_map.insert(import.name.clone(), import.clone());
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
    if decl_span_current_context {
        context.pos.pop();
    }
    global.stack.pop();
    context.id_stack.pop();
    context.decl_stack.pop();
    context.output_type_name = "".to_string();
    ModelContext {
        class_name: String::from(id.0.as_str()),
        canonical_src_filepath: context.canonical_filepath.to_path_buf(),
        comment: Some(format!(
            "Model for {} typescript interface in {:?}",
            id.0, context.relative_filepath
        )),
        src_type: Some(ModelSrcType {
            name: "Dictionary".to_string(),
            init: Some("{}".to_string()),
        }),
        state_methods: STATE_METHODS.to_string(),
        state_vars: STATE_VARS.to_string(),
        vars: var_descriptors
            .iter()
            .map(|d| d.render(&global.indent))
            .collect(),
        var_descriptors,
        enums,
        imports,
        partial_deep,
    }
}

// A smaller enum with only the declarations we
// actualy care about to make matching simpler
#[derive(Debug, Clone)]
enum ResolutionDecl {
    TsInterfaceDecl(Box<TsInterfaceDecl>),
    TsTypeAliasDecl(Box<TsTypeAliasDecl>),
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
    decl: Option<ResolutionDecl>,
    decl_context: Option<ModuleContext>,
    enums: Vec<ModelEnum>,
    omit: Vec<String>,
    partial: bool,
    partial_deep: bool,
    imports: Vec<ModelImportContext>,
    pos: Span,
    // Typescript symbol/context id of this var descriptor
    id: Id,
}

impl TypeResolution {
    fn add_import(&mut self, context: &ModuleContext) {
        if let Some(type_name) = &self.ctor_type {
            if &context.output_type_name != type_name && !is_builtin(type_name) {
                self.imports.push(ModelImportContext {
                    name: type_name.clone(),
                    src: format!("{}.gd", type_name.to_case(Case::Snake)),
                    gd_impl: self.gd_impl,
                    id: self.id.to_owned(),
                })
            }
        }
    }

    fn gd_impl(context: &ModuleContext, span: &Span) -> Self {
        let mut result = TypeResolution::new(context, "", span);
        result.gd_impl = true;
        result
    }

    fn merge_imports<'a, I>(&mut self, vals: &[TypeResolution]) {
        for t in vals {
            let mut clone = t.imports.clone();
            self.imports.append(&mut clone);
        }
    }

    fn new(context: &ModuleContext, name: &str, span: &Span) -> Self {
        let id = context
            .id_stack
            .last()
            .expect("Expected the id_stack to have at least one value");
        TypeResolution {
            type_name: String::from(name),
            ctor_type: None,
            nullable: false,
            collection: None,
            comment: None,
            literal: None,
            gd_impl: false,
            decl: None,
            decl_context: None,
            omit: Vec::new(),
            partial: false,
            partial_deep: false,
            enums: Vec::new(),
            imports: Vec::new(),
            pos: span.clone(),
            id: id.to_owned(),
        }
    }

    fn take_imports(&mut self, vals: &mut [TypeResolution]) {
        for t in vals {
            self.imports.append(&mut t.imports);
        }
    }

    fn append_comment(&mut self, comment: &str) {
        let safe_comment = comment.replace("\n", "↵");
        self.comment = if let Some(c) = self.comment.as_ref() {
            Some(format!("{}, {}", c, safe_comment))
        } else {
            Some(safe_comment)
        };
    }

    fn string_literal_value(&self) -> String {
        // TODO: this may be fragile? it counts on other places using
        // the "" to quote string literals
        self.literal
            .as_ref()
            .expect(&format!("Expect {:?} to have a string literal value", self))
            .replace("\"", "")
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
            decl: None,
            decl_context: None,
            omit: Vec::new(),
            partial: false,
            partial_deep: false,
            enums: Vec::new(),
            imports: Vec::new(),
            pos: ident.span.clone(),
            id: ident.to_id(),
        }
    }
}

fn resolve_type_ref(
    global: &mut Context,
    context: &mut ModuleContext,
    type_ref: &TsTypeRef,
) -> TypeResolution {
    if let Some(ident) = &type_ref.type_name.as_ident() {
        let id = ident.to_id();
        global.stack.push(format!("resolve_type_ref {:?}", &id));
        context.id_stack.push(id.to_owned());
        let mut resolved_type =
            if let Some((t, _)) = resolve_imported_specifier_type(global, context, &id) {
                t
            } else {
                resolve_local_specifier_type_or_builtin(global, context, type_ref, &id)
            };
        context.id_stack.pop();
        // Special case where we want to exclude the contents of this part of the tree
        // for example we encountered a GD_IMPL_DIRECTIVE on an interface union
        if resolved_type.gd_impl {
            if resolved_type.type_name == "" {
                resolved_type.type_name = (&*id.0).to_string();
                resolved_type.ctor_type = Some(resolved_type.type_name.clone());
            }
        }
        resolved_type.add_import(context);
        global.stack.pop();
        resolved_type
    } else {
        panic!(
            "IDK what to do with {:#?}\n{}",
            type_ref.type_name,
            global.get_info(context)
        );
    }
}

fn resolve_union_type(
    global: &mut Context,
    context: &mut ModuleContext,
    union_type: &TsUnionType,
) -> TypeResolution {
    if (union_type.types.len() < 2) {
        panic!(
            "At least two types required in a union type\n{}",
            global.get_info(context)
        );
    }
    let mut it = union_type.types.iter();
    let (a, b) = (it.next().unwrap(), it.next().unwrap());
    global.stack.push(format!(
        "resolve union_type ({:?} types: <\n\t{:?},\n\t{:?},\n\t ...?>)",
        union_type.types.len(),
        a,
        b
    ));
    let mut a_type = resolve_type(global, context, &a);
    let mut b_type = resolve_type(global, context, &b);
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
            .map(|t| resolve_type(global, context, &t))
            .collect();
        global.stack.push(format!(
            "union_types: {:?}",
            all_types
                .iter()
                .map(|tr| &tr.type_name)
                .collect::<Vec<&String>>()
        ));
        let mut result = if !all_types.iter().all(|rt| rt.type_name == a_type.type_name) {
            if have_directive(global, context, GD_IMPL_DIRECTIVE) {
                TypeResolution::gd_impl(context, &union_type.span)
            } else {
                if all_types.iter().all(|t| is_builtin(&t.type_name)) {
                    let mut result = TypeResolution::new(context, "any", &union_type.span);
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
                    panic!(
                        "Unsupported type, union types must all have the same godot type unless \
                        you supply {} directive or they must all be builtin types. {:?}\n{}",
                        GD_IMPL_DIRECTIVE,
                        all_types,
                        global.get_info(context)
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
        global.stack.pop();
        result
    };
    global.stack.pop();
    result
}

// type name, ctor, collection, comment
fn resolve_type(
    global: &mut Context,
    context: &mut ModuleContext,
    ts_type: &TsType,
) -> TypeResolution {
    let result = match (ts_type) {
        TsType::TsTypeRef(type_ref) => {
            global
                .stack
                .push(format!("resolve type_ref {:?}", type_ref.type_name));
            let result = resolve_type_ref(global, context, &type_ref);
            global.stack.pop();
            result
        }
        TsType::TsArrayType(array_type) => {
            global
                .stack
                .push(format!("resolve array_type {:?}", array_type));
            let mut result = resolve_type(global, context, &array_type.elem_type);
            let ctor_type = result.type_name.clone();

            result.append_comment(context.get_span_text(&array_type.span));
            let collection = result
                .collection
                .as_ref()
                .and_then(|c| Some(Box::new(c.clone())));
            result.collection = Some(ModelVarCollection {
                is_array: true,
                is_dict: false,
                nullable: false,
                item_collection: collection,
            });
            result.ctor_type = Some(ctor_type);
            global.stack.pop();
            result
        }
        TsType::TsKeywordType(kw_type) => {
            global
                .stack
                .push(format!("resolve keyword_type {:?}", kw_type));
            let mut result = match (kw_type.kind) {
                TsKeywordTypeKind::TsNumberKeyword => {
                    TypeResolution::new(context, "float", &kw_type.span)
                }
                // Godot 3 uses 64 bit ints in 64 bit builds but probably we shouldn't use this type anyways, idk
                TsKeywordTypeKind::TsBigIntKeyword => {
                    TypeResolution::new(context, "int", &kw_type.span)
                }
                TsKeywordTypeKind::TsBooleanKeyword => {
                    TypeResolution::new(context, "bool", &kw_type.span)
                }
                TsKeywordTypeKind::TsStringKeyword => {
                    TypeResolution::new(context, "String", &kw_type.span)
                }
                // used internally, probably not useful though
                TsKeywordTypeKind::TsNullKeyword => {
                    TypeResolution::new(context, "null", &kw_type.span)
                }
                TsKeywordTypeKind::TsAnyKeyword => {
                    TypeResolution::new(context, "any", &kw_type.span)
                }
                kind => panic!(
                    "IDK what to do with keyword type {:#?}\n{}",
                    kind,
                    global.get_info(context)
                ),
            };
            global.stack.pop();
            result
        }
        TsType::TsUnionOrIntersectionType(ui_type) => {
            global
                .stack
                .push("resolve union_or_intersection_type".into());
            let result = match ui_type {
                TsUnionOrIntersectionType::TsUnionType(union_type) => {
                    resolve_union_type(global, context, union_type)
                }
                TsUnionOrIntersectionType::TsIntersectionType(_) => {
                    panic!(
                        "Intersection types are not allowed\n{}",
                        global.get_info(context)
                    );
                }
            };
            global.stack.pop();
            result
        }
        TsType::TsLitType(lit_type) => {
            global
                .stack
                .push(format!("resolve TsLitType {:?}", lit_type));
            let result = resolve_lit_type(global, context, &lit_type.lit);
            global.stack.pop();
            result
        }
        // TODO: add array length check?
        TsType::TsTupleType(tuple_type) => {
            let elem_types = tuple_type
                .elem_types
                .iter()
                .map(|t| resolve_type(global, context, &t.ty))
                .collect::<Vec<_>>();
            let mut result = elem_types.get(0).unwrap().clone();
            if !elem_types.iter().all(|t| t.type_name == result.type_name) {
                panic!(
                    "Only homogenous tuple types are allowed {:#?}\n{}",
                    tuple_type.elem_types,
                    global.get_info(context)
                );
            }
            result.collection = Some(ModelVarCollection {
                is_array: true,
                is_dict: false,
                nullable: false,
                item_collection: None,
            });
            result
        }
        TsType::TsTypeLit(type_lit) => {
            panic!(
                "Type literals are forbidden: \"{}\"\n\
                Property or collection types must be a named interface: \n{}",
                context.get_span_text(&type_lit.span),
                global.get_info(context)
            );
        }
        _ => panic!(
            "IDK what to do with this type {:#?}\n{}",
            ts_type,
            global.get_info(context)
        ),
    };
    return update_resolve_from_comments(context, result);
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

fn resolve_lit_type(global: &Context, context: &ModuleContext, lit: &TsLit) -> TypeResolution {
    match (lit) {
        TsLit::Number(number) => {
            let s = if let Some(atom) = &number.raw {
                atom.to_string()
            } else {
                number.value.to_string()
            };
            let mut result = if s.contains(".") {
                TypeResolution::new(context, "float", &number.span)
            } else {
                TypeResolution::new(context, "int", &number.span)
            };
            result.comment = Some(format!("Literally {}", &s));
            result.literal = Some(s);
            result
        }
        TsLit::Str(str) => {
            let mut result = TypeResolution::new(context, "String", &str.span);
            let str_literal = format!("\"{}\"", str.value);
            result.comment = Some(format!("Literally {}", str_literal));
            result.literal = Some(str_literal);
            result
        }
        TsLit::Bool(bool) => {
            let mut result = TypeResolution::new(context, "bool", &bool.span);
            result.comment = Some(format!("Literally {}", bool.value));
            result.literal = Some(bool.value.to_string());
            result
        }
        TsLit::BigInt(big_int) => {
            let mut result = TypeResolution::new(context, "int", &big_int.span);
            result.comment = Some(format!("Literally {}", big_int.value));
            result.literal = Some(big_int.value.to_string());
            result
        }
        _ => panic!(
            "IDK what to do with this type {:#?}\n{}",
            lit,
            global.get_info(context)
        ),
    }
}

fn get_intf_model_var(
    global: &mut Context,
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
        let type_ann = prop_sig.type_ann.as_ref().expect(&format!(
            "IDK what to do with this type {:#?}\n{}",
            prop_sig,
            global.get_info(context)
        ));
        let empty = String::from("");
        global.stack.push(format!(
            "get_intf_model_var {} {:?}",
            &src_name.as_ref().unwrap_or(&empty),
            context.get_text(TERM_WIDTH)
        ));
        context.pos.push(prop_sig.span);
        context.type_directive_consumed = false;
        let resolved = resolve_type(global, context, &*type_ann.type_ann);
        context.pos.pop();
        global.stack.pop();
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
                decl_type: if let Some(c) = &resolved.collection {
                    Some(c.gd_type().to_string())
                } else if resolved.type_name != "any" {
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
    if global.debug_print {
        dbg!(type_element);
    }
    return None;
}

fn resolve_local_specifier_type(
    global: &mut Context,
    context: &mut ModuleContext,
    id: &Id,
) -> Option<TypeResolution> {
    let mut decl_resolved: Option<TypeResolution> = None;
    global
        .stack
        .push(format!("resolve_local_specifier_type {:?}", id));
    let parsed_source = Rc::clone(&context.parsed_source);
    if !context.resolving_local.contains(&id) {
        context.resolving_local.insert(id.clone());
        for node in parsed_source.module().body.iter() {
            match node {
                ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) => {
                    context.pos.push(export.span.to_owned());
                    let decl = &export.decl;
                    let resolved = resolve_type_decl(global, context, &decl, &id);
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
                    let alias_or_intf_pos = match decl {
                        Decl::TsTypeAlias(alias) => Some(alias.span.to_owned()),
                        Decl::TsInterface(intf) => Some(intf.span.to_owned()),
                        _ => None,
                    };

                    let resolved = if let Some(new_pos) = alias_or_intf_pos {
                        context.pos.push(new_pos);
                        let res = resolve_type_decl(global, context, &decl, &id);
                        context.pos.pop();
                        res
                    } else {
                        resolve_type_decl(global, context, &decl, &id)
                    };
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
    global.stack.pop();
    decl_resolved
}

fn resolve_first_type_param(
    global: &mut Context,
    context: &mut ModuleContext,
    type_ref: &TsTypeRef,
    id: &str,
) -> TypeResolution {
    let type_param = type_ref
        .type_params
        .as_ref()
        .and_then(|tp| tp.params.first())
        .expect(&format!(
            "Expected {} to have at least one type param which resolves to an interface.{}",
            id,
            global.get_info(context)
        ));
    resolve_type(global, context, type_param)
}

fn resolve_iso_date_type(context: &ModuleContext, span: &Span) -> TypeResolution {
    let mut result = TypeResolution::gd_impl(context, span);
    result.type_name = ISO_DATE_TYPE_NAME.to_string();
    result.ctor_type = Some(result.type_name.clone());
    result
}

fn resolve_local_specifier_type_or_builtin(
    global: &mut Context,
    context: &mut ModuleContext,
    type_ref: &TsTypeRef,
    id: &Id,
) -> TypeResolution {
    let mut decl_resolved: Option<TypeResolution> =
        resolve_local_specifier_type(global, context, id);
    let mut decl_type_ref_resolved: Option<TypeResolution> = None;
    let mut type_ann: Option<Box<TsType>> = None;
    let sstr = &*id.0;
    if decl_resolved.is_none() {
        if let Some(ResolutionDecl::TsTypeAliasDecl(decl)) = context.decl_stack.last().as_ref() {
            if let TsType::TsTypeRef(decl_type_ref) = decl.type_ann.as_ref() {
                if decl_type_ref == type_ref {
                    let mut r = TypeResolution::new(context, &*decl.id.to_id().0, &type_ref.span);
                    r.ctor_type = Some(r.type_name.clone());
                    r.decl = Some(ResolutionDecl::TsTypeAliasDecl(decl.to_owned()));
                    r.decl_context = Some(context.clone());
                    decl_type_ref_resolved = Some(r);
                    type_ann = Some(decl.type_ann.to_owned());
                }
            }
        }
    }
    if decl_resolved.is_some() {
        global
            .stack
            .push(format!("resolved local specifier for {}", sstr));
    } else {
        global.stack.push(format!(
            "resolve_local_specifier_type_or_builtin (fallback to builtin) for {}",
            sstr
        ));
    }
    // if we were already resolving a type that hasn't been imported
    // or we just can't find the type then maybe it's a TypeScript builtin?
    // TODO: is there a deno_ast helper for these?
    let result = decl_resolved.unwrap_or_else(|| match (sstr) {
        "Date" => {
            // gd_impl type Iso8601Date.gd must be provided
            // TODO: create reference implementation gdscript code?
            resolve_iso_date_type(context, &type_ref.span)
        }
        "Record" => {
            let ctor_type: Option<TypeResolution> = if let Some(prop_type) =
                type_ref.type_params.as_ref().and_then(|p| p.params.get(1))
            {
                Some(resolve_type(global, context, &prop_type))
            } else {
                None
            };
            let collection = ctor_type.as_ref().and_then(|c| {
                if let Some(coll) = &c.collection {
                    Some(Box::new(coll.clone()))
                } else {
                    None
                }
            });
            let collection = ctor_type
                .as_ref()
                .and_then(|ct| ct.collection.as_ref())
                .and_then(|c| Some(Box::new(c.clone())));
            let comment = if let Some(ct) = &ctor_type {
                Some(ct.type_name.clone())
            } else {
                None
            };
            let mut result =
                ctor_type.unwrap_or_else(|| TypeResolution::new(context, "Dictionary", &type_ref.span));
            if let Some(c) = comment {
                result.comment = if let Some(rc) = result.comment {
                    Some(format!("{rc} {c}"))
                } else {
                    Some(c)
                }
            }
            result.append_comment(context.get_span_text(&type_ref.span));
            result.collection = Some(ModelVarCollection {
                is_array: false,
                is_dict: true,
                nullable: false,
                item_collection: collection,
            });
            result
        }
        "Array" => {
            let ctor_type: Option<TypeResolution> = if let Some(prop_type) =
                type_ref.type_params.as_ref().and_then(|p| p.params.get(0))
            {
                Some(resolve_type(global, context, &prop_type))
            } else {
                None
            };
            let collection = ctor_type
                .as_ref()
                .and_then(|ct| ct.collection.as_ref())
                .and_then(|c| Some(Box::new(c.clone())));
            let mut result =
                ctor_type.unwrap_or_else(|| TypeResolution::new(context, "Array", &type_ref.span));
            result.append_comment(context.get_span_text(&type_ref.span));
            result.collection = Some(ModelVarCollection {
                is_array: true,
                is_dict: false,
                nullable: false,
                item_collection: collection,
            });
            result
        }
        "Omit" => {
            if global.resolve_to_intf {
                let mut resolved = resolve_first_type_param(global, context, type_ref, sstr);
                let args = type_ref.type_params.as_ref()
                    .expect(&format!("Expected {} to have type params.\n{}", sstr, global.get_info(context)));

                let mut new_omit = args.params
                    .iter()
                    .skip(1)
                    .map(|k| resolve_type(global, context, k).string_literal_value()).collect::<Vec<_>>();
                resolved.omit.append(&mut new_omit);
                resolved
            } else {
                panic!(
                "Omit<T, ...> is forbidden in this position. Unable to determine exported type name?\n{}",
                global.get_info(context)
            )}
        },
        "Partial" => {
            if let (Some(mut resolved), Some(type_ann)) = (decl_type_ref_resolved, type_ann) {
                resolved.partial = true;
                resolved
            } else if global.resolve_to_intf {
                let mut resolved = resolve_first_type_param(global, context, type_ref, sstr);
                resolved.partial = true;
                resolved
            } else {
                panic!(
                    "Partial<T> is forbidden in this position. Unable to determine exported type name?\n{}",
                    global.get_info(context)
                );
            }
        },
        "PartialDeep" => {
            if let (Some(mut resolved), Some(type_ann)) = (decl_type_ref_resolved, &type_ann) {
                resolved.partial_deep = true;
                resolved
            } else if global.resolve_to_intf {
                let mut resolved = resolve_first_type_param(global, context, type_ref, sstr);
                resolved.partial_deep = true;
                resolved
            } else {
                panic!(
                "PartialDeep<T> is forbidden in this position. Unable to determine exported type name?\n{}",
                global.get_info(context))
            }
        },
        "Readonly" => panic!(
            "Readonly<T> is forbidden in this position. Unable to determine exported type name?\n{}",
            global.get_info(context)
        ),
        _ => {
            let mut resolved_type_param: Option<TypeResolution> = None;
            if let Some(ResolutionDecl::TsInterfaceDecl(intf)) = context.decl_stack.last().cloned() {
                if let Some(type_params) = intf.type_params.as_ref() {
                    // TODO: do we search up the stacks?
                    for (i, p) in type_params.params.iter().enumerate() {
                        if &p.name.to_id() == id {
                            let type_args = &context.type_args.last().cloned();
                            resolved_type_param = if let Some(type_arg) = type_args.as_ref().and_then(|a| a.get(i)) {
                                Some(resolve_type(global, context, type_arg))
                            } else {
                                panic!("Unable to find matching type param {}\n{}",
                                id.0, global.get_info(context))
                            };
                            break;
                        }
                    }
                }
            }
            if let Some(rtp) = resolved_type_param {
                rtp
            } else if have_directive(global, context, GD_IMPL_DIRECTIVE) {
                TypeResolution::gd_impl(context, &type_ref.span)
            } else {
                if global.debug_print {
                    dbg!(type_ref);
                }
                panic!(
                    "Unable to resolve type {} while exporting {:?}.\n\
                    \tIs it a built in type or generic type parameter?\n\
                    \tYou can prefix {} with a comment containing {} or {} to skip this type.
                    \n{}",
                    &id.0,
                    context.relative_filepath,
                    context.output_type_name,
                    SKIP_DIRECTIVE,
                    GD_IMPL_DIRECTIVE,
                    global.get_info(context)
                )
            }
        }
    });
    global.stack.pop();
    result
}
fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}
fn resolve_imported_specifier_type<'a>(
    global: &mut Context,
    context: &'a ModuleContext,
    id: &Id,
) -> Option<(TypeResolution, ModuleContext)> {
    let mut result: Option<(TypeResolution, ModuleContext)> = None;
    let path = context.import_specifiers.get(id).to_owned();
    if let Some(module_path) = path {
        global.stack.push(format!(
            "resolve imported type_ref {:?} -> ${:?}",
            id, module_path
        ));
        if let Some(parsed_source) = global.imported_modules.get(module_path) {
            let parsed_source = Rc::clone(parsed_source);
            let dir = context.canonical_filepath.parent().expect(&format!(
                "Expected {:?} to have a parent",
                &context.canonical_filepath
            ));
            let relative_filepath = diff_paths(module_path, dir).expect(&format!(
                "Pathdiff {:?} relative to {:?}",
                module_path,
                context.canonical_filepath.parent()
            ));
            global
                .stack
                .push(format!("found parsed module {:?}", &relative_filepath));
            let mut import_context =
                ModuleContext::new(&relative_filepath, module_path, Rc::clone(&parsed_source));
            let module = parsed_source.module();
            extract_import_specifiers(&mut import_context, module);
            let mut resolved: Option<TypeResolution> = None;
            for node in module.body.iter() {
                if let ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) = node {
                    import_context.pos.push(export.span.to_owned());
                    global.stack.push(format!(
                        "imported export decl {:?} {}",
                        relative_filepath,
                        import_context
                            .get_text(TERM_WIDTH)
                            .chars()
                            .take(180)
                            .collect::<String>()
                    ));

                    resolved = resolve_type_decl(global, &mut import_context, &export.decl, &id);
                    import_context.pos.pop();
                    global.stack.pop();
                }
                if resolved.is_some() {
                    break;
                }
            }
            result = if let Some(resolved) = resolved {
                Some((resolved, import_context))
            } else {
                panic!(
                    "no type found for {} in {:?} imported modules\n{}",
                    id.0,
                    &relative_filepath,
                    global.get_info(context)
                );
            };
            global.stack.pop();
        } else if &*id.0 != "PartialDeep" {
            panic!(
                "Expected to be able to resolve {:?} in imported modules {}",
                id,
                global.get_info(context)
            );
        }
        global.stack.pop();
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
    global: &mut Context,
    context: &mut ModuleContext,
    decl: &Decl,
    // if this id is supplied, only match declarations for this id's symbol
    match_id: &Id,
) -> Option<TypeResolution> {
    let mut result: Option<TypeResolution> = None;
    match decl {
        Decl::TsTypeAlias(alias) => {
            result = if match_id.0 == alias.id.to_id().0 {
                global.stack.push(format!(
                    "resolve_type_decl:TsTypeAlias {}",
                    alias.id.to_id().0
                ));
                context.id_stack.push(alias.id.to_id().to_owned());
                let mut result = if have_directive(global, context, GD_IMPL_DIRECTIVE) {
                    TypeResolution::gd_impl(context, &alias.span)
                } else {
                    context
                        .decl_stack
                        .push(ResolutionDecl::TsTypeAliasDecl(alias.to_owned()));
                    let r = resolve_type(global, context, &alias.type_ann);
                    context.decl_stack.pop();
                    r
                };
                context.id_stack.pop();
                result.decl = Some(ResolutionDecl::TsTypeAliasDecl((alias.to_owned())));
                result.decl_context = Some(context.clone());
                result.add_import(context);
                global.stack.pop();
                Some(result)
            } else {
                if global.debug_print {
                    eprintln!("type alias {} != {}", alias.id.to_id().0, match_id.0);
                }
                None
            }
        }
        Decl::TsInterface(intf) => {
            result = if match_id.0 == intf.id.to_id().0 {
                let id = intf.id.to_id();
                let mut result: TypeResolution = intf.id.clone().into();
                result.decl = Some(ResolutionDecl::TsInterfaceDecl(intf.to_owned()));
                result.decl_context = Some(context.clone());
                result.ctor_type = Some(result.type_name.clone());
                result.gd_impl = have_directive(global, context, GD_IMPL_DIRECTIVE);
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
                    Conversion of class types such as {} is forbidden.\n\
                    Create an interface and have the class implement that interface instead\n\
                    {}",
                    id.0,
                    global.get_info(context)
                );
            } else {
                if global.debug_print {
                    eprintln!("Skip Class Decl {:?}", id.0);
                }
            }
        }
        Decl::Var(var_decl) => {
            // TODO: check to make sure the match id isn't in this list and panic if it is
            let names: Vec<Option<&BindingIdent>> =
                var_decl.decls.iter().map(|d| d.name.as_ident()).collect();
            if global.debug_print {
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
                    global.get_info(context)
                );
            } else {
                if global.debug_print {
                    eprintln!("Skip Class Decl {:?}", id.0);
                }
            }
        }
        Decl::TsEnum(ts_enum) => {
            let id = ts_enum.id.to_id();
            if match_id.0 == id.0 {
                context.id_stack.push(id.to_owned());
                let enum_model = ts_enum_to_model_enum(global, context, ts_enum);
                let mut resolved = TypeResolution::new(
                    context,
                    if enum_model.have_string_members {
                        "String"
                    } else {
                        "int"
                    },
                    &ts_enum.span,
                );
                context.id_stack.pop();
                resolved.append_comment(&*id.0);
                resolved.enums.push(enum_model);
                result = Some(resolved);
            }
        }
        _ => {
            dbg!(decl);
            panic!("IDK {}", global.get_info(context));
        }
    }
    result
}

fn should_skip(global: &Context, context: &ModuleContext) -> bool {
    for directive in [SKIP_DIRECTIVE, GD_IMPL_DIRECTIVE].into_iter() {
        if have_directive(global, context, directive) {
            if global.debug_print {
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

fn have_directive(global: &Context, context: &ModuleContext, directive: &str) -> bool {
    let c = context.parsed_source.comments().as_single_threaded();
    for span in context.pos.iter().rev() {
        if let Some(comments) = c.get_leading(span.lo) {
            for c in comments.iter() {
                if (global.debug_print) {
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
            if let Some(gd_type) = REGEXES.gd_type.captures(&c.text).and_then(|c| c.get(1)) {
                // we only want to consume the type directive once for a given property signature...
                // maybe we want to change this to a number later and allow multiple directives
                if !context.type_directive_consumed {
                    context.type_directive_consumed = true;
                    result = match gd_type.as_str() {
                        ISO_DATE_TYPE_NAME => resolve_iso_date_type(context, span),
                        _ => {
                            result = result.clone();
                            result.type_name = gd_type.as_str().into();
                            result
                        }
                    };
                    result.add_import(context);
                }
            }
        }
    }
    result
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
    use std::collections::HashSet;
    use std::{borrow::Borrow, collections::HashMap, path::PathBuf, rc::Rc};

    use crate::model_context::tests::indent;
    use crate::{
        extract_import_specifiers, get_intf_model,
        model_context::{ModelContext, ModelImportContext, ModelVarDescriptor, DEFAULT_INDENT},
        ts_enum_to_model_enum, Context, ModuleContext, GD_IMPL_DIRECTIVE, TYPE_DIRECTIVE,
    };
    use crate::{extract_module_models, ResolutionDecl};
    use cool_asserts::assert_panics;
    use deno_ast::swc::ast::TsTypeAliasDecl;
    use deno_ast::SourceRangedForSpanned;
    use deno_ast::{
        parse_module,
        swc::{
            ast::{
                Decl, ExportDecl, Module, ModuleDecl, ModuleItem, Stmt, TsEnumDecl, TsInterfaceDecl,
            },
            common::iter::Iter,
        },
        MediaType, ModuleSpecifier, ParseParams, ParsedSource, SourceTextInfo,
    };
    use indoc::indoc;

    const ts_intf_union: &'static str = "
        interface AKind {
            kind: 'a'
        }
        interface BKind {
            kind: 'b'
        }
        // {{}}
        type AnyKind = AKind | BKind

        export interface Intf {
            anyKindProp: AnyKind;
        }
    ";

    const ts_extend_generic: &'static str = "
        interface C { id: number; }
        // {{}}
        type B<T> = T | C;
        export interface A {
            b: B
        }
    ";

    struct TestContext {
        imports: HashMap<PathBuf, Rc<ParsedSource>>,
        filename: PathBuf,
    }

    impl TestContext {
        fn add_imports_from(&mut self, other: &[&TestContext]) {
            self.imports.extend(
                other
                    .iter()
                    .map(|o| &o.imports)
                    .flatten()
                    .map(|(k, v)| (k.clone(), Rc::clone(&v))),
            );
        }
    }

    fn parse_and_get_model<'a>(filename: &str, source: &str) -> ModelContext {
        parse_and_get_model_with_imports(filename, source, &[])
    }

    fn parse_and_get_model_with_imports<'a>(
        filename: &str,
        source: &str,
        imports: &[&'a TestContext],
    ) -> ModelContext {
        let mut models = parse_and_get_models_with_imports(filename, source, imports);
        models.truncate(1);
        models.remove(0)
    }

    fn parse_and_get_models(filename: &str, source: &str) -> Vec<ModelContext> {
        parse_and_get_models_with_imports(filename, source, &[])
    }

    fn parse_and_get_models_with_imports<'a>(
        filename: &str,
        source: &str,
        imports: &[&'a TestContext],
    ) -> Vec<ModelContext> {
        let mut test_context = parse_from_string(filename, &source);
        test_context.add_imports_from(imports);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        extract_module_models(&mut global, &mut context, parsed_source.module())
    }

    fn parse_from_string<'a>(filename: &str, source: &str) -> TestContext {
        let filename: PathBuf = filename.into();
        let specifier = ModuleSpecifier::parse(&format!("file://{}", filename.to_string_lossy()))
            .expect(&format!("can't parse filename {:?}", filename));
        let parsed_source = parse_module(ParseParams {
            specifier: specifier,
            media_type: MediaType::TypeScript,
            text_info: SourceTextInfo::new(source.into()),
            capture_tokens: true,
            maybe_syntax: None,
            scope_analysis: false,
        })
        .expect(&format!("Unable to parse {:?}", filename));

        let mut imports: HashMap<PathBuf, Rc<ParsedSource>> = HashMap::new();
        imports.insert(filename.to_path_buf(), Rc::new(parsed_source));
        TestContext { imports, filename }
    }

    fn module_context(test_context: &TestContext) -> (Context, ModuleContext, Rc<ParsedSource>) {
        let mut global = Context::new();
        global.imported_modules.extend(
            test_context
                .imports
                .iter()
                .map(|(k, v)| (k.clone(), Rc::clone(v))),
        );
        let parsed_source = test_context.imports.get(&test_context.filename).unwrap();
        let mut context = ModuleContext::new(
            &test_context.filename,
            &test_context.filename,
            Rc::clone(parsed_source),
        );
        (global, context, Rc::clone(parsed_source))
    }

    fn get_mc_exports<'a>(
        context: &mut ModuleContext,
        parsed_source: &'a Rc<ParsedSource>,
    ) -> Vec<(ResolutionDecl, &'a ExportDecl)> {
        let module = parsed_source.module();
        extract_import_specifiers(context, module);
        get_exported(module)
    }

    fn get_mc_export_intf<'a>(
        context: &mut ModuleContext,
        parsed_source: &'a Rc<ParsedSource>,
    ) -> (Box<TsInterfaceDecl>, &'a ExportDecl) {
        let mut exports = get_mc_exports(context, parsed_source);
        exports.truncate(1);
        if let (ResolutionDecl::TsInterfaceDecl(intf), export) = exports.remove(0) {
            (intf, export)
        } else {
            panic!("Expected an intf export");
        }
    }

    fn get_mc_export_type_alias<'a>(
        context: &mut ModuleContext,
        parsed_source: &'a Rc<ParsedSource>,
    ) -> (Box<TsTypeAliasDecl>, &'a ExportDecl) {
        let mut exports = get_mc_exports(context, parsed_source);
        exports.truncate(1);
        if let (ResolutionDecl::TsTypeAliasDecl(type_alias), export) = exports.remove(0) {
            (type_alias, export)
        } else {
            panic!("Expected an type alias export");
        }
    }

    fn get_exported(module: &Module) -> Vec<(ResolutionDecl, &ExportDecl)> {
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
                    Some((ResolutionDecl::TsInterfaceDecl(intf.to_owned()), export))
                } else if let Decl::TsTypeAlias(type_alias) = &export.decl {
                    Some((
                        ResolutionDecl::TsTypeAliasDecl(type_alias.to_owned()),
                        export,
                    ))
                } else {
                    None
                }
            })
            .flatten()
            .collect()
    }

    #[test]
    fn iso_date_type() {
        let src = "export interface Intf { date: Date }";
        let mut test_context = parse_from_string("iso-date-type.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
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
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let mut exports = get_mc_exports(&mut context, &parsed_source);
        assert_eq!(exports.len(), 1);
        exports.truncate(1);
        let (exported, export) = exports.remove(0);

        context.pos.push(export.span);
        let mut model: ModelContext =
            get_intf_model(&mut global, &mut context, &exported, HashSet::new(), None);
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
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
    }

    #[test]
    fn gd_impl_for_extend_generic() {
        let src = ts_extend_generic.replace("{{}}", &GD_IMPL_DIRECTIVE);
        let model = parse_and_get_model("gd-impl-extended-generic.ts", &src);
        assert_eq!(model.var_descriptors.len(), 1);
        assert_eq!(model.imports.len(), 1);
        let import = model.imports.get(0).unwrap();
        assert_eq!(import.name, "B");
        assert_eq!(import.gd_impl, true);
    }

    #[test]
    #[should_panic]
    fn no_gd_impl_for_extend_generic() {
        assert_panics!(
            {
                parse_and_get_model("extend-generic.ts", &ts_extend_generic);
            },
            includes("PartialDeep<T> is forbidden in this position.")
        );
    }

    #[test]
    fn nullable_optionals() {
        let src = format!(
            "
            interface Other {{a: 1}}
            export interface Intf {{
                obj?: Other | null;
                str?: string | null;
                float?: number | null;
                /* {}: int */
                int?: number | null;
                bool?: boolean | null;
            }}
        ",
            TYPE_DIRECTIVE
        );
        let mut test_context = parse_from_string("nullable-primitives.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
        let mut vars: HashMap<String, &ModelVarDescriptor> = HashMap::new();
        for var in model.var_descriptors.iter() {
            assert_eq!(
                var.ctor.suffix.is_some(),
                true,
                "{} has no suffix?",
                var.ctor.name
            );
            vars.insert(var.src_name.to_string(), var);
        }
        assert_eq!(
            vars.get("obj").unwrap().ctor.suffix.as_ref().unwrap(),
            ") != TYPE_NIL else null"
        );
        assert_eq!(
            vars.get("str").unwrap().ctor.suffix.as_ref().unwrap(),
            ") != TYPE_NIL else \"\""
        );
        assert_eq!(
            vars.get("int").unwrap().ctor.suffix.as_ref().unwrap(),
            ") != TYPE_NIL else 0"
        );
        assert_eq!(
            vars.get("float").unwrap().ctor.suffix.as_ref().unwrap(),
            ") != TYPE_NIL else 0.0"
        );
        assert_eq!(
            vars.get("bool").unwrap().ctor.suffix.as_ref().unwrap(),
            ") != TYPE_NIL else false"
        );
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
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
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
            ") != TYPE_NIL else \"\""
        );
        assert_eq!(
            vars.get("int").unwrap().ctor.suffix.as_ref().unwrap(),
            ") != TYPE_NIL else 0"
        );
        assert_eq!(
            vars.get("float").unwrap().ctor.suffix.as_ref().unwrap(),
            ") != TYPE_NIL else 0.0"
        );
        assert_eq!(
            vars.get("bool").unwrap().ctor.suffix.as_ref().unwrap(),
            ") != TYPE_NIL else false"
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
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
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
        let (array, arrayGeneric, record, opt_a_of_str_or_null, opt_a_of_member_or_null) = (
            vars.get("array").unwrap(),
            vars.get("arrayGeneric").unwrap(),
            vars.get("record").unwrap(),
            vars.get("optionalNullableArrayOfStringOrNull").unwrap(),
            vars.get("optionalNullableArrayOfMemberOrNull").unwrap(),
        );
        assert_eq!(
            opt_a_of_str_or_null.ctor.suffix.as_ref().unwrap(),
            ") != TYPE_NIL else \"\""
        );
        assert_eq!(
            opt_a_of_member_or_null.ctor.suffix.as_ref().unwrap(),
            ") != TYPE_NIL else null"
        );
    }

    #[test]
    fn auto_enum() {
        let src = "
        enum Enum {
            one, two, three
        }
        ";

        let mut test_context = parse_from_string("auto-enum.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let module = parsed_source.module();
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
            context.id_stack.push(e.id.to_id().to_owned());
            let model = ts_enum_to_model_enum(&global, &mut context, e);
            context.id_stack.pop();
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
        let mut test_context = parse_from_string("assign-enum.ts", &src);

        let (mut global, mut context, parsed_source) = module_context(&test_context);
        global.debug_print = true;
        let module = parsed_source.module();
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
            context.id_stack.push(e.id.to_id().to_owned());
            let model = ts_enum_to_model_enum(&global, &mut context, e);
            context.id_stack.pop();
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
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let module = parsed_source.module();
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
            context.id_stack.push(e.id.to_id().to_owned());
            let model = ts_enum_to_model_enum(&global, &mut context, e);
            context.id_stack.pop();
            assert_eq!(model.members.len(), 3);
            assert_eq!(model.members.get(0).unwrap().value, "\"one\"");
        }
    }

    #[test]
    fn any_type() {
        let src = "export interface Intf { value: any }";
        let mut test_context = parse_from_string("any-type.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
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
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
        assert_eq!(model.imports.len(), 0);
        assert_eq!(model.var_descriptors.len(), 1);
        let var = model.var_descriptors.get(0).unwrap();
        assert_eq!(var.ctor.builtin, true);
        assert_eq!(var.decl_type.as_ref().unwrap(), "Array");
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
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
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
        let (_, mut base_context, _) = module_context(&base_test_context);

        let mut test_context = parse_from_string("extends-interface.ts", &src);
        test_context.add_imports_from(&[&base_test_context]);

        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
        assert_eq!(model.var_descriptors.len(), 2);
    }

    #[test]
    fn extends_imported_interface_which_extends() {
        let base = "
            export interface C { cvalue: string; }
            export interface B extends C { bvalue: string; }
        ";
        let src = "
            import { B } from \"./base-interface.ts\";
            export interface A extends B { avalue: string; }
        ";
        let mut base_filename = "base-interface.ts";
        let mut base_test_context = parse_from_string(base_filename, &base);
        let (_, mut base_context, _) = module_context(&base_test_context);

        let mut test_context = parse_from_string("extends-interface.ts", &src);
        test_context.add_imports_from(&[&base_test_context]);

        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
        assert_eq!(model.var_descriptors.len(), 3);
    }

    #[test]
    fn extends_imported_interface_which_extends_and_imports() {
        let team_id = "
            // @typescript-to-gdscript-type: int
            export type TeamId = number;
        ";
        let base = "
            import { TeamId } from \"./team-id.ts\";
            export interface C { cvalue: TeamId; }
            export interface B extends C { bvalue: string; }
        ";
        let src = "
            import { B } from \"./base-interface.ts\";
            export interface A extends B { avalue: string; }
        ";
        let mut team_id_test_context = parse_from_string("team-id.ts", team_id);
        let mut base_test_context = parse_from_string("base-interface.ts", &base);
        let (_, mut team_id_context, _) = module_context(&team_id_test_context);
        let (_, mut base_context, _) = module_context(&base_test_context);

        let mut test_context = parse_from_string("extends-interface.ts", &src);

        test_context.add_imports_from(&[&base_test_context, &team_id_test_context]);

        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);
        global.debug_print = true;
        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
        assert_eq!(model.var_descriptors.len(), 3);
    }

    #[test]
    fn extends_imported_interface_which_extends_readonly_of_and_imports_twice() {
        let team_id = "
            // @typescript-to-gdscript-type: int
            export type TeamId = number;
        ";
        let base = "
            import { TeamId } from \"./team-id.ts\";
            export interface C { cvalue: TeamId; }
            export interface B extends C { bvalue: string; }
        ";
        let src = "
            import { B } from \"./base-interface.ts\";
            export interface A extends Readonly<B> { avalue: string; }
        ";
        let mut team_id_test_context = parse_from_string("team-id.ts", team_id);
        let mut base_test_context = parse_from_string("base-interface.ts", &base);
        let (_, mut team_id_context, _) = module_context(&team_id_test_context);
        let (_, mut base_context, _) = module_context(&base_test_context);

        let mut test_context = parse_from_string("extends-interface.ts", &src);

        test_context.add_imports_from(&[&base_test_context, &team_id_test_context]);

        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);
        global.debug_print = true;
        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
        assert_eq!(model.var_descriptors.len(), 3);
        assert_eq!(model.imports.len(), 0);
    }

    #[test]
    fn extends_imported_interface_which_extends_readonly_of_and_imports_twice_with_intf_members() {
        // this interface would generate a gdscript class that we need to import
        let d_interface = "
            export interface D {
                value: String;
            }
        ";
        let base = "
            import { D } from \"d-interface.ts\";
            export interface C { d: D; overridden_value: string }
            export interface B extends C { overridden_value: string; }
        ";
        let src = "
            import { B } from \"./base-interface.ts\";
            export interface A extends Readonly<B> { avalue: string; }
        ";
        let mut d_test_context = parse_from_string("d-interface.ts", d_interface);
        let mut base_test_context = parse_from_string("base-interface.ts", &base);
        let (_, mut d_context, _) = module_context(&d_test_context);
        let (_, mut base_context, _) = module_context(&base_test_context);

        let mut test_context = parse_from_string("extends-interface.ts", &src);

        test_context.add_imports_from(&[&base_test_context, &d_test_context]);

        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);
        global.debug_print = true;
        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
        assert_eq!(model.var_descriptors.len(), 3);
        assert_eq!(model.imports.len(), 1);
    }

    #[test]
    fn extends_imported_interface_which_extends_and_imports_readonly() {
        let team_id = "
            // @typescript-to-gdscript-type: int
            export type TeamId = number;
        ";
        let base = "
            import { TeamId } from \"./team-id.ts\";
            export interface C { readonly cvalue: TeamId; }
            export interface B extends C { bvalue: string; }
        ";
        let src = "
            import { B } from \"./base-interface.ts\";
            export interface A extends B { avalue: string; }
        ";
        let mut team_id_test_context = parse_from_string("team-id.ts", team_id);
        let mut base_test_context = parse_from_string("base-interface.ts", &base);
        let (_, mut team_id_context, _) = module_context(&team_id_test_context);
        let (_, mut base_context, _) = module_context(&base_test_context);

        let mut test_context = parse_from_string("extends-interface.ts", &src);

        test_context.add_imports_from(&[&base_test_context, &team_id_test_context]);

        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);
        global.debug_print = true;
        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
        assert_eq!(model.var_descriptors.len(), 3);
    }

    #[test]
    fn omit_type() {
        let src = "
            interface A {
                a: true;
                b: true;
            }
            export type B = Omit<A, \"a\">;
        ";
        let model = parse_and_get_model("omit-type.ts", src);
        assert_eq!(model.var_descriptors.len(), 1);
        let var_desc = model.var_descriptors.get(0).unwrap();
        assert_eq!(var_desc.name, "b");
    }

    #[test]
    #[should_panic]
    fn omit_property() {
        let src = "
            export interface A {
                a: true;
                // not supported, (what is the gdscript class name?)
                b: Omit<B, \"c\">
            }
            interface B {
                b: true;
                c: true
            }
        ";
        let model = parse_and_get_model("omit.ts", src);
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
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );

        assert_eq!(model.var_descriptors.len(), 2);
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
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );

        assert_eq!(model.var_descriptors.len(), 1);
        assert_eq!(model.var_descriptors.get(0).unwrap().name, "value");
        assert_eq!(model.imports.len(), 1);
        assert_eq!(model.imports.get(0).unwrap().name, "B")
    }

    #[test]
    fn readonly_of_property() {
        let src = "
            export interface A {
                a: true;
                // not supported, (what is the gdscript class name?)
                b: Readonly<B>
            }
            interface B {
                b: true;
                c: true
            }
        ";
        assert_panics!(
            {
                parse_and_get_model("readonly-of-property.ts", &src);
            },
            includes("Readonly<T> is forbidden in this position."),
        )
    }

    #[test]
    fn readonly_property() {
        let src = "
            export interface A {
                a: true;
                // readonly modifier is discarded
                readonly b: B
            }
            interface B {
                b: true;
                c: true
            }
        ";
        let models = parse_and_get_models("readonly-property.ts", &src);
        assert_eq!(models.len(), 2);
        let model_a = models.get(0).unwrap();
        assert_eq!(model_a.class_name, "A");
        assert_eq!(model_a.var_descriptors.len(), 2);
        assert_eq!(model_a.var_descriptors[1].ctor.name, "B");

        let model_b = models.get(1).unwrap();
        assert_eq!(model_b.class_name, "B");
    }

    #[test]
    fn extends_readonly() {
        let src = "
            interface A {
                a: true;
                c: true;
            }
            export interface B extends Readonly<A> {
                b: true;
            };
        ";
        let model = parse_and_get_model("extends-readonly.ts", &src);
        assert_eq!(model.var_descriptors.len(), 3);
    }

    #[test]
    fn property_extends_readonly() {
        let src = "
            interface A {
                a: true;
                c: true;
            }
            interface B extends Readonly<A> {
                b: true;
            };
            export interface C {
                value: B
            }
        ";
        let mut test_context = parse_from_string("property-extends-readonly.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );

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
        let (_, mut enum_context, _) = module_context(&enum_test_context);

        let mut test_context = parse_from_string("imports-enum.ts", &src);
        test_context
            .imports
            .insert(enum_filename.into(), enum_context.parsed_source.to_owned());

        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
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
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
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
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
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
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
    }

    #[test]
    #[should_panic]
    fn float_enum_member_expressions() {
        let src = "
            export enum Enum {One=1.5,Two=2.5}
            export interface A { enum: Enum; }
        ";
        let mut test_context = parse_from_string("imports-enum.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
    }

    #[test]
    fn no_type_literal_property_type() {
        let src = "
            export interface A { value: Array<{a:string}>; }
        ";
        let mut test_context = parse_from_string("literal-prop.ts", &src);
        assert_panics!(
            {
                let (mut global, mut context, parsed_source) = module_context(&test_context);
                let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

                context.pos.push(export.span);
                get_intf_model(
                    &mut global,
                    &mut context,
                    &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
                    HashSet::new(),
                    None,
                )
            },
            includes("Type literals are forbidden: \"{a:string}\"")
        )
    }

    #[test]
    fn no_class_property_type() {
        let src = "
            class Cls {}
            export interface A { value: Cls; }
        ";
        let mut test_context = parse_from_string("literal-prop.ts", &src);
        assert_panics!(
            {
                let (mut global, mut context, parsed_source) = module_context(&test_context);
                let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

                context.pos.push(export.span);
                get_intf_model(
                    &mut global,
                    &mut context,
                    &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
                    HashSet::new(),
                    None,
                )
            },
            includes("Conversion of class types such as Cls is forbidden"),
        )
    }

    #[test]
    fn dict_of_arrays() {
        let src = "
            export interface A {
                values?: Record<number, B[]>;
            }

            export interface B {
                value: string;
            }
        ";
        let mut test_context = parse_from_string("literal-prop.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let model = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
        assert_eq!(model.class_name, "A");
        assert_eq!(model.var_descriptors.len(), 1);
        let descriptor = model.var_descriptors.get(0).unwrap();
        assert_eq!(descriptor.decl_type.as_ref().unwrap(), "Dictionary");
        assert_eq!(descriptor.collection.is_some(), true);
        let collection = descriptor.collection.as_ref().unwrap();
        assert_eq!(collection.is_dict, true);
        assert_eq!(collection.item_collection.is_some(), true);
        let collection2 = collection.item_collection.as_ref().unwrap();
        assert_eq!(collection2.is_array, true)
    }

    #[test]
    fn dict_of_array_generic() {
        let src = "
            export interface A {
                values?: Record<number, Array<B>>;
            }

            export interface B {
                value: string;
            }
        ";
        let mut test_context = parse_from_string("literal-prop.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let model = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
        assert_eq!(model.class_name, "A");
        assert_eq!(model.var_descriptors.len(), 1);

        let descriptor = model.var_descriptors.get(0).unwrap();
        assert_eq!(descriptor.decl_type.as_ref().unwrap(), "Dictionary");
        assert_eq!(descriptor.collection.is_some(), true);
        let collection = descriptor.collection.as_ref().unwrap();
        assert_eq!(collection.is_dict, true);
        assert_eq!(collection.item_collection.is_some(), true);
        let collection2 = collection.item_collection.as_ref().unwrap();
        assert_eq!(collection2.is_array, true)
    }

    #[test]
    fn array_of_dict() {
        let src = "
            export interface A {
                values?: Record<number, B>[];
            }

            export interface B {
                value: string;
            }
        ";
        let mut test_context = parse_from_string("literal-prop.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let model = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
        assert_eq!(model.class_name, "A");
        assert_eq!(model.var_descriptors.len(), 1);

        let descriptor = model.var_descriptors.get(0).unwrap();
        assert_eq!(descriptor.decl_type.as_ref().unwrap(), "Array");
        assert_eq!(descriptor.collection.is_some(), true);
        let collection = descriptor.collection.as_ref().unwrap();
        assert_eq!(collection.is_array, true);
        assert_eq!(collection.item_collection.is_some(), true);
        let collection2 = collection.item_collection.as_ref().unwrap();
        assert_eq!(collection2.is_dict, true)
    }

    #[test]
    fn array_generic_of_dict() {
        let src = "
            export interface A {
                values?: Array<Record<number, B>>;
            }

            export interface B {
                value: string;
            }
        ";
        let mut test_context = parse_from_string("literal-prop.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let model = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );
        assert_eq!(model.class_name, "A");
        assert_eq!(model.var_descriptors.len(), 1);

        let descriptor = model.var_descriptors.get(0).unwrap();
        assert_eq!(descriptor.decl_type.as_ref().unwrap(), "Array");
        assert_eq!(descriptor.collection.is_some(), true);
        let collection = descriptor.collection.as_ref().unwrap();
        assert_eq!(collection.is_array, true);
        assert_eq!(collection.item_collection.is_some(), true);
        let collection2 = collection.item_collection.as_ref().unwrap();
        assert_eq!(collection2.is_dict, true)
    }

    #[test]
    fn extends_generic() {
        let src = "
            interface A {
                a: true;
            }
            interface B<T> {
                b: T;
            }
            export interface C extends B<A> {
                c: true;
            };
        ";
        let mut test_context = parse_from_string("extends-generic.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );

        assert_eq!(model.var_descriptors.len(), 2);
        assert_eq!(
            model
                .var_descriptors
                .get(0)
                .unwrap()
                .decl_type
                .as_ref()
                .unwrap(),
            "A"
        );
    }

    #[test]
    fn extends_imported_generic() {
        let base_a = "
            export interface A {
                a: true;
            }
        ";
        let base_b = "
            import { A } from \"./a.js\";
            export interface B<T> {
                b: T;
            }
        ";
        let src = "
            import { A } from \"./a.js\";
            import { B } from \"./b.js\";
            export interface C extends B<A> {
                c: true;
            };
        ";
        let a_test_context = parse_from_string("a.ts", base_a);
        let b_test_context = parse_from_string("b.ts", base_b);
        let mut test_context = parse_from_string("extends-generic.ts", &src);
        test_context.add_imports_from(&[&a_test_context, &b_test_context]);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );

        assert_eq!(model.var_descriptors.len(), 2);
        assert_eq!(
            model
                .var_descriptors
                .get(0)
                .unwrap()
                .decl_type
                .as_ref()
                .unwrap(),
            "A"
        );
    }

    #[test]
    fn no_gen_generic_interface() {
        let src = "
            interface B {}
            export interface A<B> {}
        ";
        let mut test_context = parse_from_string("gen-generic-interface.ts", &src);
        assert_panics!(
            {
                let (mut global, mut context, parsed_source) = module_context(&test_context);
                let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

                context.pos.push(export.span);
                get_intf_model(&mut global, &mut context, &ResolutionDecl::TsInterfaceDecl(intf.to_owned()), HashSet::new(), None)
            },
            includes("Conversion of generic interface types is forbidden. Unable to determine exported type name."),
        )
    }

    #[test]
    fn decl_comments() {
        let src = "

            export enum EnumEnum {One=1,Two=2}
            export enum EnumDict {One=\"one\",Two=\"two\"}
            export interface A {
                enum_enum: EnumEnum;
                enum_dict: EnumDict;
                record: Record<string, EnumEnum>;
                array1: Array<EnumDict>;
                array2: EnumEnum[];
            };
        ";
        let mut test_context = parse_from_string("decl-comments.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let (intf, export) = get_mc_export_intf(&mut context, &parsed_source);

        context.pos.push(export.span);
        let mut model: ModelContext = get_intf_model(
            &mut global,
            &mut context,
            &ResolutionDecl::TsInterfaceDecl(intf.to_owned()),
            HashSet::new(),
            None,
        );

        assert_eq!(model.var_descriptors.len(), 5);
        let enum_enum = model.var_descriptors.get(0).unwrap();
        let enum_dict = model.var_descriptors.get(1).unwrap();
        let record = model.var_descriptors.get(2).unwrap();
        let array1 = model.var_descriptors.get(3).unwrap();
        let array2 = model.var_descriptors.get(4).unwrap();
        assert_eq!(enum_enum.comment.as_ref().unwrap(), "EnumEnum");
        assert_eq!(enum_dict.comment.as_ref().unwrap(), "EnumDict");
        assert_eq!(
            record.comment.as_ref().unwrap(),
            "EnumEnum int, Record<string, EnumEnum>"
        );
        assert_eq!(
            array1.comment.as_ref().unwrap(),
            "EnumDict, Array<EnumDict>"
        );
        assert_eq!(array2.comment.as_ref().unwrap(), "EnumEnum, EnumEnum[]")
    }

    #[test]
    fn partial_extends() {
        let src = "
            interface B extends {
                a: 1
            }
            export interface A extends Partial<B> {}
        ";
        let mut model = parse_and_get_model("partial-extends.ts", &src);
        assert_eq!(model.var_descriptors.len(), 1);
        let var_desc = model.var_descriptors.get(0).unwrap();
        assert_eq!(var_desc.optional, true);
    }

    #[test]
    fn partial_type_prop() {
        let src = "
            interface C {
                c: boolean;
            }
            type B = Partial<C>;
            export interface A {
                b: B
            }
        ";
        let mut model = parse_and_get_model("partial-type-prop.ts", &src);
        assert_eq!(model.var_descriptors.len(), 1);
        let var_desc_b = model.var_descriptors.get(0).unwrap();
        assert_eq!(var_desc_b.name, "b");
        assert_eq!(var_desc_b.ctor.name, "B");
        assert_eq!(var_desc_b.optional, false);
    }

    #[test]
    fn partial_intf_prop() {
        let src = "
            interface C {
                c: boolean;
            };
            interface B extends Partial<C> {};
            export interface a {
                b: B
            }
        ";
        let mut model = parse_and_get_model("partial-intf-prop.ts", &src);
        assert_eq!(model.var_descriptors.len(), 1);
        let var_desc = model.var_descriptors.get(0).unwrap();
        assert_eq!(var_desc.optional, false);
    }

    #[test]
    fn partial_deep_extends() {
        let src = "
            import { PartialDeep } from \"type-fest\";
            interface B extends {
                a: 1
            }
            export interface A extends PartialDeep<B> {}
        ";
        let mut model = parse_and_get_model("partial-deep-extends.ts", &src);
        assert_eq!(model.var_descriptors.len(), 1);
        let var_desc = model.var_descriptors.get(0).unwrap();
        assert_eq!(var_desc.optional, true);
        assert_eq!(model.partial_deep, true);
    }

    #[test]
    fn partial_deep_type_prop() {
        let src = "
            import { PartialDeep } from \"type-fest\";
            interface C {};
            type B = PartialDeep<C>;
            export interface a {
                b: B
            }
        ";
        let mut model = parse_and_get_model("partial-deep-type-prop.ts", &src);
        assert_eq!(model.var_descriptors.len(), 1);
        let var_desc = model.var_descriptors.get(0).unwrap();
        assert_eq!(var_desc.optional, false);
        assert_eq!(var_desc.name, "b");
        assert_eq!(var_desc.ctor.name, "B");
        assert_eq!(model.partial_deep, false);
    }

    #[test]
    fn partial_deep_intf_prop() {
        let src = "
            import { PartialDeep } from \"type-fest\";
            interface C {
                c: boolean;
            }
            interface B extends PartialDeep<C> {};
            export interface a {
                b: B;
            }
        ";
        let mut model = parse_and_get_model("partial-deep-intf-prop.ts", &src);
        assert_eq!(model.var_descriptors.len(), 1);
        let var_desc = model.var_descriptors.get(0).unwrap();
        assert_eq!(var_desc.optional, false);
        assert_eq!(model.partial_deep, false);
    }

    #[test]
    fn imported_partial_deep_type_prop() {
        let src_b = "
            import { PartialDeep } from \"type-fest\";
            interface C {};
            export type B = PartialDeep<C>;
        ";
        let src = "
            import { B } from \"./partial-deep-type-prop-b.js\";
            export interface a {
                b: B
            }
        ";
        let mut model = parse_and_get_model_with_imports(
            "partial-deep-type-prop-a.ts",
            &src,
            &[&parse_from_string("partial-deep-type-prop-b.ts", &src_b)],
        );
        assert_eq!(model.var_descriptors.len(), 1);
        let var_desc = model.var_descriptors.get(0).unwrap();
        assert_eq!(var_desc.optional, false);
        assert_eq!(var_desc.name, "b");
        assert_eq!(var_desc.ctor.name, "B");
        assert_eq!(model.partial_deep, false);
        assert_eq!(model.imports.len(), 1);
        let import = model.imports.get(0).unwrap();
        assert_eq!(import.name, "B");
    }

    #[test]
    fn imported_partial_deep_intf_prop() {
        let src_b = "
            import { PartialDeep } from \"type-fest\";
            interface C {};
            export interface B extends PartialDeep<C> {};
        ";
        let src = "
            import { B } from \"./partial-deep-intf-prop-b.js\";
            export interface a {
                b: B
            }
        ";
        let mut model = parse_and_get_model_with_imports(
            "partial-deep-intf-prop-a.ts",
            &src,
            &[&parse_from_string("partial-deep-intf-prop-b.ts", &src_b)],
        );
        assert_eq!(model.var_descriptors.len(), 1);
        let var_desc = model.var_descriptors.get(0).unwrap();
        assert_eq!(var_desc.optional, false);
        assert_eq!(model.partial_deep, false);
        assert_eq!(model.imports.len(), 1);
        let import = model.imports.get(0).unwrap();
        assert_eq!(import.name, "B");
    }

    #[test]
    fn imported2_partial_deep_typeref_prop() {
        let src_d = "
            interface E {};
            export type D = E;
        ";
        let src_b = "
            import { D } from \"./src-d.js\";
            interface C {
                C: string;
                d: D;
            }
            export interface B extends C {
                d: D;
            };
        ";
        let src = "
            import { PartialDeep } from \"type-fest\";
            import { B } from \"./src-b.js\";
            export type A = PartialDeep<B>;
        ";
        let mut model = parse_and_get_model_with_imports(
            "partial-deep-intf-prop-a.ts",
            &src,
            &[
                &parse_from_string("src-b.ts", &src_b),
                &parse_from_string("src-d.ts", &src_d),
            ],
        );
        assert_eq!(model.var_descriptors.len(), 2);
        let var_desc = model.var_descriptors.get(0).unwrap();
        assert_eq!(var_desc.optional, false);
        assert_eq!(var_desc.name, "c");
        let var_desc = model.var_descriptors.get(1).unwrap();
        assert_eq!(var_desc.optional, false);
        assert_eq!(var_desc.name, "d");
        assert_eq!(model.partial_deep, true);
        assert_eq!(model.imports.len(), 1);
        let import = model.imports.get(0).unwrap();
        assert_eq!(import.name, "E");
    }

    #[test]
    fn exported_type_ref_decl() {
        let src = "
            import { PartialDeep } from \"type-fest\";
            interface C {
                c: boolean;
            }
            interface B {
                b: C;
            };
            export type A = PartialDeep<B>;
        ";
        let mut model = parse_and_get_model("exported-type-ref-decl.ts", &src);
        assert_eq!(model.var_descriptors.len(), 1);
        let var_desc = model.var_descriptors.get(0).unwrap();
        assert_eq!(var_desc.optional, false);
        assert_eq!(var_desc.name, "b");
        assert_eq!(var_desc.ctor.name, "C");
        assert_eq!(model.partial_deep, true);
    }

    #[test]
    fn exported_partial_record_decl() {
        let src_d = "
            export interface D {
                d: boolean;
            }
        ";
        let src = "
            import { D } from \"src-d.ts\";
            import { PartialDeep } from \"type-fest\";

            export interface C extends PartialDeep<D> {};
            export type B = Record<string, C>;
            export interface A {
                b: B
            };
        ";
        let mut models = parse_and_get_models_with_imports(
            "exported-partial-record-decl.ts",
            &src,
            &[&parse_from_string("src-d.ts", src_d)],
        );

        let mut model_d = parse_and_get_model("src-d1.ts", src_d);
        let mut model = models.get(0).unwrap();
        assert_eq!(model.var_descriptors.len(), 1);
        let var_desc = model.var_descriptors.get(0).unwrap();
        // it should export C and A
        assert_eq!(model.class_name, "C");
        assert_eq!(var_desc.optional, true);
        assert_eq!(var_desc.name, "d");
        assert_eq!(var_desc.ctor.name, "bool");
        assert_eq!(model.partial_deep, true);

        let mut model2 = models.get(1).unwrap();
        assert_eq!(model2.class_name, "B");

        let mut model2 = models.get(2).unwrap();
        assert_eq!(model2.class_name, "A");

        let mut test_context = parse_from_string("exported-partial-record-decl.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let mut exports = get_mc_exports(&mut context, &parsed_source);
        if let (ResolutionDecl::TsInterfaceDecl(intf), export) = exports.remove(0) {
            assert_eq!(intf.id.sym, String::from("C"));
        } else {
            panic!("Expected an intf export");
        }
        if let (ResolutionDecl::TsTypeAliasDecl(type_alias), export) = exports.remove(0) {
            assert_eq!(type_alias.id.sym, String::from("B"));
        } else {
            panic!("Expected an intf export");
        }
        if let (ResolutionDecl::TsInterfaceDecl(intf), export) = exports.remove(0) {
            assert_eq!(intf.id.sym, String::from("A"));
        } else {
            panic!("Expected an intf export");
        }
    }

    #[test]
    fn exported_type_ref_alias() {
        let src = "
            interface C {
                c: boolean;
            }
            export type A = string;
            export type B = C;
        ";
        let test_context = parse_from_string("exported-type-ref-alias.ts", &src);
        let (mut global, mut context, parsed_source) = module_context(&test_context);
        let models = extract_module_models(&mut global, &mut context, parsed_source.module());
        assert_eq!(models.len(), 1);
        let b = models.get(0).unwrap();
        assert_eq!(b.var_descriptors.len(), 1);
        let var_desc = b.var_descriptors.get(0).unwrap();
        assert_eq!(var_desc.ctor.name, "bool");
        assert_eq!(var_desc.name, "c");
    }

    #[test]
    fn gd_impl_directive() {
        let src = "
            /* @typescript-to-gdscript-gd-impl */
            type AnyKind = AKind | BKind;
            export interface A {
                a: AnyKind;
            }
        ";
        let models = parse_and_get_models("gd-impl-directive.ts", &src);
        assert_eq!(models.len(), 1);
        let model = models.get(0).unwrap();
        assert_eq!(model.imports.len(), 1);
        let import = model.imports.get(0).unwrap();
        assert_eq!(import.src, "any_kind.gd");
        assert_eq!(import.gd_impl, true);
    }

    #[test]
    fn gd_impl_directive_imported() {
        let any_kind_src = "
            /* @typescript-to-gdscript-gd-impl */
            export type AnyKind = AKind | BKind;
        ";
        let src = "
            import { AnyKind } from \"./any-kind.js\";
            export interface A {
                a: AnyKind;
            }
        ";
        let models = parse_and_get_models_with_imports(
            "gd-impl-directive-imported.ts",
            &src,
            &[&parse_from_string("any-kind.ts", any_kind_src)],
        );
        assert_eq!(models.len(), 1);
        let model = models.get(0).unwrap();
        assert_eq!(model.imports.len(), 1);
        let import = model.imports.get(0).unwrap();
        assert_eq!(import.src, "any_kind.gd");
        assert_eq!(import.gd_impl, true);
    }

    #[test]
    fn gd_impl_directive_imported_interface() {
        let any_kind_src = "
            /* @typescript-to-gdscript-gd-impl */
            export interface SomeInterface {
                interfaceValue: AKind | BKind;
            }
        ";
        let src = "
            import { SomeInterface } from \"./impl-interface.js\";
            export interface A {
                a: SomeInterface;
            }
        ";
        let models = parse_and_get_models_with_imports(
            "gd-impl-directive-imported-interface.ts",
            &src,
            &[&parse_from_string("impl-interface.ts", any_kind_src)],
        );
        assert_eq!(models.len(), 1);
        let model = models.get(0).unwrap();
        assert_eq!(model.imports.len(), 1);
        let import = model.imports.get(0).unwrap();
        assert_eq!(import.src, "some_interface.gd");
        assert_eq!(import.gd_impl, true);
    }

    #[test]
    fn skip_directive() {
        let src = "
            // @typescript-to-gdscript-skip
            export type AnyKind = AKind | BKind;
        ";
        let models = parse_and_get_models("gd-impl-directive.ts", &src);
        assert_eq!(models.len(), 0);
    }

    #[test]
    fn type_directive_iso_date() {
        let src = "
            export interface Intf {
                // @typescript-to-gdscript-type: Iso8601Date
                time: string;
            }
        ";
        let models = parse_and_get_models("type-directive-iso-date.ts", &src);
        assert_eq!(models.len(), 1);
        let model = models.get(0).unwrap();
        assert_eq!(model.imports.len(), 1);
        let import = model.imports.get(0).unwrap();
        assert_eq!(import.name, "Iso8601Date");
        assert_eq!(model.vars.len(), 1);
        let var_decl = model.vars.get(0).unwrap();
        var_decl.init == "Iso8601Date";
    }

    #[test]
    fn type_directive_imported() {
        let team_id = "
            // @typescript-to-gdscript-type: int
            export type TeamId = number;
        ";
        let src = "
            import { TeamId } from \"./team-id.ts\";
            export interface A { interface_value: TeamId; }
        ";
        let models = parse_and_get_models_with_imports(
            "type-directive-imported.ts",
            &src,
            &[&parse_from_string("team-id.ts", team_id)],
        );
        let interfaceValue = &models[0].var_descriptors[0];
        assert_eq!(interfaceValue.name, "interface_value");
        assert_eq!(interfaceValue.decl_type, Some(String::from("int")));
    }

    #[test]
    fn type_directive_local() {
        let src = "
            // @typescript-to-gdscript-type: int
            export type TeamId = number;
            export interface A { interface_value: TeamId; }
        ";
        let models = parse_and_get_models("type-directive-across-modules.ts", &src);
        let interfaceValue = &models[0].var_descriptors[0];
        assert_eq!(interfaceValue.name, "interface_value");
        assert_eq!(interfaceValue.decl_type, Some(String::from("int")));
    }
    #[test]
    fn type_directive_on_property() {
        let src = "
            export type TeamId = number;
            export interface A { 
                // @typescript-to-gdscript-type: unknown
                interface_value: TeamId; 
            }
        ";
        let models = parse_and_get_models("type-directive-on-property.ts", &src);
        let interfaceValue = &models[0].var_descriptors[0];
        assert_eq!(interfaceValue.name, "interface_value");
        assert_eq!(interfaceValue.decl_type, Some(String::from("unknown")));
    }
    #[test]
    #[ignore]
    fn type_directive_override_property() {
        let src = "
            // @typescript-to-gdscript-type: int
            export type TeamId = number;
            export interface A { 
                // @typescript-to-gdscript-type: unknown
                interface_value: TeamId; 
            }
        ";
        let models = parse_and_get_models("type-directive-override-property.ts", &src);
        let interfaceValue = &models[0].var_descriptors[0];
        assert_eq!(interfaceValue.name, "interface_value");
        assert_eq!(interfaceValue.decl_type, Some(String::from("unknown")));
    }

    #[test]
    fn type_directive_on_property_imported() {
        let team_id = "
            export type TeamId = number;
        ";
        let src = "
            import { TeamId } from \"./team-id.ts\";
            export interface A {
                // @typescript-to-gdscript-type: unknown
                interface_value: TeamId;
            }
        ";
        let models = parse_and_get_models_with_imports(
            "type-directive-on-property-imported.ts",
            &src,
            &[&parse_from_string("team-id.ts", team_id)],
        );
        let interfaceValue = &models[0].var_descriptors[0];
        assert_eq!(interfaceValue.name, "interface_value");
        assert_eq!(interfaceValue.decl_type, Some(String::from("unknown")));
    }

    #[test]
    fn type_directive_override_property_imported() {
        let team_id = "
            // @typescript-to-gdscript-type: int
            export type TeamId = number;
        ";
        let src = "
            import { TeamId } from \"./team-id.ts\";
            export interface A {
                // @typescript-to-gdscript-type: unknown
                interface_value: TeamId;
            }
        ";
        let models = parse_and_get_models_with_imports(
            "type-directive-override-property-imported.ts",
            &src,
            &[&parse_from_string("team-id.ts", team_id)],
        );
        let interfaceValue = &models[0].var_descriptors[0];
        assert_eq!(interfaceValue.name, "interface_value");
        assert_eq!(interfaceValue.decl_type, Some(String::from("unknown")));
    }

    #[test]
    #[ignore]
    fn type_directive_on_builtin_property() {
        let src = "
            export interface A { 
                // @typescript-to-gdscript-type: unknown
                interface_value: Array<number>; 
            }
        ";
        let models = parse_and_get_models("type-directive-on-builtin-property.ts", &src);
        let interfaceValue = &models[0].var_descriptors[0];
        assert_eq!(interfaceValue.name, "interface_value");
        assert_eq!(interfaceValue.decl_type, Some(String::from("unknown")));
    }
}
