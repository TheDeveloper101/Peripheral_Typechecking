use std::collections::HashMap;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::spanned::Spanned;
use syn::visit_mut::{visit_item_impl_mut, VisitMut};
use syn::{parse_macro_input, Attribute, Expr, ExprCall, ExprMethodCall, Ident, ImplItem, ItemImpl, LocalInit, Meta, Pat, PatPath, Type};

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Copy, Clone)]
enum USBType {
    Unused(EndpointType),
    Uninit(EndpointType),
    Idle(EndpointType),
    Ready(EndpointType),
    Run(EndpointType),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Copy, Clone)]
enum EndpointType {
    Disabled,
    Idle,
    Recieving,
    Transmitting,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash)]
enum Entry {
    Variable(Ident),
    Function(Ident)
}
struct Context {
    gamma: HashMap<Entry, (USBType, USBType)>,
    error: Option<String>,
    var_types: Vec<Ident>
}

#[proc_macro_attribute]
pub fn typecheck(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let mut c: Context = Context {
                    gamma: HashMap::new(),
                    error: None,
                    var_types: Vec::new()
                };
    let mut ast = parse_macro_input!(input as syn::File);
    let mut ret: proc_macro2::TokenStream = quote! {#ast};
    c.visit_file_mut(&mut ast);
    match c.error {
        None => {}
        Some(message) => {
            ret.extend(
                syn::Error::new(
                    ast.span(), 
                    format!("{message}")
                )
                .to_compile_error()
            )
        }
    }
    ret.into()
}

impl VisitMut for Context {
    fn visit_expr_method_call_mut(&mut self, i: &mut ExprMethodCall) {
        let method_type = self.gamma.get(&Entry::Function(i.method.clone())).unwrap();
        if let syn::Expr::Path(ref path) = *i.receiver {
            if let Some(ident) = path.path.get_ident() {
                let mut peripheral = &self.gamma.get(&Entry::Variable(ident.clone())).unwrap().0;
                if method_type.0 == *peripheral {
                    let end = self.gamma.get(&Entry::Function(i.method.clone()));
                    match end {
                        Some(t) => {
                            *& mut peripheral = & mut USBType::join(peripheral.clone(), t.1.clone());
                            self.visit_expr_method_call_mut(i)
                        }
                        None => {
                            self.error = Some("Improper API Usage!".to_string());
                            return;
                        }
                    }
                }
            }
        }
    }

    fn visit_item_impl_mut(&mut self, i: &mut syn::ItemImpl) {

        //get type name of struct so that we can find the variable later
        if let Type::Path(syn::TypePath { path, .. }) = &*i.self_ty {
            if let Some(ident) = path.get_ident() {
                self.var_types.push(ident.clone());
            }
        }

        for item in &i.items {
            if let ImplItem::Fn(func) = item {
                let name = &func.sig.ident;
                for attr in &func.attrs {
                    if let Some((func_name, types)) = extract_method_types(attr) {
                        if *name == func_name {
                            let (type1_str, type2_str) = parse_types(&types);
                            let type1 = str_to_type(&type1_str);
                            let type2 = str_to_type(&type2_str);
                            self.gamma.insert(Entry::Function(name.clone()), (type1, type2));
                        }
                    }
                }
            } 
        }
        self.visit_item_impl_mut(i);
    }

    fn visit_local_mut(&mut self, i: &mut syn::Local) {
        let pat = &i.pat;
        if let Pat::Ident(name) = pat {
            let var_name = &name.ident;
            let li = &i.init.clone();
            if let Expr::Call(call) = *li.clone().unwrap().expr {
                if let Expr::Path(path) = *call.func {
                    let segments: Vec<_> = path.path.segments.iter().collect();
                    if segments.len() == 2 {
                        let struct_name = &segments[0].ident;
                        let method_name = &segments[1].ident;
                        if method_name == "new" && self.var_types.contains(struct_name) {
                            // struct::new(), and struct is in our list
                            self.gamma.insert(
                                Entry::Variable(var_name.clone()),
                                (USBType::Uninit(EndpointType::Disabled), USBType::Unused(EndpointType::Disabled)),
                            );
                        }
                    }
                }
            }
        }
    }
}

impl USBType {
    fn join(t1: USBType, t2: USBType) -> USBType {
        if t1 < t2 {
            t2
        } else {
            t1
        }
    }
}


fn extract_method_types(attr: &Attribute) -> Option<(Ident, String)> {
    if let Meta::NameValue(nv) = &attr.meta {
        if let Expr::Lit(lit_str) = &nv.value {
            return Some((nv.path.get_ident()?.clone(), lit_str.lit.to_token_stream().to_string()));
        }
    }
    None
}

fn parse_types(types_str: &str) -> (String, String) {
    let types_str = types_str.trim_matches(|c| c == '(' || c == ')');
    let parts: Vec<&str> = types_str.split(',').collect();
    if parts.len() == 2 {
        let type1 = parts[0].trim().to_string();
        let type2 = parts[1].trim().to_string();
        (type1, type2)
    } else {
        ("".to_string(), "".to_string())
    }
}

fn str_to_type(type_str: &str) -> USBType {
    match type_str {
        "uninit" => USBType::Uninit(EndpointType::Disabled),
        "idle" => USBType::Idle(EndpointType::Disabled),
        "ready" => USBType::Ready(EndpointType::Idle),
        "run" => USBType::Run(EndpointType::Idle),
        "run::send" => USBType::Run(EndpointType::Transmitting),
        "run::recieve" => USBType::Run(EndpointType::Recieving),
        _ => USBType::Unused(EndpointType::Disabled),
    }
}