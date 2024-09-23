use std::collections::HashMap;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::spanned::Spanned;
use syn::visit_mut::{visit_expr_method_call_mut, VisitMut};
use syn::{parse_macro_input, ExprMethodCall, Ident};

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Copy, Clone)]
enum USBType {
    Uninit(EndpointType),
    Idle(EndpointType),
    Ready(EndpointType),
    Run(EndpointType),
    Unused(EndpointType)
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Copy, Clone)]
enum EndpointType {
    Blah
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash)]
enum Entry {
    Variable(Ident),
    Function(Ident)
}
struct Context {
    gamma: HashMap<Entry, (USBType, USBType)>,
    error: Option<String>
}

#[proc_macro_attribute]
pub fn typecheck(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let mut c: Context = Context {
                    gamma: HashMap::new(),
                    error: None
                };
    let mut ast = parse_macro_input!(input as syn::Expr);
    let mut ret: proc_macro2::TokenStream = quote! {#ast};
    c.visit_expr_mut(&mut ast);
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
                            visit_expr_method_call_mut(self, i)
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