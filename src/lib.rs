use std::any::Any;
use std::collections::{hash_map, HashMap};
use std::iter::Map;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::spanned::Spanned;
use syn::visit_mut::{visit_expr_method_call_mut, VisitMut};
use syn::{parse_macro_input, Error, ExprMethodCall, Ident};

enum USBType {
    Uninit,
    Idle,
    Ready,
    Run
}
struct Gamma {
    context: HashMap<syn::Ident, (USBType, USBType)>,
    state: USBType,
    transitions: HashMap<USBType, USBType>,
    error: Some(String)
}

#[proc_macro_attribute]
fn typecheck(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let g: Gamma = Gamma {context: HashMap::new(),
                    state: USBType::Uninit,
                    transitions: HashMap::from([
                        (USBType::Uninit, USBType::Idle),
                        (USBType::Idle, USBType::Ready),
                        (USBType::Ready, USBType::Run),
                        (USBType::Run, USBType::Idle),
                        (USBType::Run, USBType::Uninit),
                        (USBType::Idle, USBType::Idle),
                        (USBType::Ready, USBType::Ready),
                        (USBType::Run, USBType::Run)
                        ]),
                    error: None
                };
    let mut ast = parse_macro_input!(input as syn::Expr);
    let mut ret: proc_macro2::TokenStream = quote! {#ast};
    g.visit_expr_mut(ast);
    match g.error {
        None => {}
        Some(message) => {
            ret.extend(
                syn::Error::new(
                    ast.span(), 
                    format!("Improper API usage!", 
                    )
                )
                .to_compile_error()
            )
        }
    }
    ret.into()
}

impl VisitMut for Gamma {
    fn visit_expr_method_call_mut(&mut self, i: &mut ExprMethodCall) {
        let method_type = *self.context.get(&i.method).unwrap();
        if(method_type.0 == state) {
            let end = self.transitions.get(&i.method);
            match end {
                Some(t) => {
                    self.state = method_type.1;
                    visit_expr_method_call_mut(self, i)
                }
                None => {
                    self.error = Some("Improper API Usage!".to_string());
                    return;
                }
            }
        } else {
            self.error = Some("Improper API Usage!".to_string());
            return;
        }
    }
}