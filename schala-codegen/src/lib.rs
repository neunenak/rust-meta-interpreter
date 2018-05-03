#![feature(proc_macro)]
extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use syn::{Attribute, DeriveInput};

fn extract_attribute_arg_by_name(name: &str, attrs: &Vec<Attribute>) -> Option<String> {
  use syn::{Meta, Lit, MetaNameValue};
  attrs.iter().map(|attr| attr.interpret_meta()).find(|meta| {
    match meta {
      &Some(Meta::NameValue(MetaNameValue { ident, .. })) if ident.as_ref() == name => true,
      _ => false
    }
  }).and_then(|meta| {
    match meta {
      Some(Meta::NameValue(MetaNameValue { lit: Lit::Str(litstr), .. })) => Some(litstr.value()),
      _ => None,
    }
  })
}


#[proc_macro_derive(ProgrammingLanguageInterface, attributes(LanguageName, SourceFileExtension, PipelineSteps))]
pub fn derive_programming_language_interface(input: TokenStream) -> TokenStream {
  let ast: DeriveInput = syn::parse(input).unwrap();
  let name = &ast.ident;
  let attrs = &ast.attrs;

  let language_name: String = extract_attribute_arg_by_name("LanguageName", attrs).expect("LanguageName is required");
  let file_ext = extract_attribute_arg_by_name("SourceFileExtension", attrs).expect("SourceFileExtension is required");

  let tokens = quote! {
    impl ProgrammingLanguageInterface for #name {
      fn get_language_name(&self) -> String {
        #language_name.to_string()
      }

      fn get_source_file_suffix(&self) -> String {
        #file_ext.to_string()
      }
      fn execute_pipeline(&mut self, input: &str, options: &EvalOptions) -> FinishedComputation {
        let mut chain = pass_chain![self, options;
        tokenizing_stage,
        parsing_stage,
        symbol_table_stage,
        typechecking_stage,
        eval_stage
        ];
        chain(input)
      }

      fn get_stages(&self) -> Vec<String> {
        vec![
          format!("tokenizing_stage"),
          format!("parsing_stage"), //TODO handle both types of this
          format!("symbol_table_stage"),
          format!("typechecking_stage"),
          format!("eval_stage")
        ]
      }
    }
  };

  tokens.into()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
