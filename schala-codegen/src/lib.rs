#![feature(proc_macro)]
extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use syn::DeriveInput;

#[proc_macro]
pub fn print_a_thing(_input: TokenStream) -> TokenStream {
  "println!(\"Invoked from a proc macro\");".parse().unwrap()
}


#[proc_macro_derive(ProgrammingLanguageInterface, attributes(LanguageName, FileExtension, PipelineSteps))]
pub fn derive_programming_language_interface(input: TokenStream) -> TokenStream {
  let ast: DeriveInput = syn::parse(input).unwrap();
  let name = &ast.ident;
  let attrs = &ast.attrs;


  let language_name: Option<String> = attrs.iter().map(|attr| attr.interpret_meta()).find(|meta| {
    match meta {
      &Some(syn::Meta::NameValue(syn::MetaNameValue { ident, .. })) if ident.as_ref() == "LanguageName" => true,
      _ => false
    }
  }).and_then(|meta| {
    match meta {
      Some(syn::Meta::NameValue(syn::MetaNameValue { lit: syn::Lit::Str(litstr), .. })) => Some(litstr.value()),
      _ => None,
    }
  });

  println!("LANG NAME: {:?}", language_name);

  /*
  println!("ATTRS {:?}", attrs);
  let meta: Option<syn::Meta> = attrs.get(0).unwrap().interpret_meta();
  println!("META: {:?}", meta);
  match meta {
    Some(syn::Meta::NameValue(syn::MetaNameValue { lit, .. })) => {
      println!("GOT LIT: {:?}", lit);
      match lit {
        syn::Lit::Str(litstr) => println!("VAL: {}", litstr.value()),
        _ => panic!("OI")
      }
    }
    _ => panic!("YO")
  }
  */


  let tokens = quote! {
    impl ProgrammingLanguageInterface for #name {
      fn get_language_name(&self) -> String {
        "Schala".to_string()
      }

      fn get_source_file_suffix(&self) -> String {
        format!("schala")
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
