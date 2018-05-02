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

  println!("ATTRS {:?}", attrs);
  //let language_name = attrs.iter().find(


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
