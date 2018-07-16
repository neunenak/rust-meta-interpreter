#![feature(trace_macros)]
#![feature(proc_macro)]
extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;

extern crate schala_repl;

use proc_macro::TokenStream;
use syn::{Ident, Attribute, DeriveInput};

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

fn extract_attribute_list(name: &str, attrs: &Vec<Attribute>) -> Option<Vec<(Ident, Option<Vec<Ident>>)>> {
  use syn::{Meta, MetaList, NestedMeta};
  attrs.iter().find(|attr| {
    match attr.path.segments.iter().nth(0) {
      Some(segment) if segment.ident.as_ref() == name => true,
      _ => false
    }
  }).and_then(|attr| {
    match attr.interpret_meta() {
      Some(Meta::List(MetaList { nested, .. })) => {
        Some(nested.iter().map(|nested_meta| match nested_meta {
          &NestedMeta::Meta(Meta::Word(ident)) => (ident, None),
          &NestedMeta::Meta(Meta::List(MetaList { ident, nested: ref nested2, .. })) => {
            let own_args = nested2.iter().map(|nested_meta2| match nested_meta2 {
              &NestedMeta::Meta(Meta::Word(ident)) => ident,
              _ => panic!("Bad format for doubly-nested attribute list")
            }).collect();
            (ident, Some(own_args))
          },
          _ => panic!("Bad format for nested list")
        }).collect())
      },
      _ => panic!("{} must be a comma-delimited list surrounded by parens", name)
    }
  })
}

#[proc_macro_derive(ProgrammingLanguageInterface, attributes(LanguageName, SourceFileExtension, PipelineSteps))]
pub fn derive_programming_language_interface(input: TokenStream) -> TokenStream {
  use schala_repl::PassDescriptor;
  let ast: DeriveInput = syn::parse(input).unwrap();
  let name = &ast.ident;
  let attrs = &ast.attrs;

  let language_name: String = extract_attribute_arg_by_name("LanguageName", attrs).expect("LanguageName is required");
  let file_ext = extract_attribute_arg_by_name("SourceFileExtension", attrs).expect("SourceFileExtension is required");
  let passes = extract_attribute_list("PipelineSteps", attrs).expect("PipelineSteps are required");
  let pass_idents = passes.iter().map(|x| x.0);

  //let pass_names: Vec<String> = passes.iter().map(|pass| pass.0.to_string()).collect();
  let pass_descriptors = passes.iter().map(|pass| {
    let name: String = pass.0.to_string();
    let opts: Vec<String> = match &pass.1 {
      None => vec![],
      Some(opts) => opts.iter().map(|o| o.to_string()).collect(),
    };

    quote! {
      PassDescriptor {
        name: #name,
        debug_options: vec![#(format!(#opts)),*]
      }
    }
  });

  let tokens = quote! {
    use schala_repl::PassDescriptor;
    impl ProgrammingLanguageInterface for #name {
      fn get_language_name(&self) -> String {
        #language_name.to_string()
      }
      fn get_source_file_suffix(&self) -> String {
        #file_ext.to_string()
      }
      fn execute_pipeline(&mut self, input: &str, options: &EvalOptions) -> FinishedComputation {
        let mut chain = pass_chain![self, options; #(#pass_idents),* ];
        chain(input)
      }
      fn get_passes(&self) -> Vec<PassDescriptor> {
        vec![ #(#pass_descriptors),* ]
        //vec![ #(PassDescriptor { name: #pass_names.to_string(), debug_options: vec![] }),* ]
      }
    }
  };
  tokens.into()
}
