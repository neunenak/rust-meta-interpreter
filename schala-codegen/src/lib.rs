#![feature(proc_macro)]
extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;
use proc_macro::TokenStream;

#[proc_macro]
pub fn print_a_thing(_input: TokenStream) -> TokenStream {
  "println!(\"Invoked from a proc macro\");".parse().unwrap()
}


#[proc_macro_derive(ProgrammingLanguageInterface)]
pub fn derive_programming_language_interface(input: TokenStream) -> TokenStream {
  input
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
